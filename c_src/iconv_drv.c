#include <stdio.h>
#include <string.h>
#include <erl_driver.h>
#include <ei.h>
#include <iconv.h>

/*
 * R15B changed several driver callbacks to use ErlDrvSizeT and
 * ErlDrvSSizeT typedefs instead of int.
 * This provides missing typedefs on older OTP versions.
 */
#if ERL_DRV_EXTENDED_MAJOR_VERSION < 2
typedef int ErlDrvSizeT;
typedef int ErlDrvSSizeT;
#endif

typedef struct {
	ErlDrvPort port;
} iconv_data;

typedef struct {
	erlang_ref *ref;
	char *from;
	char *to;
	char *in;
	long in_size;
	char *out;
	long out_size;
	ErlDrvTermData receiver;
} convert_task;

// EI function
int ei_string_or_binary(const char *buf, int *index, char **string)
{
	int type, size;
	char *temp;
	long bin_size;

	if (ei_get_type(buf, index, &type, &size)) {
		return -1;
	}

	if (type != ERL_STRING_EXT && type != ERL_BINARY_EXT) {
		return -1;
	}

	temp = driver_alloc(sizeof(char) * (size + 1));
	if (temp == NULL) {
		return -2;
	}

	if (type == ERL_STRING_EXT) {
		if (ei_decode_string(buf, index, temp)) {
			driver_free(temp);
			return -1;
		}
	} else {
		if (ei_decode_binary(buf, index, temp, &bin_size)) {
			driver_free(temp);
			return -1;
		}
		temp[bin_size] = 0;
	}

	*string = temp;
	return 0;
}

int ei_binary(const char *buf, int *index, char **binary, long *binary_size)
{
	int type, size;
	char *temp;

	if (ei_get_type(buf, index, &type, &size) || type != ERL_BINARY_EXT) {
		return -1;
	}

	temp = driver_alloc(sizeof(char) * size);
	if (temp == NULL) {
		return -2;
	}

	if (ei_decode_binary(buf, index, temp, binary_size)) {
		driver_free(temp);
		return -1;
	}

	*binary = temp;
	return 0;
}

// Help function
static ErlDrvSSizeT answer(const int error, char **rbuf, int len)
{
	ei_x_buff result;
	int size;

	// TODO write to rbuf?
	if (error == -2) {
		if (ei_x_new_with_version(&result)
			|| ei_x_encode_tuple_header(&result, 2)
			|| ei_x_encode_atom(&result, "error")
			|| ei_x_encode_atom(&result, "enomem")) {
			return -1;
		}

		*rbuf = (char *) driver_alloc(sizeof(char) * result.index);
		memcpy(*rbuf, result.buff, result.index);
		size = result.index;
		ei_x_free(&result);

		return size;
	} else if (error < 0) {
		return (ErlDrvSSizeT) ERL_DRV_ERROR_GENERAL;
	}

	if (ei_x_new_with_version(&result)
		|| ei_x_encode_atom(&result, "ok")) {
		return -1;
	}

	*rbuf = (char *) driver_alloc(sizeof(char) * result.index);
	memcpy(*rbuf, result.buff, result.index);
	size = result.index;
	ei_x_free(&result);

	return size;
}

// Erlang port
static ErlDrvData iconv_drv_start(ErlDrvPort port, char *buff)
{
	iconv_data *data = (iconv_data *) driver_alloc(sizeof(iconv_data));
	data->port = port;

	return (ErlDrvData) data;
}

static void iconv_drv_stop(ErlDrvData handle)
{
	driver_free((iconv_data *) handle);
}

// Do convert
static void do_convert(void *data)
{
	// TODO write ext binary tem?
	convert_task *task = (convert_task *) data;
	iconv_t cd;
	size_t inleft, outleft;
	char *in_start, *out_end;
	int done = 0;
	size_t ret;

	cd = iconv_open(task->to, task->from);
	if (cd == (iconv_t) -1) {
		task->out_size = -1;
		return;
	}

	in_start = task->in;
	inleft = task->in_size;

	outleft = 2 * task->in_size;
	task->out = out_end = driver_alloc(sizeof(char) * outleft);
	if (task->out == NULL) {
		task->out_size = -4;
		iconv_close(cd);
		return;
	}

	while (done != 1) {
		if (inleft == 0) {
			// We need write some end for specific charset (UTF7)
			ret = iconv(cd, NULL, &inleft, &out_end, &outleft);
			if (ret != -1) {
				// We successfully convert, end the loop
				done = 1;
			}
		} else {
			// Convert next part of string
			ret = iconv(cd, &in_start, &inleft, &out_end, &outleft);
		}
		if (ret == (size_t) -1) {
			if (errno == EILSEQ) {
				task->out_size = -2;
				iconv_close(cd);
				return;
			} else if (errno == EINVAL) {
				task->out_size = -3;
				iconv_close(cd);
				return;
			} else if (errno == E2BIG) {
				size_t already_write = out_end - task->out;
				outleft += 2 * (inleft + 1);
				task->out = driver_realloc(task->out, already_write + outleft);
				if (task->out == NULL) {
					task->out_size = -4;
					iconv_close(cd);
					return;
				}
				out_end = task->out + already_write;
			}
		}
	}
	iconv_close(cd);

	task->out_size = out_end - task->out;
	return;
}

// Free resource
static void do_clean(void *data)
{
	convert_task *task = (convert_task *) data;
	driver_free(task->ref);
	driver_free(task->from);
	driver_free(task->to);
	driver_free(task->in);
	if(task->out != NULL) {
		// Can be NULL if driver_realloc return NULL
		driver_free(task->out);
	}
	driver_free(task);
}

// Start convert task
static ErlDrvSSizeT iconv_drv_call(
	// Driver data
	ErlDrvData port_data,
	// Command form port_call/3
	unsigned int command,
	// Erlang term in external format from port_call/3
	char *buf, ErlDrvSizeT len,
	// Returned term in external format
	char **rbuf, ErlDrvSizeT rlen,
	// I don`t know what here
	unsigned int *flags)
{
	iconv_data *data = (iconv_data *) port_data;
	convert_task *task;
	int index = 0, type, ret;

	if (command != 1
		|| ei_decode_version(buf, &index, &type)
		// {Ref, From, To, Binary}
		|| ei_decode_tuple_header(buf, &index, &ret)
		|| ret != 4) {
		return answer(-1, rbuf, rlen);
	}

	task = driver_alloc(sizeof(convert_task));

	// Ref
	task->ref = (erlang_ref *) driver_alloc(sizeof(erlang_ref));
	if (task->ref == NULL) {
		return answer(-2, rbuf, rlen);
	}
	if (ei_decode_ref(buf, &index, task->ref)) {
		driver_free(task);
		return answer(-1, rbuf, rlen);
	}

	// From
	if ((ret = ei_string_or_binary(buf, &index, &task->from))) {
		driver_free(task->ref);
		driver_free(task);
		return answer(ret, rbuf, rlen);
	}

	// To
	if ((ret = ei_string_or_binary(buf, &index, &task->to))) {
		driver_free(task->ref);
		driver_free(task->from);
		driver_free(task);
		return answer(ret, rbuf, rlen);
	}

	// Binary
	if ((ret = ei_binary(buf, &index, &task->in, &task->in_size))) {
		driver_free(task->ref);
		driver_free(task->from);
		driver_free(task->to);
		driver_free(task);
		return answer(ret, rbuf, rlen);
	}

	task->receiver = driver_caller(data->port);
	driver_async(data->port, NULL, do_convert, (void *) task, do_clean);

	return answer(1, rbuf, rlen);
}

// Send answer to caller
static void answer_ready(ErlDrvData port_data, ErlDrvThreadData task_data)
{
	iconv_data *data = (iconv_data *) port_data;
	convert_task * task = (convert_task*) task_data;
	ei_x_buff reply;

	ei_x_new_with_version(&reply);
	ei_x_encode_tuple_header(&reply, 2);
	ei_x_encode_ref(&reply, task->ref);
	ei_x_encode_tuple_header(&reply, 2);

	if (task->out_size == -1) {
		ei_x_encode_atom(&reply, "error");
		ei_x_encode_atom(&reply, "echarset");
	} else if (task->out_size == -2) {
		ei_x_encode_atom(&reply, "error");
		ei_x_encode_atom(&reply, "eilseq");
	} else if (task->out_size == -3) {
		ei_x_encode_atom(&reply, "error");
		ei_x_encode_atom(&reply, "einval");
	} else if (task->out_size == -4) {
		ei_x_encode_atom(&reply, "error");
		ei_x_encode_atom(&reply, "enomeme");
	} else {
		ei_x_encode_atom(&reply, "ok");
		ei_x_encode_binary(&reply, task->out, task->out_size);
	}

	ErlDrvTermData answer_term[] = {
		ERL_DRV_EXT2TERM, (ErlDrvTermData) reply.buff, reply.index
	};
	driver_send_term(data->port, task->receiver, answer_term, 3);

	ei_x_free(&reply);
}

ErlDrvEntry iconv_driver_entry = {
	NULL,							/* F_PTR init, N/A */
	iconv_drv_start,				/* L_PTR start, called when port is opened */
	iconv_drv_stop,					/* F_PTR stop, called when port is closed */
	NULL,							/* F_PTR output, called when erlang has sent */
	NULL,							/* F_PTR ready_input, called when input descriptor ready */
	NULL,							/* F_PTR ready_output, called when output descriptor ready */
	"iconv_drv",					/* char *driver_name, the argument to open_port */
	NULL,							/* F_PTR finish, called when unloaded */
	NULL,							/* handle */
	NULL,							/* F_PTR control, port_command callback */
	NULL,							/* F_PTR timeout, reserved */
	NULL,							/* F_PTR outputv, reserved */
	/* Added in Erlang/OTP R15B: */
	answer_ready,					/* ready_async */
	NULL,							/* flush */
	iconv_drv_call,					/* call */
	NULL,							/* event */
	ERL_DRV_EXTENDED_MARKER,		/* extended_marker */
	ERL_DRV_EXTENDED_MAJOR_VERSION,	/* major_version */
	ERL_DRV_EXTENDED_MINOR_VERSION,	/* minor_version */
	0,								/* driver_flags */
	NULL,							/* handle2 */
	NULL,							/* process_exit */
	NULL							/* stop_select */
};

DRIVER_INIT(iconv_drv) /* must match name in driver_entry */
{
    return &iconv_driver_entry;
}
