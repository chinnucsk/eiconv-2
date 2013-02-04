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
} convert_data;

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
	convert_data *task = (convert_data *) data;
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
static void do_clean_convert(void *data)
{
	convert_data *task = (convert_data *) data;
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

static int answer_enomem(char **rbuf)
{
	ei_x_buff result;
	int size;

	if (ei_x_new_with_version(&result)) return -1;
	if (ei_x_encode_tuple_header(&result, 2)) return -1;
	if (ei_x_encode_atom(&result, "error")) return -1;
	if (ei_x_encode_atom(&result, "enomem")) return -1;

	*rbuf = driver_alloc(sizeof(char) * result.index);
	memcpy(*rbuf, result.buff, result.index);
	size = result.index;
	ei_x_free(&result);

	return size;
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
	int index = 0, type, size;
	long binarySize;
	convert_data *task;
	ei_x_buff result;

	if (ei_decode_version(buf, &index, &type)) return -1;

	// {Ref, From, To, Binary}
	if (ei_decode_tuple_header(buf, &index, &size) || size != 4) return -1;

	task = driver_alloc(sizeof(convert_data));

	// Ref
	task->ref = driver_alloc(sizeof(erlang_ref));
	if (task->ref == NULL) {
		return answer_enomem(rbuf);
	}
	if (ei_decode_ref(buf, &index, task->ref)) return -1;

	// From
	if (ei_get_type(buf, &index, &type, &size)) return -1;
	task->from = driver_alloc(size + 1);
	if (task->from == NULL) {
		return answer_enomem(rbuf);
	}
	if (ei_decode_string(buf, &index, task->from)) return -1;

	// To
	if (ei_get_type(buf, &index, &type, &size)) return -1;
	task->to = driver_alloc(size + 1);
	if (task->to == NULL) {
		return answer_enomem(rbuf);
	}
	ei_decode_string(buf, &index, task->to);

	// Binary
	if (ei_get_type(buf, &index, &type, &size)) return -1;
	task->in = driver_alloc(size);
	if (task->in == NULL) {
		return answer_enomem(rbuf);
	}
	if (ei_decode_binary(buf, &index, task->in, &binarySize)) return -1;
	task->in_size = binarySize;
	task->receiver = driver_caller(data->port);

	driver_async(data->port, NULL, do_convert, (void *) task, do_clean_convert);

	if (ei_x_new_with_version(&result)) return -1;
	if (ei_x_encode_atom(&result, "ok")) return -1;

	*rbuf = driver_alloc(sizeof(char) * result.index);
	memcpy(*rbuf, result.buff, result.index);
	size = result.index;
	ei_x_free(&result);

	return size;
}

// Send answer to caller
static void convert_complete(ErlDrvData port_data, ErlDrvThreadData task_data)
{
	iconv_data *data = (iconv_data *) port_data;
	convert_data * task = (convert_data*) task_data;
	ei_x_buff answer;

	if (ei_x_new_with_version(&answer)) return;
	if (ei_x_encode_tuple_header(&answer, 2)) return;
	if (ei_x_encode_ref(&answer, task->ref)) return;
	if (ei_x_encode_tuple_header(&answer, 2)) return;

	if (task->out_size == -1) {
		if (ei_x_encode_atom(&answer, "error")) return;
		if (ei_x_encode_atom(&answer, "echarset")) return;
	} else if (task->out_size == -2) {
		if (ei_x_encode_atom(&answer, "error")) return;
		if (ei_x_encode_atom(&answer, "eilseq")) return;
	} else if (task->out_size == -3) {
		if (ei_x_encode_atom(&answer, "error")) return;
		if (ei_x_encode_atom(&answer, "einval")) return;
	} else if (task->out_size == -4) {
		if (ei_x_encode_atom(&answer, "error")) return;
		if (ei_x_encode_atom(&answer, "e2big")) return;
	} else {
		if (ei_x_encode_atom(&answer, "ok")) return;
		if (ei_x_encode_binary(&answer, task->out, task->out_size)) return;
	}

	ErlDrvTermData answer_term[] = {
		ERL_DRV_EXT2TERM, (ErlDrvTermData) answer.buff, answer.index
	};
	driver_send_term(data->port, task->receiver, answer_term, 3);

	ei_x_free(&answer);
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
	convert_complete,				/* ready_async */
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
