%% @author Slava Yurin <YurinVV@ya.ru>
%% @doc Iconv module
-module(iconv).

-behaviour(gen_server).
-export(
	[ start_link/0
	, init/1
	, handle_call/3
	, handle_cast/2
	, handle_info/2
	, code_change/3
	, terminate/2
	]).

-export([convert/3]).

%% @doc Convert string
-spec convert(string(), string(), binary()) -> {ok, binary()} | {error, atom()}.
convert(From, To, String) ->
	Ref = make_ref(),
	case erlang:port_call(iconv, 1, {Ref, From, To, String})
	of ok ->
		receive {Ref, {ok, Answer}} ->
			{ok, Answer}
		; {Ref, {error, Reason}} ->
			error(Reason, [From, To, String])
		end
	; {error, Reason} ->
		error(Reason, [From, To, String])
	end.

%% @doc Start iconv driver
-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% @doc Start Port
-spec init(term()) -> no_return().
init(_) ->
	case erl_ddll:load(code:priv_dir(iconv), iconv_drv)
	of ok ->
		ok
	; {error, already_loaded} ->
		ok
	end,
	Port = open_port({spawn_driver, "iconv_drv"}, []),
	register(iconv, Port),
	{ok, Port}.

handle_call(_, _, State) ->
	{noreply, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, Port) ->
	port_close(Port),
	ok.
