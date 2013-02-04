%% @author Slava Yurin <YurinVV@ya.ru>
%% @doc Start application module
-module(iconv_app).

-behavior(application).
-export([start/2, stop/1]).

%% @doc Start iconv application
-spec start(term(), term()) -> {ok, port()}.
start(_, _) ->
	iconv_sup:start_link().

%% @doc Stop iconv application
-spec stop(term()) -> no_return().
stop(_) ->
	ok.
