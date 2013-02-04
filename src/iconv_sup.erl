%% @author Slava Yurin <YurinVV@ya.ru>
%% @doc Supervisor for iconv port
-module(iconv_sup).

-behavior(supervisor).
-export([start_link/0, init/1]).

%% @doc Start supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

%% @doc Supervisor init
-spec init(list()) -> term().
init(_) ->
    {ok, {
		{one_for_one, 10, 100},
			[ {iconv, {iconv, start_link, []}, permanent, 5000, worker, [iconv]}
			]
		}}.
