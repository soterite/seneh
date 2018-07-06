-module(seneh_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).

-export([starting_stopping/1,
         is_tunnel_alive/1]).

all() -> [starting_stopping
          , is_tunnel_alive].

starting_stopping(_Config) ->
    application:start(seneh),
    ct:pal(default, ?MAX_IMPORTANCE, "~p", [application:which_applications()]),
    application:stop(seneh),
    ct:pal("starting_stopping FINISHED").

is_tunnel_alive(_Config) ->
    ct:pal("is_tunnel_alive starting..."),
    Myself = self(),
    application:start(seneh),
    seneh_watch:call(self(), ssh_tunnel_status),
    receive
        {Myself, Any} -> ct:pal("server_watch returned: ~w", [Any]),
                         Any = {ok, tunnel_alive}
    end,
    application:stop(seneh),
    ct:pal("is_tunnel_alive FINISHED").
