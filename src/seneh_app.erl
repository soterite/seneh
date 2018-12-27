%%%-------------------------------------------------------------------
%% @doc seneh public API
%% @end
%%%-------------------------------------------------------------------

-module(seneh_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    seneh_log:start_logger(),
    seneh_log:log_normal("seneh_app starting...~nNode name: ~p, Cookie: ~p~n", [node(), erlang:get_cookie()]),
    seneh_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
