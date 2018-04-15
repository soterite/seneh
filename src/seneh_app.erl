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
    net_kernel:start([seneh, longnames]),
    io:format("seneh_app starting...~nNode name: ~p, Cookie: ~p", [node(), erlang:get_cookie()]),
    seneh_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
