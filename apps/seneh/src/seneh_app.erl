%%%-------------------------------------------------------------------
%% @doc seneh public API
%% @end
%%%-------------------------------------------------------------------

-module(seneh_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    seneh_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
