-module(seneh_proc_watch).

-export([init/1, patrol/1, terminate/1, check/1]).

-define(FREQ, 1000).

init(EHPid) ->
    patrol([{eventHandler_pid, EHPid}]).

patrol(State) ->
    EHPid = proplists:get_value(eventHandler_pid, State),
    receive
        {EHPid, stop} -> terminate(State)
    after ?FREQ ->
        EHPid ! {self(), alive},
        patrol(State)
    end.

check(What) ->
    os:cmd("ps aux").

terminate(State) ->
    EHPid = proplists:get_value(eventHandler_pid, State),
    EHPid ! {self(), terminate}.
