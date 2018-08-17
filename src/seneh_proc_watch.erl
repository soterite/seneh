-module(seneh_proc_watch).
-vsn("1.2").

-export([
         init/1,
         loop/1,
         patrol/1 
        ]).

-define(FREQ, 1000 * 60 * 59).
-define(PS_CMD, "ps -xo cmd").
-define(S2_TUNEL, [
{"ssh -o TCPKeepAlive=yes -N -R 19721:localhost:8080 mroq@s2.mydevil.net &",
 "19721:localhost:8080"},
{"ssh -o TCPKeepAlive=yes -N -R 19722:localhost:6600 mroq@s2.mydevil.net &",
 "19722:localhost:6600"},
{"ssh -o TCPKeepAlive=yes -N -R 19723:localhost:22 mroq@s2.mydevil.net &",
 "19723:localhost:22"}]).

-record(state, { eventHandler_pid, level }).

init(EHPid) ->
    ?MODULE:loop(#state{eventHandler_pid = EHPid, level = running}).

loop(State) ->
    EHPid = State#state.eventHandler_pid,

    receive
        {EHPid, terminate}  -> NewState = State#state{level = shutdown};
        {EHPid, check}      -> ?MODULE:patrol(EHPid),
                               NewState = State;
        {EHPid, info}       -> EHPid ! {ok, {State, ?MODULE:module_info(attributes)}},
                               NewState = State
    after ?FREQ ->
        ?MODULE:patrol(EHPid),
        NewState = State
    end,

    case NewState#state.level of
         running    -> ?MODULE:loop(NewState);
         shutdown   -> {EHPid, {ok, terminating}}
    end.

patrol(MSGPid) ->
    MSGPid ! {self(), {ok, checking_now}},

    lists:foreach(fun({Cmd, Tunel}) ->
                    case find_process(Tunel) of
                        nomatch -> os:cmd(Cmd),
                                   MSGPid ! {self(), Tunel ++ " restart"};
                        _Match  -> ok
                    end
                  end, ?S2_TUNEL).

%% HELPERS
%%
find_process(Process) ->
    string:find(os:cmd(?PS_CMD), Process).
