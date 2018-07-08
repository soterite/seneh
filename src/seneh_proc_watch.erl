-module(seneh_proc_watch).

-export([init/1, patrol/1, terminate/1, check/1]).

-define(FREQ, 1000 * 60 * 59).
-define(PS_CMD, "ps -xo cmd").
-define(S2_TUNEL, [
{"ssh -o TCPKeepAlive=yes -N -R 19721:localhost:8080 mroq@s2.mydevil.net &",
 "19721:localhost:8080"},
{"ssh -o TCPKeepAlive=yes -N -R 19722:localhost:6600 mroq@s2.mydevil.net &",
 "19722:localhost:6600"},
{"ssh -o TCPKeepAlive=yes -N -R 19723:localhost:22 mroq@s2.mydevil.net &",
 "19723:localhost:22"}]).

init(EHPid) ->
    patrol([{eventHandler_pid, EHPid}]).

patrol(State) ->
    EHPid = proplists:get_value(eventHandler_pid, State),

    lists:foreach(fun({Cmd, Tunel}) ->
                    case check(Tunel) of
                        nomatch -> os:cmd(Cmd),
                                   EHPid ! {self(), Tunel ++ " restart"};
                        _Match  -> ok
                    end
                  end, ?S2_TUNEL),

    receive
        {EHPid, stop} -> terminate(State)
    after ?FREQ ->
        EHPid ! {self(), alive},
        patrol(State)
    end.

check(Process) ->
    string:find(os:cmd(?PS_CMD), Process).

terminate(State) ->
    EHPid = proplists:get_value(eventHandler_pid, State),
    EHPid ! {self(), terminate}.
