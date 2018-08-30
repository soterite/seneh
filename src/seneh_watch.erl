-module(seneh_watch).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2]).

-export([shutdown/0,
         info/0,
         check/0]).

-export([animate_watchdog/1]).

-behaviour(gen_server).

-record(state, {eventHandler_pid, watchdog_pid}).

-define(FREQ, 1000 * 60 * 59).
-define(PS_CMD, "ps -xo cmd").
-define(S2_TUNEL, [
{"ssh -o TCPKeepAlive=yes -N -R 19721:localhost:8080 mroq@s2.mydevil.net &",
 "19721:localhost:8080"},
{"ssh -o TCPKeepAlive=yes -N -R 19722:localhost:6600 mroq@s2.mydevil.net &",
 "19722:localhost:6600"},
{"ssh -o TCPKeepAlive=yes -N -R 19723:localhost:22 mroq@s2.mydevil.net &",
 "19723:localhost:22"}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shutdown() ->
    gen_server:call(?MODULE, terminate).

info() ->
    gen_server:call(?MODULE, info).

check() ->
    gen_server:call(?MODULE, check).

-spec start_link(pid()) -> pid().
start_link(Pid) ->             % to jest wołane przez proces nadrzedny
    io:format("seneh_watch starting...supervisor: ~p~n", [Pid]),
    gen_server:start_link(
        {local, ?MODULE},   % locally/globaly regisered under the name ?MODULE
        ?MODULE,            % nazwa modułu, w którym są funkcje callback - zwykle ten sam po prostu
        Pid,                 % argument funkcji init
        []).                % gen_server options

init(EHPid) ->
    Watchdog_Pid = spawn_link(?MODULE, animate_watchdog, [self()]),
    {ok, #state{eventHandler_pid = EHPid,
                watchdog_pid = Watchdog_Pid}}.

handle_call(terminate, _From, State) ->
    {stop, terminated, State};

handle_call(info, _From, State) ->
    {reply, {State, ?MODULE:module_info(attributes)}, State};

handle_call(check, _From, State) ->
    {reply, patrol(), State};

handle_call(_,_,_) -> {}.
handle_cast(_,_) -> {}.

terminate(terminated, State) ->
    MyPid = self(),
    WatchdogPid = State#state.watchdog_pid,
    WatchdogPid ! {MyPid, terminate},
    receive
        {WatchdogPid, terminated} -> ok
    after 1000 ->
        io:format("Watchdog is unavailable. We quit anyway...", [])
    end,
    nothing_else_to_do.

patrol() ->
    lists:map(fun({Cmd, Tunel}) ->
                    case find_process(Tunel) of
                        nomatch -> os:cmd(Cmd),
                                   Tunel ++ " restart";
                        _Match  -> Tunel ++ " ok"
                    end
                  end, ?S2_TUNEL).

find_process(Process) ->
    string:find(os:cmd(?PS_CMD), Process).

animate_watchdog(Pid) ->
    receive
        {Pid, terminate} -> io:format("Watchdog goes sleeping! pid:~p~n", [self()]),
                            Pid ! {self(), terminated}
    after ?FREQ ->
        Effects = ?MODULE:check(),
        io:format("watchdog run and guess what? That: ~p~n", [Effects]),
        animate_watchdog(Pid)
    end.
