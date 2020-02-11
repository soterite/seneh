-module(seneh_watch).
-include("./seneh_hdr.hrl").

-export([start/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2]).

-export([shutdown/0,
         info/0,
         check/0]).

-export([animate_watchdog/2]).

-behaviour(gen_server).

-record(state, {eventHandler_pid, watchdog_pid, process_table}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shutdown() ->
    gen_server:call(?MODULE, stop).

info() ->
    gen_server:call(?MODULE, info).

check() ->
    gen_server:call(?MODULE, check).

-spec start(pid()) -> pid().
start(Pid) ->             % to jest wołane przez proces nadrzedny
    seneh_log:log_normal("seneh_watch starting...~nprocess: ~p~nsupervisor: ~p~n", [self(), Pid]),
    gen_server:start_link(
        {local, ?MODULE},   % locally/globaly regisered under the name ?MODULE
        ?MODULE,            % nazwa modułu, w którym są funkcje callback - zwykle ten sam po prostu
        Pid,                 % argument funkcji init
        []).                % gen_server options

init(EHPid) ->
    ProcessWatcherConfig = seneh_configuration:get_process_watcher_config(),
    Watchdog_Pid = spawn_link(?MODULE, animate_watchdog, [self(), ProcessWatcherConfig#process_watcher.period]),
    {ok, #state{eventHandler_pid = EHPid,
                watchdog_pid = Watchdog_Pid,
                process_table = ProcessWatcherConfig#process_watcher.processes}}.

handle_call(stop, _From, State) ->
    {stop, normal, State};

handle_call(info, _From, State) ->
    {reply, {State, ?MODULE:module_info(attributes)}, State};

handle_call(check, _From, State) ->
    {reply, patrol(State#state.process_table), State};

handle_call(_,_,_) -> {}.
handle_cast(_,_) -> {}.

terminate(terminated, State) ->
    MyPid = self(),
    WatchdogPid = State#state.watchdog_pid,
    WatchdogPid ! {MyPid, terminate},
    receive
        {WatchdogPid, terminated} -> ok
    after 1000 ->
        seneh_log:log_normal("Watchdog is unavailable. We quit anyway...", [])
    end,
    nothing_else_to_do.

patrol(ProcessTable) ->
    Checks = lists:map(fun(Process) ->
                         case find_process(ProcessTable#process_table.ps_CMD
                                         , Process#process.activity_indicator) of
                             nomatch -> os:cmd(Process#process.start_cmd),
                                        Process#process.name ++ " restart";
                             _Match  -> Process#process.name ++ " ok"
                         end
                       end, ProcessTable#process_table.content),
    seneh_log:log_normal(string:join(Checks, "~n")).

find_process(OsCommand, Indicator) ->
    string:find(os:cmd(OsCommand), Indicator).

animate_watchdog(Pid, Freq) ->
    io:format("Freq: ~p", [Freq]),
    receive
        {Pid, terminate} -> seneh_log:log_normal("Watchdog goes sleeping! pid:~p~n", [self()]),
                            Pid ! {self(), terminated}
    after Freq ->
        ?MODULE:check(),
        animate_watchdog(Pid, Freq)
    end.
