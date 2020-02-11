-module(seneh_configuration).
-include("./seneh_hdr.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-record(state, {process_watcher, file_watcher}).

-behaviour(gen_server).

-export([start/0       % dla standalone server
        ,start_link/0  % dla supervisora
        ,stop/0
        ,init/1
        ,handle_call/3
        ,handle_cast/2
        ,terminate/2]).

-export([reload/0
        ,get_state/0
        ,get_process_watcher_config/0
        ,get_file_watcher_config/0
        ]).

% start
start() ->
    gen_server:start({local, ?MODULE},      % on jest rejestrowany pod nazwą sojego modułu
                     ?MODULE,
                     [],        % no args for init
                     []).       % no options

start_link() ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],        % no args for init
                          []).       % no options

% stop
stop() ->
    io:format("zatrzymuje", []),
    gen_server:stop(?MODULE).

% requests
get_state() ->
    gen_server:call(?MODULE, status).

get_process_watcher_config() ->
    gen_server:call(?MODULE, process_watcher_config).

get_file_watcher_config() ->
    gen_server:call(?MODULE, file_watcher_config).

reload() ->
    gen_server:cast(?MODULE, reload).

% modul
init(_what) ->
    seneh_log:log_normal("Seneh configuration process starting..."),
    State = #state{},
    {ok, State}.

terminate(_Reason, _State) ->
    seneh_log:log_normal("Seneh configuration terminates").

handle_call(status,_,State) ->
    {reply, State, State};
handle_call(process_watcher_config,_,State) ->
    case State#state.process_watcher of
         undefined -> NewState = get_configuration_from_file(State),
                      {reply, NewState#state.process_watcher, NewState};
         Value     -> {reply, Value, State}
    end;
handle_call(file_watcher_config,_,State) ->
    case State#state.file_watcher of
         undefined -> NewState = get_configuration_from_file(State),
                      {reply, NewState#state.file_watcher, NewState};
         Value     -> {reply, Value, State}
    end.

handle_cast(reload, State) ->
    seneh_log:log_normal("Seneh configuration reloading..."),
    NewState = get_configuration_from_file(State),
    seneh_log:log_normal("Seneh configuration file parsed."),
    {noreply, NewState}.

get_configuration_from_file(CurrentState) ->
    Config = parse_xml(),

    try

        [ProcessWatcher|_] = xmerl_xpath:string("//watcher[@type='process'][1]", Config),
        Processes = lists:map(fun(Process) ->
                                #process{name = get_text_val("./name/text()", Process)
                                       , start_cmd = get_text_val("./start_cmd/text()", Process)
                                       , activity_indicator = get_text_val("./activity_indicator/text()", Process)}
                              end, xmerl_xpath:string("./processes/process", ProcessWatcher)),

        [FileWatcher|_] = xmerl_xpath:string("//watcher[@type='file'][1]", Config),
        Files = lists:map(fun(File) ->
                                #file{name = get_text_val("./name/text()", File)
                                    , path = get_text_val("./url/text()", File)}
                              end, xmerl_xpath:string("./files/file", FileWatcher)),

        #state{process_watcher = #process_watcher{name = get_text_val("./name/text()", ProcessWatcher)
                                                , period = get_integer_val("./occurence/period/text()", ProcessWatcher)
                                                , processes = #process_table{content = Processes}}
             , file_watcher = #file_watcher{name = get_text_val("./name/text()", FileWatcher)
                                          , period = get_integer_val("./occurence/period/text()", FileWatcher)
                                          , files = #file_table{content = Files}}}
    catch
        error:{badmatch,_} -> seneh_log:log_normal("Abort. Configuration file parsing errors."),
        CurrentState       %% We want to maintain currently parsed configuration in case the new one is not parseable
    end.

% xml parser
parse_xml() ->
    ConfigFile = case filelib:is_file(?USER_CONFIG_FILE) of
                    true    -> ?USER_CONFIG_FILE;
                    false   -> {ok, CWD} = file:get_cwd(),
                               lists:concat([CWD, ?DEFAULT_CONFIG_FILE])
                 end,

    {Config, _} = xmerl_scan:file(ConfigFile),
    Config.

get_text_val(XPath, Element) ->
    [#xmlText{value=Value}] = xmerl_xpath:string(XPath, Element),
    Value.

get_integer_val(XPath, Element) ->
    StringValue = get_text_val(XPath, Element),
    Value = list_to_integer(StringValue),
    Value.
