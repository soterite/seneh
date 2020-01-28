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

reload() ->
    gen_server:cast(?MODULE, reload).

% modul
init(_what) ->
    io:format("init: ~p", [_what]),
    seneh_log:log_normal("Seneh configuration process starting..."),
    State = #state{},
    {ok, State}.

terminate(Reason, State) ->
    seneh_log:log_normal("stop: ~p~n ~p", [Reason, State]).

handle_call(status,_,State) ->
    {reply, State, State}.

handle_cast(reload, State) ->
    seneh_log:log_normal("Seneh configuration reloading..."),

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
                                    , path = get_text_val("./ul/text()", File)}
                              end, xmerl_xpath:string("./files/file", FileWatcher)),

        NewState = State#state{process_watcher = #process_watcher{name = get_text_val("./name/text()", ProcessWatcher)
                                                                , period = get_text_val("./occurence/period/text()", ProcessWatcher)
                                                                , processes = #process_table{content = Processes}},
                               file_watcher = #file_watcher{name = get_text_val("./name/text()", FileWatcher)
                                                          , period = get_text_val("./occurence/period/text()", FileWatcher)
                                                          , files = #file_table{content = Files}}},
        seneh_log:log_normal("Seneh configuration file parsed."),
        {noreply, NewState}
    catch
        _:_ -> seneh_log:log_normal("Abort. Configuration file parsing errors."),
             {noreply, State}             %% We want to maintan currently parsed configuration in case the new one is not parseable
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
