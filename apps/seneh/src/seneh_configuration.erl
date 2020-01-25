-module(seneh_configuration).
-include("./seneh_hdr.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-record(state, {process_watcher}).

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
    [ProcessWatcher|_] = xmerl_xpath:string("//watcher[@type='process'][1]", Config),
    %% Processes = lists:map(fun(Process) ->
    %%                         #process{name = get_text_val("./name/text()", Process)
    %%                                , start_cmd = get_text_val("./start_cmd/text()", Process)
    %%                                , activity_indicator = get_text_val("./activity_indicator/text()", Process)}
    %%                       end, xmerl_xpath:string("./processes", ProcessWatcher)),
    Processes = lists:map(fun(Process) ->
                            #process{name = get_text_val("./name/text()", Process)
                                   , start_cmd = get_text_val("./start_cmd/text()", Process)
                                   , activity_indicator = get_text_val("./activity_indicator/text()", Process)}

                          end, xmerl_xpath:string("./processes", ProcessWatcher)),

    NewState = State#state{process_watcher = #process_watcher{
        name = get_text_val("./name/text()", ProcessWatcher)
      , period = get_text_val("./occurence/period/text()", ProcessWatcher)
      , processes = #process_table{content = Processes}
        }},
    {noreply, NewState}.


% xml parser
parse_xml() ->
    {Config, _} = xmerl_scan:file(?USER_CONFIG_FILE),
    Config.

get_text_val(XPath, Element) ->
    [#xmlText{value=Value}] = xmerl_xpath:string(XPath, Element),
    Value.
