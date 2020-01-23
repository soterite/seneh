-module(seneh_configuration).
-include("./seneh_hdr.hrl").

-behaviour(gen_server).

-export([start/0       % dla standalone server
        ,start_link/0  % dla supervisora
        ,stop/0
        ,init/1
        ,handle_call/3
        ,handle_cast/2
        ,terminate/2]).

-export([status_call/0
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
status_call() ->
    gen_server:call(?MODULE, status).

% modul
init(_what) ->
    io:format("init: ~p", [_what]),
    NewState = [],
    {ok, NewState}.

terminate(Reason, State) ->
    io:format("stop: ~p~n ~p", [Reason, State]).

handle_call(_,_,_) ->
    io:format("Robie", []),
    {reply, "call", []}.

handle_cast(_,_) ->
    {noreply, "cast", []}.
