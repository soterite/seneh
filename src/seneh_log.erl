-module(seneh_log).
-include("seneh_hdr.hrl").

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, terminate/2]).
-export([start_logger/0, log_normal/1, log_normal/2]).

init(_) ->
    {ok, []}.

handle_event({log_normal, Message}, State) ->
    {Y, M, D} = erlang:date(),
    {H, Mi, S} = erlang:time(),
    write_to_logfile(format_string("/====== Normal log entry  ~p-~p-~p ~p:~p:~p~n~p~n\\======",
                                   [Y, M, D, H, Mi, S, Message])),
    {ok, [Message | State], hibernate}.

handle_call(_, State) ->
    {ok, ok, State}.

terminate(_, _) ->
    io:format("Closing log at ~p~n", [?LOG_FILE]),
    ok.

write_to_logfile(Msg) ->
    {ok, Io} = file:open(?LOG_FILE, [append]),
    case file:write(Io, Msg) of
        ok  -> ok;
        {error, Reason} -> io:format("Write error: ~p", [Reason])
    end,
    ok = file:close(Io).

format_string(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

start_logger() ->
    gen_event:start({local, ?LOGGER_BUS}),
    gen_event:add_handler(?LOGGER_BUS, seneh_log, []).

log_normal(Format, Data) ->
    log_normal(format_string(Format, Data)).

log_normal(Msg) ->
    gen_event:notify(?LOGGER_BUS, {log_normal, Msg}).
