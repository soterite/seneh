-module(seneh_log).
-include("seneh_hdr.hrl").

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, terminate/2]).
-export([start_logger/0, log_normal/1, log_normal/2]).

init(_) ->
    {ok, []}.

handle_event({log_normal, MsgParts}, State) ->
    {Y, M, D} = erlang:date(),
    {H, Mi, S} = erlang:time(),
    write_to_logfile(lists:flatten([#log_message{format = "~n/====== Normal log entry  ~p-~p-~p ~p:~p:~p~n",
                                                 data = [Y, M, D, H, Mi, S]},
                                    MsgParts,
                                    #log_message{format = "~n/======"}])),
    {ok, [MsgParts | State], hibernate}.

handle_call(_, State) ->
    {ok, ok, State}.

terminate(_, _) ->
    io:format("Closing log at ~p~n", [?LOG_FILE]),
    ok.

write_to_logfile(MsgParts) ->
    {ok, Io} = file:open(?LOG_FILE, [append]),
    lists:foreach(fun(Msg) -> io:format(Io, Msg#log_message.format, Msg#log_message.data) end,
                  MsgParts),
    ok = file:close(Io).

start_logger() ->
    gen_event:start({local, ?LOGGER_BUS}),
    gen_event:add_handler(?LOGGER_BUS, seneh_log, []).

log_normal(Format, Data) ->
    log_normal([#log_message{format = Format, data = Data}]).

log_normal(MsgParts) when is_list(MsgParts) ->
    case (io_lib:printable_list(MsgParts) orelse io_lib:printable_unicode_list(MsgParts)) of
        true  -> log_normal([#log_message{format = MsgParts}]);
        false -> case lists:all(fun(Msg) -> is_record(Msg, log_message) end, MsgParts) of
                    true  -> gen_event:notify(?LOGGER_BUS, {log_normal, MsgParts});
                    false -> log_normal([#log_message{data = MsgParts}])
                 end
    end;
log_normal(Msg) ->
    log_normal([#log_message{data = Msg}]).
