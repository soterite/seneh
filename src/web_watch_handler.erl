-module(web_watch_handler).
-include("./seneh_hdr.hrl").
-export([init/2]).

init(Req0, State) ->
    seneh_watch:check(),
    Tail = erlang:list_to_binary(os:cmd("tail " ++ ?LOG_FILE)),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        Tail,
        Req0),
    {ok, Req, State}.