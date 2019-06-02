%%%-------------------------------------------------------------------
%% @doc seneh public API
%% @end
%%%-------------------------------------------------------------------

-module(seneh_app).
-include("./seneh_hdr.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    seneh_log:start_logger(),
    seneh_log:log_normal("seneh_app starting...~nNode name: ~p, Cookie: ~p~n, HTTP: ~p~n", [node(), erlang:get_cookie(), ?HTTP_PORT]),
    Dispatch = cowboy_router:compile(
        [                      % Routes
         {                     % Host - also: {HostMatch, Constraints, PathList}
          '_',                 % HostMatch
          [                    % PathList
           {                   % Path - also {PathMatch, Constraints, Handler, InitialState
            "/",               % PathMatch
            web_watch_handler, % Handler
            []                 % InitialState
           },
           {"/watchdog", cowboy_static, {priv_file, seneh, "static/watchdog.html"}} % built-in handler for static files
          ]
         }
        ]),

    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, ?HTTP_PORT}],
                                 #{env => #{dispatch => Dispatch}}),
    seneh_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
