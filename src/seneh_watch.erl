-module(seneh_watch).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2
]).

-behaviour(gen_server).

-spec start_link() -> pid().
start_link() ->             % to jest wołane przez proces nadrzedny
    gen_server:start_link(
        {local, ?MODULE},   % locally/globaly regisered under the name ?MODULE
        ?MODULE,            % nazwa modułu, w którym są funkcje callback - zwykle ten sam po prostu
        [{}],                 % argument funkcji init
        []).                % gen_server options

init(Args) ->
    io:format("seneh_watch starting with Args: ~p~n", [Args]),
    {ok, []}.

handle_call(ssh_tunnel_status, From, State) ->
    From ! {ok, tunnel_alive},
    State;
handle_call(_,_,_) -> {}.
handle_cast(_,_) -> {}.
