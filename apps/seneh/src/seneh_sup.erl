%%%-------------------------------------------------------------------
%% @doc seneh top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(seneh_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    io:format("Seneh supervisor starting...", []),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Configuration = #{id     => configuration,
                     start  => {seneh_configuration, start_link, []},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [seneh_configuration]},

    WatcherChild = #{id     => watcher, % to jest wewnetrzny identyfikator supervisora - Obowiazkowy
                     start  => {seneh_watch, start, [self()]}, % funkcja startująca childa. Obowiazkowa. Chyba może być (powinno?) to tylko start_link
                     restart => permanent, % opcjonalnie i defaultowo. Może też być temporary (never restarted)
                                           % lub transient (restarted gdy exit reason inny niż normal
                     shutdown => 5000, % defaultowo i opcjonalnie. Najpierw exit(Child, shutdown) a po milisekundach exit(Child kill).
                                       % inne: brutal_kill (zawsze uzywa exit(Child, kill)) albo
                                       % infinity (default jeśli type jest supervisor). Tylko exit(Child, shutdown)
                     type => worker, % defaultowo. Może też być supervisor
                     modules => [seneh_watch]},

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,        % dziiecko moze sie maksymalnie zrestartowac tyle razy (MaxR)
                 period => 1},          % w jednej sekundzie (MaxT).

    {ok, { SupFlags, [Configuration, WatcherChild]} }.
%% internal functions
