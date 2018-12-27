-module(seneh_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../src/seneh_hdr.hrl").

-export([all/0]).

-export([starting_stopping/1
       , logger_run/1]).

all() -> [starting_stopping
        , logger_run].

starting_stopping(_Config) ->
    application:start(seneh),
    ct:pal(default, ?MAX_IMPORTANCE, "~p", [application:which_applications()]),
    application:stop(seneh),
    ct:pal("starting_stopping FINISHED").

logger_run(_Config) ->
    meck:new(file, [unstick, passthrough]),
    meck:expect(file, open, fun(File,Arg2) ->
                                ct:pal("Application opens the file: ~p", [File]),
                                meck:passthrough(File, Arg2)
                            end),
    meck:expect(file, close, fun(File) ->
                                ct:pal("Application closes the file: ~p", [File]),
                                meck:passthrough(File)
                             end),

    LogItem = "Now something different",
    meck:expect(file, write, fun(Where, Msg) ->
                                {match, _} = re:run(Msg, LogItem),
                                meck:passthrough(Where, Msg)
                             end),

    seneh_log:start_logger(),
    seneh_log:log_normal(LogItem),  % should fail if not the same is being written

    meck:unload(file).
