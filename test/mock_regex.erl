%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

-module(mock_regex, [CapturePid, Stubs]).
-author('Tim Watson <watson.timothy@gmail.com>').

-behavior(urlconf.gen_regexp).
-export([compile/1, match/2, matches/2]).

-import(io).
-import(lists).
-import(lib_test).

-export([compiled_regex/0, verify_expectations/1, shutdown/0]).

compile(Thing) ->
    CapturePid ! {update, {compile, Thing}},
    respond(Thing, compiled_regex()).

match(Path, Thing) ->
    CapturePid ! {update, {match, [Path,Thing]}},
    respond([Path, Thing], ok).

matches(Path, Thing) ->
    CapturePid ! {update, {matches, [Path, Thing]}},
    respond([Path, Thing], ok).

respond(Thing, Alt) ->
    case lists:keysearch(Thing, 1, Stubs) of
        {value, {Thing, Resp}} -> Resp
        ;
        _ -> Alt
    end.

compiled_regex() -> {ok, compiled_rx}.

verify_expectations(E) ->
    CapturePid ! {getState, self()},
    receive
        States ->
            lists:map(
                fun({_Fun, _Term}=Expected) ->
                    case lists:member(Expected, States) of
                        true -> ok
                        ;
                        false ->
                            shutdown(),
                            ct:fail(
                                io:format("Expected ~p in ~p but was not.", [Expected, States])
                            )
                    end
                end,
                E
            )
    end.

shutdown() -> CapturePid ! shutdown.
