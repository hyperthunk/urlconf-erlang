%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

%% module annotations
-module(server_SUITE).
-author('Tim Watson <watson.timothy@gmail.com>').

%% compilation directives
-compile(export_all).

%% stdlib/kernel imports
-import(ct).
%% -import(lib_test, [assert_equals/2, assert_throws/2]).
-import(proplists, [get_value/2]).

%% stdlib/kernel imports
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% contrib package/module imports
%%-import(lib_test.stubs).
%%-import(lib_test.suite_util).
-import(urlconf_server).

%% contrib includes
-include("test.hrl").
-include_lib("../include/regexp.hrl").

%% public api exports

all() ->
    %% lib_test.suite_util:register_module_exports(?MODULE).
    ?EXPORT_TESTS(?MODULE).

end_per_testcase(TestCase, Config) ->
    catch (urlconf_server:stop()),
    ok.

initialization_minus_config_should_explode(_Config) ->
    ?assertEqual(
        {stop, {ebadinit, noconfig}},
        urlconf_server:init([])
    ).

proper_init_should_compile_regexp(_Config) ->
    ListenerPid = log_calls(),
    Mod = mock_regex:new(ListenerPid, []),
    Rx = "/home/user/(.*)",
    Mapping = {
        Rx,
        foo.bar.baz,
        get_user
    },
    urlconf_server:init({Mod, [Mapping]}),
    Mod:verify_expectations([{compile, Rx}]),
    Mod:shutdown().

bad_regex_should_cause_init_to_error() ->
    ?TESTDOC("Bad regex grammer should cause gen_server#init to fail").

bad_regex_should_cause_init_to_error(_Config) ->
    ListenerPid = log_calls(),
    Msg = "expected-error-message", ExpectedError = {error, Msg},
    Rx = "/bad/regex/*",
    Mod = mock_regex:new(ListenerPid, [{Rx, ExpectedError}]),
    Mapping = {
        Rx,
        package.module1,
        function1
    },
    ?assertEqual(
        {stop, {ebadregex, Msg}},
        urlconf_server:init({Mod, [Mapping]})
    ),
    Mod:shutdown().

good_init_should_return_compiled_regex_as_server_state() ->
    ?TESTDOC("The server should stash the compiled regex as gen_server process state").

good_init_should_return_compiled_regex_as_server_state(_Config) ->
    Mod = mock_regex:new(self(), []),
    Regex = "/notification/(inbound|outbound)/(.*)",
    Handler = package.mod2, Func = view_stuff,
    Mapping = {Regex, Handler, Func},
    ExpectedState = {Mod, [{Regex, {compiled_rx, Handler, Func}}]},
    ?assertEqual(
        {ok, ExpectedState},
        urlconf_server:init({Mod, [Mapping]})
    ),
    Mod:shutdown().

%% api tests

cast_is_unsupported(_Config) ->
    {reply, {enoapi, cast_unsupported}, _} =
        urlconf_server:handle_cast(msg, state).

info_is_ignored(_Config) ->
    {noreply, state} = urlconf_server:handle_info(info, state).

hup_should_call_init_for_new_state() ->
    ?TESTDOC("Sending a 'HUP' message should result in recompilation of all mappings").

hup_should_call_init_for_new_state(_Config) ->
    OrigMod = mock_regex:new(log_calls(), []),
    %% set up new mappings...
    ListenerPid = log_calls(),
    %% create a new mock
    NewMod = mock_regex:new(ListenerPid, []),
    %% kick 'em around a bit....
    try
        OrigMapping = {
            "regex/ignored",
            original.handler,
            handle_request
        },
        urlconf_server:start({OrigMod, [OrigMapping]}),
        UpdatedRx = "new/regex",
        NewMapping = {
            UpdatedRx,
            new.handler,
            handle_request
        },
        urlconf_server:reconfigure(NewMod, [NewMapping]),
        NewMod:verify_expectations([{compile, UpdatedRx}])
    after
        NewMod:shutdown(),
        OrigMod:shutdown()
    end.

lookup_dispatcher_should_explode_unless_uri_present() ->
    ?TESTDOC("Trying to lookup a dispatcher without a decent uri will fail").

lookup_dispatcher_should_explode_unless_uri_present(_Config) ->
    DummyRx = mock_regex:new(log_calls(), []),
    urlconf_server:start({
        DummyRx,
        [{ "/notification/(.*)/(.*)/",  %% \1=>direction, \2=>interface
            some.handler.mod,
            handle_request
        }]
    }),
    ?assertEqual(
        {error, {ebadarg, invalid_uri}},
        urlconf_server:lookup_dispatcher(invalid_uri)
    ).

lookup_for_match_should_include_numbered_groups_if_submatches_present() ->
    ?TESTDOC("When match groups exist within the result, they should be returned also").

lookup_for_match_should_include_numbered_groups_if_submatches_present(_Config) ->
    Path = "/notification/outbound/http/",
    RegexResp =
        #'urlconf.regexp.match'{
            match = Path,
            groups = [ "outbound", "http" ]
        },
    DummyRE = mock_regex:new(log_calls(), [{[Path, compiled_rx],RegexResp}]),
    urlconf_server:start({
        DummyRE,
        [{ "/notification/(.*)/(.*)/",
            my.handler,
            handle_request
        }]
    }),
    ?assertEqual(
        {mfa, {my.handler, handle_request, ["outbound", "http"]}},
        urlconf_server:lookup_dispatcher(Path)
    ).

lookup_should_include_named_groups_where_mapping_exists() ->
    ?TESTDOC("Named matches are supported only via configuration data").

lookup_should_include_named_groups_where_mapping_exists(_Config) ->
    Path = "notification/outbound/smpp/",
    RegexResp =
        #'urlconf.regexp.match'{
            match = Path,
            groups = [ "outbound", "smpp" ]
        },
    RE = mock_regex:new(log_calls(), [{["/" ++ Path, compiled_rx], RegexResp}]),
    urlconf_server:start({
        RE,
        [{ "/notification/(.*)/(.*)/",
            my.handler,
            {handle_request, [direction,interface]} %% named arguments (of sorts)
        }]
    }),
    Result = urlconf_server:lookup_dispatcher(Path),
    urlconf_server:stop(),
    ?assertEqual(
        {mfa, {my.handler, handle_request, [{direction, "outbound"}, {interface, "smpp"}]}},
        Result
    ).

named_arguments_supported_by_regex_impl(_) ->
    Path = "/users/fwnext-retail/billing-address",
    Rx = "/users/(?<uname>.*)/(?<subcomponent>.*)$",
    Config = {urlconf_regex, [
        {Rx, my.controllers.users, {my_user_function, [uname, subcomponent]}}
    ]},
    urlconf_server:start(Config),
    Result = urlconf_server:lookup_dispatcher(Path),
    urlconf_server:stop(),
    ?assertEqual(
        {mfa, {
            my.controllers.users,
            my_user_function,
            [
                {uname, "fwnext-retail"},
                {subcomponent, "billing-address"}
            ]
        }}, Result).

log_calls() ->
    spawn(fun() -> state_loop([]) end).

state_loop(State) ->
    receive
        {getState, PID} -> PID ! State
        ;
        {update, NewState} -> state_loop([NewState|State])
        ;
        shutdown -> exit(normal)
    end.
