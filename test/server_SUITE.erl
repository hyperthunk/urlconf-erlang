%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

%% module annotations
-module(server_SUITE).
-author('Tim Watson <watson.timothy@gmail.com>').

%% compilation directives
-compile(export_all).

%% stdlib/kernel imports
-import(ct).
-import(lib_test, [assert_equals/2, assert_throws/2]).
-import(proplists, [get_value/2]).

%% stdlib/kernel imports
-include_lib("common_test/include/ct.hrl").

%% contrib package/module imports
-import(lib_test.stubs).
-import(lib_test.suite_util).
-import(urlconf.server).

%% contrib includes
-include_lib("lib_test/include/test_support.hrl").
-include_lib("../include/regexp.hrl").

%% public api exports

all() -> lib_test.suite_util:register_module_exports(?MODULE).

init_per_testcase(TestCase, Config) ->
    NewConfig = lib_test.suite_util:load_trace_configuration(TestCase, Config),
    io:format("~p~n", [NewConfig]),
    NewConfig.

end_per_testcase(_TestCase, Config) ->
    lib_test.suite_util:unload_trace_configuration(Config).

%% gen_server:init tests

initialization_minus_config_should_explode() ->
    ?TESTDOC("Lack of config data should cause an exception to be thrown").

initialization_minus_config_should_explode(_Config) ->
    lib_test:assert_equals(
        {stop, {ebadinit, noconfig}},
        server:init([])
    ).

proper_init_should_compile_regexp() ->
    ?TESTDOC("Initialization function should compile all regex").

proper_init_should_compile_regexp(_Config) ->
    ListenerPid = lib_test.stubs:log_calls(),
    Mod = mock_regex:new(ListenerPid, []),
    Rx = "/home/user/(.*)",
    Mapping = {
        Rx,
        foo.bar.baz,
        get_user
    },
    server:init({Mod, [Mapping]}),
    Mod:verify_expectations([{compile, Rx}]),
    Mod:shutdown().

bad_regex_should_cause_init_to_error() ->
    ?TESTDOC("Bad regex grammer should cause gen_server#init to fail").

bad_regex_should_cause_init_to_error(_Config) ->
    ListenerPid = lib_test.stubs:log_calls(),
    Msg = "expected-error-message", ExpectedError = {error, Msg},
    Rx = "/bad/regex/*",
    Mod = mock_regex:new(ListenerPid, [{Rx, ExpectedError}]),
    Mapping = {
        Rx,
        package.module1,
        function1
    },
    lib_test:assert_equals(
        {stop, {ebadregex, Msg}},
        server:init({Mod, [Mapping]})
    ),
    Mod:shutdown().

cool_init_should_return_compiled_regex_as_server_state() ->
    ?TESTDOC("The server should stash the compiled regex as gen_server process state").

cool_init_should_return_compiled_regex_as_server_state(_Config) ->
    Mod = mock_regex:new(self(), []),
    Regex = "/notification/(inbound|outbound)/(.*)",
    Handler = package.mod2, Func = view_stuff,
    Mapping = {Regex, Handler, Func},
    ExpectedState = {Mod, [{Regex, {compiled_rx, Handler, Func}}]},
    lib_test:assert_equals(
        {ok, ExpectedState},
        server:init({Mod, [Mapping]})
    ),
    Mod:shutdown().

%% api tests

cast_is_unsupported(_Config) ->
    {reply, {enoapi, cast_unsupported}, _} =
        server:handle_cast(msg, state).

info_is_ignored(_Config) ->
    {noreply, state} = server:handle_info(info, state).

hup_should_call_init_for_new_state() ->
    ?TESTDOC("Sending a 'HUP' message should result in recompilation of all mappings").

hup_should_call_init_for_new_state(_Config) ->
    OrigMod = mock_regex:new(lib_test.stubs:log_calls(), []),
    %% set up new mappings...
    ListenerPid = lib_test.stubs:log_calls(),
    %% create a new mock
    NewMod = mock_regex:new(ListenerPid, []),
    %% kick 'em around a bit....
    try
        OrigMapping = {
            "regex/ignored",
            original.handler,
            handle_request
        },
        server:start({OrigMod, [OrigMapping]}),
        UpdatedRx = "new/regex",
        NewMapping = {
            UpdatedRx,
            new.handler,
            handle_request
        },
        server:reconfigure(NewMod, [NewMapping]),
        NewMod:verify_expectations([{compile, UpdatedRx}])
    after
        NewMod:shutdown(),
        OrigMod:shutdown()
    end.

lookup_dispatcher_should_explode_unless_uri_present() ->
    ?TESTDOC("Trying to lookup a dispatcher without a decent uri will fail").

lookup_dispatcher_should_explode_unless_uri_present(_Config) ->
    DummyRx = mock_regex:new(self(), []),
    server:start({
        DummyRx,
        [{ "/notification/(.*)/(.*)/",  %% \1=>direction, \2=>interface
            some.handler.mod,
            handle_request
        }]
    }),
    lib_test:assert_equals(
        {error, {ebadarg, invalid_uri}},
        server:lookup_dispatcher(invalid_uri)
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
    DummyRE = mock_regex:new(self(), [{[Path, compiled_rx],RegexResp}]),
    server:start({
        DummyRE,
        [{ "/notification/(.*)/(.*)/",
            my.handler,
            handle_request
        }]
    }),
    lib_test:assert_equals(
        {mfa, {my.handler, handle_request, ["outbound", "http"]}},
        server:lookup_dispatcher(Path)
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
    RE = mock_regex:new(self(), [{["/" ++ Path, compiled_rx], RegexResp}]),
    server:start({
        RE,
        [{ "/notification/(.*)/(.*)/",
            my.handler,
            {handle_request, [direction,interface]} %% named arguments (of sorts)
        }]
    }),
    lib_test:assert_equals(
        {mfa, {my.handler, handle_request, [{direction, "outbound"}, {interface, "smpp"}]}},
        server:lookup_dispatcher(Path)
    ).

named_arguments_supported_by_regex_impl(_) ->
    Path = "/users/fwnext-retail/billing-address",
    Rx = "/users/(?<uname>.*)/(?<subcomponent>.*)$",
    Config = {
        urlconf.regex,
        [{Rx, my.controllers.users, {
            my_user_function,
            [uname, subcomponent]
        }}]
    },
    server:start(Config),
    lib_test:assert_same(
        {mfa, {
            my.controllers.users,
            my_user_function,
            [
                {uname, "fwnext-retail"},
                {subcomponent, "billing-address"}
            ]
        }},
        server:lookup_dispatcher(Path)
    ).
