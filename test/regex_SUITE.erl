%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

%% module annotations
-module(regex_SUITE).
-author('Tim Watson <watson.timothy@gmail.com>').

%% compilation directives
-compile(export_all).

%% stdlib/kernel imports
-import(ct).
-import(lib_test, [assert_equals/2, assert_throws/2, assert_true/1]).
-import(proplists, [get_value/2]).

%% stdlib/kernel imports
-include_lib("common_test/include/ct.hrl").

%% contrib imports/includes
-import(lib_test.stubs).
-import(lib_test.suite_util).
-import(urlconf.regex).

-include_lib("lib_test/include/test_support.hrl").
-include_lib("../include/regexp.hrl").

%% public api exports

all() -> lib_test.suite_util:register_module_exports(?MODULE).

%%init_per_testcase(TestCase, Config) ->
%%    NewConfig = lib_test.suite_util:load_trace_configuration(TestCase, Config),
%%    io:format("~p~n", [NewConfig]),
%%    NewConfig.
%%
%%end_per_testcase(_TestCase, Config) ->
%%    lib_test.suite_util:unload_trace_configuration(Config).

%% compiling regex

good_compile_should_take_a_string_and_return_a_record() ->
    ?TESTDOC("Compilation takes a string and returns a record").

good_compile_should_take_a_string_and_return_a_record(_Config) ->
    {ok, #'urlconf.regexp'{}} = regex:compile("([0-9]?).*").

compiled_regexp_should_contain_resulting_forms(_Config) ->
    {ok, #'urlconf.regexp'{ compiled=Compiled }} = regex:compile(".*"),
    assert_true(Compiled =/= undefined).

unparsable_regex_should_return_error_tuple(_Config) ->
    {error, {_Reason, _Details}} = regex:compile("[a-c]{5)").

%% matching

matching_an_uncompiled_regex_explodes(_Config) ->
    RawRx = #'urlconf.regexp'{ compiled=undefined },
    assert_throws(
        fun() -> regex:match("ignored", RawRx) end,
        {ebadarg, regexp_uncompiled}
    ).

match_failure_should_return_atom_nomatch(_Config) ->
    {ok, Rx} = regex:compile("[a-zA-Z]*"),
    assert_equals(nomatch, regex:match("123456", Rx)).

good_match_should_return_a_record(_Config) ->
    {ok, Rx} = regex:compile("[\\w]*@bt\.com"),
    Input = "timwatson@bt.com",
    #'urlconf.regexp.match'{
        match=Input,
        groups=[]
    } = regex:match(Input, Rx).

capturing_groups_should_appear_in_match_record(_Config) ->
    {ok, Rx} = regex:compile("([\\w]*)@(bt\.com)"),
    Input = "timwatson@bt.com",
    #'urlconf.regexp.match'{
        match=Input,
        groups=["timwatson", "bt.com"]
    } = regex:match(Input, Rx).

match_should_respect_custom_flags(_Config) ->
    {ok, Rx} = regex:compile("(?<NAME>[\\w]*)@(?<DOMAIN>bt\.com)"),
    Input = "timwatson@bt.com",
    #'urlconf.regexp.match'{
        match=Input,
        groups=["bt.com"]
    } = regex:match(Input,
        Rx#'urlconf.regexp'{
            flags=[notempty, {capture, [
                0,          %% forces .re to include the complete match string
                'DOMAIN'    %% named capturing group
            ], list}]
        }
    ).
