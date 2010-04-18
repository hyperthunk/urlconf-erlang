%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

%% module annotations
-module(regex_SUITE).
-author('Tim Watson <watson.timothy@gmail.com>').

%% compilation directives
-compile(export_all).

%% stdlib/kernel imports
-import(ct).
-import(proplists, [get_value/2]).

%% stdlib/kernel imports
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

%% contrib imports/includes
%%-import(lib_test.stubs).
%%-import(lib_test.suite_util).
-import(urlconf_regex).

-include("test.hrl").
-include_lib("../include/regexp.hrl").

%% public api exports

all() ->
    %% lib_test.suite_util:register_module_exports(?MODULE).
    ?EXPORT_TESTS(?MODULE).

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
    {ok, #'urlconf.regexp'{}} = urlconf_regex:compile("([0-9]?).*").

compiled_regexp_should_contain_resulting_forms(_Config) ->
    {ok, #'urlconf.regexp'{ compiled=Compiled }} = urlconf_regex:compile(".*"),
    ?assert(Compiled =/= undefined).

unparsable_regex_should_return_error_tuple(_Config) ->
    {error, {_Reason, _Details}} = urlconf_regex:compile("[a-c]{5)").

%% matching

matching_an_uncompiled_regex_explodes(_Config) ->
    RawRx = #'urlconf.regexp'{ compiled=undefined },
    try (urlconf_regex:match("ignored", RawRx))
        of _ -> ct:fail("expected exit/throw but match succeeded")
    catch
        _:{ebadarg, regexp_uncompiled} -> ok
    end.

match_failure_should_return_atom_nomatch(_Config) ->
    {ok, Rx} = urlconf_regex:compile("[a-zA-Z]*"),
    ?assertEqual(nomatch, urlconf_regex:match("123456", Rx)).

good_match_should_return_a_record(_Config) ->
    {ok, Rx} = urlconf_regex:compile("[\\w]*@bt\.com"),
    Input = "timwatson@bt.com",
    #'urlconf.regexp.match'{
        match=Input,
        groups=[]
    } = urlconf_regex:match(Input, Rx).

capturing_groups_should_appear_in_match_record(_Config) ->
    {ok, Rx} = urlconf_regex:compile("([\\w]*)@(bt\.com)"),
    Input = "timwatson@bt.com",
    #'urlconf.regexp.match'{
        match=Input,
        groups=["timwatson", "bt.com"]
    } = urlconf_regex:match(Input, Rx).

match_should_respect_custom_flags(_Config) ->
    {ok, Rx} = urlconf_regex:compile("(?<NAME>[\\w]*)@(?<DOMAIN>bt\.com)"),
    Input = "timwatson@bt.com",
    ?assertMatch(#'urlconf.regexp.match'{match=Input,groups=["bt.com"]},
        urlconf_regex:match(Input,
            Rx#'urlconf.regexp'{
                flags=[notempty, {capture, [
                    0,          %% forces .re to include the complete match string
                    'DOMAIN'    %% named capturing group
                ], list}]
            }
        )).
