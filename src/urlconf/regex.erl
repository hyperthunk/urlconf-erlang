%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

%% public api exports

%% module annotations

-module(urlconf.regex).
-author('Tim Watson <watson.timothy@gmail.com>').

%% application imports/includes

-include("regexp.hrl").

%% stdlib/kernel imports/includes

%% hack to keep cover tool happy...
-import(ets).

%% contrib imports/includes

%% public api exports

-export([compile/1, match/2]).

-define(CFLAGS, [
    caseless,
    {newline, anycrlf}
]).

-define(RFLAGS, [
    notempty,
    {capture, all, list}
]).

%% public api

compile(Regex) when is_list(Regex) ->
    Parsed =
    try .re:compile(Regex, ?CFLAGS) of
        {ok, Compiled} ->
            {ok, #'urlconf.regexp'{
                text_string=Regex,
                compiled=Compiled,
                flags=?RFLAGS,
                api_mod=.re
            }}
        ;
        {error, _}=Error ->
            Error
    catch
        Class:Reason ->
            {error, Reason}
    end.

match(_, #'urlconf.regexp'{ compiled=undefined }) ->
    throw({ebadarg, regexp_uncompiled})
;
match(Input, #'urlconf.regexp'{ compiled=CRX, flags=Flags }) ->
    match_result(.re:run(Input, CRX, Flags)).

match_result(nomatch) ->
    nomatch
;
match_result({match, [Match|Matches]}) ->
    #'urlconf.regexp.match'{
        match=Match,
        groups=Matches
    }.
