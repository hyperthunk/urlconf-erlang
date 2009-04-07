%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

%% module annotations
-module(urlconf.application).
-author('Tim Watson <watson.timothy@gmail.com>').

%% compilation directives

-behavior(.application).

-export([start/2, stop/1]).

%% stdlib/kernel imports/includes

%% public api

start(_Type, Args) ->
    io:format("starting ~p~n", [?MODULE]),
    ok.
    %%File = proplists:get_value(filename, Args),
    %%Config = file:consult(File),
    %%{ok, _} = httpd.urlconf.server:start_link(Config).

stop(_State) ->
    ok.

%% private api


