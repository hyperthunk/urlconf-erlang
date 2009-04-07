%% @author Tim Watson <timwatson@munotify.com>
%% @copyright 2008 author.
%% regexp record definitions

%% internal representation of a regular expression
-record('urlconf.regexp', {

    %% textual representation of the rx
    text_string,

    %% compiled representation - NFA/DFA (implementation specific - optional)
    compiled,

    %% #'urlconf.regexp'.flags
    %% Flags = [Flag]
    %% Flag = {Name, Value}
    %%
    %% flags for use at runtime and/or compile time (implementation specific - optional)
    flags,


    %% #'urlconf.regexp'.api_mod::atom()
    %%
    %% the module used to build this regexp
    api_mod

   }).

%% regexp match result
-record('urlconf.regexp.match', {

    %% overall match string
    match,

    %% collection of sub-matches (match groups)
    groups

    }).

%% internal representation of an operation on a regular expression

%%-record(urlconf.regex_ops, {

%%    }).

