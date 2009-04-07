%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

%% @doc Request Mapping Server - maps uri to dispatcher based on configuration data

-module(urlconf.server).
-vsn("0.4").
-author('Tim Watson <watson.timothy@gmail.com>').

-behavior(gen_server).

-include("regexp.hrl").

-import(io).
-import(lists).
-import(logger).
-import(proc_lib).
-import(gen_server).

%% hack to keep 'cover' tool happy....
-import(ets).

%% gen_server exports

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% public api exports

-export([start/1, start_link/1, reconfigure/2, lookup_dispatcher/1]).

%% public api

start(Config) ->
    start_link(Config).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%gen_loop(Config) ->
%%    register(?MODULE, self()),
%%    State = init(Config),
%%    proc_lib:init_ack({ok, self()}),
%%    gen_server:enter_loop(?MODULE, [], State).

%% lookup_dispatcher(Uri) -> Dispatcher | {error, Why}
%%
%% Types
%%      Dispatcher = {mfa, {M,F,A}} | {fun, Fun::fun()}
%%
%% Attempts to locate an MFA tuple (or Fun) for the supplied uri.
%%
lookup_dispatcher(Uri) ->
    gen_server:call(?MODULE, {lookup_dispatcher, Uri}).

%% reconfigure(Mod, Mappings) -> {ok, reconfigured}
%%
%% Types
%%      Mod = ModuleDef = atom() | ParameterizedModule = {atom(), PID[, term()]}
%%
%% Short cut to simulate code_change (as when an app up(or down)grade takes place).
%% Terminates the server in the face or any errors.
%%
reconfigure(Mod, Mappings) ->
    gen_server:call(?MODULE, {reconfigure, {Mod, Mappings}}).

%% gen_server callbacks

init({Mod, Mappings}) ->
    try
        Cache = compile_regex(Mod, Mappings),
        {ok, {Mod,Cache}}
    catch
        _Class:Reason -> {stop, Reason}
    end
;
init(_Other) ->
    {stop, {ebadinit, noconfig}}.

%% synchronous call handlers

handle_call({reconfigure, {Mod, Mappings}}, _From, _State) ->
    {ok, {Mod, Cache}} = init({Mod, Mappings}),
    %% TODO: you should return {stop, Reason} if init fails....
    NewState = {Mod, Cache},
    {reply, {ok, reconfigured}, NewState}
;
handle_call({lookup_dispatcher, [$/|_]=URI}, _From, {RE, Mappings}=State) ->
    Reply = match(URI, RE, Mappings),
    {reply, Reply, State}
;
handle_call({lookup_dispatcher, URI}, From, State)
    when is_list(URI) ->
        handle_call({lookup_dispatcher, [$/|URI]}, From, State)
;
handle_call({lookup_dispatcher, _}, _From, State) ->
    {reply, {error, {ebadarg, invalid_uri}}, State}
;
handle_call(stop, _From, State) ->
    {stop, reason, stopped, State}.

%% cast api handler(s)

handle_cast(_Msg, State) ->
    {reply, {enoapi, cast_unsupported}, State}.

%% info handlers

%% Default case
handle_info(_Info, State) ->
    {noreply, State}.

%% termination handler(s)

terminate(Reason, _State) ->
    ct:pal("Terminating: ~p~n", [Reason]).

code_change(_,_,_) -> throw({enoimpl, not_implemented}).

%% private api

match(Path, RE, [{_, {Regex, Mod, Func}}|Mappings]) ->
    case match(Path, RE, {Regex, Mod, Func}) of
        nomatch ->
            match(Path, RE, Mappings)
        ;
        Match ->
            {mfa, match(Match, Mod, Func)}
    end
;
match(Path, RE,
    {#'urlconf.regexp'{
        flags=Flags
    }=Regex, _, {_, [_|_]=NamedArgs}}) ->

    UpgradedFlags = lists:keystore(capture, 1,
        Flags, {capture, [0|NamedArgs], list}),
    RE:match(
        Path,
        Regex#'urlconf.regexp'{
            flags=UpgradedFlags
        }
    )
;
match(Path, RE, {Regex, _, _}) ->
    RE:match(Path, Regex)
;
match(
    #'urlconf.regexp.match'{ groups=Groups },
    Mod, Func) when is_atom(Func) ->
    {Mod, Func, Groups}
;
match(
    #'urlconf.regexp.match'{ groups=Groups }, Mod,
    {Func, [_|_]=NamedArgs}
) ->
    {Mod, Func, lists:zip(NamedArgs, Groups)}
;
match(_, _, []) ->
    nomatch.

compile_regex(RE, [{Regex, Mod, Func}|Mappings]) ->
    case RE:compile(Regex) of
        {ok, Compiled} ->
            [{Regex, {Compiled, Mod, Func}}|compile_regex(RE, Mappings)]
        ;
        {error, Reason} -> throw({ebadregex, Reason})
    end
;
compile_regex(_, []) ->
    [].

invalid_mod() -> throw({ebadinit, invalid_regex_module}).
