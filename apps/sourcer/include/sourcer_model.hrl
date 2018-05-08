%%% TODO the specs in this file are not fully implemented yet!

-type path() :: string().
-type project_ref() :: path().
%% Projects are either OTP applications/libraries, or
%% they have a configuration file at their root location
%% (default is 'rebar.config') that specifies where sources
%% and dependencies are to be found.
%% Rebar3-style local deps are recognized:
%%     apps/ | lib/ | libs/ | deps/ | _build/*/lib/
-record(project, {
        name :: string(),
        location :: path(),
        includes = ["include"] :: [path()],
        sources = ["src"] :: [path()],
        deps = [] :: [project_ref()],
        options = [] :: [any()]
    }).
-type project() :: #project{}.

-type pos() :: {non_neg_integer(), non_neg_integer()}.
-type range() :: {pos(), pos()} | 'none'.
-type location() :: #{'uri' := uri(), 'range' := range()}.

-type key() :: {'module',atom()} |
                {'function',atom(),integer()} |
                {'macro',atom(),integer(),Index::integer()} |
                {'type',atom(),integer()} |
                {'include', atom()} |
                {'include_lib', atom()} |
                {'record', atom()}.
-type ctx() :: [key()].

-record(def, {
        ctx :: ctx(),
        name_range = 'none' :: range(),
        info = #{} :: #{_=>_}
    }).
-type def() :: #def{}.

-record(ref, {
        ctx :: ctx(),
        range :: range()
    }).
-type ref() :: #ref{}.

-type uri() :: binary().
-type text() :: unicode:chardata().

-record(model, {
        vsn = 1 :: non_neg_integer(),
        defs = [] :: [def()],
        refs = [] :: [ref()]
    }).

-type model() :: #model{} | 'empty' | 'not_available'.

-record(db_entry, {
        model :: model(),
        text :: binary() | path(),
        includes = [] :: [path()]
    }).
-type db_entry() :: #db_entry{}.
-record(db, {
    models = dict:new() :: dict:dict(uri(), db_entry()),
    deps = digraph:new([acyclic]) :: digraph:graph()
}).
-type db() :: #db{}.

-record(workspace, {
        projects = [] :: [project()]
    }).
-type workspace() :: #workspace{}.


%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LPAR, '(').
-define(RPAR, ')').
-define(LCURL, '{').
-define(RCURL, '}').

-define(k(X), {X,_,_,_}).
-define(v(X), {_,_,_,X}).
