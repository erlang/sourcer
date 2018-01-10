-type path() :: string().
-type project_ref() :: path().
%% Projects are either OTP applications/libraries, or
%% they have a configuration file at their root location
%% (default is 'rebar.config') that specifies where sources
%% and dependencies are to be found.
%% Rebar3-style local deps are recognized: apps | lib | libs | deps.
%% TODO what about _build/lib?
-record(project, {
    location :: path(),
    includes = [] :: [path()],
    sources= [] :: [path()],
    libs = [] :: [project_ref()],
    options = [] :: [any()]
    }).
-type project() :: #project{}.

-record(workspace, {
    projects = [] :: [project()]
    }).
-type workspace() :: #workspace{}.

-type pos() :: {non_neg_integer(), non_neg_integer()}.
-type range() :: {pos(), pos()}.
-type key() :: {module,atom()} |
                {function,atom(),integer()} |
                {macro,atom(),integer(),Index::integer()} |
                {type,atom(),integer()}.
-type ctx() :: [key()].

-type def() :: {ctx(), Name::range(), Info :: #{}} |
                {ctx(), Name::range(), Full::range(), Info :: #{}}.
-type ref() :: {ctx(), range()}.

-type uri() :: binary().
-type info() :: {ctx(), any()}.

-record(model, {
    vsn = 1 :: integer(),
    defs = [] :: [def()],
    refs = [] :: [ref()]
}).

-type model() :: #model{} | 'empty' | 'not_available'.

-type db() :: #{uri() => model()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LPAR, '(').
-define(RPAR, ')').
-define(LCURL, '{').
-define(RCURL, '}').

-define(k(X), {X,_,_,_}).
