-module(sourcer).

-export_type([
              location/0,
              token/0,
              tokens/0,
              fork_type/0,
              fork_path/0,
              open_brace/0,
              close_brace/0,
              form/0
             ]).

-type location() :: map().

-type open_brace() :: '(' | '<<' | '[' | '{'.
-type close_brace() :: ')' | '>>' | ']' | '}'.

-type form() :: tuple().

-type fork_type() :: 'ws' | 'pp'.
-type fork_path() :: 'source' | 'expanded'.

-type token() :: {Kind::atom(), Attributes::map()}
          | {Kind::'fork', Type::fork_type(), Source::tokens(), Expanded::tokens()}.

-type tokens() :: [token()].

