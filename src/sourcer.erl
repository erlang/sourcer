-module(sourcer).

-export_type([
              location/0,
              offset/0,
              token/0,
              tokens/0,
              open_brace/0,
              close_brace/0,
              form/0
             ]).

-type location() :: {erl_anno:line(), erl_anno:column(), offset()}.
-type offset() :: non_neg_integer().

-type token() :: {atom(), map()}.
-type tokens() :: [token()].

-type open_brace() :: '(' | '<<' | '[' | '{'.
-type close_brace() :: ')' | '>>' | ']' | '}'.

-type form() :: tuple().
