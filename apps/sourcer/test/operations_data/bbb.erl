-module(bbb).
-include("aaa.hrl").

bar() ->
    ?MMM,
    aaa:baz().
