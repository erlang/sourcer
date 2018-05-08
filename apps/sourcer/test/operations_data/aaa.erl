-module(aaa).
-include("aaa.hrl").

foo() ->
    bbb:bar().

baz() ->
    ?MMM,
    ok.

mmm() ->
    ok.
