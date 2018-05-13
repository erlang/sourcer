-module(aaa).
-include("aaa.hrl").

foo() ->
    bbb:bar().

%% extra
baz() ->
    ?MMM,
    ok.

mmm() ->
    ok.
