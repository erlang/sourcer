-module(sourcer_iterator_tests).

-include_lib("eunit/include/eunit.hrl").

iterate_tokens_source_test_() ->
    Fun = fun(X)->X*3 end,
    [
     ?_assertMatch([3,6],
                   sourcer_iterator:iterate_tokens_source([1,2], Fun)),
     ?_assertMatch([3, 9, 6],
                   sourcer_iterator:iterate_tokens_source([1,{fork, [3], [4]}, 2],
                                                          Fun)),
     ?_assertMatch([3, 9, 15, 6],
                   sourcer_iterator:iterate_tokens_source([1,{fork, [3, {fork, [5], []}], [4]}, 2],
                                                          Fun)),
     ?_assertMatch([3, 9, 6],
                   sourcer_iterator:iterate_tokens_source([1,{fork, [3, {fork, [], [5]}], [4]}, 2],
                                                          Fun)),
     ?_assertMatch([],
                   sourcer_iterator:iterate_tokens_source([], Fun))
    ].

iterate_tokens_expanded_test_() ->
    Fun = fun(X)->X*3 end,
    [
     ?_assertMatch([3,6],
                   sourcer_iterator:iterate_tokens_expanded([1,2], Fun)),
     ?_assertMatch([3, 12, 6],
                   sourcer_iterator:iterate_tokens_expanded([1,{fork, [3], [4]}, 2],
                                                            Fun)),
     ?_assertMatch([3, 12, 6],
                   sourcer_iterator:iterate_tokens_expanded([1,{fork, [3], [4, {fork, [5], []}]}, 2],
                                                            Fun)),
     ?_assertMatch([3, 12, 18, 6],
                   sourcer_iterator:iterate_tokens_expanded([1,{fork, [3], [4, {fork, [5], [6]}]}, 2],
                                                            Fun)),
     ?_assertMatch([],
                   sourcer_iterator:iterate_tokens_expanded([], Fun))
    ].

