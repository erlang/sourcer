-module(sourcer_parse_tests).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").

split_test_() ->
    [
     ?_assertEqual([], sourcer_parse:split([])),
     ?_assertEqual([[a,b,c]], sourcer_parse:split([a,b,c])),
     ?_assertEqual([[a,{dot, 1}],[d,e]], sourcer_parse:split([a,{dot, 1},d,e])),
     ?_assertEqual([[a,{dot, 1}],[d,e,{dot, 1}]], sourcer_parse:split([a,{dot, 1},d,e,{dot, 1}]))
    ].
