-module(sourcer_util_tests).

-include_lib("eunit/include/eunit.hrl").

middle_test_() ->
    [
     ?_assertEqual([], sourcer_util:middle([])),
     ?_assertEqual([], sourcer_util:middle([1])),
     ?_assertEqual([], sourcer_util:middle([1,2])),
     ?_assertEqual([2], sourcer_util:middle([1,2,3])),
     ?_assertEqual([2,3], sourcer_util:middle([1,2,3,4]))
    ].

split_at_brace_test_() ->
    [
     ?_assertMatch({[],
                    [{atom,#{value:=a}},{',',_},{atom,#{value:=b}},{white_space,_},{atom,#{value:=c}}]},
                   sourcer_util:split_at_brace(scan("a,b c"))),
     ?_assertMatch({[{'(',_},{')',_}],
                    [{'->',_},{atom,#{value:=a}}]},
                   sourcer_util:split_at_brace(scan("()->a"))),
     ?_assertMatch({[{'(',_},{atom,#{value:=a}},{white_space,_},{atom,#{value:=b}},{')',_}],
                    []},
                   sourcer_util:split_at_brace(scan("(a b)"))),
     ?_assertMatch({[{'(',_},{atom,#{value:=a}},{white_space,_},{atom,#{value:=b}},{')',_}],
                    [{atom,#{value:=c}},{white_space,_},{atom,#{value:=d}}]},
                   sourcer_util:split_at_brace(scan("(a b)c d"))),
     ?_assertMatch({[{'(',_},{atom,#{value:=a}},{'(',_},{atom,#{value:=b}},{white_space,_},
                     {atom,#{value:=c}},{')',_},{atom,#{value:=d}},{')',_}],
                    [{atom,#{value:=e}}]},
                   sourcer_util:split_at_brace(scan("(a(b c)d)e"))),
     ?_assertMatch({[{'(',_},{atom,#{value:=a}},{',',_},{'[',_},{atom,#{value:=b}},{',',_},
                     {atom,#{value:=c}},{']',_},{',',_},{atom,#{value:=d}},{')',_}],
                    [{atom,#{value:=e}}]},
                   sourcer_util:split_at_brace(scan("(a,[b,c],d)e"))),
     ?_assertMatch({[], []},
                   sourcer_util:split_at_brace([]))
    ].

split_at_comma_test_() ->
    [
     ?_assertMatch([[{atom, #{value:=a}}],
                    [{atom,#{value:=b}},{white_space,_},{atom,#{value:=c}}],
                    [{atom,#{value:=d}}]],
                   sourcer_util:split_at_comma(scan("a,b c,d"))),
     ?_assertMatch([[{atom,#{value:=a}}],
                    [{'(',_},{atom,#{value:=b}},{white_space,_},{atom,#{value:=c}},{',',_},{atom,#{value:=d}},{')',_}],
                    [{atom,#{value:=e}},{white_space,_},{atom,#{value:=f}}]],
                   sourcer_util:split_at_comma(scan("a,(b c,d),e f"))),
     ?_assertMatch([[{atom,#{value:=a}}],
                    [{'[',_},{atom,#{value:=b}},{white_space,_},{atom,#{value:=c}},{',',_},{atom,#{value:=d}},{']',_}],
                    [{atom,#{value:=e}},{white_space,_},{atom,#{value:=f}}]],
                   sourcer_util:split_at_comma(scan("a,[b c,d],e f"))),
     ?_assertMatch([],
                   sourcer_util:split_at_comma([]))
    ].

filter_tokens_test_() ->
    Input =     [{comment,#{}},
                 {white_space,#{}},
                 {atom,#{value=>foo}},
                 {'(',#{}},
                 {')',#{}},
                 {'->',#{}},
                 {white_space,#{}},
                 {comment,#{}},
                 {white_space,#{}},
                 {atom,#{value=>ok}},
                 {dot,#{}}],

    [
     ?_assertMatch([{atom,#{value:=foo}},{'(',_},{')',_},{'->',_},{atom,#{value:=ok}},{dot,_}],
                   sourcer_util:filter_tokens(Input))
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scan(D) ->
    {ok, Ts, _} = sourcer_scan:string(D),
    Ts.

