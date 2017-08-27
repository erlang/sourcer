-module(sourcer_parse_util_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SUT, sourcer_parse_util).

compact_newlines_test_() ->
    [
     ?_assertMatch([{comment,_,"%a",_},
                     {comment,_,"%b",_},
                     {comment,_,"%%c",_}],
                   ?SUT:compact_newlines(scan("%a\n%b\n%%c\n"))),
     ?_assertMatch([{comment,_,"%a",_},
                     {white_space,_,"\n",_},
                     {comment,_,"%b",_},
                     {white_space,_,"\n",_},
                     {comment,_,"%%c",_}],
                   ?SUT:compact_newlines(scan("%a\n\n%b\n\n\n%%c\n")))
    ].

split_at_semicolon_name_test_() ->
    [
     ?_assertMatch([[{atom, _, _, a},
                     {',', _, _, _},
                     {atom, _, _, b}]],
                   ?SUT:split_at_semicolon_name(scan("a,b"))),
     ?_assertMatch([[{atom, _, _, a},
                     {';',_, _, _},
                     {atom, _, _, b},
                     {',',_, _, _},
                     {atom, _, _, c}]],
                   ?SUT:split_at_semicolon_name(scan("a;b,c"))),
     ?_assertMatch([[{atom, _, _, a},{';',_, _, _},{atom, _, _, a},{',',_, _, _},{atom, _, _, b}]],
                   ?SUT:split_at_semicolon_name(scan("a;a,b"))),
     ?_assertMatch([[{atom, _, _, a},{'(',_, _, _},{atom, _, _, b},{')',_, _, _}],
                    [{atom, _, _, a},{'(',_, _, _},{atom, _, _, e},{')',_, _, _}]],
                   ?SUT:split_at_semicolon_name(scan("a(b);a(e)"))),
     ?_assertMatch([[{atom, _, _, a},{'(',_, _, _},{atom, _, _, b},{')',_, _, _},{';',_, _, _},
                     {atom, _, _, c},{'(',_, _, _},{atom, _, _, d},{')',_, _, _}]],
                   ?SUT:split_at_semicolon_name(scan("a(b);c(d)")))
    ].

split_at_semicolon_paren_test_() ->
    [
     ?_assertMatch([[{atom, _, _, a},{',',_, _, _},{atom, _, _, b},{',',_, _, _}, {atom, _paren_, _, c}]],
                   ?SUT:split_at_semicolon_paren(scan("a,b,c"))),
     ?_assertMatch([[{atom, _, _, a},{';',_, _, _},{atom, _, _, b},{',',_, _, _},{atom, _paren_, _, c}]],
                   ?SUT:split_at_semicolon_paren(scan("a;b,c"))),
     ?_assertMatch([[{'(',_, _, _},{atom, _, _, b},{')',_, _, _},{atom, _, _, zz}],
                    [{'(',_, _, _},{atom, _, _, e},{')',_, _, _},{atom, _, _, xx}]],
                   ?SUT:split_at_semicolon_paren(scan("(b)zz;(e)xx"))),
     ?_assertMatch([],
                   ?SUT:split_at_semicolon_paren([]))
    ].

split_at_token_test_() ->
    [
     ?_assertMatch({[{atom, _, _, a}], []},
                   ?SUT:split_at_token(scan("a"), dot)),
     ?_assertMatch({[{atom, _, _, a}],[{atom, _, _, b}]},
                   ?SUT:split_at_token(scan("a.b"), '.')),
     ?_assertMatch({[{atom, _, _, a}], [{atom, _, _, b}, {dot, _, _, _}]},
                   ?SUT:split_at_token(scan("a.b."), '.')),
     ?_assertMatch({[], [{atom, _, _, a}]},
                   ?SUT:split_at_token(scan(".a"), '.')),
     ?_assertMatch({[], []},
                   ?SUT:split_at_token([], dot))
    ].


scan(D) ->
    {ok, Ts, _} = sourcer_scan:string(D),
    Ts.
