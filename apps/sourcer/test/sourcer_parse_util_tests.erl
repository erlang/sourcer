-module(sourcer_parse_util_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SUT, sourcer_parse_util).

extract_top_comments_test_() ->
    [
     ?_assertMatch({none,
                  [{atom,_,"hello",hello}]},
            ?SUT:extract_top_comments(scan_ws("\nhello"))),
     ?_assertMatch({{{0,1},{2,8}},
                  [{atom,_,"hello",hello}]},
            ?SUT:extract_top_comments(scan_ws("%a\n%bc\n  %%cde\nhello"))),
     ?_assertMatch({{{0,1},{0,6}},
                  [{atom,_,"hello",hello}]},
            ?SUT:extract_top_comments(scan_ws("%afgg\n   \n\nhello"))),
     ?_assertMatch({{{4,1},{4,7}},
                  [{atom,_,"hello",hello}]},
            ?SUT:extract_top_comments(scan_ws("%12345\n%123\n\n\n%%1234\nhello")))
    ].

take_until_token_test_() ->
    [
     ?_assertEqual({scan("a"), none, []},
                   ?SUT:take_until_token(scan("a"), dot)),
     ?_assertEqual({scan("a"), {dot,{0,2},".",undefined}, scan("   b")},
                   ?SUT:take_until_token(scan("a. b"), dot)),
     ?_assertEqual({scan("a"), {dot,{0,2},".",undefined}, scan("   b.")},
                   ?SUT:take_until_token(scan("a. b."), dot)),
     ?_assertEqual({scan("fun a,b end"), {',',{0,12},",",undefined}, scan("             b")},
                   ?SUT:take_until_token(scan("fun a,b end, b"), ',')),
     ?_assertEqual({scan("fun a/2"), {',',{0,8},",",undefined}, scan("         b")},
                   ?SUT:take_until_token(scan("fun a/2, b"), ',')),
     ?_assertEqual({scan("fun((1,2)->X)"), {',',{0,14},",",undefined}, scan("               b")},
                   ?SUT:take_until_token(scan("fun((1,2)->X), b"), ',')),
     ?_assertEqual({[], {dot,{0,1},".",undefined}, scan("  a")},
                   ?SUT:take_until_token(scan(". a"), dot)),
     ?_assertEqual({[], none, []},
                   ?SUT:take_until_token([], dot))
    ].

take_until_matching_token_test_() ->
    [
        ?_assertMatch({{'(',_,_,_},[],none,[]},
            ?SUT:take_until_matching_token({'(', 1,2,3}, [])),
        ?_assertMatch({{'(',_,_,_},[{atom,_,_,a},{',',_,_,_},{atom,_,_,b}], none, []},
            ?SUT:take_until_matching_token({'(', 1,2,3}, scan("a,b"))),
        ?_assertMatch({{'(',_,_,_},[{atom,_,_,a},{',',_,_,_},{'(',_,_,_},{atom,_,_,b}], none, []},
            ?SUT:take_until_matching_token({'(', 1,2,3}, scan("a,(b"))),
        ?_assertMatch({{'(',_,_,_}, [{atom,_,_,a},{',',_,_,_},{atom,_,_,b}], {')',_,_,_}, []},
            ?SUT:take_until_matching_token({'(', 1,2,3}, scan("a,b)"))),
        ?_assertMatch({{'if',_,_,_}, [], {'end',_,_,_}, []},
            ?SUT:take_until_matching_token({'if', 1,2,3}, scan("end"))),
        ?_assertMatch({{'(',_,_,_},[{'[',_,_,_},{']',_,_,_}],{')',_,_,_}, [{atom,_,_,a}]},
            ?SUT:take_until_matching_token({'(', 1,2,3}, scan("[])a"))),
        ?_assertMatch({{'(',_,_,_},[{'(',_,_,_},{')',_,_,_}],{')',_,_,_}, [{atom,_,_,a}]},
            ?SUT:take_until_matching_token({'(', 1,2,3}, scan("())a"))),
        ?_assertMatch({{'(',_,_,_},[{'(',_,_,_},{atom,_,_,b},{',',_,_,_},{atom,_,_,c},{')',_,_,_}],{')',_,_,_}, [{atom,_,_,a},{',',_,_,_}]},
            ?SUT:take_until_matching_token({'(', 1,2,3}, scan("(b,c))a,")))
    ].

split_at_token_test_() ->
    [
    ?_assertEqual([
                {scan("a"),none}
            ],
                ?SUT:split_at_token(scan("a"), dot)),
    ?_assertEqual([
                {scan("a"), {dot,{0,2},".",undefined}},
                {scan("   b"), none}
            ],
                ?SUT:split_at_token(scan("a. b"), dot)),
    ?_assertEqual([
                {scan("a"), {dot,{0,2},".",undefined}},
                {[{atom,{1,1},"b",b}], {dot,{1,2},".",undefined}}
            ],
                ?SUT:split_at_token(scan("a.\nb."), dot)),
    ?_assertEqual([
                {scan("a"), {dot,{0,2},".",undefined}},
                {scan("b", {0,4}), {dot,{0,5},".",undefined}}
            ],
                ?SUT:split_at_token(scan("a. b."), dot)),
    ?_assertEqual([{[], {dot,{0,1},".",undefined}}, {scan("  a"), none}],
                   ?SUT:split_at_token(scan(". a"), dot)),
    ?_assertEqual([{scan("a"), {',',{0,2},",",undefined}},
                    {scan("  {b,c}"), {',',{0,8},",",undefined}},
                    {scan("        d"), none}
                    ],
                ?SUT:split_at_token(scan("a,{b,c},d"), ',')),
    ?_assertMatch([],
                ?SUT:split_at_token([], dot))
    ].

middle_test_() ->
    [
        ?_assertMatch([y,z], ?SUT:middle([x,y,z,w])),
        ?_assertMatch([y], ?SUT:middle([x,y,z])),
        ?_assertMatch([], ?SUT:middle([x,y])),
        ?_assertMatch([], ?SUT:middle([x])),
        ?_assertMatch([], ?SUT:middle([]))
    ].

take_block_list_test_() ->
    [
        ?_assertEqual({[scan(" a")], scan("   b")},
            ?SUT:take_block_list(scan("(a)b"))
        ),
        ?_assertEqual({[scan("    a")], scan("          b")},
            ?SUT:take_block_list(scan("fun a end b"))
        ),
        ?_assertEqual({[scan(" (a)"),scan("     b")], scan("       c")},
            ?SUT:take_block_list(scan("((a),b)c"))
        )
    ].

scan(D) ->
    {ok, Ts, _} = sourcer_scan:string(D),
    sourcer_scan:filter_ws_tokens(Ts).

scan_ws(D) ->
    {ok, Ts, _} = sourcer_scan:string(D),
    Ts.

scan(D, P0) ->
    {ok, Ts, _} = sourcer_scan:string(D, P0),
    sourcer_scan:filter_ws_tokens(Ts).
