-module(sourcer_scan_tests).

-include_lib("eunit/include/eunit.hrl").

-define(D(X), begin Y=X, io:format("~p~n", [Y]), Y end).

basic_tokens_test_() ->
    [
        same_as_erl_scan("a"),
        same_as_erl_scan("A"),
        same_as_erl_scan(" "),
        same_as_erl_scan("\" a\""),
        same_as_erl_scan("\"a\nb\""),
        same_as_erl_scan("\"z\\x{faca}z\""),
        same_as_erl_scan("\"\\s\""),
        same_as_erl_scan("$\\s"),
        same_as_erl_scan("$\\023"),
        same_as_erl_scan("$\\x{faca}"),
        same_as_erl_scan("3"),
        same_as_erl_scan("4#3"),
        same_as_erl_scan("16#f"),
        same_as_erl_scan("'try'"),
        same_as_erl_scan("% h"),
        same_as_erl_scan("2.5"),
        same_as_erl_scan("2.5e3"),
        same_as_erl_scan("2.5e-3"),
        same_as_erl_scan("."),
        same_as_erl_scan("try"),
        same_as_erl_scan("a("),
        same_as_erl_scan("2.5."),
        
        same_as_erl_scan("a ("),
        same_as_erl_scan("a\n("),
        same_as_erl_scan("a() -> b."),
        same_as_erl_scan("\"b\"\n\"b\"."),
        same_as_erl_scan("\"b\n  b\"."),

        same_as_erl_scan("$\\x{0206}"),
        same_as_erl_scan("\"\\x{0206}\""),
        same_as_erl_scan("$Ȇ"),
        same_as_erl_scan("\"Ȇ\""),
        same_as_erl_scan("$生"),
        same_as_erl_scan("\"生é\""),
        same_as_erl_scan("%生")
    ].

special_tokens_test_() ->
    [
     ?_assertMatch({ok, [
                        {white_space, {0,1}, "\n", "\n"},
                        {white_space, {1,1}, "\n", "\n"},
                        {white_space, {2,1}, "  ", "  "},
                        {atom, {2,3}, "a", 'a'}], 
                    {2,4}},
                   sourcer_scan:string("\n\n  a")),
     ?_assertMatch({ok, [{macro, {0,1}, "?a", '?a'}], {0,3}},
                   sourcer_scan:string("?a")),
     ?_assertMatch({ok, [{macro, {0,1}, "?A", '?A'}], {0,3}},
                   sourcer_scan:string("?A")),
     ?_assertMatch({ok, [{macro, {0,1}, "??a", '??a'}], {0,4}},
                   sourcer_scan:string("??a")),
     ?_assertMatch({ok, [{macro, {0,1}, "??A", '??A'}], {0,4}},
                   sourcer_scan:string("??A")),
     ?_assertMatch({ok, [{atom, {0,1}, "a", 'a'},
                         {white_space, {0,2}, " ", " "},
                         {macro, {0,3}, "?a", '?a'},
                         {white_space, {0,5}, " ", " "},
                         {atom, {0,6}, "a", 'a'}],
                    {0,7}},
                   sourcer_scan:string("a ?a a"))
    ].

dot_test_() ->
    [
        ?_assertMatch({ok, [{dot,{0,1},".",undefined},
                            {white_space,{0,2}," ",undefined}
                            ], {0,3}},
                        sourcer_scan:string(". "))
    ].

filter_test_() ->
    T = [
        {atom, 1, 2, a},
        {white_space, 1, 2, 3},
        {atom, 1, 2, b},
        {comment, 1, 2, 3},
        {atom, 1, 2, c}
    ],
    [
        ?_assertMatch([
                {atom, 1, 2, a},
                {atom, 1, 2, b},
                {atom, 1, 2, c}
            ], sourcer_scan:filter_tokens(T, [])),
        ?_assertMatch([
                {atom, 1, 2, a},
                {white_space, 1, 2, 3},
                {atom, 1, 2, b},
                {comment, 1, 2, 3},
                {atom, 1, 2, c}
            ], sourcer_scan:filter_tokens(T, [return])),
        ?_assertMatch([
                {atom, 1, 2, a},
                {atom, 1, 2, b},
                {comment, 1, 2, 3},
                {atom, 1, 2, c}
            ], sourcer_scan:filter_tokens(T, [return_comments])),
        ?_assertMatch([
                {atom, 1, 2, a},
                {white_space, 1, 2, 3},
                {atom, 1, 2, b},
                {atom, 1, 2, c}
            ], sourcer_scan:filter_tokens(T, [return_white_space])),
        ?_assertMatch([
                {atom, 1, 2, a},
                {white_space, 1, 2, 3},
                {atom, 1, 2, b},
                {comment, 1, 2, 3},
                {atom, 1, 2, c}
            ], sourcer_scan:filter_tokens(T, [return_white_space,return_comments]))
    ].

same_as_erl_scan(Text) ->
    {ok, Tokens0, Pos} = erl_scan:string(Text, {0, 1}, [return, text]),
    Tokens = lists:map(fun sourcer_scan:convert_token/1, Tokens0),
    ?_assertMatch(
        {ok, Tokens, Pos},
        sourcer_scan:string(Text)
    ).

line_info_test_() ->
    [
        ?_assertEqual([{0,0,0,""}], 
            sourcer_scan:line_info("")),
        ?_assertEqual([{0,0,3,""}], 
            sourcer_scan:line_info("aaa")),
        ?_assertEqual([{0,0,3,""},{1,4,3,""}], 
            sourcer_scan:line_info("aaa\nbbb")),
        ?_assertEqual([{0,0,3,""},{1,4,3,""}], 
            sourcer_scan:line_info("aaa\rbbb")),
        ?_assertEqual([{0,0,3,""},{1,5,3,""}], 
            sourcer_scan:line_info("aaa\r\nbbb")),
        ?_assertEqual([
            {0,0,0,""},
            {1,1,11,"        "},
            {2,13,2,""},
            {3,16,0,""},
            {4,17,11,"    \t  "},
            {5,29,0,""}
            ], 
        sourcer_scan:line_info("
        aaa
rr

    \t  last
")),
        ?_assertEqual([
            {0,0,0,""},
            {1,1,11,"        "},
            {2,14,2,""},
            {3,17,0,""},
            {4,18,11,"    \t  "},
            {5,30,0,""}
            ], 
        sourcer_scan:line_info("\n        aaa\r\nrr\r\r    \t  last\n"))
    ].

select_text(String, Lines) ->
    [sourcer_parse_util:get_line_text(String, L) || L<-Lines].

select_text(Text) ->
    select_text(Text, sourcer_scan:line_info(Text)).

line_info_2_test_() ->
    [
        ?_assertEqual([
            "",
            "        aaa",
            "rr",
            "",
            "    \t  last",
            ""
        ], select_text("
        aaa
rr

    \t  last
")),
        ?_assertEqual([
            "aa",
            "bb",
            "cc",
            "zz"
        ], select_text("aa\rbb\r\ncc\nzz"))
    ].
