%% coding: utf-8
%% Description: TODO: Add description to sourcer_scanner_tests
-module(sourcer_scan_tests).

%%
%% Include files
%%

-include("sourcer.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(D(X), begin Y=X, io:format("~p~n", [Y]), Y end).

tokens_test_() ->
    [?_assertMatch({ok, [{atom, #{line := 0, offset := 0, length := 1, text :="a"}}],
                    {0,2,1}},
                   test_scan("a")),
     ?_assertMatch({ok, [{var, #{line := 0, offset := 0, length := 1, text :="A"}}],
                    {0,2,1}},
                   test_scan("A")),
     ?_assertMatch({ok, [{macro, #{line := 0, offset := 0, length := 2, text :="?a"}}],
                    {0,3,2}},
                   test_scan("?a")),
     ?_assertMatch({ok, [{macro, #{line := 0, offset := 0, length := 2, text :="?A"}}],
                    {0,3,2}},
                   test_scan("?A")),
     ?_assertMatch({ok, [{macro_str, #{line := 0, offset := 0, length := 3, text :="??a"}}],
                    {0,4,3}},
                   test_scan("??a")),
     ?_assertMatch({ok, [{macro_str, #{line := 0, offset := 0, length := 3, text :="??A"}}],
                    {0,4,3}},
                   test_scan("??A")),
     ?_assertMatch({ok, [{white_space, #{line := 0, offset := 0, length := 1, text := <<" ">>}}],
                    {0,2,1}},
                   test_scan(" ")),
     ?_assertMatch({ok, [{string, #{line := 0, offset := 0, length := 4, text := "\" a\""}}],
                    {0,5,4}},
                   test_scan("\" a\"")),
     ?_assertMatch({ok, [{string, #{line := 0, offset := 0, length := 4, text := "\" a\""}}],
                    {0,5,4}},
                   test_scan("\" a")),
     ?_assertMatch({ok, [{string, #{line := 0, offset := 0, length := 5, text := "\"a\nb\""}}],
                    {1,3,5}},
                   test_scan("\"a\nb\"")),
     ?_assertMatch({ok, [{string, #{line := 0, offset := 0, length := 12, text := "\"z\\x{faca}z\""}}],
                    {0,13,12}},
                   test_scan("\"z\\x{faca}z\"")),
     ?_assertMatch({ok, [{string, #{line := 0, offset := 0, length := 4, text := "\"\\s\""}}],
                    {0,5,4}},
                   test_scan("\"\\s\"")),
     ?_assertMatch({ok, [{char, #{line := 0, offset := 0, length := 3, text := "$\\s"}}],
                    {0,4,3}},
                   test_scan("$\\s")),
     ?_assertMatch({ok, [{char, #{line := 0, offset := 0, length := 5, text := "$\\023"}}],
                    {0,6,5}},
                   test_scan("$\\023")),
     ?_assertMatch({ok, [{char, #{line := 0, offset := 0, length := 9, text := "$\\x{faca}"}}],
                    {0,10,9}},
                   test_scan("$\\x{faca}")),
     ?_assertMatch({ok, [{integer, #{line := 0, offset := 0, length := 1, text := "3"}}],
                    {0,2,1}},
                   test_scan("3")),
     ?_assertMatch({ok, [{integer, #{line := 0, offset := 0, length := 3, text := "4#3"}}],
                    {0,4,3}},
                   test_scan("4#3")),
     ?_assertMatch({ok, [{integer, #{line := 0, offset := 0, length := 4, text := "16#f"}}],
                    {0,5,4}},
                   test_scan("16#f")),
     ?_assertMatch({ok, [{atom, #{line := 0, offset := 0, length := 5, text := "'try'"}}],
                    {0,6,5}},
                   test_scan("'try'")),
     ?_assertMatch({ok, [{comment, #{line := 0, offset := 0, length := 3, text := <<"% h">>}}],
                    {0,4,3}},
                   test_scan("% h")),
     ?_assertMatch({ok, [{comment, #{line := 0, offset := 0, length := 3, text := <<"% h">>}},
                         {white_space, _}],
                    {1,1,4}},
                   test_scan("% h\n")),
     ?_assertMatch({ok, [{float, #{line := 0, offset := 0, length := 3, text := "2.5"}}],
                    {0,4,3}},
                   test_scan("2.5")),
     ?_assertMatch({ok, [{float, #{line := 0, offset := 0, length := 5, text := "2.5e3"}}],
                    {0,6,5}},
                   test_scan("2.5e3")),
     ?_assertMatch({ok, [{float, #{line := 0, offset := 0, length := 6, text := "2.5e-3"}}],
                    {0,7,6}},
                   test_scan("2.5e-3")),
     ?_assertMatch({ok, [{dot, #{line := 0, offset := 0, length := 1, text := "."}}],
                    {0,2,1}},
                   test_scan(".")),
     ?_assertMatch({ok, [{'try', #{line := 0, offset := 0, length := 3, text := "try"}}],
                    {0,4,3}},
                   test_scan("try")),
     ?_assertMatch({error, {{0,1},erl_scan_local,{base,99}},{0,3}},
                   test_scan("99#3")),
     ?_assertMatch({error, {{0,2},erl_scan_local,{illegal,character}},{0,8}},
                   test_scan("\"\\x{aaa"))
    ].

scanner_test_() ->
    [?_assertMatch({ok, [{atom, #{line := 0, offset := 0, length := 1, text :="a"}},
                         {'(', #{line := 0, column := 2, offset := 1, length := 1, text :="("}}],
                    {0,3,2}},
                   test_scan("a(")),
     ?_assertMatch({ok, [{float, #{line := 0, offset := 0, length := 3, text := "2.5"}},
                         {dot, #{line := 0, column := 4, offset := 3, length := 1, text := "."}}],
                    {0,5,4}},
                   test_scan("2.5.")),
     ?_assertMatch({ok, [{atom, #{line := 0, offset := 0, length := 1, text :="a"}},
                         {white_space, #{line := 0, offset := 1, length := 1, text := <<" ">>}},
                         {'(', #{line := 0, offset := 2, length := 1, text :="("}}],
                    {0,4,3}},
                   test_scan("a (")),
     ?_assertMatch({ok, [{atom, #{line := 0, offset := 0, length := 1, text :="a"}},
                         {white_space, #{line := 0, offset := 1, length := 1, text := <<"\n">>}},
                         {'(', #{line := 1, offset := 2, length := 1, text :="("}}],
                    {1,2,3}},
                   test_scan("a\n(")),
     ?_assertMatch({ok, [{atom, #{line := 0, offset := 0, length := 1, text := "a"}},
                         {white_space, #{line := 0, offset := 1, length := 2, text := <<"\n ">>}},
                         {'(', #{line := 1, offset := 3, length := 1, text := "("}}],
                    {1,3,4}},
                   test_scan("a\n (")),
     ?_assertMatch({ok, [{atom, #{line := 0, offset := 0, length := 1, text :="a"}},
                         {white_space, #{line := 0, offset := 1, length := 1, text := <<" ">>}},
                         {macro, #{line := 0, offset := 2, length := 2, text := "?a"}},
                         {white_space, #{line := 0, offset := 4, length := 1, text := <<" ">>}},
                         {atom, #{line := 0, offset := 5, length := 1, text := "a"}}],
                    {0,7,6}},
                   test_scan("a ?a a")),
     ?_assertMatch({ok,[{atom,#{length := 1,line := 0,offset := 0,text := "a"}},
                        {white_space,#{length := 1,line := 0,offset := 1,text := <<" ">>}},
                        {'->',#{length := 2,line := 0,offset := 2,text := "->"}},
                        {white_space,#{length := 1,line := 0,offset := 4,text := <<" ">>}},
                        {atom,#{length := 1,line := 0,offset := 5,text := "b"}},
                        {dot,#{length := 1,line := 0,offset := 6,text := "."}}],
                    {0,8,7}},
                   test_scan("a -> b.")),
     ?_assertMatch({ok, [{atom, #{line := 0, offset := 0, length := 1, text := "a"}},
                         {white_space, #{line := 0, offset := 1, length := 2, text := <<"\n ">>}},
                         {atom, #{line := 1, offset := 3, length := 1, text := "b"}},
                         {dot, #{line := 1, offset := 4, length := 2, text := ".\n"}},
                         {white_space, #{line := 2, offset := 6, length := 1, text := <<" ">>}},
                         {atom, #{line := 2, offset := 7, length := 1, text := "x"}}],
                    {2,3,8}},
                   test_scan("a\n b.\n x")),
     ?_assertMatch({ok, [{atom, #{line := 3, offset := 5, length := 1, text :="a"}}],
                    {3,5,6}},
                   test_scan("a", 3, 4, 5))
    ].

offset1_test() ->
    Str1 = "a ",
    Str2 = "b",
    {ok, T, {L,C,O}} = test_scan(Str1++Str2),
    {ok, T1, {L1,C1,O1}} = test_scan(Str1),
    {ok, T2, {L2,C2,O2}} = test_scan(Str2, L1, C1, O1),
    [?_assertMatch(T, T1++T2),
     ?_assertMatch({L,C,O}, {L2,C2,O2})].

offset2_test() ->
    Str1 = "a\n",
    Str2 = "b",
    {ok, T, {L,C,O}} = test_scan(Str1++Str2),
    {ok, T1, {L1,C1,O1}} = test_scan(Str1),
    {ok, T2, {L2,C2,O2}} = test_scan(Str2, L1, C1, O1),
    [?_assertMatch(T, T1++T2),
     ?_assertMatch({L,C,O}, {L2,C2,O2})].

offset_test_() ->
    [
     offset1_test(),
     offset2_test()
    ].

filter_test_() ->
    [
     ].

unicode_test_() ->
    [
     ?_assertMatch({ok, [{char,#{line := 0, offset := 0, length := 9, text := "$\\x{0206}"}}], {0,10,9}},
                   test_scan("$\\x{0206}")),
     ?_assertMatch({ok, [{string, #{line := 0, offset := 0, length := 10, text := "\"\\x{0206}\""}}], {0,11,10}},
                   test_scan("\"\\x{0206}\"")),
     ?_assertMatch({ok, [{char, #{line := 0, offset := 0, length := 2, text := "$Ȇ"}}], {0,3,2}},
                   test_scan("$Ȇ")),
     ?_assertMatch({ok, [{string, #{line := 0, offset := 0, length := 3, text := "\"Ȇ\""}}], {0,4,3}},
                   test_scan("\"Ȇ\"")),
     ?_assertMatch({ok, [{char, #{line := 0, offset := 0, length := 2, text := "$生"}}], {0,3,2}},
                   test_scan("$生")),
     ?_assertMatch({ok, [{string, #{line := 0, offset := 0, length := 3, text := "\"生\""}}], {0,4,3}},
                   test_scan("\"生\"")),
     ?_assertMatch({ok, [{comment, #{line := 0, offset := 0, length := 2, text := <<"%生"/utf8>>}}], {0,3,2}},
                   test_scan("%生"))
    ].

multiline_string_test_() ->
    [?_assertMatch({ok, [{string, #{line := 0, offset := 0,length := 3, text := "\"b\""}},
                         {white_space, #{line := 0, offset := 3, length := 1, text := <<"\n">>}},
                         {string, #{line := 1, offset := 4,length := 3, text := "\"b\""}},
                         {dot, #{line := 1, offset := 7, length := 1, text := "."}}], {1,5,8}},
                   test_scan("\"b\"\n\"b\".")),
     ?_assertMatch({ok, [{string, #{line := 0, offset := 0,length := 5,text := "\"b\nb\""}},
                         {dot, #{line := 1, offset := 5, length := 1, text := "."}}], {1,4,6}},
                   test_scan("\"b\nb\"."))
    ].

string_test_() ->
    S = "hej 5",
    [
     ?_assertEqual(sourcer_scan:string(S, {0, 1, 0}), sourcer_scan:string(S))
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_scan(S) ->
    test_scan(S, 0, 1, 0).

test_scan(S, L, C, O) ->
    sourcer_scan:string(S, {L, C, O}).
