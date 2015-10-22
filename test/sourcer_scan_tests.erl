%% coding: utf-8
%% Description: TODO: Add description to sourcer_scanner_tests
-module(sourcer_scan_tests).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
-include("sourcer_token.hrl").

-define(D(X), begin Y=X, io:format("~p~n", [Y]), Y end).

%%
%% Exported Functions
%%

tokens_test_() ->
	[?_assertEqual({ok, [#token{kind = atom, attrs=#attrs{line = 0, offset = 0, length = 1, text="a"}, value = a}],
					{0,2,1}},
				   test_scan("a")),
	 ?_assertEqual({ok, [#token{kind = var, attrs=#attrs{line = 0, offset = 0, length = 1, text="A"}, value = 'A'}],
					{0,2,1}},
				   test_scan("A")),
	 ?_assertEqual({ok, [#token{kind = macro, attrs=#attrs{line = 0, offset = 0, length = 2, text="?a"}, value = '?a'}],
					{0,3,2}},
				   test_scan("?a")),
	 ?_assertEqual({ok, [#token{kind = macro, attrs=#attrs{line = 0, offset = 0, length = 2, text="?A"}, value = '?A'}],
					{0,3,2}},
				   test_scan("?A")),
	 ?_assertEqual({ok, [#token{kind = macro, attrs=#attrs{line = 0, offset = 0, length = 3, text="??a"}, value = '??a'}],
					{0,4,3}},
				   test_scan("??a")),
	 ?_assertEqual({ok, [#token{kind = macro, attrs=#attrs{line = 0, offset = 0, length = 3, text="??A"}, value = '??A'}],
					{0,4,3}},
				   test_scan("??A")),
	 ?_assertEqual({ok, [#token{kind = white_space, attrs=#attrs{line = 0, offset = 0, length = 1, text= <<" ">>}}],
					{0,2,1}},
				   test_scan(" ")),
	 ?_assertEqual({ok, [#token{kind = string, attrs=#attrs{line = 0, offset = 0, length = 4, text= "\" a\""}, value=" a"}],
					{0,5,4}},
				   test_scan("\" a\"")),
	 ?_assertEqual({ok, [#token{kind = string, attrs=#attrs{line = 0, offset = 0, length = 4, text= "\" a\""}, value=" a"}],
					{0,5,4}},
				   test_scan("\" a")),
	 ?_assertEqual({ok, [#token{kind = string, attrs=#attrs{line = 0, offset = 0, length = 5, text= "\"a\nb\""}, value="a\nb"}],
					{1,3,5}},
				   test_scan("\"a\nb\"")),
	 ?_assertEqual({ok, [#token{kind = string, attrs=#attrs{line = 0, offset = 0, length = 12, text= "\"z\\x{faca}z\""}, value="z\x{faca}z"}],
					{0,13,12}},
				   test_scan("\"z\\x{faca}z\"")),
	 ?_assertEqual({ok, [#token{kind = string, attrs=#attrs{line = 0, offset = 0, length = 4, text= "\"\\s\""}, value="\s"}],
					{0,5,4}},
				   test_scan("\"\\s\"")),
	 ?_assertEqual({ok, [#token{kind = char, attrs=#attrs{line = 0, offset = 0, length = 3, text= "$\\s"}, value=$\s}],
					{0,4,3}},
				   test_scan("$\\s")),
	 ?_assertEqual({ok, [#token{kind = char, attrs=#attrs{line = 0, offset = 0, length = 5, text= "$\\023"}, value=$\023}],
					{0,6,5}},
				   test_scan("$\\023")),
	 ?_assertEqual({ok, [#token{kind = char, attrs=#attrs{line = 0, offset = 0, length = 9, text= "$\\x{faca}"}, value=16#faca}],
					{0,10,9}},
				   test_scan("$\\x{faca}")),
	 ?_assertEqual({ok, [#token{kind = integer, attrs=#attrs{line = 0, offset = 0, length = 1, text= "3"}, value=3}],
					{0,2,1}},
				   test_scan("3")),
	 ?_assertEqual({ok, [#token{kind = integer, attrs=#attrs{line = 0, offset = 0, length = 3, text= "4#3"}, value=3}],
					{0,4,3}},
				   test_scan("4#3")),
	 ?_assertEqual({ok, [#token{kind = integer, attrs=#attrs{line = 0, offset = 0, length = 4, text= "16#f"}, value=16#f}],
					{0,5,4}},
				   test_scan("16#f")),
	 ?_assertEqual({ok, [#token{kind = atom, attrs=#attrs{line = 0, offset = 0, length = 5, text= "'try'"}, value='try'}],
					{0,6,5}},
				   test_scan("'try'")),
	 ?_assertEqual({ok, [#token{kind = comment, attrs=#attrs{line = 0, offset = 0, length = 3, text= <<"% h">>}}],
					{0,4,3}},
				   test_scan("% h")),
	 ?_assertEqual({ok, [#token{kind = float, attrs=#attrs{line = 0, offset = 0, length = 3, text= "2.5"}, value=2.5}],
					{0,4,3}},
				   test_scan("2.5")),
	 ?_assertEqual({ok, [#token{kind = float, attrs=#attrs{line = 0, offset = 0, length = 5, text= "2.5e3"}, value=2.5e3}],
					{0,6,5}},
				   test_scan("2.5e3")),
	 ?_assertEqual({ok, [#token{kind = float, attrs=#attrs{line = 0, offset = 0, length = 6, text= "2.5e-3"}, value=2.5e-3}],
					{0,7,6}},
				   test_scan("2.5e-3")),
	 ?_assertEqual({ok, [#token{kind = dot, attrs=#attrs{line = 0, offset = 0, length = 1, text= "."}}],
					{0,2,1}},
				   test_scan(".")),
	 ?_assertEqual({ok, [#token{kind = 'try', attrs=#attrs{line = 0, offset = 0, length = 3, text= "try"}}],
					{0,4,3}},
				   test_scan("try"))
	].

scanner_test_() ->
	[?_assertEqual({ok, [#token{kind = atom, attrs=#attrs{line = 0, offset = 0, length = 1, text="a"}, value = a},
						 #token{kind = '(', attrs=#attrs{line = 0, offset = 1, length = 1, text="("}}],
					{0,3,2}},
				   test_scan("a(")),
	 ?_assertEqual({ok, [#token{kind = float, attrs=#attrs{line = 0, offset = 0, length = 3, text= "2.5"}, value=2.5},
						 #token{kind = dot, attrs=#attrs{line = 0, offset = 3, length = 1, text= "."}}],
					{0,5,4}},
				   test_scan("2.5.")),
	 ?_assertEqual({ok, [#token{kind = atom, attrs=#attrs{line = 0, offset = 0, length = 1, text="a"}, value = a},
						 #token{kind = white_space, attrs=#attrs{line = 0, offset = 1, length = 1, text= <<" ">>}},
						 #token{kind = '(', attrs=#attrs{line = 0, offset = 2, length = 1, text="("}}],
					{0,4,3}},
				   test_scan("a (")),
	 ?_assertEqual({ok, [#token{kind = atom, attrs=#attrs{line = 0, offset = 0, length = 1, text="a"}, value = a},
						 #token{kind = white_space, attrs=#attrs{line = 0, offset = 1, length = 1, text= <<"\n">>}},
						 #token{kind = '(', attrs=#attrs{line = 1, offset = 2, length = 1, text="("}}],
					{1,2,3}},
				   test_scan("a\n(")),
	 ?_assertEqual({ok, [#token{kind = atom, attrs=#attrs{line = 0, offset = 0, length = 1, text="a"}, value = a},
						 #token{kind = white_space, attrs=#attrs{line = 0, offset = 1, length = 2, text= <<"\n ">>}},
						 #token{kind = '(', attrs=#attrs{line = 1, offset = 3, length = 1, text="("}}],
					{1,3,4}},
				   test_scan("a\n (")),
	 ?_assertEqual({ok, [#token{kind = atom, attrs=#attrs{line = 0, offset = 0, length = 1, text="a"}, value = a},
						 #token{kind = white_space, attrs=#attrs{line = 0, offset = 1, length = 1, text= <<" ">>}},
						 #token{kind = macro, attrs=#attrs{line = 0, offset = 2, length = 2, text="?a"}, value='?a'},
						 #token{kind = white_space, attrs=#attrs{line = 0, offset = 4, length = 1, text= <<" ">>}},
						 #token{kind = atom, attrs=#attrs{line = 0, offset = 5, length = 1, text="a"}, value = a}],
					{0,7,6}},
				   test_scan("a ?a a")),
	 ?_assertEqual({ok, [#token{kind = atom, attrs=#attrs{line = 0, offset = 0, length = 1, text="a"}, value = a},
						 #token{kind = '(', attrs=#attrs{line = 0, offset = 1, length = 1, text="("}},
						 #token{kind = ')', attrs=#attrs{line = 0, offset = 2, length = 1, text=")"}},
						 #token{kind = white_space, attrs=#attrs{line = 0, offset = 3, length = 1, text= <<" ">>}},
						 #token{kind = '->', attrs=#attrs{line = 0, offset = 4, length = 2, text="->"}},
						 #token{kind = white_space, attrs=#attrs{line = 0, offset = 6, length = 1, text= <<" ">>}},
						 #token{kind = atom, attrs=#attrs{line = 0, offset = 7, length = 1, text="b"}, value = b},
						 #token{kind = dot, attrs=#attrs{line = 0, offset = 8, length = 1, text = "."}}],
					{0,10,9}},
				   test_scan("a() -> b.")),
	 ?_assertEqual({ok, [#token{kind = atom, attrs=#attrs{line = 0, offset = 0, length = 1, text="a"}, value = a},
						 #token{kind = '(', attrs=#attrs{line = 0, offset = 1, length = 1, text="("}},
						 #token{kind = ')', attrs=#attrs{line = 0, offset = 2, length = 1, text=")"}},
						 #token{kind = white_space, attrs=#attrs{line = 0, offset = 3, length = 1, text= <<" ">>}},
						 #token{kind = '->', attrs=#attrs{line = 0, offset = 4, length = 2, text="->"}},
						 #token{kind = white_space, attrs=#attrs{line = 0, offset = 6, length = 2, text= <<"\n ">>}},
						 #token{kind = atom, attrs=#attrs{line = 1, offset = 8, length = 1, text="b"}, value = b},
						 #token{kind = dot, attrs=#attrs{line = 1, offset = 9, length = 1, text = "."}},
						 #token{kind = white_space, attrs=#attrs{line = 1, offset = 10, length = 1, text= <<"\n">>}},
						 #token{kind = white_space, attrs=#attrs{line = 2, offset = 11, length = 1, text= <<" ">>}},
						 #token{kind = atom, attrs=#attrs{line = 2, offset = 12, length = 1, text="x"}, value = x}],
					{2,3,13}},
				   test_scan("a() ->\n b.\n x")),
	 ?_assertEqual({ok, [#token{kind = atom, attrs=#attrs{line = 3, offset = 5, length = 1, text="a"}, value = a}],
					{3,5,6}},
				   test_scan("a", 3, 4, 5))
	].

offset1_test() ->
	Str1 = "a ",
	Str2 = "b",
	{ok, T, {L,C,O}} = test_scan(Str1++Str2),
	{ok, T1, {L1,C1,O1}} = test_scan(Str1),
	{ok, T2, {L2,C2,O2}} = test_scan(Str2, L1, C1, O1),
	[?_assertEqual(T, T1++T2),
	 ?_assertEqual({L,C,O}, {L2,C2,O2})].

offset2_test() ->
	Str1 = "a\n",
	Str2 = "b",
	{ok, T, {L,C,O}} = test_scan(Str1++Str2),
	{ok, T1, {L1,C1,O1}} = test_scan(Str1),
	{ok, T2, {L2,C2,O2}} = test_scan(Str2, L1, C1, O1),
	[?_assertEqual(T, T1++T2),
	 ?_assertEqual({L,C,O}, {L2,C2,O2})].

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
	 ?_assertMatch({ok, [{token,char,#attrs{line=0,offset=0,length=9,text="$\\x{0206}"},$\x{0206}}],{0,10,9}}, test_scan("$\\x{0206}")),
	 ?_assertMatch({ok,[{token,string,#attrs{line=0,offset=0,length=10,text="\"\\x{0206}\""},"\x{0206}"}],{0,11,10}}, test_scan("\"\\x{0206}\"")),
	 ?_assertMatch({ok,[{token,char,#attrs{line=0,offset=0,length=2,text="$Ȇ"},$Ȇ}],{0,3,2}}, test_scan("$Ȇ")),
	 ?_assertMatch({ok,[{token,string,#attrs{line=0,offset=0,length=3,text="\"Ȇ\""},"Ȇ"}],{0,4,3}}, test_scan("\"Ȇ\"")),
	 ?_assertMatch({ok,[{token,char,#attrs{line=0,offset=0,length=2,text="$生"},$生}],{0,3,2}}, test_scan("$生")),
	 ?_assertMatch({ok,[{token,string,#attrs{line=0,offset=0,length=3,text="\"生\""},"生"}],{0,4,3}}, test_scan("\"生\"")),
	 ?_assertMatch({ok,[{token,comment,#attrs{line=0,offset=0,length=2,text= <<"%生"/utf8>>},_}],{0,3,2}}, test_scan("%生"))
	].

multiline_string_test_() ->
	[?_assertEqual({ok, [#token{kind = string, attrs=#attrs{line = 0, offset = 0,length = 3, text = "\"b\""}, value = "b"},
						 #token{kind = white_space, attrs=#attrs{line = 0, offset = 3, length = 1, text= <<"\n">>}},
						 #token{kind = string, attrs=#attrs{line = 1, offset = 4,length = 3, text = "\"b\""}, value = "b"},
						 #token{kind = dot, attrs=#attrs{line = 1, offset = 7, length = 1, text = "."}}], {1,5,8}},
				   test_scan("\"b\"\n\"b\".")),
	 ?_assertEqual({ok, [#token{kind = string, attrs=#attrs{line = 0, offset = 0,length = 5,text = "\"b\nb\""}, value = "b\nb"},
						 #token{kind = dot, attrs=#attrs{line = 1, offset = 5, length = 1, text = "."}}], {1,4,6}},
				   test_scan("\"b\nb\"."))
	].

string_test_() ->
	S = "hej 5",
	[
	 ?_assertEqual(sourcer_scan:string(S, {0, 1, 0}), sourcer_scan:string(S))
	].

test_scan(S) ->
	test_scan(S, 0, 1, 0).

test_scan(S, L, C, O) ->
	sourcer_scan:string(S, {L, C, O}).

