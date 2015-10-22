-module(sourcer_parse_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/sourcer_parse.hrl").

fix_macro_tokens_test_() ->
    {_, Y} = ok(sourcer_parse:scan("?a,?B")),
    X = hd(Y),
    [
     ?_assertMatch([
                    {macro,#{line:=0,column:=1,text:="?a"},'a'},
                    {',',_},
                    {macro,#{line:=0,column:=4,text:="?B"},'B'}
                   ],
                   sourcer_parse:fix_macro_tokens(X))
    ].

split_at_dot_test_() ->
    [
     ?_assertMatch([[{atom,_,a},{',',_},{atom,_,b},{',',_},{atom,_,c}]],
                   sourcer_parse:split_at_dot(scan("a,b,c"))),
     ?_assertMatch([[{atom,_,a}],[{atom,_,b},{',',_},{atom,_,c}]],
                   sourcer_parse:split_at_dot(scan("a. b,c"))),
     ?_assertMatch([[{atom,_,a}],[{atom,_,b},{',',_},{atom,_,c}]],
                   sourcer_parse:split_at_dot(scan("a. b,c."))),
     ?_assertMatch([],
                   sourcer_parse:split_at_dot(scan("")))
    ].

group_top_comments_test_() ->
    [
     ?_assertMatch([{comments,_,["a","b"],1},
                    {comments,_,["c"],2},
                    {atom,_,hello}],
                   sourcer_parse:group_top_comments(scan("%a\n%b\n%%c\nhello"))),
     ?_assertMatch([{comments,_,["a"],1},
                    {comments,_,["b"],1},
                    {atom,_,hello}],
                   sourcer_parse:group_top_comments(scan("%a\n\n%b\nhello")))
    ].

split_at_semicolon_name_test_() ->
    [
     ?_assertMatch([[{atom,_,a},{',',_},{atom,_,b},{',',_},{atom,_,c}]],
                   sourcer_parse:split_at_semicolon_name(scan("a,b,c"))),
     ?_assertMatch([[{atom,_,a},{';',_},{atom,_,b},{',',_},{atom,_,c}]],
                   sourcer_parse:split_at_semicolon_name(scan("a;b,c"))),
     ?_assertMatch([[{atom,_,a},{';',_},{atom,_,a},{',',_},{atom,_,b}]],
                   sourcer_parse:split_at_semicolon_name(scan("a;a,b"))),
     ?_assertMatch([[{atom,_,a},{'(',_},{atom,_,b},{')',_}],
                    [{atom,_,a},{'(',_},{atom,_,e},{')',_}]],
                   sourcer_parse:split_at_semicolon_name(scan("a(b);a(e)"))),
     ?_assertMatch([[{atom,_,a},{'(',_},{atom,_,b},{')',_},{';',_},
                     {atom,_,c},{'(',_},{atom,_,d},{')',_}]],
                   sourcer_parse:split_at_semicolon_name(scan("a(b);c(d)"))),
     ?_assertMatch([],
                   sourcer_parse:split_at_semicolon_name([]))
    ].

split_at_semicolon_test_() ->
    [
     ?_assertMatch([[{atom,_,a},{',',_},{atom,_,b},{',',_},{atom,_,c}]],
                   sourcer_parse:split_at_semicolon(scan("a,b,c"))),
     ?_assertMatch([[{atom,_,a},{';',_},{atom,_,b},{',',_},{atom,_,c}]],
                   sourcer_parse:split_at_semicolon(scan("a;b,c"))),
     ?_assertMatch([[{'(',_},{atom,_,b},{')',_},{atom,_,zz}],
                    [{'(',_},{atom,_,e},{')',_},{atom,_,xx}]],
                   sourcer_parse:split_at_semicolon(scan("(b)zz;(e)xx"))),
     ?_assertMatch([],
                   sourcer_parse:split_at_semicolon([]))
    ].

split_at_comma_test_() ->
    [
     ?_assertMatch([[{atom,_,a}],
                    [{atom,_,b},{white_space,_," "},{atom,_,c}],
                    [{atom,_,d}]],
                   sourcer_parse:split_at_comma(scan("a,b c,d"))),
     ?_assertMatch([[{atom,_,a}],
                    [{'(',_},{atom,_,b},{white_space,_," "},{atom,_,c},{',',_},{atom,_,d},{')',_}],
                    [{atom,_,e},{white_space,_," "},{atom,_,f}]],
                   sourcer_parse:split_at_comma(scan("a,(b c,d),e f"))),
     ?_assertMatch([[{atom,_,a}],
                    [{'[',_},{atom,_,b},{white_space,_," "},{atom,_,c},{',',_},{atom,_,d},{']',_}],
                    [{atom,_,e},{white_space,_," "},{atom,_,f}]],
                   sourcer_parse:split_at_comma(scan("a,[b c,d],e f"))),
     ?_assertMatch([],
                   sourcer_parse:split_at_comma([]))
    ].

split_at_brace_test_() ->
    [
     ?_assertMatch({[],
                    [{atom,_,a},{',',_},{atom,_,b},{white_space,_," "},{atom,_,c}]},
                   sourcer_parse:split_at_brace(scan("a,b c"))),
     ?_assertMatch({[{'(',_},{')',_}],
                    [{'->',_},{atom,_,a}]},
                   sourcer_parse:split_at_brace(scan("()->a"))),
     ?_assertMatch({[{'(',_},{atom,_,a},{white_space,_," "},{atom,_,b},{')',_}],
                    []},
                   sourcer_parse:split_at_brace(scan("(a b)"))),
     ?_assertMatch({[{'(',_},{atom,_,a},{white_space,_," "},{atom,_,b},{')',_}],
                    [{atom,_,c},{white_space,_," "},{atom,_,d}]},
                   sourcer_parse:split_at_brace(scan("(a b)c d"))),
     ?_assertMatch({[{'(',_},{atom,_,a},{'(',_},{atom,_,b},{white_space,_," "},{atom,_,c},{')',_},{atom,_,d},{')',_}],
                    [{atom,_,e}]},
                   sourcer_parse:split_at_brace(scan("(a(b c)d)e"))),
     ?_assertMatch({[{'(',_},{atom,_,a},{',',_},{'[',_},{atom,_,b},{',',_},{atom,_,c},{']',_},{',',_},{atom,_,d},{')',_}],
                    [{atom,_,e}]},
                   sourcer_parse:split_at_brace(scan("(a,[b,c],d)e"))),
     ?_assertMatch({[], []},
                   sourcer_parse:split_at_brace([]))
    ].

convert_attributes_test_() ->
    Toks = [
            {'-',[{line,1},{column,1},{text,"-"}]},
            {atom,[{line,1},{column,2},{text,"module"}],module},
            {'(',[{line,2},{column,1},{text,"("}]},
            {atom,[{line,2},{column,2},{text,"asx"}],asx},
            {')',[{line,2},{column,5},{text,")"}]},
            {dot,[{line,2},{column,6},{text,"."}]}
           ],
    [
     ?_assertMatch({
                    [
                     {'-',#{line:=1,column:=1,text:="-",offset:=0}},
                     {atom,#{line:=1,column:=2,text:="module",offset:=1},module},
                     {'(',#{line:=2,column:=1,text:="(",offset:=7}},
                     {atom,#{line:=2,column:=2,text:="asx",offset:=8},asx},
                     {')',#{line:=2,column:=5,text:=")",offset:=11}},
                     {dot,#{line:=2,column:=6,text:=".",offset:= 12}}
                    ],
                    13
                   }, sourcer_parse:convert_attributes(Toks))
    ].

filter_tokens_test_() ->
    Input =     [{comment,[{line,1},{column,1},{text,"%%hej"}],"%%hej"},
                 {white_space,[{line,1},{column,6},{text,"\n"}],"\n"},
                 {atom,[{line,2},{column,1},{text,"foo"}],foo},
                 {'(',[{line,2},{column,4},{text,"("}]},
                 {')',[{line,2},{column,5},{text,")"}]},
                 {'->',[{line,2},{column,6},{text,"->"}]},
                 {white_space,[{line,2},{column,8},{text,"\n  "}],"\n  "},
                 {comment,[{line,3},{column,3},{text,"% cmt"}],"% cmt"},
                 {white_space,[{line,3},{column,8},{text,"\n  "}],"\n  "},
                 {atom,[{line,4},{column,3},{text,"ok"}],ok},
                 {dot,[{line,4},{column,5},{text,".\n"}]}],

    [
     ?_assertMatch([{atom,_,foo},{'(',_},{')',_},{'->',_},{atom,_,ok},{dot,_}],
                   sourcer_parse:filter_tokens(Input))
    ].

parse_string_test_() ->
    [
     ?_assertMatch({clause, _, [], [], [{atom,_,a}]},
                   sourcer_parse:parse_clause(scan("foo()->a"))),
     ?_assertMatch({clause,_,
                    [[{atom,_,x}],[{atom,_,y}]],
                    [],
                    [{atom,_,a},{',',_},{atom,_,b}]},
                   sourcer_parse:parse_clause(scan("foo(x,y)->a,b"))),
     ?_assertMatch({[{function,_,foo,0,
                      [{clause,_,[],[],[{atom,_,ok}]}]}],
                    #context{}},
                   ok(sourcer_parse:string("foo()->ok. "))),
     ?_assertMatch({[{function,_,foo,0,
                      [{clause,_,
                        [],
                        [{atom,_,x},{';',_},{atom,_,y},{',',_},{atom,_,z}],
                        [{atom,_,ok},{';',_},{atom,_,ok}]}]}],
                    #context{}},
                   ok(sourcer_parse:string("foo() when x;y,z->ok;ok. "))),
     ?_assertMatch({[{function,_,foo,1,
                      [{clause,_,
                        [[{atom,_,x}]],
                        [],
                        [{atom,_,ok}]},
                       {clause,_,
                        [[{atom,_,y}]],
                        [],
                        [{atom,_,m}]}]}],
                    #context{}},
                   ok(sourcer_parse:string("foo(x)->ok;foo(y)->m. "))),
     ?_assertMatch({[{record,_,z,[]}],
                    #context{}},
                   ok(sourcer_parse:string("-record(z,{}). "))),
     ?_assertMatch({[{record,_,z,
                      [{field,_,a,[],[]},
                       {field,_,b,[],[{atom,_,i}]},
                       {field,_,c,[{atom,_,d}],[]},
                       {field,_,e,[{atom,_,f}],[{atom,_,g}]}]}],
                    #context{}},
                   ok(sourcer_parse:string("-record(z,{a,b::i,c=d,e=f::g}). "))),
     ?_assertMatch({[{type,_,{atom,_,i},
                      [],
                      [{atom,_,g},{'(',_},{')',_}]}],
                    #context{}},
                   ok(sourcer_parse:string("-type i()::g(). "))),
     ?_assertMatch({[{type,_,{atom,_,i},
                      [[{var,_,'X'}]],
                      [{'[',_},{var,_,'X'},{']',_}]}],
                    #context{}},
                   ok(sourcer_parse:string("-type i(X)::[X]. "))),
     ?_assertMatch({[{opaque,_,{atom,_,i},
                      [],
                      [{atom,_,g},{'(',_},{')',_}]}],
                    #context{}},
                   ok(sourcer_parse:string("-opaque i()::g(). "))),
     ?_assertMatch({[{export, _, [[{atom,_,a},{'/',_},{integer,_,2}],
                                  [{atom,_,b},{'/',_},{integer,_,3}]]}],
                    #context{}},
                   ok(sourcer_parse:string("-export([a/2, b/3]). "))),
     ?_assertMatch({[{import,_,[{atom,_,ss}],
                      [[{atom,_,a},{'/',_},{integer,_,2}],
                       [{atom,_,b},{'/',_},{integer,_,3}]]}],
                    #context{}},
                   ok(sourcer_parse:string("-import(ss,[a/2, b/3]). "))),
     ?_assertMatch({[{spec,_,'$this$',{atom,_,f},
                      [{[[{atom,_,a}],
                         [{atom,_,b}]],
                        [{atom,_,c}]}]}],
                    #context{}},
                   ok(sourcer_parse:string("-spec f(a,b)->c. "))),
     ?_assertMatch({[{spec,_,'$this$',{atom,_,f},
                      [{[[{atom,_,a}],
                         [{atom,_,b}]],
                        [{atom,_,c}]},
                       {[[{atom,_,d}],
                         [{atom,_,e}]],
                        [{atom,_,f}]}]}],
                    #context{}},
                   ok(sourcer_parse:string("-spec f(a,b)->c;(d,e)->f. "))),
     ?_assertMatch({[{callback,_,{atom,_,f},
                      {[[{atom,_,a}],[{atom,_,b}]],
                       [{atom,_,c}]}}
                    ],
                    #context{}},
                   ok(sourcer_parse:string("-callback f(a,b)->c. "))),
     ?_assertMatch({[{module,#{comments:=[{comments,_,[" module info"],3}]},x},
                     {spec,#{comments:=[{comments,_,["fun1"],1}]},
                      '$this$',{atom,_,fun1},
                      [{[],[{atom,_,ok}]}]},
                     {function,#{comments:=[{comments,_,[[]],2}]},fun1,0,
                      [{clause,_,[],[],
                        [{atom,_,ok}]}]}],
                    {[], []}},
                   parse("%%% module info\n"
                         "-module(x).\n"
                         "%fun1\n"
                         "-spec fun1()->'ok'.\n"
                         "%%\n"
                         "fun1() -> ok.\n"
                        )),
     ?_assertMatch({[],#context{}},
                   ok(sourcer_parse:string("")))
    ].

parse_context_test_() ->
    [
     ?_assertMatch({[{define,_,{var,_,'X'},2,
                      [{var,_,'A'},{var,_,'B'}],
                      [{var,_,'A'}]}],
                    {[{var,_,'X'}], []}},
                   parse("-define(X(A,B), A). ")),
     ?_assertMatch({[{define,_,{var,_,'X'},0,
                      [],
                      [{'[',_},{atom,_,s},{',',_}]}],
                    {[{var,_,'X'}], []}},
                   parse("-define(X, [s,). ")),
     ?_assertMatch({[{define,_,{var,_,'X'}, 0, [],
                      [{'[',_},{atom,_,s},{',',_}]},
                     {undef,_,{var,_,'X'}}],
                    {[], []}},
                   parse("-define(X, [s,). -undef(X). ")),
     ?_assertMatch({[{ifdef,_,{var,_,'X'}}],
                    {[], [{var,_,'X'}]}},
                   parse("-ifdef(X). ")),
     ?_assertMatch({[{ifndef,_,{var,_,'X'}}],
                    {[], [{'not', {var,_,'X'}}]}},
                   parse("-ifndef(X). ")),
     ?_assertMatch({[{ifdef,_,{var,_,'X'}}, {else,_}],
                    {[], [{'not',{var,_,'X'}}]}},
                   parse("-ifdef(X). -else. ")),
     ?_assertMatch({[{ifndef,_,{var,_,'X'}}, {else,_}],
                    {[], [{var,_,'X'}]}},
                   parse("-ifndef(X). -else. ")),
     ?_assertMatch({[{ifdef,_,{var,_,'X'}}, {else,_},{endif,_}],
                    {[], []}},
                   parse("-ifdef(X). -else. -endif. "))
    ].

is_active_context_test_() ->
    [
     ?_assertEqual(true,
                   sourcer_parse:is_active_context(
                     #context{defines=sets:from_list([x]),
                              active=[]
                             })),
     ?_assertEqual(true,
                   sourcer_parse:is_active_context(
                     #context{defines=sets:from_list([x]),
                              active=[x]
                             })),
     ?_assertEqual(true,
                   sourcer_parse:is_active_context(
                     #context{defines=sets:from_list([{'not', x}]),
                              active=[]
                             })),
     ?_assertEqual(false,
                   sourcer_parse:is_active_context(
                     #context{defines=sets:from_list([{'not', x}]),
                              active=[x]
                             })),
     ?_assertEqual(false,
                   sourcer_parse:is_active_context(
                     #context{defines=sets:from_list([x]),
                              active=[{'not', x}]
                             })),
     ?_assertEqual(false,
                   sourcer_parse:is_active_context(
                     #context{defines=sets:from_list([]),
                              active=[x]
                             })),
     ?_assertEqual(true,
                   sourcer_parse:is_active_context(
                     #context{defines=sets:from_list([]),
                              active=[{'not',x}]
                             })),
     ?_assertEqual(false,
                   sourcer_parse:is_active_context(
                     #context{defines=sets:from_list([x]),
                              active=[{'not',x},x]
                             })),
     ?_assertEqual(true,
                   sourcer_parse:is_active_context(#context{}))
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%

scan(D) ->
    {ok, Ts, _} = erl_scan:string(D, {0,1}, [return,text]),
    Ts.

parse(S) ->
    {F, C} = ok(sourcer_parse:string(S)),
    {F, ctxt(C)}.

ctxt(#context{defines=D, active=A}) ->
    {sets:to_list(D), A}.

ok({ok, R}) ->
    R.
