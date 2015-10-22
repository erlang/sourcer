-module(sourcer_parse_tests).

-include_lib("eunit/include/eunit.hrl").
-include("sourcer_parse.hrl").

split_at_dot_test_() ->
    [
     ?_assertMatch([[a,b,c]],
                   sourcer_parse:split_at_dot([a,b,c])),
     ?_assertMatch([[a],[b,c]],
                   sourcer_parse:split_at_dot([a,{dot,0},b,c])),
     ?_assertMatch([[a],[b,c]],
                   sourcer_parse:split_at_dot([a,{dot,0},b,c,{dot,0}])),
     ?_assertMatch([],
                   sourcer_parse:split_at_dot([]))
    ].

extract_top_comments_test_() ->
    [
     ?_assertMatch({[{comment,#{text:=<<"%a">>}},
                     {comment,#{text:=<<"%b">>}},
                     {comment,#{text:=<<"%%c">>}}],
                    [{atom,#{value:=hello}}]},
                   sourcer_parse:extract_top_comments(scan("%a\n%b\n%%c\nhello"))),
     ?_assertMatch({[{comment,#{text:=<<"%a">>}},
                     {comment,#{text:=<<"%b">>}},
                     {comment,#{text:=<<"%%c">>}}],
                    [{atom,#{value:=hello}}]},
                   sourcer_parse:extract_top_comments(scan("%a\n\n%b\n%%c\nhello")))
    ].

 split_at_semicolon_name_test_() ->
     [
      ?_assertMatch([[{atom, #{value:=a}},
                      {atom, #{value:=b}},
                      {atom, #{value:=c}}]],
                    sourcer_parse:split_at_semicolon_name([{atom, #{value=>a}},
                                                           {atom, #{value=>b}},
                                                           {atom, #{value=>c}}])),
     ?_assertMatch([[{atom, #{value:=a}},
                     {';',_},
                     {atom, #{value:=b}},
                     {',',_},
                     {atom, #{value:=c}}]],
                   sourcer_parse:split_at_semicolon_name(scan("a;b,c"))),
     ?_assertMatch([[{atom, #{value:=a}},{';',_},{atom, #{value:=a}},{',',_},{atom, #{value:=b}}]],
                   sourcer_parse:split_at_semicolon_name(scan("a;a,b"))),
     ?_assertMatch([[{atom, #{value:=a}},{'(',_},{atom, #{value:=b}},{')',_}],
                    [{atom, #{value:=a}},{'(',_},{atom, #{value:=e}},{')',_}]],
                   sourcer_parse:split_at_semicolon_name(scan("a(b);a(e)"))),
     ?_assertMatch([[{atom, #{value:=a}},{'(',_},{atom, #{value:=b}},{')',_},{';',_},
                     {atom, #{value:=c}},{'(',_},{atom, #{value:=d}},{')',_}]],
                   sourcer_parse:split_at_semicolon_name(scan("a(b);c(d)"))),
      ?_assertMatch([],
                    sourcer_parse:split_at_semicolon_name([]))
     ].

split_at_semicolon_test_() ->
    [
     ?_assertMatch([[{atom, #{value:=a}},{',',_},{atom, #{value:=b}},{',',_},{atom, #{value:=c}}]],
                   sourcer_parse:split_at_semicolon(scan("a,b,c"))),
     ?_assertMatch([[{atom, #{value:=a}},{';',_},{atom, #{value:=b}},{',',_},{atom, #{value:=c}}]],
                   sourcer_parse:split_at_semicolon(scan("a;b,c"))),
     ?_assertMatch([[{'(',_},{atom, #{value:=b}},{')',_},{atom, #{value:=zz}}],
                    [{'(',_},{atom, #{value:=e}},{')',_},{atom, #{value:=xx}}]],
                   sourcer_parse:split_at_semicolon(scan("(b)zz;(e)xx"))),
     ?_assertMatch([],
                   sourcer_parse:split_at_semicolon([]))
    ].

split_at_comma_test_() ->
    [
     ?_assertMatch([[{atom, #{value:=a}}],
                    [{atom,#{value:=b}},{white_space,_},{atom,#{value:=c}}],
                    [{atom,#{value:=d}}]],
                   sourcer_parse:split_at_comma(scan("a,b c,d"))),
     ?_assertMatch([[{atom,#{value:=a}}],
                    [{'(',_},{atom,#{value:=b}},{white_space,_},{atom,#{value:=c}},{',',_},{atom,#{value:=d}},{')',_}],
                    [{atom,#{value:=e}},{white_space,_},{atom,#{value:=f}}]],
                   sourcer_parse:split_at_comma(scan("a,(b c,d),e f"))),
     ?_assertMatch([[{atom,#{value:=a}}],
                    [{'[',_},{atom,#{value:=b}},{white_space,_},{atom,#{value:=c}},{',',_},{atom,#{value:=d}},{']',_}],
                    [{atom,#{value:=e}},{white_space,_},{atom,#{value:=f}}]],
                   sourcer_parse:split_at_comma(scan("a,[b c,d],e f"))),
     ?_assertMatch([],
                   sourcer_parse:split_at_comma([]))
    ].

split_at_brace_test_() ->
    [
     ?_assertMatch({[],
                    [{atom,#{value:=a}},{',',_},{atom,#{value:=b}},{white_space,_},{atom,#{value:=c}}]},
                   sourcer_parse:split_at_brace(scan("a,b c"))),
     ?_assertMatch({[{'(',_},{')',_}],
                    [{'->',_},{atom,#{value:=a}}]},
                   sourcer_parse:split_at_brace(scan("()->a"))),
     ?_assertMatch({[{'(',_},{atom,#{value:=a}},{white_space,_},{atom,#{value:=b}},{')',_}],
                    []},
                   sourcer_parse:split_at_brace(scan("(a b)"))),
     ?_assertMatch({[{'(',_},{atom,#{value:=a}},{white_space,_},{atom,#{value:=b}},{')',_}],
                    [{atom,#{value:=c}},{white_space,_},{atom,#{value:=d}}]},
                   sourcer_parse:split_at_brace(scan("(a b)c d"))),
     ?_assertMatch({[{'(',_},{atom,#{value:=a}},{'(',_},{atom,#{value:=b}},{white_space,_},
                     {atom,#{value:=c}},{')',_},{atom,#{value:=d}},{')',_}],
                    [{atom,#{value:=e}}]},
                   sourcer_parse:split_at_brace(scan("(a(b c)d)e"))),
     ?_assertMatch({[{'(',_},{atom,#{value:=a}},{',',_},{'[',_},{atom,#{value:=b}},{',',_},
                     {atom,#{value:=c}},{']',_},{',',_},{atom,#{value:=d}},{')',_}],
                    [{atom,#{value:=e}}]},
                   sourcer_parse:split_at_brace(scan("(a,[b,c],d)e"))),
     ?_assertMatch({[], []},
                   sourcer_parse:split_at_brace([]))
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
                   sourcer_parse:filter_tokens(Input))
    ].

-define(F(T), #{forms:=T}).
-define(FC(T,C), {#{forms:=T}, C}).

parse_string_test_() ->
    [
     ?_assertMatch({clause, _, [], [], [{atom,#{value:=a}}]},
                   sourcer_parse:parse_clause(scan("foo()->a"))),
     ?_assertMatch({clause,_,
                    [[{atom,#{value:=x}}],[{atom,#{value:=y}}]],
                    [],
                    [{atom,#{value:=a}},{',',_},{atom,#{value:=b}}]},
                   sourcer_parse:parse_clause(scan("foo(x,y)->a,b"))),
     ?_assertMatch(?F([{function,_,foo,0,
                      [{clause,_,[],[],[{atom,#{value:=ok}}]}]}]),
                   parse2("foo()->ok. ")),
     ?_assertMatch(?F([{function,_,foo,0,
                      [{clause,_,
                        [],
                        [{atom,#{value:=x}},{';',_},{atom,#{value:=y}},{',',_},{atom,#{value:=z}}],
                        [{atom,#{value:=ok}},{';',_},{atom,#{value:=ok}}]}]}]),
                   parse2("foo() when x;y,z->ok;ok. ")),
     ?_assertMatch(?F([{function,_,foo,1,
                      [{clause,_,
                        [[{atom,#{value:=x}}]],
                        [],
                        [{atom,#{value:=ok}}]},
                       {clause,_,
                        [[{atom,#{value:=y}}]],
                        [],
                        [{atom,#{value:=m}}]}]}]),
                   parse2("foo(x)->ok;foo(y)->m. ")),
     ?_assertMatch(?F([{record,_,z,[]}]),
                   parse2("-record(z,{}). ")),
     ?_assertMatch(?F([{record,_,z,
                      [{field,_,a,[],[]},
                       {field,_,b,[],[{atom,#{value:=i}}]},
                       {field,_,c,[{atom,#{value:=d}}],[]},
                       {field,_,e,[{atom,#{value:=f}}],[{atom,#{value:=g}}]}]}]),
                   parse2("-record(z,{a,b::i,c=d,e=f::g}). ")),
     ?_assertMatch(?F([{type,_,{atom,#{value:=i}},
                      [],
                      [{atom,#{value:=g}},{'(',_},{')',_}]}]),
                   parse2("-type i()::g(). ")),
     ?_assertMatch(?F([{type,_,{atom,#{value:=i}},
                      [[{var,#{value:='X'}}]],
                      [{'[',_},{var,#{value:='Y'}},{']',_}]}]),
                   parse2("-type i(X)::[Y]. ")),
     ?_assertMatch(?F([{opaque,_,{atom,#{value:=i}},
                      [],
                      [{atom,#{value:=g}},{'(',_},{')',_}]}]),
                   parse2("-opaque i()::g(). ")),
     ?_assertMatch(?F([{export, _, [[{atom,#{value:=a}},{'/',_},{integer,#{value:=2}}],
                                  [{atom,#{value:=b}},{'/',_},{integer,#{value:=3}}]]}]),
                   parse2("-export([a/2, b/3]). ")),
     ?_assertMatch(?F([{import,_,[{atom,#{value:=ss}}],
                      [[{atom,#{value:=a}},{'/',_},{integer,#{value:=2}}],
                       [{atom,#{value:=b}},{'/',_},{integer,#{value:=3}}]]}]),
                   parse2("-import(ss,[a/2, b/3]). ")),
     ?_assertMatch(?F([{spec,_,'$this$',{atom,#{value:=f}},
                      [{[[{atom,#{value:=a}}],
                         [{atom,#{value:=b}}]],
                        [{atom,#{value:=c}}]}]}]),
                   parse2("-spec f(a,b)->c. ")),
     ?_assertMatch(?F([{spec,_,'$this$',{atom,#{value:=f}},
                      [{[[{atom,#{value:=a}}],
                         [{atom,#{value:=b}}]],
                        [{atom,#{value:=c}}]},
                       {[[{atom,#{value:=d}}],
                         [{atom,#{value:=e}}]],
                        [{atom,#{value:=f}}]}]}]),
                   parse2("-spec f(a,b)->c;(d,e)->f. ")),
     ?_assertMatch(?F([{callback,_,{atom,#{value:=f}},
                      {[[{atom,#{value:=a}}],[{atom,#{value:=b}}]],
                       [{atom,#{value:=c}}]}}
                    ]),
                   parse2("-callback f(a,b)->c. ")),
     ?_assertMatch(?F([{unknown,_,[{var,#{value:='C'}}]}]),
                   parse2("C. ")),
     ?_assertMatch(?FC([{module,#{},x},
                       {spec,#{},
                        '$this$',{atom,#{value:=fun1}},
                        [{[],[{atom,#{value:=ok}}]}]},
                       {function,#{},fun1,0,
                        [{clause,_,[],[],
                          [{atom,#{value:=ok}}]}]}],
                       {[], []}),
                   parse("%%% module info\n"
                         "-module(x).\n"
                         "%fun1\n"
                         "-spec fun1()->'ok'.\n"
                         "%%\n"
                         "fun1() -> ok.\n"
                        )),
     ?_assertMatch({#{all:=[], forms:=[]},#context{}},
                   ok(sourcer_parse:string("", #context{})))
    ].

parse_context_test_() ->
    [
     ?_assertMatch(?FC([{define,_,{var, #{value:='X'}},2,
                      [{var,#{value:='A'}},{var,#{value:='B'}}],
                      [{var,#{value:='A'}}]}],
                    {['X'], []}),
                   parse("-define(X(A,B), A). ")),
     ?_assertMatch(?FC([{define,_,{var, #{value:='X'}},0,
                      [],
                      [{'[',_},{atom,#{value:=s}},{',',_}]}],
                    {['X'], []}),
                   parse("-define(X, [s,). ")),
     ?_assertMatch(?FC([{define,_,{var, #{value:='X'}}, 0, [],
                      [{'[',_},{atom,#{value:=s}},{',',_}]},
                     {undef,_,{var,#{value:='X'}}}],
                    {[], []}),
                   parse("-define(X, [s,). -undef(X). ")),
     ?_assertMatch(?FC([{ifdef,_,{var, #{value:='X'}}}],
                    {[], ['X']}),
                   parse("-ifdef(X). ")),
     ?_assertMatch(?FC([{ifndef,_,{var,#{value:='X'}}}],
                    {[], [{'not', 'X'}]}),
                   parse("-ifndef(X). ")),
     ?_assertMatch(?FC([{ifdef,_,{var,#{value:='X'}}}, {else,_}],
                    {[], [{'not','X'}]}),
                   parse("-ifdef(X). -else. ")),
     ?_assertMatch(?FC([{ifndef,_,{var,#{value:='X'}}}, {else,_}],
                    {[], ['X']}),
                   parse("-ifndef(X). -else. ")),
     ?_assertMatch(?FC([{ifdef,_,{var,#{value:='X'}}}, {else,_},{endif,_}],
                    {[], []}),
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
    {ok, Ts, _} = sourcer_scan:string(D),
    Ts.

parse2(S) ->
    {F, _} = ok(sourcer_parse:string(S, #context{})),
    F.

parse(S) ->
    {F, C} = ok(sourcer_parse:string(S, #context{})),
    {F, ctxt(C)}.

ctxt(#context{defines=D, active=A}) ->
    {sets:to_list(D), A}.

ok({ok, R, _}) ->
    R;
ok({ok, R}) ->
    R.
