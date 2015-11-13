-module(sourcer_parse_tests).

-include_lib("eunit/include/eunit.hrl").
-include("sourcer_parse.hrl").


parse_string_test_() ->
    [
     ?_assertMatch([{function,_,foo,0,
                     [{clause,_,[],[],[{atom,#{value:=ok}}]}]}],
                   parse2("foo()->ok. ")),
     ?_assertMatch([{function,_,foo,0,
                     [{clause,_,
                       [],
                       [{atom,#{value:=x}},{';',_},{atom,#{value:=y}},{',',_},{atom,#{value:=z}}],
                       [{atom,#{value:=ok}},{';',_},{atom,#{value:=ok}}]}]}],
                   parse2("foo() when x;y,z->ok;ok. ")),
     ?_assertMatch([{function,_,foo,1,
                     [{clause,_,
                       [[{atom,#{value:=x}}]],
                       [],
                       [{atom,#{value:=ok}}]},
                      {clause,_,
                       [[{atom,#{value:=y}}]],
                       [],
                       [{atom,#{value:=m}}]}]}],
                   parse2("foo(x)->ok;foo(y)->m. ")),
     ?_assertMatch([{record,_,z,[]}],
                   parse2("-record(z,{}). ")),
     ?_assertMatch([{record,_,z,
                     [{field,_,a,[],[]},
                      {field,_,b,[],[{atom,#{value:=i}}]},
                      {field,_,c,[{atom,#{value:=d}}],[]},
                      {field,_,e,[{atom,#{value:=f}}],[{atom,#{value:=g}}]}]}],
                   parse2("-record(z,{a,b::i,c=d,e=f::g}). ")),
     ?_assertMatch([{type,_,{atom,#{value:=i}},
                     [],
                     [{atom,#{value:=g}},{'(',_},{')',_}]}],
                   parse2("-type i()::g(). ")),
     ?_assertMatch([{type,_,{atom,#{value:=i}},
                     [[{var,#{value:='X'}}]],
                     [{'[',_},{var,#{value:='Y'}},{']',_}]}],
                   parse2("-type(i(X)::[Y]). ")),
     ?_assertMatch([{opaque,_,{atom,#{value:=i}},
                     [],
                     [{atom,#{value:=g}},{'(',_},{')',_}]}],
                   parse2("-opaque i()::g(). ")),
     ?_assertMatch([{opaque,_,{atom,#{value:=i}},
                     [],
                     [{atom,#{value:=g}},{'(',_},{')',_}]}],
                   parse2("-opaque(i()::g()). ")),
     ?_assertMatch([{export, _, [[{atom,#{value:=a}},{'/',_},{integer,#{value:=2}}],
                                 [{atom,#{value:=b}},{'/',_},{integer,#{value:=3}}]]}],
                   parse2("-export([a/2, b/3]). ")),
     ?_assertMatch([{import,_,[{atom,#{value:=ss}}],
                     [[{atom,#{value:=a}},{'/',_},{integer,#{value:=2}}],
                      [{atom,#{value:=b}},{'/',_},{integer,#{value:=3}}]]}],
                   parse2("-import(ss,[a/2, b/3]). ")),

     ?_assertMatch([{export_type, _, [_]}],
                   parse2("-export_type([s]). ")),
     ?_assertMatch([{compile, _, [{atom, #{value:=x}}]}],
                   parse2("-compile(x). ")),
     ?_assertMatch([{vsn, _, [{string, _}]}],
                   parse2("-vsn(\"d\"). ")),
     ?_assertMatch([{on_load, _, [_]}],
                   parse2("-on_load(d). ")),
     ?_assertMatch([{behaviour, _, _}],
                   parse2("-behaviour(x). ")),
     ?_assertMatch([{behaviour, _, _}],
                   parse2("-behavior(x). ")),
     ?_assertMatch([{attribute, _, asd, _}],
                   parse2("-asd(ff). ")),


     ?_assertMatch([{spec,_,'$this$',{atom,#{value:=f}},
                     [{[[{atom,#{value:=a}}],
                        [{atom,#{value:=b}}]],
                       [{atom,#{value:=c}}]}]}],
                   parse2(
                     "%%%%%fun1\n"
                     "-spec f(a,b)->c. ")),
     ?_assertMatch([{spec,_,'$this$',{atom,#{value:=f}},
                     [{[[{atom,#{value:=a}}],
                        [{atom,#{value:=b}}]],
                       [{atom,#{value:=c}}]},
                      {[[{atom,#{value:=d}}],
                        [{atom,#{value:=e}}]],
                       [{atom,#{value:=f}}]}]}],
                   parse2("-spec f(a,b)->c;(d,e)->f. ")),
     ?_assertMatch([{callback,_,{atom,#{value:=f}},
                     {[[{atom,#{value:=a}}],[{atom,#{value:=b}}]],
                      [{atom,#{value:=c}}]}}
                   ],
                   parse2(
                     "%%%%fun1\n"
                     "-callback f(a,b)->c. ")),
     ?_assertMatch([{unknown,_,[{var,#{value:='C'}}]}],
                   parse2("C. ")),
     ?_assertMatch({error, _, _},
                   sourcer_parse:string("$\\x{f", #context{})),
     ?_assertMatch({[{module,#{},x},
                     {spec,#{},
                      '$this$',{atom,#{value:=fun1}},
                      [{[],[{atom,#{value:=ok}}]}]},
                     {function,#{},fun1,0,
                      [{clause,_,[],[],
                        [{atom,#{value:=ok}}]}]}],
                    {[], []}},
                   parse("%%% module info\n"
                         "-module(x).\n"
                         "%fun1\n"
                         "-spec fun1()->'ok'.\n"
                         "%%\n"
                         "fun1() -> ok.\n"
                        )),
     ?_assertMatch({[],#context{}},
                   ok(sourcer_parse:string("", #context{})))
    ].

parse_context_test_() ->
    [
     ?_assertMatch({[{define,_,{var, #{value:='X'}},2,
                      [{var,#{value:='A'}},{var,#{value:='B'}}],
                      [{var,#{value:='A'}}]}],
                    {['X'], []}},
                   parse("-define(X(A,B), A). ")),
     ?_assertMatch({[{define,_,{var, #{value:='X'}},0,
                      [],
                      [{'[',_},{atom,#{value:=s}},{',',_}]}],
                    {['X'], []}},
                   parse("-define(X(), [s,). ")),
     ?_assertMatch({[{define,_,{var, #{value:='X'}}, -1, none,
                      [{'[',_},{atom,#{value:=s}},{',',_}]},
                     {undef,_,{var,#{value:='X'}}}],
                    {[], []}},
                   parse("-define(X, [s,). -undef(X). ")),
     ?_assertMatch({[{ifdef,_,{var, #{value:='X'}}}],
                    {[], ['X']}},
                   parse("-ifdef(X). ")),
     ?_assertMatch({[{ifndef,_,{var,#{value:='X'}}}],
                    {[], [{'not', 'X'}]}},
                   parse("-ifndef(X). ")),
     ?_assertMatch({[{ifdef,_,{var,#{value:='X'}}}, {else,_}],
                    {[], [{'not','X'}]}},
                   parse("-ifdef(X). -else. ")),
     ?_assertMatch({[{ifndef,_,{var,#{value:='X'}}}, {else,_}],
                    {[], ['X']}},
                   parse("-ifndef(X). -else. ")),
     ?_assertMatch({[{ifdef,_,{var,#{value:='X'}}}, {else,_},{endif,_}],
                    {[], []}},
                   parse("-ifdef(X). -else. -endif. "))
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%

parse2(S) ->
    {F, _} = ok(sourcer_parse:string(S, #context{})),
    F.

parse(S) ->
    {F, C} = ok(sourcer_parse:string(S, #context{})),
    {F, ctxt(C)}.

ctxt(#context{defines=D, active=A, macros=_M}) ->
    {sets:to_list(D), A}.

ok({ok, R, _}) ->
    R;
ok({ok, R}) ->
    R.
