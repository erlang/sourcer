%%% Parse top level structure of a source file. That is, function bodies 
%%% are kept as token lists and will get processed further on.

-module(sourcer_parse).

-export([parse/1]).

-define(DEBUG, 1).
-include("debug.hrl").

-spec parse([sourcer_scan:token()]) -> {ok, [any()]}.
parse(Tokens) ->
    Forms = split_forms(Tokens),
    Parsed = lists:flatten([parse_form(F) || F<-Forms]),
    {ok, Parsed}.

split_forms(Tokens) ->
    {A, B} = sourcer_parse_util:split_at_token(Tokens, dot),
    % TODO
    [A].

parse_form(Tokens) ->
    {NewTs, TopComments} = preprocess_form(Tokens),
    do_parse_form(NewTs, TopComments).

%% keep track of macro context
preprocess_form(Ts) ->
    {TopComments, Ts0} = extract_top_comments(Ts),
    ?D(Ts),
    Ts2 = sourcer_scan:filter_ws_comment_tokens(Ts),
    {Ts2, sourcer_parse_util:compact_newlines(TopComments)}.

extract_top_comments(Toks) ->
    extract_top_comments(Toks, []).

extract_top_comments([{comment, _, _, _}=C|Toks], Acc) ->
    extract_top_comments(Toks, [C|Acc]);
extract_top_comments([{white_space, _, _, "\n"++_}=C|Toks], Acc) ->
    extract_top_comments(Toks, [C|Acc]);
extract_top_comments([{white_space, _, _, _}|Toks], Acc) ->
    extract_top_comments(Toks, Acc);
extract_top_comments(Toks, Acc) ->
    {lists:reverse(Acc), Toks}.

compact_comments([]) ->
    undefined;
compact_comments(L) ->
    Level = sourcer_parse_util:comment_level(hd(L)),
    Fun = fun({comment, _, _, C}, Acc) -> [sourcer_parse_util:skip_percent(C)|Acc] end,
    {comments, element(2, hd(L)), lists:reverse(lists:foldl(Fun, [], L)), Level}.

do_parse_form(Toks, Comments) ->
    do_parse_form_1(Toks, compact_comments(Comments)).

do_parse_form_1([], Comments) ->
    [];
do_parse_form_1([{atom,_,_,_}|_]=Ts, Comments) ->
    parse_function(Ts, Comments);
do_parse_form_1([{'-',_,_,_}|_]=Ts, Comments) ->
    parse_attribute(Ts, Comments);
do_parse_form_1(Ts, Comments) ->
    parse_unknown(Ts, Comments).

parse_function([{atom, Pos, _, Name}|_]=Ts, Comments) ->
    Clauses = parse_clauses(Ts),
    Arity = length(element(3, hd(Clauses))),
    {function, Pos, Name, Arity, Clauses, Comments}.

parse_clauses(Ts) ->
    R = sourcer_parse_util:split_at_semicolon_name(Ts),
    [parse_clause(L) || L<-R].

parse_clause(Ts) ->
    [{_,P,_,_} | Toks] = Ts,
    {Args0, Rest} = sourcer_parse_util:split_at_token(Toks, '('),
    {Args,_} = sourcer_parse_util:split_at_token(sourcer_parse_util:middle(Args0), ','),
    {Guards0, Body} = sourcer_parse_util:split_at_token(Rest, '->'),
    Guards = case Guards0 of
                 [] ->
                     [];
                 [_|T] ->
                     T
             end,
    {clause, P, Args, Guards, Body}.

parse_attribute([{'-', Pos, _, _},{atom,_, _, 'ifdef'},{'(', _},Name,{')',_},{dot,_,_,_}], Comments) ->
    {ifdef, Pos, Name, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'ifndef'},{'(', _},Name,{')',_},{dot,_,_,_}], Comments) ->
    {ifndef, Pos, Name, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'else'},{dot,_,_,_}], Comments) ->
    {else, Pos, Comments}; 
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'endif'},{dot,_,_,_}], Comments) ->
    {endif, Pos, Comments}; 
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'undef'},{'(', _},Name,{')',_},{dot,_,_,_}], Comments) ->
    {undef, Pos, Name, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'define'}|Ts], Comments) ->
    [Name | Args0] = sourcer_parse_util:middle(Ts),
    {Args, Arity, Value} = case Args0 of
                               [{'(',_}|_] ->
                                   {A,B} = sourcer_util:split_at_brace(Args0),
                                   Args1 = [hd(X) || X<-sourcer_util:split_at_comma(sourcer_parse_util:middle(A))],
                                   {
                                    Args1, length(Args1),
                                    case B of [] -> []; _ -> tl(B) end
                                   };
                               _ ->
                                   {none, -1, tl(Args0)}
                           end,
    {define, Pos, Name, Arity, Args, Value, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'record'}|Ts], Comments) ->
    [{atom, _, _, Name}, {',', _} | Def] = sourcer_parse_util:middle(Ts),
    {record, Pos, Name, record_def(Def), Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'type'}|Ts], Comments) ->
    Ts1 = case hd(Ts) of
              {'(',_} ->
                  sourcer_parse_util:middle(Ts);
              _ ->
                  Ts
          end,
    {[Name|Args0], Def} = sourcer_parse_util:split_at(Ts1, '::'),
    Args = sourcer_util:split_at_comma(sourcer_parse_util:middle(Args0)),
    {type, Pos, Name, Args, Def, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'opaque'}|Ts], Comments) ->
    Ts1 = case hd(Ts) of
              {'(',_} ->
                  sourcer_parse_util:middle(Ts);
              _ ->
                  Ts
          end,
    {[Name|Args0], Def} = sourcer_parse_util:split_at(Ts1, '::'),
    Args = sourcer_parse_util:split_at_comma(sourcer_parse_util:middle(Args0)),
    {opaque, Pos, Name, Args, Def, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'spec'}|Ts], Comments) ->
    {M, F, Sigs} = parse_spec(Ts),
    {spec, Pos, M, F, Sigs, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'callback'}|Ts], Comments) ->
    {_, F, [Sigs]} = parse_spec(Ts),
    {callback, Pos, F, Sigs, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'export'}|Ts], Comments) ->
    Fs = sourcer_parse_util:split_at_comma(sourcer_util:middle(sourcer_parse_util:middle(Ts))),
    {export, Pos, Fs, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'export_type'}|Ts], Comments) ->
    Fs = sourcer_parse_util:split_at_comma(sourcer_util:middle(sourcer_parse_util:middle(Ts))),
    {export_type, Pos, Fs, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'import'}|Ts], Comments) ->
    {M, Fs0} = sourcer_parse_util:split_at(sourcer_parse_util:middle(Ts), ','),
    Fs = sourcer_parse_util:split_at_comma(sourcer_parse_util:middle(Fs0)),
    {import, Pos, M, Fs, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'module'},{'(',_,_,_},{atom,_, _, Name}|_], Comments) ->
    {module, Pos, Name, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'compile'}|Ts], Comments) ->
    {compile, Pos, sourcer_parse_util:middle(Ts), Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'vsn'}|Vsn], Comments) ->
    {vsn, Pos, sourcer_parse_util:middle(Vsn), Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'on_load'}|Ts], Comments) ->
    {on_load, Pos, sourcer_parse_util:middle(Ts), Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'behaviour'}|Ts], Comments) ->
    {behaviour, Pos, sourcer_parse_util:middle(Ts), Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'behavior'}|Ts], Comments) ->
    {behaviour, Pos, sourcer_parse_util:middle(Ts), Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'include'},{'(',_},{string,_, _, Str}|_], Comments) ->
    {include, Pos, Str, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, 'include_lib'},{'(',_},{string,_, _, Str}|_], Comments) ->
    {include_lib, Pos, Str, Comments};
parse_attribute([{'-', Pos, _, _},{atom,_, _, Name}|Ts], Comments) ->
    {attribute, Pos, Name, Ts, Comments}.

parse_unknown([H|_]=Ts, Comments) ->
    {unknown, element(2, H), Ts, Comments}.

record_def(Ts) ->
    Fields = sourcer_parse_util:split_at_comma(sourcer_parse_util:middle(Ts)),
    Fun = fun([{atom,Pos=_, _, Name}|TypeDef]) ->
                  Def0 = case TypeDef of
                             [{'=',_}|_] ->
                                 tl(TypeDef);
                             _ ->
                                 TypeDef
                         end,
                  {Type, Def} = sourcer_parse_util:split_at(Def0, '::'),
                  {field, Pos, Name, Type, Def}
          end,
    lists:map(Fun, Fields).

parse_spec(Ts) ->
    {H, Rest} = sourcer_parse_util:split_at(Ts, '('),
    {M, F} = case sourcer_parse_util:split_at(H, ':') of
                 {[Fx], []} ->
                     {'$this$', Fx};
                 {[Mx], [Fx]} ->
                     {Mx, Fx}
             end,
    Cls = sourcer_parse_util:split_at_semicolon([{'(',1}|Rest]),
    Fun = fun(X) ->
                  {Args0,Return} = sourcer_parse_util:split_at_brace(X),
                  Args = sourcer_parse_util:split_at_comma(sourcer_parse_util:middle(Args0)),
                  {Args, tl(Return)}
          end,
    Sigs = lists:map(Fun, Cls),
    {M, F, Sigs}.

predef_macros(Module, File) ->
    Machine = list_to_atom(erlang:system_info(machine)),
    Anno = #{line=>1},
    [
     {'FILE',-1, [], [{string, Anno#{value=>File}}]},
     {'LINE',-1, [], [{integer, Anno#{value=>1}}]},
     {'MODULE', -1, [], [{atom, Anno#{value=>Module}}]},
     {'MODULE_STRING', -1, [], [{string, Anno#{value=>atom_to_list(Module)}}]},
     {'MACHINE',-1, [], [{atom, Anno#{value=>Machine}}]},
     {Machine,-1, [], [{atom, Anno#{value=>true}}]}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

extract_top_comments_test_() ->
    [
     ?_assertMatch({[{comment,_,"%a",_},
                     {white_space,_,_,_},
                     {comment,_,"%b",_},
                     {white_space,_,_,_},
                     {comment,_,"%%c",_},
                     {white_space,_,_,_}],
                    [{atom,_,_,hello}]},
                   extract_top_comments(scan("%a\n%b\n  %%c\nhello"))),
     ?_assertMatch({[{comment,_,"%a",_},
                     {white_space,_,"\n",_},
                     {white_space,_,_,_},
                     {comment,_,"%b",_},
                     {white_space,_,_,_},
                     {white_space,_,_,_},
                     {white_space,_,_,_},
                     {comment,_,"%%c",_},
                     {white_space,_,_,_}],
                    [{atom,_,_,hello}]},
                   extract_top_comments(scan("%a\n\n%b\n\n\n%%c\nhello")))
    ].

parse1(String) ->
    {ok, Tokens, _} = sourcer_scan:string(String),
    parse_form(Tokens).

parse_form_test_() ->
    [
        ?_assertMatch({
            function, {0,1}, f, 0, [
                {clause, _, [], [], [{atom,_, _, a}|_]}
            ], undefined
        }, parse1("f()->a.")),
        ?_assertMatch({
            function, {0,1}, f, 0, [
                {clause, _, [], [_], [{atom,_, _, a}|_]},
                {clause, _, [], [_], [{atom,_, _, b}|_]}
            ], undefined
        }, parse1("f(X)->a;f(Y)->b.")),
        ?_assertMatch({
            module, {0,1}, x, undefined
        }, parse1("-module(x).")),
        ?_assertMatch({
            endif, {0,1}, undefined
        }, parse1("-endif.")),
        ?_assertMatch({
            endif, {1,1}, {comments, {0, 1}, ["hi"], 1}
        }, parse1("%hi\n-endif.")),
        ?_assertMatch({
            unknown, {0,1}, _, undefined
        }, parse1("34."))
    ].

parse_test_x() ->
    [
        ?_assertMatch([
            {module, {0,1}, x, undefined},
            {define, {0,1}, 'X', []}
        ], parse(scan("-module(x). -define(X,true).")))
    ].
    
-ifdef(OLD).

has_macros_test_() ->
    [
     ?_assertEqual(false,
                   has_macros_1([{atom, 0}])),
     ?_assertEqual(true,
                   has_macros_1([{atom, 0},{macro, 1}])),
     ?_assertEqual(false,
                   has_macros_1([]))
    ].

detect_macro_arity_test_() ->
    [
     ?_assertEqual(-1, detect_macro_arity(scan("hello"))),
     ?_assertEqual(0, detect_macro_arity(scan("()"))),
     ?_assertEqual(1, detect_macro_arity(scan("(hello)"))),
     ?_assertEqual(2, detect_macro_arity(scan("(hello,h(1,2))")))
    ].

get_macro_def_test_() ->
    [
     ?_assertMatch([],
                   get_macro_def({macro, #{value=>z}}, -1,
                                 #context{macros=[{x, -1, none, [{atom, r}]}]})),
     ?_assertMatch([{z, -1, none, [{atom, r}]}],
                   get_macro_def({macro, #{value=>z}}, -1,
                                 #context{macros=[{z, -1, none, [{atom, r}]}]})),
     ?_assertMatch([{z, -1, _, [{atom, r}]}],
                   get_macro_def({macro, #{value=>z}}, 1,
                                 #context{macros=[{z, -1, none, [{atom, r}]}]})),
     ?_assertMatch([],
                   get_macro_def({macro, #{value=>z}}, 1,
                                 #context{macros=[{z, 2, none, [{atom, r}]}]})),
     ?_assertMatch([{z, -1, _, [{atom, r}]},{z, 1, _, [{atom, r}]}],
                   get_macro_def({macro, #{value=>z}}, 1,
                                 #context{macros=[{z, -1, none, [{atom, r}]}, {z, 1, [], [{atom, r}]}]})),
     ?_assertMatch([],
                   get_macro_def({macro, #{value=>z}}, -1,
                                 #context{}))
    ].

one_step_expand_test_() ->
    [
     ].

expand_macros_test_() ->
    [
     ?_assertMatch([{atom, 0}, {macro, _, _, x}}, {atom, 1}],
                   expand_macros([{atom, 0}, {macro, #{value=>x}}, {atom, 1}],
                                 #context{})),
     ?_assertMatch([{atom, 0}, {macro, _, _, x}}, {atom, 1}],
                   expand_macros([{atom, 0}, {macro, #{value=>x}}, {atom, 1}],
                                 #context{macros=[{z, -1, none, [{atom, r}]}]})),
     ?_assertMatch([{atom, 0}, {atom, r}, {atom, 1}],
                   expand_macros([{atom, 0}, {macro, #{value=>x}}, {atom, 1}],
                                 #context{macros=[{x, -1, none, [{atom, r}]}]})),
     ?_assertMatch([{atom, 0}, {atom, r}, {atom, q}, {atom, 1}],
                   expand_macros([{atom, 0}, {macro, #{value=>x}}, {atom, 1}],
                                 #context{macros=[{x, -1, none, [{atom, r}, {atom, q}]}]})),
     ?_assertMatch([{atom, 0}, {atom, 1}],
                   expand_macros([{atom, 0}, {atom, 1}],
                                 #context{})),
     ?_assertMatch([],
                   expand_macros([],
                                 #context{}))
    ].

preprocess_test_() ->
    %% TODO convert tokens !!!
    [
     ?_assertMatch([[{'-',_, _, _}, 
                     {atom,_, _, module}},
                     {'(',_ _, _},
                     {atom,_, _, mod}},
                     {')',_ _, _}],
                    [{string,_, _, "test1.erl"}},
                     {integer,_, _, 1}},
                     {atom,_, _, mod}},
                     {string,_, _, "mod"}}]],
                   preprocess("-module(mod). ?FILE ?LINE ?MODULE ?MODULE_STRING.\n",
                              #context{macros=predef_macros(mod, "test1.erl")})),
     ?_assertMatch([[{atom,_, _, 'BEAM'}},{atom,_, _, true}}]],
                   preprocess("?MACHINE ?BEAM.\n",
                              #context{macros=predef_macros(mod, "test1.erl")})),
     ?_assertMatch([_,[{atom,_, _, a}},{atom,_, _, a}},{atom,_, _, a}},{'(',_ _, _},{')',_ _, _}],_],
                   preprocess("-define(A, a). ?A ?'A' ?A(). -undef(A).\n")),
     ?_assertMatch([_, _, [{atom,_, _, a}},{atom,_, _, b}}], _],
                   preprocess("-define(a, a). -define(a(), b). ?a ?a(). -undef(a).\n")),
     ?_assertMatch([_, _, [{atom,_, _, a}}],_, _],
                   preprocess("-define(A, a). -define(B, ?A). ?B. -undef(A). -undef(B).\n")),
     ?_assertMatch([_,[{atom,_, _, zx}}],_],
                   preprocess("-define(z, zx). ?z. -undef(z).\n")),
     ?_assertMatch([_, [{atom,_, _, x}},{atom,_, _, x}}],_],
                   preprocess("-define('Z', x). ?'Z' ?Z. -undef('Z').\n")),
     ?_assertMatch([_, [{atom,_, _, a1}},
                        {'+',_},
                        {atom,_, _, a}},
                        {'+',_},
                        {integer,_, _, 2}}],
                    _],
                   preprocess("-define(A(X,Y), X+Y). ?A(a1,a+2). -undef(A).\n")),
     ?_assertMatch([_, [{atom,_, _, a1}},
                        {atom,_, _, a}},
                        {'(',_},
                        {')',_},
                        {'+',_},
                        {integer,_, _, 1}}],
                    _],
                   preprocess("-define(A(X), X). ?A(a1) ?A(a()+1). -undef(A).\n")),
     ?_assertMatch([_, [{string,_, _, "?"}}], _],
                   preprocess("-define(as(X), ??X). ?as(v). -undef(as).\n")),
%%      ?_assertMatch([_, [{error,{_,{redefine,'A'}}}], _],
%%                    preprocess("-define(A(X), X). -define(A(Y), Y). -undef(A).\n")),
%%      ?_assertMatch([{integer,_,1},{'{',_},{integer,_,2},{dot,_}],
%%                    preprocess("-define(A(Q,W,E), Q{W). ?A(1,2,3). -undef(A).\n")),
%%      ?_assertMatch([{atom,_,v},{dot,_}],
%%                    preprocess("-define(A(X), X). -define(B(X), ?A(v)). ?B(z). -undef(A). -undef(B).\n")),
%%      ?_assertMatch([{error,{_,{circular,'C',1}}}],
%%                    preprocess("-define(C(X), ?D(X)). -define(D(X), ?C(X)). ?C(0). -undef(C). -undef(D).\n")),
%%      ?_assertMatch([{error,{_,{undefined,'Z',none}}}],
%%                    preprocess("?Z.\n")),
%%      ?_assertMatch([{error,{_,{undefined,'Z',2}}}],
%%                    preprocess("?Z(3,4).\n")),
%%      ?_assertMatch([{error,{_,missing_parenthesis}},{atom,_,x},{dot,_}],
%%                    preprocess("-define(X, x)0. -undef(X). x.\n")),
%%      ?_assertMatch([{error,{_,{arg_error,'A'}}}],
%%                    preprocess("-define(A(X), X). ?A(f[). -undef(A).\n")),
%%      ?_assertMatch([{error,{_,{undefined,'A',5}}}],
%%                    preprocess("?A(1,2,3,4,5).\n")),
%%      ?_assertMatch([{error,{_,{redefine_predef,'LINE'}}},{atom,_,x},{dot,_}],
%%                    preprocess("-define(LINE, 1). x.\n")),
%%      ?_assertMatch([{error,{_,{bad,define}}},{atom,_,x},{dot,_}],
%%                    preprocess("-define{X, 1}. x.\n")),
%%      ?_assertMatch([{error,{_,{bad,define}}},{atom,_,x},{dot,_}],
%%                    preprocess("-define(W(A, A), A). x.\n")),
%%      ?_assertMatch([{error,{_,{undefined,'I',none}}}],
%%                    preprocess("-define(F, ?G). -define(G, ?H). -define(H, ?I). ?F. -undef(F). -undef(G). -undef(H).\n")),
%%      ?_assertMatch([{integer,_,3},{dot,_}],
%%                    preprocess("-define(F, ?G). -define(G, ?H). -define(H, ?I). -define(I, 3). ?F. -undef(F). -undef(G). -undef(H). -undef(I).\n")),
%%      ?_assertMatch([{error,{_,{call,"?'?'"}}}],
%%                    preprocess("??A.\n")),
     ?_assertMatch([],
                   preprocess("\n"))
    ].

-endif.

%%%%%%%

scan(D) ->
    {ok, Ts, _} = sourcer_scan:string(D),
    Ts.

preprocess(S) ->
    {ok, R, _} = sourcer_scan:string(S),
    R2 = sourcer_util:filter_tokens(R),
    R3 = sourcer_parse_util:split_at_token(R2, dot),
    {R4,_} = lists:map(fun do_preprocess/1, R3),
    %%     ?debugVal(R4),
    R4.

do_preprocess(L) ->
    {NewTs, _} = preprocess_form(L),
    NewTs.

-endif.



