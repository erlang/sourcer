%%% Copyright 2015 Vlad Dumitrescu
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

-module(sourcer_parse).

-export([
         string/2,
         tokens/2
        ]).

-include("sourcer.hrl").
-include("sourcer_parse.hrl").

-spec string(string(), context()) -> {'ok', {[sourcer:form()], context()}} | {'error', any()}.
string(D, Context) when is_list(D), is_record(Context, context) ->
    case sourcer_scan:string(D) of
        {ok, Toks, _} ->
            {ok, tokens(Toks, Context)};
        _Err ->
            _Err
    end.

-spec tokens(sourcer:tokens(), context()) -> {[sourcer:form()], context()}.
tokens(Toks, Context) ->
    Raw = split_at_dot(Toks),
    parse_forms(Raw, Context).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_forms(Toks, Context) ->
    lists:mapfoldl(fun parse_form/2, Context, Toks).

%% keep track of macro context
parse_form(Ts, Context) ->
    Ts0 = expand_macros(Ts, Context),
    {TopComments, Ts1} = extract_top_comments(Ts0),
    Ts2 = sourcer_util:filter_tokens(Ts1),
    {Form, Ctx} = do_parse_form(Ts2, Context, []),
    Form2 = update_attributes(Form,
                              #{active=>is_active_context(Context),
                                comments=>compact_comments(TopComments)}),
    {Form2, Ctx}.

update_attributes(Form, NewAttrs) ->
    Attrs0 = element(2, Form),
    Attrs1 = maps:merge(Attrs0, NewAttrs),
    setelement(2, Form, Attrs1).

extract_top_comments(Toks) ->
    extract_top_comments(Toks, []).

extract_top_comments([{comment, _}=C|Toks], Acc) ->
    extract_top_comments(Toks, [C|Acc]);
extract_top_comments([{white_space, _}|Toks], Acc) ->
    extract_top_comments(Toks, Acc);
extract_top_comments(Toks, Acc) ->
    {lists:reverse(Acc), Toks}.

%% top_comment(Comments) ->
%%     lists:filter(fun({comment,_})->true; (_)->false end, Comments).
%%
%% %% group comments with same level and no spaces inbetween
%% group_comments(C) ->
%%     case skip_white_at_start(C) of
%%         [] ->
%%             [];
%%         [H|_]=C1 ->
%%             Level = comment_level(H),
%%             {G, Rest} = get_first_group(Level, C1, []),
%%             [G] ++ group_comments(Rest)
%%     end.
%%
%% get_first_group(_, [], Acc) ->
%%     {lists:reverse(Acc), []};
%% get_first_group(Level, [H|T]=L, Acc) ->
%%     case comment_level(H) of
%%         Level ->
%%             get_first_group(Level, T, [H|Acc]);
%%         _ ->
%%             {lists:reverse(Acc), L}
%%     end.
%%
%% skip_white_at_start(L) ->
%%     lists:dropwhile(fun({white_space,_})->true; (_)->false end, L).

compact_comments([]) ->
    {comments, none, [], 0};
compact_comments(L) ->
    Level = comment_level(hd(L)),
    Fun = fun({comment, #{value:=C}}, Acc) -> [skip_percent(C)|Acc] end,
    {comments, element(2, hd(L)), lists:reverse(lists:foldl(Fun, [], L)), Level}.

comment_level({comment, #{value:="%%%%"++_}}) -> 4;
comment_level({comment, #{value:="%%%"++_}}) -> 3;
comment_level({comment, #{value:="%%"++_}}) -> 2;
comment_level({comment, #{value:="%"++_}}) -> 1.

skip_percent("%"++L) ->
    skip_percent(L);
skip_percent(L) ->
    L.

%% TODO includes and include_lib!
expand_macros(Tokens, Context) ->
    case one_step_expand(Tokens, Context) of
        {ok, Toks, _Ctx} ->
            Toks;
        {more, Toks, Ctx} ->
            expand_macros(Toks, Ctx)
    end.

one_step_expand(Tokens, Context) ->
    one_step_expand(Tokens, Context, [], false).

one_step_expand([], Ctx, R, true) ->
    {more, lists:reverse(R), Ctx};
one_step_expand([], Ctx, R, false) ->
    {ok, lists:reverse(R), Ctx};
one_step_expand([{macro,_}=H|T], Context, Acc, More) ->
    Arity = detect_macro_arity(T),
    Def = get_macro_def(H, Arity, Context),
    More2 = More orelse has_macros(Def),
    Repl = case Def of
               [] ->
                   [H];
               _ ->
                   {_,_,_,V}=hd(Def),
                   lists:reverse(V)
           end,
    one_step_expand(T, Context, Repl++Acc, More2);
one_step_expand([H|T], Context, Acc, More) ->
    one_step_expand(T, Context, [H|Acc], More).

get_macro_def({macro, #{name:=Name}}, Arity, #context{macros=M}) ->
    Pred = fun
              ({MyName, MyArity, _, _}) when MyName==Name, MyArity==-1;
                                             MyName==Name, MyArity==Arity ->
                   true;
              (_X) ->
                   false
           end,
    Defs = lists:filter(Pred, M),
    Defs.

has_macros(Def) ->
    lists:any(fun({macro, #{value:=V}})->has_macros_1(V); (_)-> false end, Def).

has_macros_1(Def) ->
    lists:any(fun({macro, _})->true; (_)-> false end, Def).

detect_macro_arity([{'(',_}|_]=Ts) ->
    M = sourcer_util:split_at_comma(sourcer_util:middle(Ts)),
    length(M);
detect_macro_arity(_) ->
    -1.

%% TODO set 'active' attribute on form, if
do_parse_form([{atom,_}|_]=Ts, Context, Comments) ->
    {parse_function(Ts, lists:reverse(Comments)), Context};
do_parse_form([{'-',_}|_]=Ts, Context, Comments) ->
    Attr = parse_attribute(Ts, lists:reverse(Comments)),
    NewContext = get_context(Attr, Context),
    {Attr, NewContext};
do_parse_form(Ts, Context, _Comments) ->
    {parse_unknown(Ts), Context}.

get_context({'define', _, {_,#{value:=Name}}, Arity, Args, Value}, #context{defines=Defs, macros=Macros}=Ctx) ->
    Ctx#context{defines=sets:add_element(Name, Defs),
                macros=[{Name, Arity, Args, Value}|Macros]};
get_context({'undef', _, {_, #{value:=Name}}}, #context{defines=Defs, macros=Macros}=Ctx) ->
    Ctx#context{defines=sets:del_element(Name, Defs),
                macros=lists:filter(fun({X, _, _, _})->
                                            X =/= Name
                                    end,
                                    Macros)};
get_context({'ifdef', _, {_,#{value:=Name}}}, #context{active=Active}=Ctx) ->
    Ctx#context{active=[Name|Active]};
get_context({'ifndef', _, {_,#{value:=Name}}}, #context{active=Active}=Ctx) ->
    Ctx#context{active=[{'not',Name}|Active]};
get_context({'else', _}, #context{active=[Crt|Active]}=Ctx) ->
    New = case Crt of
              {'not', X} ->
                  X;
              X ->
                  {'not', X}
          end,
    Ctx#context{active=[New|Active]};
get_context({'endif', _}, #context{active=[_|Active]}=Ctx) ->
    Ctx#context{active=Active};
get_context({'include', _, Name}, #context{included=Included}=Ctx) ->
    Ctx#context{included=[{include, Name} | Included]};
get_context({'include_lib', _, Name}, #context{included=Included}=Ctx) ->
    Ctx#context{included=[{include_lib, Name} | Included]};
get_context(_, Context) ->
    Context.

parse_unknown([H|_]=Ts) ->
    {unknown, element(2, H), Ts}.

parse_attribute([{'-', Pos},{atom,#{value:='ifdef'}},{'(', _},Name,{')',_}], Comments) ->
    {ifdef, Pos#{comments=>Comments}, Name};
parse_attribute([{'-', Pos},{atom,#{value:='ifndef'}},{'(', _},Name,{')',_}], Comments) ->
    {ifndef, Pos#{comments=>Comments}, Name};
parse_attribute([{'-', Pos},{atom,#{value:='else'}}], Comments) ->
    {else, Pos#{comments=>Comments}};
parse_attribute([{'-', Pos},{atom,#{value:='endif'}}], Comments) ->
    {endif, Pos#{comments=>Comments}};
parse_attribute([{'-', Pos},{atom,#{value:='undef'}},{'(', _},Name,{')',_}], Comments) ->
    {undef, Pos#{comments=>Comments}, Name};
parse_attribute([{'-', Pos},{atom,#{value:='define'}}|Ts], Comments) ->
    [Name | Args0] = sourcer_util:middle(Ts),
    {Args, Arity, Value} = case Args0 of
                        [{'(',_}|_] ->
                            {A,B} = sourcer_util:split_at_brace(Args0),
                            Args1 = [hd(X) || X<-sourcer_util:split_at_comma(sourcer_util:middle(A))],
                            {
                             Args1, length(Args1),
                             case B of [] -> []; _ -> tl(B) end
                            };
                        _ ->
                            {none, -1, tl(Args0)}
                    end,
    {define, Pos#{comments=>Comments}, Name, Arity, Args, Value};
parse_attribute([{'-', Pos},{atom,#{value:='record'}}|Ts], Comments) ->
    [{atom, #{value:=Name}}, {',', _} | Def] = sourcer_util:middle(Ts),
    {record, Pos#{comments=>Comments}, Name, record_def(Def)};
parse_attribute([{'-', Pos},{atom,#{value:='type'}}|Ts], Comments) ->
    Ts1 = case hd(Ts) of
              {'(',_} ->
                  sourcer_util:middle(Ts);
              _ ->
                  Ts
          end,
    {[Name|Args0], Def} = split_at(Ts1, '::'),
    Args = sourcer_util:split_at_comma(sourcer_util:middle(Args0)),
    {type, Pos#{comments=>Comments}, Name, Args, Def};
parse_attribute([{'-', Pos},{atom,#{value:='opaque'}}|Ts], Comments) ->
    Ts1 = case hd(Ts) of
              {'(',_} ->
                  sourcer_util:middle(Ts);
              _ ->
                  Ts
          end,
    {[Name|Args0], Def} = split_at(Ts1, '::'),
    Args = sourcer_util:split_at_comma(sourcer_util:middle(Args0)),
    {opaque, Pos#{comments=>Comments}, Name, Args, Def};
parse_attribute([{'-', Pos},{atom,#{value:='spec'}}|Ts], Comments) ->
    {M, F, Sigs} = parse_spec(Ts),
    {spec, Pos#{comments=>Comments}, M, F, Sigs};
parse_attribute([{'-', Pos},{atom,#{value:='callback'}}|Ts], Comments) ->
    {_, F, [Sigs]} = parse_spec(Ts),
    {callback, Pos#{comments=>Comments}, F, Sigs};
parse_attribute([{'-', Pos},{atom,#{value:='export'}}|Ts], Comments) ->
    Fs = sourcer_util:split_at_comma(sourcer_util:middle(sourcer_util:middle(Ts))),
    {export, Pos#{comments=>Comments}, Fs};
parse_attribute([{'-', Pos},{atom,#{value:='export_type'}}|Ts], Comments) ->
    Fs = sourcer_util:split_at_comma(sourcer_util:middle(sourcer_util:middle(Ts))),
    {export_type, Pos#{comments=>Comments}, Fs};
parse_attribute([{'-', Pos},{atom,#{value:='import'}}|Ts], Comments) ->
    {M, Fs0} = split_at(sourcer_util:middle(Ts), ','),
    Fs = sourcer_util:split_at_comma(sourcer_util:middle(Fs0)),
    {import, Pos#{comments=>Comments}, M, Fs};
parse_attribute([{'-', Pos},{atom,#{value:='module'}},{'(',_},{atom,#{value:=Name}}|_], Comments) ->
    {module, Pos#{comments=>Comments}, Name};
parse_attribute([{'-', Pos},{atom,#{value:='compile'}}|Ts], Comments) ->
    {compile, Pos#{comments=>Comments}, sourcer_util:middle(Ts)};
parse_attribute([{'-', Pos},{atom,#{value:='vsn'}}|Vsn], Comments) ->
    {vsn, Pos#{comments=>Comments}, sourcer_util:middle(Vsn)};
parse_attribute([{'-', Pos},{atom,#{value:='on_load'}}|Ts], Comments) ->
    {on_load, Pos#{comments=>Comments}, sourcer_util:middle(Ts)};
parse_attribute([{'-', Pos},{atom,#{value:='behaviour'}}|Ts], Comments) ->
    {behaviour, Pos#{comments=>Comments}, sourcer_util:middle(Ts)};
parse_attribute([{'-', Pos},{atom,#{value:='behavior'}}|Ts], Comments) ->
    {behaviour, Pos#{comments=>Comments}, sourcer_util:middle(Ts)};
parse_attribute([{'-', Pos},{atom,#{value:='include'}},{'(',_},{string,#{value:=Str}}|_], Comments) ->
    {include, Pos#{comments=>Comments}, Str};
parse_attribute([{'-', Pos},{atom,#{value:='include_lib'}},{'(',_},{string,#{value:=Str}}|_], Comments) ->
    {include_lib, Pos#{comments=>Comments}, Str};
parse_attribute([{'-', Pos},{atom,#{value:=Name}}|Ts], Comments) ->
    {attribute, Pos#{comments=>Comments}, Name, Ts}.

parse_function([{atom, Pos=#{value:=Name}}|_]=Ts, Comments) ->
    Clauses = parse_clauses(Ts),
    Arity = length(element(3, hd(Clauses))),
    {function, Pos#{comments=>Comments}, Name, Arity, Clauses}.

parse_clauses(Ts) ->
    R = split_at_semicolon_name(Ts),
    [parse_clause(L) || L<-R].

parse_clause(Ts) ->
    [{_,P} | Toks] = Ts,
    {Args0, Rest} = sourcer_util:split_at_brace(Toks),
    Args = sourcer_util:split_at_comma(sourcer_util:middle(Args0)),
    {Guards0, Body} = split_at_arrow(Rest),
    Guards = case Guards0 of
                 [] ->
                     [];
                 [_|T] ->
                     T
             end,
    {clause, P, Args, Guards, Body}.

%% Split token list at dots (not included in result).
split_at_dot(L) ->
    split_at_dot(L, [], []).

split_at_dot([], R, []) ->
    lists:reverse(R);
split_at_dot([], Acc, Crt) ->
    lists:reverse(Acc, [lists:reverse(Crt)]);
split_at_dot([{dot,_}|Ts], Acc, Crt) ->
    split_at_dot(Ts, [lists:reverse(Crt)|Acc], []);
split_at_dot([H|Ts], Acc, Crt) ->
    split_at_dot(Ts, Acc, [H|Crt]).

%% Split token list at "; Name (", where Name is the same
%% as the first token which must be an atom.
%% Semicolon is not included in result.
split_at_semicolon_name([H|_]=L) ->
    {atom, #{value:=Name}} = H,
    split_at_semicolon_name(L , Name, [], []).

split_at_semicolon_name([], _, R, []) ->
    lists:reverse(R);
split_at_semicolon_name([], _, Acc, Crt) ->
    lists:reverse(Acc, [lists:reverse(Crt)]);
split_at_semicolon_name([{';', _}, {atom, #{value:=Name}}=H1, {'(', _}=H2|T],
                        Name, Acc, Crt) ->
    split_at_semicolon_name([H1, H2|T], Name, [lists:reverse(Crt)|Acc], []);
split_at_semicolon_name([H|T], Name, Acc, Crt) ->
    split_at_semicolon_name(T, Name, Acc, [H|Crt]).

%% Split token list at "; ("
split_at_semicolon(L) ->
    split_at_semicolon(L , [], []).

split_at_semicolon([], R, []) ->
    lists:reverse(R);
split_at_semicolon([], Acc, Crt) ->
    lists:reverse(Acc, [lists:reverse(Crt)]);
split_at_semicolon([{';', _}, {'(', _}=H2|T], Acc, Crt) ->
    split_at_semicolon([H2|T], [lists:reverse(Crt)|Acc], []);
split_at_semicolon([H|T], Acc, Crt) ->
    split_at_semicolon(T, Acc, [H|Crt]).

split_at_arrow(L) ->
    split_at(L, '->').

split_at(L, Delim) ->
    split_at(L, Delim, []).

split_at([], _, R) ->
    {lists:reverse(R), []};
split_at([{Delim, _}|Rest], Delim, R) ->
    {lists:reverse(R), Rest};
split_at([H|Rest], Delim, R) ->
    split_at(Rest, Delim, [H|R]).

record_def(Ts) ->
    Fields = sourcer_util:split_at_comma(sourcer_util:middle(Ts)),
    Fun = fun([{atom,Pos=#{value:=Name}}|TypeDef]) ->
                  Def0 = case TypeDef of
                             [{'=',_}|_] ->
                                 tl(TypeDef);
                             _ ->
                                 TypeDef
                         end,
                  {Type, Def} = split_at(Def0, '::'),
                  {field, Pos, Name, Type, Def}
          end,
    lists:map(Fun, Fields).

parse_spec(Ts) ->
    {H, Rest} = split_at(Ts, '('),
    {M, F} = case split_at(H, ':') of
                 {[Fx], []} ->
                     {'$this$', Fx};
                 {[Mx], [Fx]} ->
                     {Mx, Fx}
             end,
    Cls = split_at_semicolon([{'(',1}|Rest]),
    Fun = fun(X) ->
                  {Args0,Return} = sourcer_util:split_at_brace(X),
                  Args = sourcer_util:split_at_comma(sourcer_util:middle(Args0)),
                  {Args, tl(Return)}
          end,
    Sigs = lists:map(Fun, Cls),
    {M, F, Sigs}.

is_active_context(#context{defines=Defs, active=Acts}) ->
    Fun = fun(A, Acc) -> Acc andalso is_active_define(A, Defs) end,
    lists:foldl(Fun, true, Acts).

is_active_define({'not', A}, Defs) ->
    sets:is_element({'not', A}, Defs) orelse not sets:is_element(A, Defs);
is_active_define(A, Defs) ->
    sets:is_element(A, Defs) andalso not sets:is_element({'not', A}, Defs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

split_at_dot_test_() ->
    [
     ?_assertMatch([[a,b,c]],
                   split_at_dot([a,b,c])),
     ?_assertMatch([[a],[b,c]],
                   split_at_dot([a,{dot,0},b,c])),
     ?_assertMatch([[a],[b,c]],
                   split_at_dot([a,{dot,0},b,c,{dot,0}])),
     ?_assertMatch([],
                   split_at_dot([]))
    ].

extract_top_comments_test_() ->
    [
     ?_assertMatch({[{comment,#{text:=<<"%a">>}},
                     {comment,#{text:=<<"%b">>}},
                     {comment,#{text:=<<"%%c">>}}],
                    [{atom,#{value:=hello}}]},
                   extract_top_comments(scan("%a\n%b\n%%c\nhello"))),
     ?_assertMatch({[{comment,#{text:=<<"%a">>}},
                     {comment,#{text:=<<"%b">>}},
                     {comment,#{text:=<<"%%c">>}}],
                    [{atom,#{value:=hello}}]},
                   extract_top_comments(scan("%a\n\n%b\n%%c\nhello")))
    ].

split_at_semicolon_name_test_() ->
    [
     ?_assertMatch([[{atom, #{value:=a}},
                     {atom, #{value:=b}},
                     {atom, #{value:=c}}]],
                   split_at_semicolon_name([{atom, #{value=>a}},
                                            {atom, #{value=>b}},
                                            {atom, #{value=>c}}])),
     ?_assertMatch([[{atom, #{value:=a}},
                     {';',_},
                     {atom, #{value:=b}},
                     {',',_},
                     {atom, #{value:=c}}]],
                   split_at_semicolon_name(scan("a;b,c"))),
     ?_assertMatch([[{atom, #{value:=a}},{';',_},{atom, #{value:=a}},{',',_},{atom, #{value:=b}}]],
                   split_at_semicolon_name(scan("a;a,b"))),
     ?_assertMatch([[{atom, #{value:=a}},{'(',_},{atom, #{value:=b}},{')',_}],
                    [{atom, #{value:=a}},{'(',_},{atom, #{value:=e}},{')',_}]],
                   split_at_semicolon_name(scan("a(b);a(e)"))),
     ?_assertMatch([[{atom, #{value:=a}},{'(',_},{atom, #{value:=b}},{')',_},{';',_},
                     {atom, #{value:=c}},{'(',_},{atom, #{value:=d}},{')',_}]],
                   split_at_semicolon_name(scan("a(b);c(d)")))
    ].

split_at_semicolon_test_() ->
    [
     ?_assertMatch([[{atom, #{value:=a}},{',',_},{atom, #{value:=b}},{',',_},{atom, #{value:=c}}]],
                   split_at_semicolon(scan("a,b,c"))),
     ?_assertMatch([[{atom, #{value:=a}},{';',_},{atom, #{value:=b}},{',',_},{atom, #{value:=c}}]],
                   split_at_semicolon(scan("a;b,c"))),
     ?_assertMatch([[{'(',_},{atom, #{value:=b}},{')',_},{atom, #{value:=zz}}],
                    [{'(',_},{atom, #{value:=e}},{')',_},{atom, #{value:=xx}}]],
                   split_at_semicolon(scan("(b)zz;(e)xx"))),
     ?_assertMatch([],
                   split_at_semicolon([]))
    ].

is_active_context_test_() ->
    [
     ?_assertEqual(true,
                   is_active_context(
                     #context{defines=sets:from_list([x]),
                              active=[]
                             })),
     ?_assertEqual(true,
                   is_active_context(
                     #context{defines=sets:from_list([x]),
                              active=[x]
                             })),
     ?_assertEqual(true,
                   is_active_context(
                     #context{defines=sets:from_list([{'not', x}]),
                              active=[]
                             })),
     ?_assertEqual(false,
                   is_active_context(
                     #context{defines=sets:from_list([{'not', x}]),
                              active=[x]
                             })),
     ?_assertEqual(false,
                   is_active_context(
                     #context{defines=sets:from_list([x]),
                              active=[{'not', x}]
                             })),
     ?_assertEqual(false,
                   is_active_context(
                     #context{defines=sets:from_list([]),
                              active=[x]
                             })),
     ?_assertEqual(true,
                   is_active_context(
                     #context{defines=sets:from_list([]),
                              active=[{'not',x}]
                             })),
     ?_assertEqual(false,
                   is_active_context(
                     #context{defines=sets:from_list([x]),
                              active=[{'not',x},x]
                             })),
     ?_assertEqual(true,
                   is_active_context(#context{}))
    ].

parse_clause_test_() ->
    [
     ?_assertMatch({clause, _, [], [], [{atom,#{value:=a}}]},
                   parse_clause(scan("foo()->a"))),
     ?_assertMatch({clause,_,
                    [[{atom,#{value:=x}}],[{atom,#{value:=y}}]],
                    [],
                    [{atom,#{value:=a}},{',',_},{atom,#{value:=b}}]},
                   parse_clause(scan("foo(x,y)->a,b")))
    ].

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
                   get_macro_def({macro, #{name=>z}}, -1,
                                 #context{macros=[{x, -1, none, [{atom, r}]}]})),
     ?_assertMatch([{z, -1, none, [{atom, r}]}],
                   get_macro_def({macro, #{name=>z}}, -1,
                                 #context{macros=[{z, -1, none, [{atom, r}]}]})),
     ?_assertMatch([{z, -1, _, [{atom, r}]}],
                   get_macro_def({macro, #{name=>z}}, 1,
                                 #context{macros=[{z, -1, none, [{atom, r}]}]})),
     ?_assertMatch([],
                   get_macro_def({macro, #{name=>z}}, 1,
                                 #context{macros=[{z, 2, none, [{atom, r}]}]})),
     ?_assertMatch([{z, -1, _, [{atom, r}]},{z, 1, _, [{atom, r}]}],
                   get_macro_def({macro, #{name=>z}}, 1,
                                 #context{macros=[{z, -1, none, [{atom, r}]}, {z, 1, [], [{atom, r}]}]})),
     ?_assertMatch([],
                   get_macro_def({macro, #{name=>z}}, -1,
                                 #context{}))
     ].

one_step_expand_test_() ->
    [
     ].

expand_macros_test_() ->
    [
     ?_assertMatch([{atom, 0}, {macro, #{name:=x}}, {atom, 1}],
                   expand_macros([{atom, 0}, {macro, #{name=>x}}, {atom, 1}],
                                 #context{})),
     ?_assertMatch([{atom, 0}, {macro, #{name:=x}}, {atom, 1}],
                   expand_macros([{atom, 0}, {macro, #{name=>x}}, {atom, 1}],
                                 #context{macros=[{z, -1, none, [{atom, r}]}]})),
     ?_assertMatch([{atom, 0}, {atom, r}, {atom, 1}],
                   expand_macros([{atom, 0}, {macro, #{name=>x}}, {atom, 1}],
                                 #context{macros=[{x, -1, none, [{atom, r}]}]})),
     ?_assertMatch([{atom, 0}, {atom, r}, {atom, q}, {atom, 1}],
                   expand_macros([{atom, 0}, {macro, #{name=>x}}, {atom, 1}],
                                 #context{macros=[{x, -1, none, [{atom, r}, {atom, q}]}]})),
     ?_assertMatch([{atom, 0}, {atom, 1}],
                   expand_macros([{atom, 0}, {atom, 1}],
                                 #context{})),
     ?_assertMatch([],
                   expand_macros([],
                                 #context{}))
     ].

%%%%%%%

scan(D) ->
    {ok, Ts, _} = sourcer_scan:string(D),
    Ts.

-endif.



