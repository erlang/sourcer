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

string(D, Context) when is_list(D), is_record(Context, context) ->
    case sourcer_scan:string(D) of
        {ok, Toks, _} ->
            {ok, tokens(Toks, Context)};
        _Err ->
            _Err
    end.

tokens(Toks, Context) ->
    Raw = split_at_dot(Toks),
    {Parsed, NewContext} = parse_forms(Raw, Context),
    {#{all=>Toks, rawForms=>Raw, forms=>Parsed}, NewContext}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_forms(Toks, Context) ->
    lists:mapfoldl(fun parse_form/2, Context, Toks).

%% keep track of macro context
parse_form(Ts, Context) ->
    {TopComments, Ts1} = extract_top_comments(Ts),
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

top_comment(Comments) ->
    lists:filter(fun({comment,_})->true; (_)->false end, Comments).

%% group comments with same level and no spaces inbetween
group_comments(C) ->
    case skip_white_at_start(C) of
        [] ->
            [];
        [H|_]=C1 ->
            Level = comment_level(H),
            {G, Rest} = get_first_group(Level, C1, []),
            [G] ++ group_comments(Rest)
    end.

get_first_group(_, [], Acc) ->
    {lists:reverse(Acc), []};
get_first_group(Level, [H|T]=L, Acc) ->
    case comment_level(H) of
        Level ->
            get_first_group(Level, T, [H|Acc]);
        _ ->
            {lists:reverse(Acc), L}
    end.

skip_white_at_start(L) ->
    lists:dropwhile(fun({white_space,_})->true; (_)->false end, L).

compact_comments([]) ->
    {comments, none, [], 0};
compact_comments(L) ->
    Level = comment_level(hd(L)),
    Fun = fun({comment, #{value:=C}}, Acc) -> [skip_percent(C)|Acc] end,
    {comments, element(2, hd(L)), lists:reverse(lists:foldl(Fun, [], L)), Level}.

comment_level({comment, #{text:="%%%%"++_}}) -> 4;
comment_level({comment, #{text:="%%%"++_}}) -> 3;
comment_level({comment, #{text:="%%"++_}}) -> 2;
comment_level({comment, #{text:="%"++_}}) -> 1;
comment_level(_) -> 0.

skip_percent("%"++L) ->
    skip_percent(L);
skip_percent(L) ->
    L.

%% TODO set 'active' attribute on form, if
do_parse_form([{atom,_}|_]=Ts, Context, Comments) ->
    {parse_function(Ts, lists:reverse(Comments)), Context};
do_parse_form([{'-',_}|_]=Ts, Context, Comments) ->
    Attr = parse_attribute(Ts, lists:reverse(Comments)),
    NewContext = get_context(Attr, Context),
    {Attr, NewContext};
do_parse_form(Ts, Context, _Comments) ->
    {parse_unknown(Ts), Context}.

get_context({'define', _, {_,#{value:=Name}}, Arity, _, _}, #context{defines=Defs, macros=Macros}=Ctx) ->
    Ctx#context{defines=sets:add_element(Name, Defs),
                macros=[{Name, Arity}|Macros]};
get_context({'undef', _, {_, #{value:=Name}}}, #context{defines=Defs, macros=Macros}=Ctx) ->
    Ctx#context{defines=sets:del_element(Name, Defs),
                macros=lists:filter(fun({X, _})->
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
    {Args, Value} = case Args0 of
                        [] ->
                            [];
                        [{'(',_}|_] ->
                            {A,B} = sourcer_util:split_at_brace(Args0),
                            {
                             [hd(X) || X<-sourcer_util:split_at_comma(sourcer_util:middle(A))],
                             case B of [] -> []; _->tl(B) end
                            };
                        _ ->
                            {[], tl(Args0)}
                    end,
    Arity = length(Args),
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
split_at_semicolon_name([]) ->
    [];
split_at_semicolon_name(L) ->
    {atom, #{value:=Name}} = hd(L),
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

