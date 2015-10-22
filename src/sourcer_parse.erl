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
         scan/1,
         string/1,
         tokens/1,
         string/2,
         tokens/2
        ]).

-include("sourcer_parse.hrl").

%-spec scan(string()) -> {ok, {[[token()]], [{[token()], [token()]}]}}.
scan(D) ->
    %% TODO why not call sourcer_scan?
    case erl_scan_local:string(D, {0,1}, [return, text]) of
        {ok, Toks, _} ->
            {Toks1, _} = convert_attributes(Toks),
            Toks2 = split_at_dot(fix_macro_tokens(Toks1)),
            Toks3 = lists:delete([], [group_top_comments(F) || F<-Toks2]),
            Filtered = filter_whitespace_comments(Toks3),
            {ok, {Toks2, Filtered}};
        Err ->
            Err
    end.

string(D) when is_list(D) ->
    string(D, #context{}).

string(D, Context) when is_list(D), is_record(Context, context) ->
    case scan(D) of
        {ok, {_FormToks, FilteredTokens}} ->
            {ok, tokens(FilteredTokens, Context)};
        Err ->
            Err
    end.

tokens(Toks) ->
    tokens(Toks, #context{}).

tokens(Toks, Context) ->
    lists:mapfoldl(fun parse_form/2, Context, Toks).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filter_whitespace_comments(Toks) ->
    [filter_tokens(T) || T<-Toks].

group_top_comments(Toks) ->
    group_top_comments(Toks, []).

group_top_comments([{comment, _, _}=C, {white_space, _, _}|Toks], Acc) ->
    group_top_comments(Toks, [C|Acc]);
group_top_comments([{comment, _, _}=C|Toks], Acc) ->
    group_top_comments(Toks, [C|Acc]);
group_top_comments([{white_space, _, _}=C|Toks], Acc) ->
    group_top_comments(Toks, [C|Acc]);
group_top_comments(Toks, Acc) ->
    [compact_comments(X) || X<-group_comments(lists:reverse(Acc))] ++ Toks.

top_comment(Comments) ->
    lists:filter(fun({comment,_,_})->true; (_)->false end, Comments).

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
    lists:dropwhile(fun({white_space,_,_})->true; (_)->false end, L).

compact_comments([]) ->
    {comments, none, [], 0};
compact_comments(L) ->
    Level = comment_level(hd(L)),
    Fun = fun({comment, _, C}, Acc) -> [skip_percent(C)|Acc] end,
    {comments, element(2, hd(L)), lists:reverse(lists:foldl(Fun, [], L)), Level}.

comment_level({comment, _, "%%%%"++_}) -> 4;
comment_level({comment, _, "%%%"++_}) -> 3;
comment_level({comment, _, "%%"++_}) -> 2;
comment_level({comment, _, "%"++_}) -> 1;
comment_level(_) -> 0.

skip_percent("%"++L) ->
    skip_percent(L);
skip_percent(L) ->
    L.

%% keep track of macro context
%% TODO make sure the context from includes is computed and used
parse_form(Ts, Context) ->
    {Form, Ctx} = do_parse_form(Ts, Context, []),
    Attrs0 = element(2, Form),
    Attrs1 = Attrs0#{active=>is_active_context(Context)},
    Form2 = setelement(2, Form, Attrs1),
    {Form2, Ctx}.

do_parse_form(Ts, Context, Comments) ->
    %% TODO set 'active' attribute on form, if
    case element(1, hd(Ts)) of
        atom ->
            {parse_function(Ts, lists:reverse(Comments)), Context};
        '-' ->
            Attr = parse_attribute(Ts, lists:reverse(Comments)),
            NewContext = get_context(Attr, Context),
            {Attr, NewContext};
        comments ->
            do_parse_form(tl(Ts), Context, [hd(Ts)|Comments]);
        _ ->
            {parse_unknown(Ts), Context}
    end.

get_context({'define', _, Name, Arity, _, _}, #context{defines=Defs, macros=Macros}=Ctx) ->
    Ctx#context{defines=sets:add_element(no_attrs(Name), Defs),
                macros=[{no_attrs(Name), Arity}|Macros]};
get_context({'undef', _, Name}, #context{defines=Defs, macros=Macros}=Ctx) ->
    Ctx#context{defines=sets:del_element(no_attrs(Name), Defs),
                macros=lists:filter(fun({X, _})-> X =/= no_attrs(Name) end, Macros)};
get_context({'ifdef', _, Name}, #context{active=Active}=Ctx) ->
    Ctx#context{active=[Name|Active]};
get_context({'ifndef', _, Name}, #context{active=Active}=Ctx) ->
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

parse_attribute([{'-', Pos},{atom,_,'ifdef'},{'(', _},Name,{')',_}], Comments) ->
    {ifdef, Pos#{comments=>Comments}, Name};
parse_attribute([{'-', Pos},{atom,_,'ifndef'},{'(', _},Name,{')',_}], Comments) ->
    {ifndef, Pos#{comments=>Comments}, Name};
parse_attribute([{'-', Pos},{atom,_,'else'}], Comments) ->
    {else, Pos#{comments=>Comments}};
parse_attribute([{'-', Pos},{atom,_,'endif'}], Comments) ->
    {endif, Pos#{comments=>Comments}};
parse_attribute([{'-', Pos},{atom,_,'undef'},{'(', _},Name,{')',_}], Comments) ->
    {undef, Pos#{comments=>Comments}, Name};
parse_attribute([{'-', Pos},{atom,_,'define'}|Ts], Comments) ->
    [Name | Args0] = sourcer_util:middle(Ts),
    {Args, Value} = case Args0 of
                        [] ->
                            [];
                        [{'(',_}|_] ->
                            {A,B} = split_at_brace(Args0),
                            {
                             [hd(X) || X<-split_at_comma(sourcer_util:middle(A))],
                             case B of [] -> []; _->tl(B) end
                            };
                        _ ->
                            {[], tl(Args0)}
                    end,
    Arity = length(Args),
    {define, Pos#{comments=>Comments}, Name, Arity, Args, Value};
parse_attribute([{'-', Pos},{atom,_,'record'}|Ts], Comments) ->
    [{atom, _, Name}, {',', _} | Def] = sourcer_util:middle(Ts),
    {record, Pos#{comments=>Comments}, Name, record_def(Def)};
parse_attribute([{'-', Pos},{atom,_,'type'}|Ts], Comments) ->
    Ts1 = case hd(Ts) of
              {'(',_} ->
                  sourcer_util:middle(Ts);
              _ ->
                  Ts
          end,
    {[Name|Args0], Def} = split_at(Ts1, '::'),
    Args = split_at_comma(sourcer_util:middle(Args0)),
    {type, Pos#{comments=>Comments}, Name, Args, Def};
parse_attribute([{'-', Pos},{atom,_,'opaque'}|Ts], Comments) ->
    Ts1 = case hd(Ts) of
              {'(',_} ->
                  sourcer_util:middle(Ts);
              _ ->
                  Ts
          end,
    {[Name|Args0], Def} = split_at(Ts1, '::'),
    Args = split_at_comma(sourcer_util:middle(Args0)),
    {opaque, Pos#{comments=>Comments}, Name, Args, Def};
parse_attribute([{'-', Pos},{atom,_,'spec'}|Ts], Comments) ->
    {M, F, Sigs} = parse_spec(Ts),
    {spec, Pos#{comments=>Comments}, M, F, Sigs};
parse_attribute([{'-', Pos},{atom,_,'callback'}|Ts], Comments) ->
    {_, F, [Sigs]} = parse_spec(Ts),
    {callback, Pos#{comments=>Comments}, F, Sigs};
parse_attribute([{'-', Pos},{atom,_,'export'}|Ts], Comments) ->
    Fs = split_at_comma(sourcer_util:middle(sourcer_util:middle(Ts))),
    {export, Pos#{comments=>Comments}, Fs};
parse_attribute([{'-', Pos},{atom,_,'export_type'}|Ts], Comments) ->
    Fs = split_at_comma(sourcer_util:middle(sourcer_util:middle(Ts))),
    {export_type, Pos#{comments=>Comments}, Fs};
parse_attribute([{'-', Pos},{atom,_,'import'}|Ts], Comments) ->
    {M, Fs0} = split_at(sourcer_util:middle(Ts), ','),
    Fs = split_at_comma(sourcer_util:middle(Fs0)),
    {import, Pos#{comments=>Comments}, M, Fs};
parse_attribute([{'-', Pos},{atom,_,'module'},{'(',_},{atom,_,Name}|_], Comments) ->
    {module, Pos#{comments=>Comments}, Name};
parse_attribute([{'-', Pos},{atom,_,'compile'}|Ts], Comments) ->
    {compile, Pos#{comments=>Comments}, sourcer_util:middle(Ts)};
parse_attribute([{'-', Pos},{atom,_,'vsn'}|Vsn], Comments) ->
    {vsn, Pos#{comments=>Comments}, sourcer__util:middle(Vsn)};
parse_attribute([{'-', Pos},{atom,_,'on_load'}|Ts], Comments) ->
    {on_load, Pos#{comments=>Comments}, sourcer_util:middle(Ts)};
parse_attribute([{'-', Pos},{atom,_,'behaviour'}|Ts], Comments) ->
    {behaviour, Pos#{comments=>Comments}, sourcer_util:middle(Ts)};
parse_attribute([{'-', Pos},{atom,_,'behavior'}|Ts], Comments) ->
    {behaviour, Pos#{comments=>Comments}, sourcer_util:middle(Ts)};
parse_attribute([{'-', Pos},{atom,_,'include'},{'(',_},{string,_,Str}|_], Comments) ->
    {include, Pos#{comments=>Comments}, Str};
parse_attribute([{'-', Pos},{atom,_,'include_lib'},{'(',_},{string,_,Str}|_], Comments) ->
    {include_lib, Pos#{comments=>Comments}, Str};
parse_attribute([{'-', Pos},{atom,_,Name}|Ts], Comments) ->
    {attribute, Pos#{comments=>Comments}, Name, Ts}.

parse_function([{atom, Pos, Name}|_]=Ts, Comments) ->
    Clauses = parse_clauses(Ts),
    Arity = length(element(3, hd(Clauses))),
    {function, Pos#{comments=>Comments}, Name, Arity, Clauses}.

parse_clauses(Ts) ->
    R = split_at_semicolon_name(Ts),
    [parse_clause(L) || L<-R].

parse_clause(Ts) ->
    [H |Toks] = Ts,
    {Args0, Rest} = split_at_brace(Toks),
    Args = split_at_comma(sourcer_util:middle(Args0)),
    {Guards0, Body} = split_at_arrow(Rest),
    Guards = case Guards0 of
                 [] ->
                     [];
                 [_|T] ->
                     T
             end,
    {clause, element(2, H), Args, Guards, Body}.

%% Split token list at dots (not included in result).
split_at_dot(L) ->
    split_at_dot(L , [], []).

split_at_dot([], R, []) ->
    lists:reverse(R);
split_at_dot([], Acc, Crt) ->
    lists:reverse(Acc, [lists:reverse(Crt)]);
split_at_dot([{dot, _}|T], Acc, Crt) ->
    split_at_dot(T, [lists:reverse(Crt)|Acc], []);
split_at_dot([H|T], Acc, Crt) ->
    split_at_dot(T, Acc, [H|Crt]).

%% Split token list at "; Name (", where Name is the same
%% as the first token which must be an atom.
%% Semicolon is not included in result.
split_at_semicolon_name([]) ->
    [];
split_at_semicolon_name(L) ->
    {atom, _, Name} = hd(L),
    split_at_semicolon_name(L , Name, [], []).

split_at_semicolon_name([], _, R, []) ->
    lists:reverse(R);
split_at_semicolon_name([], _, Acc, Crt) ->
    lists:reverse(Acc, [lists:reverse(Crt)]);
split_at_semicolon_name([{';', _}, {atom, _, Name}=H1, {'(', _}=H2|T],
                        Name, Acc, Crt) ->
    split_at_semicolon_name([H1, H2|T], Name, [lists:reverse(Crt)|Acc], []);
split_at_semicolon_name([H|T], Name, Acc, Crt) ->
    split_at_semicolon_name(T, Name, Acc, [H|Crt]).

split_at_semicolon([]) ->
    [];
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

%% Split token list at top-level commas (not included in result),
%% while keeping track of brace levels.
split_at_comma([])->
    [];
split_at_comma(L)->
    split_at_comma(L, [], []).

split_at_comma([], Acc, Crt) ->
    lists:reverse([lists:reverse(Crt) | Acc]);
split_at_comma([H|T], Acc, Crt) when element(1, H)==',' ->
    split_at_comma(T, [lists:reverse(Crt)|Acc], []);
split_at_comma([H|T], Acc, Crt) ->
    case token_pair(element(1, H)) of
        none ->
            split_at_comma(T, Acc, [H|Crt]);
        End ->
            {L1, L2} = split_at_brace(T, End, [H]),
            split_at_comma(L2, Acc, lists:reverse(L1)++Crt)
    end.


%% Split L at the first Tok, keeping track of brace levels.
%% We assume that the start brace is the first element of the list.
split_at_brace([]) ->
    {[], []};
split_at_brace([H|T]=L) ->
    case token_pair(element(1, H)) of
        none ->
            {[], L};
        End ->
            split_at_brace(T, End, [H])
    end.

split_at_brace([], _, Acc) ->
    {lists:reverse(Acc), []};
split_at_brace([H|T], End, Acc) when element(1,H)==End ->
    {lists:reverse([H|Acc]), T};
split_at_brace([H|T], End, Acc) ->
    case token_pair(element(1, H)) of
        none ->
            split_at_brace(T, End, [H|Acc]);
        Other ->
            {L1, L2} = split_at_brace(T, Other, [H]),
            split_at_brace(L2, End, lists:reverse(L1)++Acc)
    end.

token_pair('(') ->
    ')';
token_pair('[') ->
    ']';
token_pair('{') ->
    '}';
token_pair('<<') ->
    '>>';
token_pair(_) ->
    none.

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

filter_tokens(Toks) ->
    lists:filter(fun filter_token/1, Toks).

filter_token(E) ->
    case element(1, E) of
        white_space ->
            false;
        comment ->
            false;
        _ ->
            true
    end.

record_def(Ts) ->
    Fields = split_at_comma(sourcer_util:middle(Ts)),
    Fun = fun([{atom,Pos,Name}|TypeDef]) ->
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
                  {Args0,Return} = split_at_brace(X),
                  Args = split_at_comma(sourcer_util:middle(Args0)),
                  {Args, tl(Return)}
          end,
    Sigs = lists:map(Fun, Cls),
    {M, F, Sigs}.

no_attrs(T) ->
    setelement(2, T, 0).

is_active_context(#context{defines=Defs, active=Acts}) ->
    Fun = fun(A, Acc) -> Acc andalso is_active_define(A, Defs) end,
    lists:foldl(Fun, true, Acts).

is_active_define({'not', A}, Defs) ->
    sets:is_element({'not', A}, Defs) orelse not sets:is_element(A, Defs);
is_active_define(A, Defs) ->
    sets:is_element(A, Defs) andalso not sets:is_element({'not', A}, Defs).

fix_macro_tokens(Toks) ->
    fix_macro_tokens(Toks, []).

fix_macro_tokens([], Acc) ->
    lists:reverse(Acc);
fix_macro_tokens([{'?',P1},{atom,P2,A}|T], Acc) ->
    fix_macro_tokens(T, [{macro, mash_pos(P1, P2), A}|Acc]);
fix_macro_tokens([{'?',P1},{var,P2,A}|T], Acc) ->
    fix_macro_tokens(T, [{macro, mash_pos(P1, P2), A}|Acc]);
fix_macro_tokens([H|T], Acc) ->
    fix_macro_tokens(T, [H|Acc]).

mash_pos(#{}=P1,#{text:=T2}) ->
    P1#{text=>[$?|T2]}.

convert_attributes(Toks) when is_list(Toks) ->
    lists:mapfoldl(fun convert_attributes/2, 0, Toks).

convert_attributes(T, Ofs) ->
    Attrs = element(2, T),
    Length = length(erl_anno:text(Attrs)),
    NewAttrs = maps:from_list([{length,Length},{offset,Ofs}|Attrs]),
    {setelement(2, T, NewAttrs), Ofs+Length}.

