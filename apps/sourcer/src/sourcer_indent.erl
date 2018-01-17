%% Author: jakob
%% Created: 2006-jan-28
%% Description:
-module(sourcer_indent).

-export([
%%         indent_line/4,
         lines/1,
         lines/2
        ]).

%%
%%-define(DEBUG, 1).

-include("debug.hrl").
%-include("include/sourcer_token.hrl").

-define(k(X), {X,_,_,_}).
-define(kv(K,V), {K,_,_,V}).
-define(line(X), {_,{X,_},_,_}).
-define(col(X), {_,{_,X},_,_}).

%% TODO: change into multiples of IndentW
default_indent_prefs() ->
    [{before_binary_op, 4},
     {after_binary_op, 4},
     {before_arrow, 2},
     {after_arrow, 4},
     {after_unary_op, 4},
     {clause, 4},
     {'case', 4},
     {'try', 4},
     {'catch', 4},
     {'after', 4},
     {function_parameters, 2},
     {'fun', 3},
     {fun_body, 5},
     {paren, 1},
     {'<<', 2},
     {end_paren, 0},
     {comment_3, 0},
     {comment_2, 0},
     {comment_1, 48},

     {indentW, 4},
     {use_tabs, false},
     {tab_len, 8}
    ].

%%
%% API Functions

lines(S) ->
    lines(S, []).

lines(S, Prefs) ->
    Tokens = sourcer_scan:tokens(S),
    Lines = array:from_list(split_lines(S)),
    Ls = do_indent_lines(0, Tokens, Lines, get_prefs(Prefs)),
    unicode:characters_to_list(array:to_list(Ls)).

%%
%% Local Functions
%%

-record(i, {anchor, indent_line, current, in_block, prefs}).

get_prefs([], OldP, Acc) ->
    Acc ++ OldP;
get_prefs([{Key, Value} | Rest], OldP, Acc) ->
    P = lists:keydelete(Key, 1, OldP),
    get_prefs(Rest, P, [{Key, Value} | Acc]).

get_prefs(Prefs) ->
    get_prefs(Prefs, default_indent_prefs(), []).

do_indent_lines(LineNr, Tokens0, Lines0, Prefs) ->
    case fetch_form(LineNr, Tokens0) of
        eof -> Lines0;
        {{FormTokens, _Start, LastLoc} = Form, Tokens} ->
            ToCol = indent(LineNr, FormTokens, Prefs),
            L = reindent_line(array:get(LineNr, Lines0), ToCol, Prefs),
            Lines = array:set(LineNr, L, Lines0),
            case LastLoc of
                {LineNr, _} ->
                    do_indent_lines(LineNr+1, Tokens, Lines, Prefs);
                _ ->
                    ReScan = rescan_form(Form, Lines),
                    do_indent_lines(LineNr+1, [ReScan|Tokens], Lines, Prefs)
            end
    end.

%% TODO: value 4 is hardcoded! Should use indentation width here

indent(LineN, Tokens, Prefs) ->
    A0 = {start,{-1,1},"",undefined},
    I = #i{anchor=A0, indent_line=LineN, current=0, prefs=Prefs,
           in_block=true},
    try
        i_form_list(Tokens, I),
        ?D(no_catch),
        {4, I#i.in_block}
    catch
        throw:{indent, A, C, Inblock} ->
            ?D({indent, A, C, Inblock}),
            {get_indent_of(A, C), Inblock};
        throw:{indent_eof, A, C, Inblock} ->
            ?D({indent_eof, A, C, Inblock}),
            {get_indent_of(A, C), Inblock};
        throw:{indent_to, N, Inblock} ->
            ?D(N),
            {N, Inblock};
        error:_E ->
            ?D(_E),
            io:format("~p:~p: ~P ~p~n",[?MODULE, ?LINE, _E, 20, erlang:get_stacktrace()]),
            {0, true}
    end.

get_indent_of({_,eof,_,_}, C) ->
    C;
get_indent_of({_, {_L, CA}, _, _}=_A, C) ->
    ?D({CA, C, _A}),
    CA+C-1.

split_lines(Str) ->
    split_lines(Str, []).

split_lines([$$, C| Rest], Line) ->
    split_lines(Rest, [C, $$|Line]);
split_lines("\r\n" ++ Rest, Line) ->
    [lists:reverse(Line, "\r\n")|split_lines(Rest,[])];
split_lines("\n" ++ Rest, Line) ->
    [lists:reverse(Line, "\n")|split_lines(Rest,[])];
split_lines([C|Rest], Line) ->
    split_lines(Rest, [C|Line]);
split_lines([], Line) ->
    [lists:reverse(Line)].

fetch_form(Line, [{_, _, {End, _}}=Form|Rest])
  when Line =< End ->
    {Form, Rest};
fetch_form(Line, [_|Rest]) ->
    fetch_form(Line, Rest);
fetch_form(_, []) ->
    eof.

%% rescan after indentation so we get the new locations on the tokens
rescan_form({_, {FromLine, _}=Loc, {LastLine,_}}, Lines) ->
    Src = fetch_lines(Lines, LastLine, FromLine, []),
    pick_form(sourcer_scan:tokens(Src, Loc, LastLine), FromLine, LastLine).

%% pick the form we re-scanned most often the first but if form starts on
%% previous lines form it can be wrong, most often a comment after dot
pick_form([{_, {FromLine, _}, {LastLine, _}}=Form|_], FromLine, LastLine) ->
    Form;
pick_form([_|T], FromLine, LastLine) ->
    pick_form(T, FromLine, LastLine).

fetch_lines(Lines, NextLine, First, Acc)
  when NextLine >= First ->
    Src = array:get(NextLine, Lines) ++ Acc,
    fetch_lines(Lines, NextLine-1, First, Src);
fetch_lines(_Lines, _NextLine, _First, Acc) ->
    Acc.

%%
reindent_line(Line, N, Prefs) ->
    L = reindent_line(Line, N),
    entab(L, proplists:get_value(use_tabs, Prefs), proplists:get_value(tab_len, Prefs)).

reindent_line(" " ++ S, I) ->
    reindent_line(S, I);
reindent_line("\t" ++ S, I) ->
    reindent_line(S, I);
reindent_line(S, I) when is_integer(I), I>0 ->
    lists:duplicate(I, $ ) ++ S;
reindent_line(S, I) when is_integer(I) ->
    S.

entab(S, false, _Tablength) ->
    S;
entab(S, true, Tablength) when Tablength < 2->
    S;
entab(S, true, Tablength) ->
    {Spaces, Line} = string:take(S, "\s\t"),
    N = lists:foldl(fun($\s, N) -> N+1; ($\t, N) -> N+Tablength end, 0, Spaces),
    lists:append([lists:duplicate($\t, N div Tablength),
                  lists:duplicate($\s, N rem Tablength), Line]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i_check_aux([?line(L)| _],
            #i{indent_line=IL, anchor=A, current=C, in_block=Inblock})
  when L >= IL ->
    {indent, A, C, Inblock};
i_check_aux([?k(eof) | _], #i{anchor=A, current=C, in_block=Inblock}) ->
    {indent_eof, A, C, Inblock};
i_check_aux([eof | _], #i{anchor=A, current=C, in_block=Inblock}) ->
    {indent_eof, A, C, Inblock};
i_check_aux([], I) ->
    i_check_aux([eof], I);
i_check_aux(_, _) ->
    not_yet.

i_check(T, I) ->
    case i_check_aux(T, I) of
        not_yet ->
            not_yet;
        Throw ->
            ?D({T, I#i{prefs=[]}}),
            throw(Throw)
    end.

indent_by(Key, Prefs) ->
    proplists:get_value(Key, Prefs, 0).

head([H | _]) -> H;
head(H) -> H.

i_with(W, I) ->
    I#i{current=indent_by(W, I#i.prefs)}.

i_with(W, A, I) ->
    I#i{current=indent_by(W, I#i.prefs), anchor=head(A)}.

i_with_old_or_new_anchor(none, ANew, I) ->
    i_with(none, ANew, I);
i_with_old_or_new_anchor(AOld, _ANew, I) ->
    i_with(none, AOld, I).

i_par_list(R0, I0) ->
    I1 = I0#i{in_block=false},
    R1 = i_kind('(', R0, I1),
    I2 = i_with(end_paren, R0, I1),
    R2 = i_parameters(R1, I1),
    i_end_paren(R2, I2).

i_expr([], _I, _A) ->
    {[], eof};
i_expr(R0, I0, A) ->
    R1 = i_comments(R0, I0),
    I1 = i_with_old_or_new_anchor(A, R1, I0),
    R2 = i_1_expr(R1, I1),
    ?D({i_expr, R1}),
    case i_sniff(R1) of
        string ->
            case i_sniff(i_kind(string, R1, I1)) of
                string ->
                    i_expr(R2, I1, A);
                _ ->
                    i_expr_rest(R2, I1, I1#i.anchor)
            end;
        macro ->
            case i_sniff(i_kind(macro, R1, I1)) of
                macro ->
                    i_expr(R2, i_with(after_binary_op, I1), A);
                _ ->
                    i_expr_rest(R2, I1, I1#i.anchor)
            end;
        _ ->
            i_expr_rest(R2, I1, I1#i.anchor)
    end.

i_expr_rest(R0, I, A) ->
    case i_sniff(R0) of
        '(' -> % function call
            I1 = i_with(function_parameters, A, I),
            R1 = i_par_list(R0, I1),
            i_expr_rest(R1, I1, A);
        eof ->
            {R0, A};
        '#' -> % record something
            ?D(I),
            i_record(R0, I);
        ':' -> % external function call
            R1 = i_kind(':', R0, I),
            R2 = i_1_expr(R1, I),
            {R3, A1} = i_expr_rest(R2, I, A),
            {R3, A1};
        '||' -> % list comprehension
            R1 = i_kind('||', R0, I),
            R2 = i_expr_list(R1, I),
            {R2, A};
        '<=' -> % within binary comprehension
            R1 = i_kind('<=', R0, I),
            {R2, _A} = i_expr(R1, i_with(after_binary_op, I), none),
            {R2, A};
        '=' -> % match/assignment
            R1 = i_binary_op(R0, i_with(before_binary_op, I)),
            {R2, _A} = i_expr(R1, i_with(after_binary_op, I), none),
            {R2, A};
        '=>' -> % maps
            R1 = i_binary_op(R0, i_with(before_binary_op, I)),
            {R2, _A} = i_expr(R1, i_with(after_binary_op, I), none),
            {R2, A};
        ':=' -> % maps
            R1 = i_binary_op(R0, i_with(before_binary_op, I)),
            {R2, _A} = i_expr(R1, i_with(after_binary_op, I), none),
            {R2, A};
        _ ->
            case is_binary_op(i_sniff(R0)) of
                true ->
                    ?D({A, R0}),
                    R1 = i_binary_op(R0, i_with(before_binary_op, I)),
                    {R2, _A} = i_expr(R1, i_with(after_binary_op, I), A),
                    {R2, A};
                false ->
                    ?D({R0, A}),
                    {R0, A}
            end
    end.

i_expr_list(R, I) ->
    i_expr_list(R, I, none).

i_expr_list(R0, I0, A0) ->
    R1 = i_comments(R0, I0),
    ?D(R1),
    {R2, A1} = i_expr(R1, I0, A0),
    ?D({R2, A1}),
    I1 = i_with_old_or_new_anchor(A0, A1, I0),
    case i_sniff(R2) of
        ',' ->
            R3 = i_kind(',', R2, I1),
            i_expr_list(R3, I1, I1#i.anchor);
        _ ->
            R2
    end.

i_binary_expr_list(R, I) ->
    i_binary_expr_list(R, I, none).

i_binary_expr_list(R0, I0, A0) ->
    R1 = i_comments(R0, I0),
    ?D(R1),
    case i_sniff(R1) of
        '>>' ->
            R1;
        _ ->
            {R2, A1} = i_binary_expr(R1, I0),
            I1 = i_with_old_or_new_anchor(A0, A1, I0),
            case i_sniff(R2) of
                ',' ->
                    R3 = i_kind(',', R2, I1),
                    i_binary_expr_list(R3, I1, I1#i.anchor);
                Kind when Kind=:='||'; Kind=:='<='; Kind=:='<-' ->
                    % binary comprehension
                    R3 = i_kind('||', R2, I1),
                    i_binary_expr_list(R3, I1, I1#i.anchor);
                _ ->
                    R2
            end
    end.

i_binary_expr(R0, I0) ->
    {R1, A1} = i_binary_sub_expr(R0, I0),
    I1 = i_with(none, A1, I0),
    ?D(head(R1)),
    R2 = case i_sniff(R1) of
             Kind when Kind==':'; Kind=='/' ->
                 R11 = i_kind(Kind, R1, I1),
                 i_binary_specifiers(R11, I1);
             _ ->
                 R1
         end,
    {R2, A1}.

i_binary_sub_expr(R0, I0) ->
    case i_sniff(R0) of
        Kind when Kind=='('; Kind=='<<'; Kind==macro ->
            i_expr(R0, I0, none);
        Kind when Kind==var; Kind==string; Kind==integer; Kind==char ->
            R1 = i_comments(R0, I0),
            R2 = i_kind(Kind, R1, I0),
            {i_1_expr(R2, I0), hd(R1)}
    end.

i_binary_specifiers(R0, I) ->
    R1 = i_binary_specifier(R0, I),
    ?D(R1),
    case i_sniff(R1) of
        Kind when Kind==':'; Kind=='-'; Kind=='/' ->
            R2 = i_kind(Kind, R1, I),
            i_binary_specifiers(R2, I);
        _ ->
            ?D(R1),
            R1
    end.

i_binary_specifier(R0, I) ->
    case i_sniff(R0) of
        '(' ->
            {R1, _A} = i_expr(R0, I, none),
            R1;
        Kind when Kind==var; Kind==string; Kind==integer; Kind==atom; Kind==char ->
            R1 = i_comments(R0, I),
            R2 = i_kind(Kind, R1, I),
            i_1_expr(R2, I)
    end.

i_predicate_list(R, I) ->
    i_predicate_list(R, I, none).

i_predicate_list(R0, I0, A0) ->
    R1 = i_comments(R0, I0),
    {R2, A1} = i_expr(R1, I0, A0),
    I1 = i_with_old_or_new_anchor(A0, A1, I0),
    case i_sniff(R2) of
        Kind when Kind==','; Kind==';' ->
            R3 = i_kind(Kind, R2, I1),
            i_predicate_list(R3, I1, I1#i.anchor);
        _ ->
            {R2, A1}
    end.

i_binary_op(R0, I) ->
    i_one(R0, I).

i_end_paren_or_expr_list(R, I0) ->
    i_check(R, I0),
    case i_sniff(R) of
        Kind when Kind=='}'; Kind==']'; Kind==')' ->
            R;
        _ ->
            I1 = i_with(none, R, I0),
            i_expr_list(R, I1)
    end.

i_end_or_expr_list(R, I0) ->
    i_check(R, I0),
    case i_sniff(R) of
        'end' ->
            R;
        _ ->
            I1 = i_with(none, R, I0),
            i_expr_list(R, I1)
    end.

i_1_expr([?k(atom) | _] = R, I) ->
    i_one(R, I);
i_1_expr([?k(integer), ?k(dot) | _] = R, I) ->
    i_two(R, I);
i_1_expr([?k(integer) | _] = R, I) ->
    i_one(R, I);
i_1_expr([?k(string) | _] = R, I) ->
    i_one(R, I);
i_1_expr([?k(macro) | _] = R, I) ->
    i_macro(R, I);
i_1_expr([?k(float) | _] = R, I) ->
    i_one(R, I);
i_1_expr([?k(var) | _] = R, I) ->
    i_one(R, I);
i_1_expr([?k(char) | _] = R, I) ->
    i_one(R, I);
i_1_expr([?k(Kind) | _] = R0, I0) when Kind=='{'; Kind=='['; Kind=='(' ->
    R1 = i_kind(Kind, R0, I0),
    I1 = i_with(paren, R0, I0),
    R2 = i_end_paren_or_expr_list(R1, I1#i{in_block=false}),
    I2 = i_with(end_paren, R0, I0),
    i_end_paren(R2, I2);
i_1_expr([?k('<<') | _] = R0, I0) ->
    R1 = i_kind('<<', R0, I0),
    I1 = i_with('<<', R0, I0),
    R2 = i_binary_expr_list(R1, I1#i{in_block=false}),
    I2 = i_with(end_paren, R0, I0),
    i_kind('>>', R2, I2);
i_1_expr([?k('#') | _] = L, I) ->
    ?D('#'),
    {R, _A} = i_record(L, I#i{in_block=false}),
    R;
i_1_expr([?k('case') | _] = R, I) ->
    i_case(R, I);
i_1_expr([?k('if') | _] = R, I) ->
    i_if(R, I);
i_1_expr([?k('begin') | _] = R0, I0) ->
    R1 = i_kind('begin', R0, I0),
    I1 = i_with('case', R0, I0),
    R2 = i_end_or_expr_list(R1, I1#i{in_block=false}),
    i_block_end('begin', R0, R2, I0);
%%     R1 = i_kind('begin', R0, I0),
%%     I1 = i_with('case', R0, I0),
%%     R2 = i_end_or_expr_list(R1, I1#i{in_block=false}),
%%     i_block_end(T#token.kind, R2, I0);
i_1_expr([?k('receive') | _] = R, I) ->
    i_receive(R, I);
i_1_expr([?k('fun')=T | R0], I) ->
    I1 = i_with('fun', T, I),
    case i_sniff(R0) of
        '(' ->
            R1 = i_fun_clause_list(R0, I1),
            i_kind('end', R1, I);
        var ->
            case i_sniff(tl(R0)) of
                '(' ->
                    R1 = i_fun_clause_list(R0, I1#i{current=I1#i.current+1}),
                    i_kind('end', R1, I);
                _ ->
                    {R1, _A} = i_expr(R0, I1, none),
                    R1
            end;
        _ ->
            {R1, _A} = i_expr(R0, I1, none),
            R1
    end;
i_1_expr([?k('try') | _] = R, I) ->
    ?D(R),
    i_try(R, I);
i_1_expr(R0, I) ->
    R1 = i_comments(R0, I),
    case is_unary_op(R1) of
        true ->
            R2 = i_one(R1, I),
            i_1_expr(R2, i_with(after_unary_op, R2, I));
        false ->
            R1
    end.

i_macro(R0, I) ->
    R = i_one(R0, I),
    i_macro_rest(R, I).

i_macro_rest(R0, I) ->
    case i_sniff(R0) of
        Paren when Paren=:='('; Paren=:='{'; Paren=:='[' ->
            R1 = i_kind(Paren, R0, I),
            R2 = i_parameters(R1, I),
            R3 = i_end_paren(R2, I),
            i_macro_rest(R3, I);
        K when K=:=':'; K=:=','; K=:=';'; K=:=')'; K=:='}'; K=:=']'; K=:='>>'; K=:='of';
               K=:='end'; K=:='->'; K =:= '||' ->
            R0;
        K ->
            case sourcer_scan:reserved_word(K) of
                true ->
                    R0;
                _ ->
                    case is_binary_op(K) of
                        false ->
                            R2 = i_comments(R0, I),
                            i_one(R2, I);
                        true ->
                            R0
                    end
            end
    end.

i_if(R0, I0) ->
    I1 = I0#i{in_block=true},
    R1 = i_kind('if', R0, I1),
    I2 = i_with('case', R0, I1),
    R2 = i_if_clause_list(R1, I2, none),
    i_block_end('if', R0, R2, I1).

i_case(R0, I0) ->
    I1 = I0#i{in_block=true},
    R1 = i_kind('case', R0, I1),
    I2 = i_with('case', R0, I1),
    {R2, _A} = i_expr(R1, I2#i{in_block=false}, none),
    R3 = i_kind('of', R2, I2),
    R4 = i_clause_list(R3, I2),
    i_block_end('case', R0, R4, I1).

i_receive(R0, I0) ->
    I1 = I0#i{in_block=true},
    R1 = i_kind('receive', R0, I1),
    I2 = i_with('case', R0, I1),
    R2 = case i_sniff(R1) of
             'after' ->
                 R1;
             _ ->
                 i_clause_list(R1, I2)
         end,
    R4 = case i_sniff(R2) of
             'after' ->
                 I3 = i_with('after', R2, I1),
                 R3 = i_kind('after', R2, I1),
                 i_after_clause(R3, I3);
             _ ->
                 R2
         end,
    i_block_end('receive', R0, R4, I1).

i_try(R0, I0) ->
    I1 = I0#i{in_block=true},
    R1 = i_kind('try', R0, I1),
    I2 = i_with('try', R0, I1),
    R2 = i_expr_list(R1, I2),
    ?D(R2),
    R3 = case i_sniff(R2) of
             'of' ->
                 R21 = i_kind('of', R2, I1),
                 i_clause_list(R21, I2);
             _ ->
                 R2
         end,
    R4 = case i_sniff(R3) of
             'catch' ->
                 R31 = i_kind('catch', R3, I1),
                 I11 = i_with('catch', R3, I1),
                 i_catch_clause_list(R31, I11);
             _ ->
                 R3
         end,
    R5 = case i_sniff(R4) of
             'after' ->
                 R41 = i_kind('after', R4, I1),
                 I12 = i_with('after', R4, I1),
                 i_expr_list(R41, I12);
             _ ->
                 R4
         end,
    i_block_end('try', R0, R5, I0).

is_binary_op([T | _]) ->
    is_binary_op(T);
is_binary_op(?k(Kind)) ->
    is_binary_op(Kind);
is_binary_op(Op) ->
    lists:member(Op, ['andalso', 'orelse', 'div', 'rem',
                      'band', 'and', 'bor', 'bxor', 'bsl',
                      'bsr', 'or', 'xor', '<-', '=', '==', '/=',
                      '<', '>', '=<', '>=',
                      '=/=', '=:=', ':', '+', '-', '*', '/', '!',
                      '++', '--', '.', '#', '|', '::']).

is_unary_op([T | _]) ->
    is_unary_op(T);
is_unary_op(?k(Op)) ->
    lists:member(Op, ['not', '-', '?', 'catch']).

i_block_end(_Begin, R0, R1, I0) ->
    I1 = i_with(end_paren, R0, I0),
    i_kind('end', R1, I1).

i_one(R0, I) ->
    ?D({i_one, R0, I}),
    [_ | R] = i_comments(R0, I),
    R.

i_two(R0, I) ->
    R1 = i_one(R0, I),
    i_one(R1, I).

i_parameters(R, I) ->
    i_check(R, I),
    case i_sniff(R) of
        ')' ->
            R;
        _ ->
            i_expr_list(R, I#i{in_block=false})
    end.

i_record([?k('#') | R0], I0) ->
    I = I0#i{in_block=false},
    R1 = i_comments(R0, I),
    ?D(R1),
    R2 = case i_sniff(R1) of
             atom ->
                 i_atom_or_macro(R1, I);
             macro ->
                 i_atom_or_macro(R1, I);
             _ ->
                 R1
         end,
    ?D(R2),
    case i_sniff(R2) of
        '.' ->
            R3 = i_kind('.', R2, I),
            {R4, _A} = i_expr(R3, I, none),
            ?D(R4),
            {R4, I#i.anchor};
        '{' ->
            i_expr(R2, I, I#i.anchor);
        '[' ->
            i_expr(R2, I, none);
        '?' ->
            i_expr(R2, I, none);
        _ ->
            {R2, hd(R1)}
    end.

comment_kind(?kv(comment, "%%%"++_)) ->
    comment_3;
comment_kind(?kv(comment, "%%"++_)) ->
    comment_2;
comment_kind(?kv(comment, "%"++_)) ->
    comment_1;
comment_kind(C) ->
    error({badarg, C}).

i_comments([?k(white_space) | Rest], I) ->
    %% scanner add white_space
    i_comments(Rest, I);
i_comments([?k(comment) = C | Rest], I) ->
    case comment_kind(C) of
        comment_1 ->
            i_check([C], i_with(comment_1, C, I));
        comment_2 ->  %% context dependent
            i_check([C], I);
        comment_3 ->
            i_check([C], i_with(comment_3, C, I))
    end,
    i_comments(Rest, I);
i_comments(Rest, I) ->
    i_check(Rest, I),
    Rest.

skip_comments([]) ->
    [];
skip_comments([?k(comment) | Rest]) ->
    skip_comments(Rest);
skip_comments([?k(white_space) | Rest]) -> %% scanner add white_space
    skip_comments(Rest);
skip_comments(Rest) ->
    Rest.

i_atom_or_macro(R0, I) ->
    case i_sniff(R0) of
        atom ->
            i_kind(atom, R0, I);
        macro ->
            {R, _} = i_expr(R0, I, none),
            R
    end.

i_kind(Kind, R0, I) ->
    R1 = i_comments(R0, I),
    [?k(Kind) | R2] = R1,
    R2.

i_end_paren(R0, I) ->
    R1 = i_comments(R0, I),
    i_end_paren_1(R1, I).

i_end_paren_1([?k(Kind) | _] = R, I) when Kind==')'; Kind=='}'; Kind==']'; Kind=='>>'; Kind==eof ->
    i_kind(Kind, R, I).

i_form_list(R0, I) ->
    R = i_form(R0, I),
    i_form_list(R, I).

i_form(R0, I) ->
    R1 = i_comments(R0, I),
    case i_sniff(R1) of
        '-' ->
            i_declaration(R1, I);
        _ ->
            R2 = i_clause(R1, I),
            i_dot_or_semi(R2, I)
    end.

i_dot_or_semi(R, I) ->
    case i_sniff(R) of
        DS when DS==dot; DS==';' ->
            i_kind(DS, R, I);
        _ ->
            R
    end.

i_declaration(R0, I) ->
    i_check(R0, I),
    R1 = i_kind('-', R0, I),
    case skip_comments(R1) of
        [?kv(atom, 'spec') | _] ->
            R2 = i_kind(atom, R1, I),
            i_spec(R2, I);
        [?kv(atom, 'type') | _] ->
            R2 = i_kind(atom, R1, I),
            i_type(R2, I);
        _ ->
            {R2, _A} = i_expr(R1, I, none),
            i_kind(dot, R2, I)
    end.

i_type(R0, I) ->
    {R1, _A1} = i_expr(R0, I, none),
    i_kind(dot, R1, I).

i_spec_expr(R0, I) ->
    {R1, _A} = i_expr(R0, I, none),
    case i_sniff(R1) of
        'when' ->
            R11 = i_kind('when', R1, I),
            R12 = i_spec_aux(R11, I),
            R12;
        _ ->
            R1
    end.

i_spec_aux(R0, I0) ->
    R1 = i_spec_expr(R0, I0),
    case i_sniff(R1) of
        '->' ->
            R2 = i_kind('->', R1, I0),
            I1 = i_with(after_arrow, R0, I0),
            i_spec_expr(R2, I1);
        _ ->
            R1
    end.

i_spec_list(R0, I) ->
    R1 = i_spec_aux(R0, I),
    case i_sniff(R1) of
        ';' ->
            R2 = i_kind(';', R1, I),
            i_spec_list(R2, I);
        _ ->
            R1
    end.

i_spec(R0, I) ->
    R = case i_sniff(R0) of
            '(' ->
                R1 = i_kind('(', R0, I),
                R2 = i_spec_list(R1, I),
                R3 = i_kind(')', R2, I),
                R3;
            _ ->
                i_spec_list(R0, I)
        end,
    i_dot_or_semi(R, I).

i_fun_clause(R0, I0) ->
    R1 = i_comments(R0, I0),
    {R2, A} = i_expr(R0, I0, none),
    I1 = i_with(before_arrow, A, I0#i{in_block=false}),
    R3 = case i_sniff(R2) of
             'when' ->
                 R21 = i_kind('when', R2, I1),
                 {R22, _A} = i_predicate_list(R21, I1),
                 R22;
             _ ->
                 R2
         end,
    R4 = i_kind('->', R3, I1),
    I2 = i_with(fun_body, R1, I0),
    i_expr_list(R4, I2#i{in_block=true}).

i_fun_clause_list(R, I) ->
    R0 = i_fun_clause(R, I),
    case i_sniff(R0) of
        ';' ->
            R1 = i_kind(';', R0, I),
            i_fun_clause_list(R1, I);
        _ ->
            R0
    end.

i_after_clause(R0, I0) ->
    {R1, _A} = i_expr(R0, I0, none),
    R2 = i_kind('->', R1, I0),
    i_expr_list(R2, I0#i{in_block=true}).

i_clause(R0, I) ->
    {R1, A} = i_expr(R0, I, none),
    I1 = i_with(before_arrow, A, I),
    R2 = case i_sniff(R1) of
             'when' ->
                 R11 = i_kind('when', R1, I1),
                 {R12, _A} = i_predicate_list(R11, I1),
                 R12;
             _ ->
                 R1
         end,
    I2 = I1#i{in_block=true},
    R3 = i_kind('->', R2, I2),
    I3 = i_with(after_arrow, I2),
    R = i_expr_list(R3, I3),
    ?D(R),
    R.

i_clause_list(R, I) ->
    ?D(R),
    R0 = i_clause(R, I),
    ?D(R0),
    case i_sniff(R0) of
        ';' ->
            R1 = i_kind(';', R0, I),
            i_clause_list(R1, I);
        _ ->
            R0
    end.

i_if_clause(R0, I0) ->
    {R1, A} = i_predicate_list(R0, I0),
    I1 = i_with(before_arrow, A, I0),
    R2 = i_kind('->', R1, I1),
    I2 = I1#i{in_block=true},
    I3 = i_with(after_arrow, I2),
    R = i_expr_list(R2, I3),
    ?D(R),
    {R, A}.

i_if_clause_list(R0, I0, A0) ->
    {R1, A1} = i_if_clause(R0, I0),
    ?D({A1, R1}),
    I1 = i_with_old_or_new_anchor(A0, A1, I0),
    ?D(I1),
    case i_sniff(R1) of
        ';' ->
            ?D(a),
            R2 = i_kind(';', R1, I0),
            i_if_clause_list(R2, I1, A1);
        _ ->
            ?D(b),
            R1
    end.

i_catch_clause(R0, I0) ->
    R1 = i_comments(R0, I0),
    ?D(R1),
    R2 = case i_sniff(R1) of
             atom -> i_kind(atom, R1, I0);
             var -> i_kind(var, R1, I0)
         end,
    ?D(R2),
    R3 = case i_sniff(R2) of
             ':' ->
                 R21 = i_kind(':', R2, I0),
                 ?D(R21),
                 {R22, _A} = i_expr(R21, I0, none),
                 ?D(R22),
                 R22;
             _ ->
                 R2
         end,
    I1 = i_with(before_arrow, R1, I0),
    R4 = case i_sniff(R3) of
             'when' ->
                 R31 = i_kind('when', R3, I1),
                 {R32, _A0} = i_predicate_list(R31, I1),
                 R32;
             _ ->
                 R3
         end,
    ?D(R4),
    R5 = i_kind('->', R4, I1),
    ?D(R5),
    I2 = i_with(clause, R1, I0),
    R = i_expr_list(R5, I2),
    R.

i_catch_clause_list(R, I) ->
    R0 = i_catch_clause(R, I),
    ?D(R0),
    case i_sniff(R0) of
        ';' ->
            R1 = i_kind(';', R0, I),
            ?D(R1),
            i_catch_clause_list(R1, I);
        _ ->
            R0
    end.

i_sniff(L) ->
    case skip_comments(L) of
        [] ->
            eof;
        [?k(Kind) | _] ->
            Kind
    end.

%% scan(S) ->
%%     case sourcer_scan:tokens(S) of
%%         {ok, T, _} ->
%%             {ok, T};
%%         Error ->
%%             Error
%%     end.
