-module(sourcer_indent).

-export([
%%         indent_line/4,
         lines/1,
         lines/2
        ]).

-ifdef(DEBUG).
-define(D(T), io:format("~p\n", [{??T, ?MODULE, ?LINE, T}])).
-else.
-define(D(T), ok).
-endif.


%-include("include/sourcer_token.hrl").

-define(k(X), {X,_,_,_}).
-define(kv(K,V), {K,_,_,V}).
-define(line(X), {_,{X,_},_,_}).
-define(col(X), {_,{_,X},_,_}).

%% TODO: change into multiples of IndentW
default_indent_prefs() ->
    [{after_op, 4},        %% indentW
     {before_arrow, 2},    %% indentW div 2
     {after_arrow, 4},     %% indentW
     {clause, 4},          %% indentW
     {'icr', 8},           %% indentW * 2 from 'case' or 'try' ... icr = if, case, receive
     {parameters, 2},      %% indentW div 2
     {'when', 6},          %% indentW + indentW div 2
     {'after_when', 10},   %% indentW * 2 + indentW div 2 from 'case' or 'try' ...
     {'fun', 4},           %% indentW
     {fun_body, 8},        %% indentW * 2
     {paren, 1},           %% after '(' '[' '#{'
     {delimiter, -1},      %% line starts with '|' or ',' ';'
     {delimiter_bin,  1},  %% line starts with ',' after a '<<'
     {delimiter_clause,  2},  %% line starts with ';' between icr clauses
     {delimiter_spec, -2},    %% line starts with '|' in specs or types
     {'<<', 2},            %% '<<' is 2 wide
     {end_paren, -1},      %% go left 1 when ')' '}' ']'
     {end_paren2, -2},     %% go left 2 when '>>'
     {end_block, 0},       %% end aligns with icr start
     {record_def, 2},      %% indentW div 2
     {comment_3, 0},       %% start of the line
     {comment_2, 0},       %% start of the expression
     {comment_1, 48},      %% column 48

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

-record(i, {indent_line, anchor, current}).

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
    I = #i{anchor=[], indent_line=LineN, current=none},
    try
        i_form_list(Tokens, I),
        ?D(no_catch),
        4
    catch
        throw:{indent, A, C} ->
            ?D({indent, A, C}),
            get_indent_of(A, C, Prefs);
        throw:{indent_eof, A, C} ->
            ?D({indent_eof, A, C}),
            get_indent_of(A, C, Prefs);
        throw:{indent_to, N} ->
            ?D(N),
            N;
        error:_E ->
            ?D(_E),
            io:format("~p:~p: Error: ~P~n  ~P~n",
                      [?MODULE, ?LINE, _E, 20, erlang:get_stacktrace(), 20]),
            error(parse_error),
            0
    end.

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
reindent_line("\n", _, _) -> "\n";
reindent_line("\r\n", _, _) -> "\r\n";
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
            #i{indent_line=IL, anchor=A, current=C})
  when L >= IL ->
    {indent, A, C};
i_check_aux([?k(eof) | _], #i{anchor=A, current=C}) ->
    {indent_eof, A, C};
i_check_aux([eof | _], #i{anchor=A, current=C}) ->
    {indent_eof, A, C};
i_check_aux([], I) ->
    i_check_aux([eof], I);
i_check_aux(_, _) ->
    not_yet.

i_check(T, I) ->
    case i_check_aux(T, I) of
        not_yet ->
            not_yet;
        Throw ->
            ?D({T, I#i{}}),
            throw(Throw)
    end.

indent_by(Key, Prefs) ->
    proplists:get_value(Key, Prefs, 0).

get_indent_of([{What, ?col(CA)}=_A|_], {C, ?col(Exp)}, Prefs) when is_atom(What), is_atom(C) ->
    ?D({_A, _C}),
    Col0 = CA+indent_by(What, Prefs),
    Extra = indent_by(C, Prefs),
    min(Col0+Extra, Exp)-1;
get_indent_of([{What, ?col(CA)}=_A|_], C, Prefs) when is_atom(What), is_atom(C) ->
    ?D({_A, _C}),
    Col0 = CA+indent_by(What, Prefs),
    Extra = indent_by(C, Prefs),
    Col0+Extra-1;
get_indent_of([], _, _) ->
    0.

head([H | _]) -> H;
head(H) -> H.

i_with(W, I) ->
    I#i{current=W}.

push(Tag, #i{anchor=[{_,A0}|As]} = I) ->
    I#i{current=none, anchor=[{Tag, A0}|As]}.

push(Tag, A, #i{anchor=As} = I) ->
    I#i{current=none, anchor=[{Tag, head(skip_comments(A))}|As]}.

pop(#i{anchor=[{_,_}|As]} = I) ->
    I#i{anchor=As}.

pop_until({_,A}, #i{anchor=As0} = I) ->
    As = lists:dropwhile(fun({_,W}) -> W /= A end, As0),
    I#i{anchor=As};
pop_until(What, #i{anchor=As0} = I) when is_atom(What) ->
    As = lists:dropwhile(fun({W,_}) -> W /= What end, As0),
    I#i{anchor=As}.

keep_one({_, A}, #i{anchor=[{_,A}|_]} = I) ->
    I;
keep_one({_, A}, #i{anchor=[{_,_},{_,A}|_]} = I) ->
    I;
keep_one(Until, #i{anchor=[{_,_}|As]} = I) ->
    keep_one(Until, I#i{anchor=As}).

top(#i{anchor=[Top|_]}) ->
    Top.

i_expr([], I, _A) ->
    {[], I};
i_expr(R0, I0, A) ->
    R1 = i_comments(R0, I0),
    R2 = i_1_expr(R1, I0),
    I1 = push(none, R1, I0),
    ?D({i_expr, R1}),
    case i_sniff(R1) of
        string ->
            case i_sniff(i_kind(string, R1, I1)) of
                string ->
                    i_expr(R2, I1, A);
                _ ->
                    i_expr_rest(R2, I1, top(I1))
            end;
        macro ->
            case i_sniff(i_kind(macro, R1, I1)) of
                macro ->
                    i_expr(R2, push(after_op, I1), A);
                _ ->
                    i_expr_rest(R2, I1, top(I1))
            end;
        _ ->
            i_expr_rest(R2, I1, A)
    end.

i_expr_rest(R0, I, A) ->
    case i_sniff(R0) of
        '(' -> % function call
            R1 = i_par_list(R0, I),
            case is_binary_op(i_sniff(R1)) of
                true -> i_expr_rest(R1, I, A);
                false -> {R1, I}
            end;
        eof ->
            {R0, I};
        ':' -> % external function call
            R1 = i_kind(':', R0, I),
            R2 = i_1_expr(R1, I),
            i_expr_rest(R2, I, A);
        '<=' -> % within binary comprehension
            R1 = i_kind('<=', R0, I),
            {R2, _A} = i_expr(R1, push(after_op, I), A),
            {R2, I};
        '=' -> % match/assignment
            R1 = i_binary_op(R0, I),
            {R2, _A} = i_expr(R1, push(after_op, I), top(I)),
            {R2, I};
        '=>' -> % maps
            R1 = i_binary_op(R0, I),
            {R2, _A} = i_expr(R1, push(after_op, I), A),
            {R2, I};
        ':=' -> % maps
            R1 = i_binary_op(R0, I),
            {R2, _A} = i_expr(R1, push(after_op, I), A),
            {R2, I};
        '|' -> %% List
            {R0, I};
        '::' -> %% type
            R1 = i_kind('::', R0, I),
            {R2, _A} = i_type(R1, push(before_arrow, I), A),
            {R2, I};
        _ ->
            case is_binary_op(i_sniff(R0)) of
                true ->
                    ?D({A, R0}),
                    I1 = pop_until(A, I),
                    R1 = i_binary_op(R0, I1),
                    {R2, _A} = i_expr(R1, I1, A),
                    {R2, I};
                false ->
                    ?D({R0, A}),
                    {R0, I}
            end
    end.

i_par_list(R0, I0) ->
    R1 = i_kind('(', R0, I0),
    I1 = case top(I0) of
             {_, ?k(F)=A} when F =:= 'atom'; F =:= 'var' ->
                 I10 = push(paren, R0, pop(I0)),
                 push(parameters, A, I10);
             _ ->
                 push(paren, R0, I0)
         end,
    i_end_paren_or_expr_list(R1,I1).

i_end_paren_or_expr_list(R0, I0) ->
    i_check(R0, I0),
    {R1, I} = i_expr_list(R0, I0, top(I0)),
    i_end_paren(R1, I, top(I0)).

i_end_or_expr_list(R, I0) ->
    i_check(R, I0),
    case i_sniff(R) of
        'end' ->
            R;
        _ ->
            I1 = push(none, R, I0),
            i_expr_list(R, I1)
    end.

i_expr_list(R, I) ->
    {Rest, _} = i_expr_list(R, I, top(I)),
    Rest.

i_expr_list(R0, I0, A0) ->
    R1 = i_comments(R0, I0),
    ?D(R1),
    {R2, I11} = i_expr(R1, I0, A0),
    I1 = keep_one(A0, I11),
    ?D({R2, I1, I11}),
    case i_sniff(R2) of
        Delim when Delim =:= ','; Delim =:= '|' ->
            I10 = pop_until(A0, I0),
            {_, Start} = case A0 of
                             {parameters,_} -> top(pop(I10));
                             {record_def,_} -> top(pop(I10));
                             _ -> top(I10)
                         end,
            R3 = i_kind(Delim, R2, i_with({delimiter, Start}, I1)),
            i_expr_list(R3, I1, A0);
        '||' ->
            R3 = i_kind('||', R2, I1),
            i_expr_list(R3, push(clause, element(2,A0), pop_until(A0,I1)), A0);
        _ ->
            {R2, I1}
    end.

i_binary_expr_list(R0, I0) ->
    {R, I} = i_binary_expr_list(R0, I0, top(I0)),
    {_, A0} = top(pop_until(top(I0), I)),
    i_kind('>>', R, i_with({end_paren2, A0}, I)).

i_binary_expr_list(R0, I0, A0) ->
    R1 = i_comments(R0, I0),
    ?D(R1),
    {R2, I1} = i_binary_expr(R1, I0),
    I2 = keep_one(A0, I1),
    case i_sniff(R2) of
        ',' ->
            R3 = i_kind(',', R2, push(delimiter_bin, pop_until(A0, I2))),
            i_binary_expr_list(R3, I2, A0);
        Kind when Kind=:='||'; Kind=:='<='; Kind=:='<-' ->
            %% binary comprehension
            R3 = i_kind('||', R2, I2),
            i_expr_list(R3, push(clause, pop_until(A0,I2)), A0);
        _ ->
            {R2, I2}
    end.

i_binary_expr(R0, I0) ->
    {R1, I1} = i_binary_sub_expr(R0, I0),
    ?D(head(R1)),
    case i_sniff(R1) of
        Kind when Kind==':'; Kind=='/' ->
            R11 = i_kind(Kind, R1, I1),
            {i_binary_specifiers(R11, I1), I1};
        _ ->
            {R1, I1}
    end.

i_binary_sub_expr(R0, I0) ->
    case i_sniff(R0) of
        Kind when Kind=='('; Kind=='<<'; Kind==macro ->
            i_expr(R0, I0, top(I0));
        Kind when Kind==var; Kind==string; Kind==integer; Kind==char ->
            R1 = i_comments(R0, I0),
            R2 = i_kind(Kind, R1, I0),
            {i_1_expr(R2, I0), push(none, R1, I0)};
        _ ->
            {R0, I0}
    end.

i_binary_specifiers(R0, I) ->
    R1 = i_binary_specifier(R0, I),
    ?D(R1),
    case i_sniff(R1) of
        Kind when Kind==':'; Kind=='-'; Kind=='/' ->
            R2 = i_kind(Kind, R1, I),
            i_binary_specifiers(R2, I);
        '*' -> %% Allowed in typespecs
            R2 = i_kind('*', R1, I),
            i_binary_specifiers(R2, I);
        _ ->
            ?D(R1),
            R1
    end.

i_binary_specifier(R0, I) ->
    case i_sniff(R0) of
        '(' ->
            {R1, _A} = i_expr(R0, I, top(I)),
            R1;
        Kind when Kind==var; Kind==string; Kind==integer; Kind==atom; Kind==char ->
            R1 = i_comments(R0, I),
            i_kind(Kind, R1, I)
    end.

i_predicate_list(R, I) ->
    i_predicate_list(R, I, top(I)).

i_predicate_list(R0, I0, A0) ->
    R1 = i_comments(R0, I0),
    {R2, I1} = i_expr(R1, I0, A0),
    case i_sniff(R2) of
        Kind when Kind==','; Kind==';' ->
            R3 = i_kind(Kind, R2, I1),
            i_predicate_list(R3, I1, A0);
        _ ->
            {R2, I1}
    end.

i_binary_op(R0, I) ->
    i_one(R0, I).

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
i_1_expr([?k(var), ?k('#') | _] = L, I) ->
    {R, _A} = i_record(L, I),
    R;
i_1_expr([?k(var) | _] = R, I) ->
    i_one(R, I);
i_1_expr([?k(char) | _] = R, I) ->
    i_one(R, I);
i_1_expr([?k(Kind) | _] = R0, I0) when Kind=='{'; Kind=='['; Kind=='(' ->
    R1 = i_kind(Kind, R0, I0),
    I1 = push(paren, R0, I0),
    i_end_paren_or_expr_list(R1, I1);
i_1_expr([?k('<<') | _] = R0, I0) ->
    R1 = i_kind('<<', R0, I0),
    I1 = push('<<', R0, I0),
    i_binary_expr_list(R1, I1);
i_1_expr([?k('#') | _] = L, I) ->
    {R, _A} = i_record(L, I),
    R;
i_1_expr([?k('case') | _] = R, I) ->
    i_case(R, I);
i_1_expr([?k('if') | _] = R, I) ->
    i_if(R, I);
i_1_expr([?k('begin') | _] = R0, I0) ->
    R1 = i_kind('begin', R0, I0),
    I1 = push(clause, R0, I0),
    R2 = i_end_or_expr_list(R1, I1),
    i_block_end('begin', R0, R2, I0);
i_1_expr([?k('receive') | _] = R, I) ->
    i_receive(R, I);
i_1_expr([?k('fun')=T | R0], I) ->
    I1 = push('fun', T, I),
    case i_sniff(R0) of
        '(' ->
            R1 = i_fun_clause_list(R0, I1, top(I1)),
            i_kind('end', R1, push(none, I1));
        var ->
            case i_sniff(tl(R0)) of
                '(' ->
                    R1 = i_fun_clause_list(R0, I1, top(I1)),
                    i_kind('end', R1, push(none, I1));
                _ ->
                    {R1, _A} = i_expr(R0, I1, top(I1)),
                    R1
            end;
        _ ->
            {R1, _A} = i_expr(R0, I1, top(I1)),
            R1
    end;
i_1_expr([?k('try') | _] = R, I) ->
    ?D(R),
    i_try(R, I);
i_1_expr([?k('...') | _] = R, I) ->
    i_one(R, I);
i_1_expr(R0, I) ->
    R1 = i_comments(R0, I),
    case is_unary_op(R1) of
        true ->
            R2 = i_one(R1, I),
            i_1_expr(R2, push(after_op, R2, I));
        false ->
            R1
    end.

i_macro(R0, I) ->
    R = i_one(R0, I),
    i_macro_rest(R, I).

i_macro_rest(R0, I) ->
    case i_sniff(R0) of
        '(' ->
            R1 = i_par_list(R0, I),
            i_macro_rest(R1, I);
        Paren when Paren=:='{'; Paren=:='[' ->
            R1 = i_kind(Paren, R0, I),
            I1 = push(paren, R0, I),
            R3 = i_end_paren_or_expr_list(R1, I1),
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

i_if(R0, I1) ->
    R1 = i_kind('if', R0, I1),
    I2 = push(clause, R0, I1),
    R2 = i_if_clause_list(R1, I2, top(I2)),
    i_block_end('if', R0, R2, I1).

i_case(R0, I1) ->
    R1 = i_kind('case', R0, I1),
    I2 = push(clause, R0, I1),
    {R2, _A} = i_expr(R1, I2, top(I2)),
    R3 = i_kind('of', R2, I1),
    R4 = i_clause_list(R3, I2, icr),
    i_block_end('case', R0, R4, I1).

i_receive(R0, I1) ->
    R1 = i_kind('receive', R0, I1),
    I2 = push(clause, R0, I1),
    R2 = case i_sniff(R1) of
             'after' ->
                 R1;
             _ ->
                 i_clause_list(R1, I2, icr)
         end,
    R4 = case i_sniff(R2) of
             'after' ->
                 I3 = push('after', R2, I1),
                 R3 = i_kind('after', R2, I1),
                 i_after_clause(R3, I3);
             _ ->
                 R2
         end,
    i_block_end('receive', R0, R4, I1).

i_try(R0, I1) ->
    R1 = i_kind('try', R0, I1),
    I2 = push(clause, R0, I1),
    R2 = i_expr_list(R1, I2),
    ?D(R2),
    R3 = case i_sniff(R2) of
             'of' ->
                 R21 = i_kind('of', R2, push(none, I2)),
                 i_clause_list(R21, I2, icr);
             _ ->
                 R2
         end,
    R4 = case i_sniff(R3) of
             'catch' ->
                 R31 = i_kind('catch', R3, push(none, I2)),
                 I11 = push(clause, R3, I1),
                 i_clause_list(R31, I11, icr);
             _ ->
                 R3
         end,
    R5 = case i_sniff(R4) of
             'after' ->
                 R41 = i_kind('after', R4, push(none, I2)),
                 I12 = push(clause, R4, I1),
                 i_expr_list(R41, I12);
             _ ->
                 R4
         end,
    i_block_end('try', R0, R5, I1).

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
                      '++', '--', '.', '#', '|']).

is_unary_op([T | _]) ->
    is_unary_op(T);
is_unary_op(?k(Op)) ->
    lists:member(Op, ['not', '-', '?', 'catch']).

i_block_end(_Begin, R0, R1, I0) ->
    I1 = push(end_block, R0, I0),
    i_kind('end', R1, I1).

i_one(R0, I) ->
    ?D({i_one, R0, I}),
    [_ | R] = i_comments(R0, I),
    R.

i_two(R0, I) ->
    R1 = i_one(R0, I),
    i_one(R1, I).

i_record(R00, I) ->
    {R0,A0} = case i_sniff(R00) of
                  '#' ->
                      R01 = i_kind('#', R00, I),
                      {R01, head(R01)};
                  'var' ->
                      R01 = i_kind('var', R00, I),
                      {i_kind('#', R01, I), head(R00)}
         end,
    R1 = i_comments(R0, I),
    ?D(R1),
    {R2,T} = case i_sniff(R1) of
                 atom ->
                     {i_atom_or_macro(R1, I),record};
                 macro ->
                     {i_atom_or_macro(R1, I),record};
                 '{' ->
                     {R1, map};
                 _ ->
                     {R1, undefined}
             end,
    ?D(R2),
    case i_sniff(R2) of
        '.' ->
            R3 = i_kind('.', R2, I),
            R4 = i_atom_or_macro(R3, I),
            case i_sniff(R4) of
                '#' -> i_record(R4, I);
                _ -> {R4, I}
            end;
        '{' when T =:= map ->
            R3 = i_kind('{', R2, I),
            I1 = push(paren, R2, I),
            {i_end_paren_or_expr_list(R3, I1), I};
        '{' ->
            R3 = i_kind('{', R2, I),
            I10 = push(paren, R2, I),
            I1 = push(record_def, A0, I10),
            {i_end_paren_or_expr_list(R3, I1), I};
        '[' ->
            i_expr(R2, I, top(I));
        '?' ->
            i_expr(R2, I, top(I));
        _ ->
            {R2, I}
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
            i_check([C], push(comment_1, C, I));
        comment_2 ->  %% context dependent
            i_check([C], I);
        comment_3 ->
            i_check([C], push(comment_3, C, I))
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
            {R, _} = i_expr(R0, I, top(I)),
            R
    end.

i_kind(Kind, R0, I) ->
    R1 = i_comments(R0, I),
    [?k(Kind) | R2] = R1,
    R2.

i_end_paren(R0, I0, A) ->
    {paren, BegParen} = top(pop_until(paren, pop_until(A, I0))),
    I = i_with({end_paren, BegParen}, I0),
    R1 = i_comments(R0, I),
    i_end_paren_1(R1, I).

i_end_paren_1([?k(Kind) | _] = R, I) when Kind==')'; Kind=='}'; Kind==']'; Kind==eof ->
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
            R3 = i_clause(R1, push(none, R1, I), after_arrow),
            i_dot_or_semi(R3, I)
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
            i_spec(R2, push(spec, R1, I));
        [?kv(atom, Type) | _] when Type =:= 'type'; Type =:= 'opaque' ->
            R2 = i_kind(atom, R1, I),
            i_typedef(R2, push(type, R1, I));
        _ ->
            {R2, _A} = i_expr(R1, push(none, R1, I), head(R0)),
            i_kind(dot, R2, I)
    end.

i_typedef(R0, I0) ->
    {R1, I1} = i_expr(R0, I0, top(I0)),
    R2 = i_kind('::', R1, I1),
    I2 = push(before_arrow, I1),
    R3 = i_type(R2, I2, top(I2)),
    i_kind(dot, R3, I0).

i_type(R0, I0, A0) ->
    {R1, I1} = case i_sniff(R0) of
                   'fun' -> i_type_fun(R0, I0);
                   _ -> i_expr(R0, I0, A0)
               end,
    case i_sniff(R1) of
        '|' ->
            I = keep_one(A0, I1),
            R2 = i_kind('|', R1, push(delimiter_spec, I)),
            i_type(R2, I, A0);
        '::' ->
            I = keep_one(A0, I1),
            R2 = i_kind('|', R1, push(delimiter_spec, I)),
            i_type(R2, I1, A0);
        _ ->
            {R1, I1}
    end.

i_type_fun(R0, I0) ->
    R1 = i_kind('fun', R0, I0),
    R2 = i_kind('(', R1, I0),
    I1 = push(none, R0, I0),
    I2 = push(paren, R1, I1),
    {R3, _} = i_spec_expr(R2, I0, top(I0)),
    R4 = i_kind('->', R3, I2),
    {R5, _} = i_spec_expr(R4, push(after_arrow, R3, I2), top(I2)),
    R6 = i_kind(')', R5, push(none, R1, I0)),
    {R6, I1}.

i_spec_expr(R0, I0, A0) ->
    {R1, I1} = i_type(R0, I0, A0),
    case i_sniff(R1) of
        'when' ->
            R11 = i_kind('when', R1, I1),
            i_spec_expr(R11, I1, top(I1));
        ',' ->
            R11 = i_kind(',', R1, push(delimiter_spec, I1)),
            i_spec_expr(R11, I1, top(I1));
        _ ->
            {R1,I1}
    end.

i_spec_aux(R0, I0) ->
    {R1,I1} = case i_sniff(R0) of
                  atom ->
                      R10 = i_kind(atom, R0, I0),
                      {R10, push(none, R10, I0)};
                  '(' ->
                      {R0, I0}
              end,
    R2 = i_kind('(', R1, I1),
    {R3, _I2} = i_spec_expr(R2, I1, top(I1)),
    R4 = i_kind(')', R3, I1),
    case i_sniff(R4) of
        '->' ->
            R5 = i_kind('->', R4, I1),
            I3 = push(after_arrow, R0, I1),
            {R6, _} = i_spec_expr(R5, I3, top(I3)),
            {R6, push(none, R1, I1)};
        _ ->
            {R4, I1}
    end.

i_spec_list(R0, I0, A0) ->
    {R1,I1} = i_spec_aux(R0, I0),
    case i_sniff(R1) of
        ';' ->
            I2 = keep_one(A0, I1),
            R2 = i_kind(';', R1, I2),
            i_spec_list(R2, I2, A0);
        _ ->
            R1
    end.

i_spec(R0, I) ->
    R = i_spec_list(R0, I, top(I)),
    i_dot_or_semi(R, I).

i_fun_clause(R0, I0, A0) ->
    R1 = i_comments(R0, I0),
    {R2, I10} = i_expr(R1, I0, top(I0)),
    I1 = push(before_arrow, I10),
    R3 = case i_sniff(R2) of
             'when' ->
                 I11 = push('when', I0),
                 R21 = i_kind('when', R2, I11),
                 I12 = push('after_when', I11),
                 {R22, _A} = i_predicate_list(R21, I12),
                 R22;
             _ ->
                 R2
         end,
    R4 = i_kind('->', R3, I1),
    I2 = push(fun_body, pop_until(A0, I0)),
    {i_expr_list(R4, I2), I10}.

i_fun_clause_list(R, I0, A0) ->
    {R0,I1} = i_fun_clause(R, I0, A0),
    I2 = keep_one(A0, I1),
    case i_sniff(R0) of
        ';' ->
            R1 = i_kind(';', R0, push(delimiter_clause, A0, I2)),
            i_fun_clause_list(R1, I2, A0);
        _ ->
            R0
    end.

i_after_clause(R0, I0) ->
    {R1, _A} = i_expr(R0, I0, top(I0)),
    R2 = i_kind('->', R1, I0),
    i_expr_list(R2, push(icr, I0)).

i_clause(R0, I0, Tag) ->
    {R1, I10} = i_expr(R0, I0, top(I0)),
    I1 = push(before_arrow, I10),
    {R4,I3} = case i_sniff(R1) of
                  'when' ->
                      R2 = i_kind('when', R1, I1),
                      When = case Tag of
                                 icr -> 'after_when';
                                 _ -> 'when'
                             end,
                      I2 = push(When, I0),
                      {R3, _A} = i_predicate_list(R2, I2),
                      {R3,I2};
                  _ ->
                      {R1, I1}
              end,
    R5 = i_kind('->', R4, I3),
    I5 = push(Tag, I0),
    R = i_expr_list(R5, I5),
    ?D(R),
    R.

i_clause_list(R, I, Tag) ->
    ?D(R),
    R0 = i_clause(R, I, Tag),
    ?D(R0),
    case i_sniff(R0) of
        ';' ->
            R1 = i_kind(';', R0, push(delimiter_clause, I)),
            i_clause_list(R1, I, Tag);
        _ ->
            R0
    end.

i_if_clause(R0, I0, A0) ->
    {R1, I1} = i_predicate_list(R0, I0),
    R2 = i_kind('->', R1, I1),
    I2 = push(icr, pop_until(A0, I1)),
    R = i_expr_list(R2, I2),
    ?D(R),
    {R, I1}.

i_if_clause_list(R0, I0, A0) ->
    {R1, I1} = i_if_clause(R0, I0, A0),
    ?D({A1, R1}),
    I2 = keep_one(A0, I1),
    ?D(I1),
    case i_sniff(R1) of
        ';' ->
            R2 = i_kind(';', R1, push(delimiter_clause, pop_until(A0, I2))),
            i_if_clause_list(R2, I2, A0);
        _ ->
            ?D(b),
            R1
    end.

i_sniff(L) ->
    case skip_comments(L) of
        [] ->
            eof;
        [?k(Kind) | _] ->
            Kind
    end.
