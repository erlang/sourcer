-module(sourcer_indent).

-export([
         line/2, line/3,
         all/1, all/2,
         lines/3, lines/4
        ]).

%%-define(DEBUG, true).
-ifdef(DEBUG).
-define(D(T), io:format(user, "~p\n", [{??T, ?MODULE, ?LINE, T}])).
-define(D(F,A), io:format(user, "~w:~w: " ++ F, [?MODULE, ?LINE|A])).
-else.
-define(D(T), ok).
-define(D(F,A), ok).
-endif.

-define(k(X), {X,_,_,_}).
-define(kv(K,V), {K,_,_,V}).
-define(line(X), {_,{X,_},_,_}).
-define(col(X), {_,{_,X},_,_}).
-define(loc(X), {_,X,_,_}).

default_indent_prefs(Prefs) ->
    IndentW = proplists:get_value(indentW, Prefs, 4),

    [{after_op, IndentW},                         %% indentW
     {before_arrow, IndentW div 2},               %% indentW div 2
     {after_arrow, IndentW},                      %% indentW
     {clause, IndentW},                           %% indentW
     {'icr', IndentW * 2},                        %% indentW * 2 from 'case' or 'try' ... icr = if, case, receive
     {parameters, IndentW div 2},                 %% indentW div 2
     {'when', IndentW + IndentW div 2},           %% indentW + indentW div 2
     {'after_when', IndentW * 2 + IndentW div 2}, %% indentW * 2 + indentW div 2 from 'case' or 'try' ...
     {'fun', IndentW},                            %% indentW
     {fun_body, IndentW * 2},                     %% indentW * 2
     {paren, 1},                                  %% after '(' '[' '#{'
     {delimiter, -1},                             %% line starts with '|' or ',' ';'
     {delimiter_bin,  1},                         %% line starts with ',' after a '<<'
     {delimiter_clause,  2},                      %% line starts with ';' between icr clauses
     {delimiter_spec, -2},                        %% line starts with '|' in specs or types
     {'<<', 2},                                   %% '<<' is 2 wide
     {end_paren, -1},                             %% go left 1 when ')' '}' ']'
     {end_paren2, -2},                            %% go left 2 when '>>'
     {end_block, 0},                              %% end aligns with icr start
     {record_def, IndentW div 2},                 %% indentW div 2
     {unary_op, IndentW div 2},                   %% indentW div 2
     {comment_3, 0},                              %% start of the line
     {comment_2, 0},                              %% start of the expression
     {comment_1, 48},                             %% column 48

     {indentW, IndentW},
     {use_tabs, false},
     {tab_len, IndentW},
     {suggest, false}                             %% Indent empty lines and after eof when editing code
    ].

%%
%% API Functions

-spec all(InputSrc::string()) ->
          OutSrc::string().
all(S) ->
    all(S, []).

-spec all(InputSrc::string(),
            Opts::[{atom(), integer()|boolean()}]) ->
          OutSrc::string().
all(S, Prefs0) ->
    Tokens = sourcer_scan:tokens(S),
    Lines = array:from_list(sourcer_scan:split_lines(S)),
    Prefs = get_prefs(Prefs0),
    Stop = fun(Line, I) -> check_indent_lines(Line, I, Prefs) end,
    Ls = do_indent_lines(0, all, Tokens, Lines, Stop, Prefs),
    unicode:characters_to_list(array:to_list(Ls)).

-spec lines(From::non_neg_integer(),
            To::non_neg_integer(),
            InputSrc::string()) ->
          Changed::[{LineNr::integer(), OrigLine::string(), IndentedLine::string()}].
lines(From, To, Src) ->
    lines(From, To, Src, []).

-spec lines(From::non_neg_integer(),
            To::non_neg_integer(),
            InputSrc::string(),
            Opts::[{atom(), integer()|boolean()}]) ->
          Changed::[{LineNr::integer(), OrigLine::string(), IndentedLine::string()}].
lines(From, To, Src, Prefs0) ->
    Tokens = sourcer_scan:tokens(Src, {0,1}, To+1),
    Lines = array:from_list(sourcer_scan:split_lines(Src)),
    Prefs = get_prefs(Prefs0),
    Stop = fun(Line, I) -> check_indent_lines(Line, I, Prefs) end,
    Ls = do_indent_lines(From, To, Tokens, Lines, Stop, Prefs),
    pick_lines(From, To, Lines, Ls).

-spec line(LineNr::non_neg_integer(),
           InputSrc::string()) ->
          Line :: string().
line(Line, Src) ->
    line(Line, Src, []).

-spec line(LineNr::non_neg_integer(),
           InputSrc::string(),
           Opts::[{atom(), integer()|boolean()}]) ->
          Line :: string().
line(Line, Src, Prefs0) ->
    Tokens = sourcer_scan:tokens(Src, {0,1}, Line+1),
    Lines = array:from_list(sourcer_scan:split_lines(Src)),
    Prefs = get_prefs([{suggest, true}|Prefs0]),
    Stop = fun(This, I) -> check_indent_lines(This, I, Prefs) end,
    Ls = do_indent_lines(Line, Line, Tokens, Lines, Stop, Prefs),
    array:get(Line, Ls).

%%
%% Local Functions
%%

-record(i, {indent_line, anchor, current, check, skip}).

get_prefs([], OldP, Acc) ->
    maps:from_list(Acc ++ OldP);
get_prefs([{Key, Value} | Rest], OldP, Acc) ->
    P = lists:keydelete(Key, 1, OldP),
    get_prefs(Rest, P, [{Key, Value} | Acc]).

get_prefs(Prefs) ->
    get_prefs(Prefs, default_indent_prefs(Prefs), []).

do_indent_lines(LineNr, To, _Forms, Lines, _Stop, _Prefs)
  when LineNr > To ->
    Lines;
do_indent_lines(LineNr0, To, Forms0, Lines0, Stop, Prefs) ->
    case fetch_form(LineNr0, Forms0) of
        eof ->
            erase(?MODULE),
            Lines0;
        {Form, Forms1} ->
            put(?MODULE, LineNr0),
            Indent = indent(Form, LineNr0, Stop),
            {LineNr, Forms, Lines} =
                do_indent_cont(Indent, LineNr0, Lines0, Form, Forms1, Prefs),
            do_indent_lines(LineNr, To, Forms, Lines, Stop, Prefs)
    end.

do_indent_cont({eof, LineNr, ToCol}, Nr, Lines0, _Form, Forms,
               #{suggest:=Suggest} = Prefs) ->
    case Suggest of
        true when LineNr =:= Nr ->
            {_Changed, Lines} = reindent_line(LineNr, Lines0, ToCol, Prefs),
            {LineNr, Forms, Lines};
        _ ->
            {LineNr, Forms, Lines0}
    end;
do_indent_cont({IndLine, ToCol, Skip}, LineNr0, Lines0, Form0, Forms, Prefs) ->
    %% IndLine is less then LineNr0 if scan_error
    LineNr = max(IndLine, LineNr0),
    {Changed, Lines} = reindent_line(LineNr, Lines0, ToCol, Prefs),
    case Form0 of
        {_, _, {LineNr,_}} ->
            {LineNr+1, Forms, Lines};
       _ ->
            Form = update_line_tokens(Changed, Skip, LineNr, ToCol, Form0),
            {LineNr+1, [Form|Forms], Lines}
    end.

fetch_form(Line, [{_, _, {End, _}}=Form|Rest])
  when Line =< End ->
    {Form, Rest};
fetch_form(Line, [_|Rest]) ->
    fetch_form(Line, Rest);
fetch_form(_, []) ->
    eof.
update_line_tokens(false, undefined, _LineNr, _Col, Form) ->
    Form;
update_line_tokens(true, undefined, LineNr, Col, {Tokens, Start, End}) ->
    {update_token_1(Tokens, LineNr, Col+1), Start, End};
update_line_tokens(Changed, Skip, LineNr, Col, {Tokens0, _, End}) ->
    [Skip|Tokens] = lists:dropwhile(fun(T) -> Skip =/= T end, Tokens0),
    case Tokens of
        [] ->
            {[], End, End};
        [?loc(Start)|_] when not Changed ->
            {Tokens, Start, End};
        [?loc(Start)|_] ->
            {update_token_1(Tokens, LineNr, Col+1), Start, End}
    end.

update_token_1([{T,{LineNr,FromCol},S,V}=_Tok|Rest], LineNr, Col) ->
    [{T,{LineNr,Col},S,V}|update_token_line(Rest, LineNr, Col-FromCol)];
update_token_1([T|Rest], LineNr, Col) ->
    [T|update_token_1(Rest,LineNr,Col)];
update_token_1([], _, _) ->
    [].

update_token_line([{T,{LineNr,Col},S,V}=_Tok|Rest], LineNr, Move) ->
    [{T,{LineNr,Col+Move},S,V}|update_token_line(Rest, LineNr, Move)];
update_token_line(OtherLines, _, _) ->
    OtherLines.

%%
reindent_line(LineNr, Lines, Col, Prefs) ->
    Src = array:get(LineNr, Lines),
    case reindent_line_0(Src, Col, Prefs) of
        {true, New} -> {true, array:set(LineNr, New, Lines)};
        {false, _} -> {false, Lines}
    end.

reindent_line_0("\n"=L, _, #{suggest:=false}) -> {false, L};
reindent_line_0("\r\n"=L, _, #{suggest:=false}) -> {false, L};
reindent_line_0(Line, Col, Prefs) ->
    TabLen  = indent_by(tab_len, Prefs),
    UseTabs = indent_by(use_tabs, Prefs),
    case strip_ws(Line, 0, 0) of
        {_, Col, 0} when Col =:= Col ->
            {false, Line};
        {Str, _, _} when Col =:= 0 ->
            {true, Str};
        {Str, S, T} ->
            case S+T*TabLen of
                Col ->
                    %% Need to update forms with new column
                    %% since scanner counts \t as 1 char
                    {true, Line};
                _ ->
                    New = lists:duplicate(Col, $ ) ++ Str,
                    %% ?D("~2w => ~2w ~s~n", [S+T*TabLen, Col, Str]),
                    {true, entab(New, UseTabs, TabLen)}
            end
    end.

strip_ws([$\s|Str], S, T) ->
    strip_ws(Str, S+1, T);
strip_ws([$\t|Str], S, T) ->
    strip_ws(Str, S, T+1);
strip_ws(Str, S, T) ->
    {Str, S, T}.

entab(S, false, _Tablength) ->
    S;
entab(S, true, Tablength) when Tablength < 2->
    S;
entab(S, true, Tablength) ->
    {Spaces, Line} = string:take(S, "\s\t"),
    N = lists:foldl(fun($\s, N) -> N+1; ($\t, N) -> N+Tablength end, 0, Spaces),
    lists:append([lists:duplicate($\t, N div Tablength),
                  lists:duplicate($\s, N rem Tablength), Line]).

pick_lines(From, To, Orig, Indented) when From =< To ->
    Input  = array:get(From, Orig),
    Indent = array:get(From, Indented),
    case Input =:= Indent of
        true -> pick_lines(From+1, To, Orig, Indented);
        false -> [{From, Input, Indent}|
                  pick_lines(From+1, To, Orig, Indented)]
    end;
pick_lines(_From, _To, _Orig, _Indented) ->
    [].

newlines(Str) ->
    newlines(Str, 0).

newlines([$\n|Str], N) ->
    newlines(Str, N+1);
newlines([_|Str], N) ->
    newlines(Str, N);
newlines([], N) ->
    N.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TODO: value 4 is hardcoded! Should use indentation width here
indent({Tokens,_,_}, Line, Fun) ->
    I = #i{anchor=[], current=none, indent_line=Line, check=Fun},
    try
        i_form_list(Tokens, I)
    catch
        throw:{indent, Res} -> Res
    end.

%% Uses process dictionary for storing the last checked LINE number it
%% really should be in I#i{} record but that is not contained when parsing the
%% code, so it's a large update to fix that.
check_indent_lines({string,{Line,Col},Str,_Str1}, I, Prefs) ->
    NLs = newlines(Str),  %% Count NewLines it may be a multiline string
    Wanted = get(?MODULE),
    case Line <  Wanted of
        true -> put(?MODULE, max(Wanted, Line+NLs+1));
        false -> skip_or_indent(Line, Col-1, get_indent_of(I, Prefs), NLs, I)
    end;
check_indent_lines(?loc({Line,Col}), I, Prefs) ->
    case Line < get(?MODULE) of
        true -> ok;
        false -> skip_or_indent(Line, Col-1, get_indent_of(I, Prefs), 0, I)
    end;
check_indent_lines(eof, I, Prefs) ->
    ToCol = get_indent_of(I, Prefs),
    throw({indent, {eof, get(?MODULE), ToCol}});
check_indent_lines({parse_error, Line}, #i{skip=Skip}, _) ->
    throw({indent, {Line+1, 0, Skip}}).

skip_or_indent(Line, Col, Col, NewLines,_) ->
    put(?MODULE, Line+NewLines+1);
skip_or_indent(Line, _Col, ToCol, _NewLines, #i{skip=Skip}) ->
    %% ?D("~4.w: ~2w ~2w~n", [Line, _Col, ToCol]),
    throw({indent, {Line, ToCol, Skip}}).

i_check([Head|_], #i{check=Check}=I) ->
    Check(Head, I);
i_check([], #i{check=Check}=I) ->
    Check(eof, I);
i_check(Other, #i{check=Check}=I) ->
    Check(Other, I).

indent_by(none, _) -> 0;
indent_by(Key, Prefs) ->
    maps:get(Key, Prefs, 0).


get_indent_of(#i{anchor=[{paren, _}, {Type, _}|_]}=I, Prefs)
  when Type =:= parameters;
       Type =:= record_def ->
    get_indent_of(pop(I#i{current=none}), Prefs);
get_indent_of(#i{anchor=[{What, ?col(CA)}=_A|_],
                 current={C, ?col(Exp)}}, Prefs)
  when is_atom(What), is_atom(C) ->
    Col0 = CA+indent_by(What, Prefs),
    Extra = indent_by(C, Prefs),
    min(Col0+Extra, Exp)-1;
get_indent_of(#i{anchor=[{What, ?col(CA)}=_A|_],
                 current=C}, Prefs)
  when is_atom(What), is_atom(C) ->
    Col0 = CA+indent_by(What, Prefs),
    Extra = indent_by(C, Prefs),
    Col0+Extra-1;
get_indent_of(_, _) ->
    0.

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

i_with(W, I) ->
    I#i{current=W}.

head([H | _]) -> H;
head(H) -> H.

top(#i{anchor=[Top|_]}) ->
    Top.

i_form_list([?line(Line)|_] = R0, I0) ->
    try i_form(R0, I0) of
        {R0, _}  -> %% We are looping something is wrong
            i_check({parse_error, Line}, I0);
        {R, #i{skip=Skip}} ->
            i_form_list(R, I0#i{skip=Skip})
    catch error:_E ->
            ?D("~p:~p: @~w: Error:~n ~P~n  ~P~n",
               [?MODULE, ?LINE, Line, _E, 20, erlang:get_stacktrace(), 20]),
            ?D(error(parse_error)),
            i_check({parse_error, Line}, I0)
    end;
i_form_list([], I) ->
    i_check([], I).

i_expr(R0, I0, A) ->
    R1 = i_comments(R0, I0),
    R2 = i_1_expr(R1, I0),
    I1 = push(none, R1, I0),
    case i_sniff(R1) of
        string ->
            case i_sniff(i_kind(string, R1, I1)) of
                string ->
                    i_expr(R2, I1, A);
                _ ->
                    i_expr_rest(R2, I1, A)
            end;
        macro ->
            case i_sniff(i_kind(macro, R1, I1)) of
                macro ->
                    i_expr(R2, push(after_op, I1), A);
                _ ->
                    i_expr_rest(R2, I1, A)
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
        '#' -> % record something
            {R1, _} = i_record(R0, push(after_op,I)),
            i_expr_rest(R1, I, A);
        ':' -> % external function call
            R1 = i_kind(':', R0, I),
            R2 = i_1_expr(R1, I),
            i_expr_rest(R2, I, A);
        '<=' -> % within binary comprehension
            R1 = i_kind('<=', R0, I),
            {R2, _A} = i_expr(R1, push(after_op, I), A),
            {R2, I};
        '=' -> % match/assignment
            R1 = i_binary_op(R0, push(before_arrow, I)),
            {R2, _A} = case A of
                           {after_op, _} ->
                               I1 = pop_until(A, I),
                               i_expr(R1, I1, top(I1));
                           _ ->
                               I1 = push(after_op, I),
                               i_expr(R1, I1, top(I1))
                       end,
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
            R1 = i_kind('::', R0, push(delimiter_spec, I)),
            {R2, _A} = i_type(R1, push(before_arrow, I), top(I)),
            {R2, I};
        _ ->
            case is_binary_op(i_sniff(R0)) of
                true ->
                    I2 = case align_binary_op(pop_until(A, I)) of
                             true ->
                                 push(none, keep_one(A, I));
                             false ->
                                 push(clause, keep_one(A, I))
                         end,
                    R1 = i_binary_op(R0, I2),
                    {R2, _} = i_expr(R1, I2, A),
                    {R2, I};
                false ->
                    {R0, I}
            end
    end.

align_binary_op(I) ->
    case top(I) of
        {paren, _} ->
            case top(pop(I)) of
                {parameters,_} -> false;
                _ -> true
            end;
        {Anchor,_} ->
            Align = [after_op, unary_op, 'when', 'after_when', clause],
            lists:member(Anchor, Align)
    end.

i_par_list(R0, I0) ->
    R1 = i_kind('(', R0, push(before_arrow,I0)),
    I1 = case top(I0) of
             {_, ?k(F)} when F =:= 'atom'; F =:= 'var'; F=:= 'macro' ->
                 I10 = push(parameters, I0),
                 push(paren, R0, I10);
             _ ->
                 push(paren, R0, I0)
         end,
    i_end_paren_or_expr_list(R1,I1).

i_end_paren_or_expr_list(R0, I0) ->
    i_end_paren_or_expr_list(R0, I0, top(I0)).

i_end_paren_or_expr_list(R0, I0, A0) ->
    case i_sniff(R0) of
        Kind when Kind==')'; Kind=='}'; Kind==']'; Kind==eof ->
            i_end_paren(R0, I0, A0);
        _ ->
            {R1, I} = i_expr_list(R0, I0, top(I0)),
            case i_sniff(R1) of
                Kind when Kind==')'; Kind=='}'; Kind==']'; Kind==eof ->
                    i_end_paren(R1, I, A0);
                '->' -> %% Type or Macro def
                    R2 = i_kind('->', R1, I0),
                    i_end_paren_or_expr_list(R2, I0);
                _ ->
                    R0
            end
    end.

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
    {R2, I11} = i_expr(R1, I0, top(I0)),
    I1 = keep_one(A0, I11),
    case i_sniff(R2) of
        Delim when Delim =:= ','; Delim =:= '|' ->
            {_, Start} = top(pop_until(A0, I0)),
            R3 = i_kind(Delim, R2, i_with({delimiter, Start}, I1)),
            i_expr_list(R3, I1, A0);
        '||' ->
            R3 = i_kind('||', R2, I1),
            I2 = push(clause, element(2,A0), pop_until(A0,I1)),
            i_expr_list(R3, I2, top(I2));
        _ ->
            {R2, I1}
    end.

i_binary_expr_list(R0, I0) ->
    {R, I} = i_binary_expr_list(R0, I0, top(I0)),
    {_, A0} = top(pop_until(top(I0), I)),
    i_kind('>>', R, i_with({end_paren2, A0}, I)).

i_binary_expr_list(R0, I0, A0) ->
    R1 = i_comments(R0, I0),
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
        string ->
            i_expr(R0, I0, top(I0));
        Kind when Kind==var; Kind==integer; Kind==char; Kind==float ->
            R1 = i_comments(R0, I0),
            R2 = i_kind(Kind, R1, I0),
            {i_1_expr(R2, I0), push(none, R1, I0)};
        _ ->
            {R0, I0}
    end.

i_binary_specifiers(R0, I) ->
    R1 = i_binary_specifier(R0, I),
    case i_sniff(R1) of
        Kind when Kind==':'; Kind=='-'; Kind=='/' ->
            R2 = i_kind(Kind, R1, I),
            i_binary_specifiers(R2, I);
        '*' -> %% Allowed in typespecs
            R2 = i_kind('*', R1, I),
            i_binary_specifiers(R2, I);
        _ ->
            R1
    end.

i_binary_specifier(R0, I) ->
    case i_sniff(R0) of
        '(' ->
            {R1, _A} = i_expr(R0, I, top(I)),
            R1;
        'macro' ->
            i_1_expr(R0, I);
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
            I2 = keep_one(A0, I1),
            i_predicate_list(R3, I2, A0);
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
i_1_expr([?k('fun') | _] = R, I) ->
    i_fun(R, I);
i_1_expr([?k('try') | _] = R, I) ->
    i_try(R, I);
i_1_expr([?k('...') | _] = R, I) ->
    i_one(R, I);
i_1_expr(R0, I) ->
    R1 = i_comments(R0, I),
    case is_unary_op(R1) of
        true ->
            R2 = i_one(R1, push(unary_op, I)),
            I1 = push(unary_op, R1, I),
            {R3,_} = i_expr(R2, I1, top(I1)),
            R3;
        false ->
            R1
    end.

i_fun([?k('fun')=T | R0] = R00, I) ->
    I1 = push('fun', T, I),
    case i_sniff(R0) of
        '(' ->
	    case i_sniff(i_one(R0, push(none,I1))) of
		'(' ->
		    {R1, _} = i_type_fun(R00, I1),
		    R1;
		_ ->
		    R1 = i_fun_clause_list(R0, I1, top(I1)),
                    case i_sniff(R1) of
                        'end' -> i_kind('end', R1, push(none, I1));
                        _ -> R1  %% fun in spec
                    end
	    end;
        var ->
            case i_sniff(tl(R0)) of
                '(' ->
                    R1 = i_fun_clause_list(R0, I1, top(I1)),
                    case i_sniff(R1) of
                        'end' -> i_kind('end', R1, push(none, I1));
                        _ -> R1 %% fun in spec
                    end;
                _ ->
                    {R1, _A} = i_expr(R0, I1, top(I1)),
                    R1
            end;
        _ ->
            {R1, _A} = i_expr(R0, I1, top(I1)),
            R1
    end.

i_macro(R0, I) ->
    R = i_one(R0, I),
    i_macro_rest(R, push(none, R0, I)).

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
            case erl_scan:reserved_word(K) of
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
                 I3 = push(clause, R2, I1),
                 R3 = i_kind('after', R2, push(none, I2)),
                 i_after_clause(R3, I3);
             _ ->
                 R2
         end,
    i_block_end('receive', R0, R4, I1).

i_try(R0, I1) ->
    R1 = i_kind('try', R0, I1),
    I2 = push(clause, R0, I1),
    R2 = i_expr_list(R1, I2),
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
                      '++', '--', '.', '#', '|',
		      '=>', ':=',
		      '..', '::']).

is_unary_op([T | _]) ->
    is_unary_op(T);
is_unary_op(?k(Op)) ->
    lists:member(Op, ['not', '-', '?', 'catch', 'bnot']).

i_block_end(_Begin, R0, R1, I0) ->
    I1 = push(end_block, R0, I0),
    i_kind('end', R1, I1).

i_one(R0, I) ->
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
            I10 = push(record_def, A0, I),
            R3 = i_kind('{', R2, I10),
            I1 = push(paren, R2, I10),
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
            ?line(L) = C,
            i_check([C], push(comment_1, setelement(2, C, {L,1}), I));
        comment_2 ->  %% context dependent
            i_check([C], I);
        comment_3 ->
            ?line(L) = C,
            i_check([C], push(comment_3, setelement(2, C, {L,1}), I))
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
            i_macro(R0, I)
    end.

i_kind(Kind, R0, I) ->
    R1 = i_comments(R0, I),
    case R1 of
        [?k(Kind) | R2] -> R2;
        _ -> error({expected, Kind, R0})
    end.

i_end_paren(R0, I0, A) ->
    {paren, BegParen} = top(pop_until(paren, pop_until(A, I0))),
    I = i_with({end_paren, BegParen}, I0),
    R1 = i_comments(R0, I),
    i_end_paren_1(R1, I).

i_end_paren_1([?k(Kind) | _] = R, I) when Kind==')'; Kind=='}'; Kind==']'; Kind==eof ->
    i_kind(Kind, R, I).

i_form(R0, I) ->
    R1 = i_comments(R0, I),
    case i_sniff(R1) of
        '-' ->
            i_declaration(R1, I);
        _ ->
            I1 = push(none, R1, I),
            {R3, I2} = i_clause(R1, I1, top(I1), after_arrow),
            {R4, I3} = i_dot_or_semi(R3, I2),
            {R4, I3}
    end.

i_dot_or_semi(R, I) ->
    case i_sniff(R) of
        DS when DS==dot; DS==';' ->
            {i_kind(DS, R, I), I#i{skip=hd(R)}};
        eof ->
            i_check([], I);
        _ ->
            {R, I}
    end.

i_dot(R, I) ->
    {i_kind(dot, R, I), I#i{skip=hd(R)}}.

i_declaration(R0, I) ->
    i_check(R0, I),
    R1 = i_kind('-', R0, I),
    case skip_comments(R1) of
        [?kv(atom, Spec) | _] when Spec =:= 'spec'; Spec =:= 'callback'->
            R2 = i_kind(atom, R1, I),
            i_spec(R2, push(spec, R1, I));
        [?kv(atom, Type) | _] when Type =:= 'type'; Type =:= 'opaque' ->
            R2 = i_kind(atom, R1, I),
            i_typedef(R2, push(type, R1, I));
        [?kv(atom, 'define') | _] ->
            R2 = i_kind(atom, R1, I),
            i_macro_def(R2, push(clause, R1, I));
        _ ->
            {R2, _A} = i_expr(R1, push(none, R1, I), head(R0)),
            i_dot(R2, I)
    end.

i_typedef(R0, I0) ->
    {R1, I1} = i_expr(R0, I0, top(I0)),
    i_dot(R1, I1).

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
            R2 = i_kind('::', R1, push(delimiter_spec, I)),
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
    R6 = case i_sniff(R3) of
             '->' ->
                 R4 = i_kind('->', R3, I2),
                 I3 = push(after_arrow, I1),
                 {R5, _} = i_spec_expr(R4, I3, top(I3)),
                 R5;
             _ ->
                 R3
         end,
    R7 = i_kind(')', R6, push(none, R1, I0)),
    {R7, I1}.

i_spec_expr(R0, I0, A0) ->
    {R1, I1} = i_type(R0, I0, A0),
    case i_sniff(R1) of
        'when' ->
            I2 = push(none, pop_until(A0, I0)),
            R11 = i_kind('when', R1, I2),
            {R2, _} = i_spec_expr(R11, I2, top(I2)),
            {R2, I1};
        ',' ->
            R11 = i_kind(',', R1, push(delimiter_spec, I1)),
            I2 = keep_one(A0, I1),
            i_spec_expr(R11, I2, A0);
        _ ->
            {R1,I1}
    end.

i_spec_aux(R0, I0) ->
    {R2,I2} =
        case i_sniff(R0) of
            '(' ->
                R1 = i_kind('(', R0, push(none, I0)),
                {R1, I0};
            _ ->
                R10 = i_atom_or_macro(R0, I0),
                R12 = case i_sniff(R10) of
                          ':' ->
                              R11 = i_kind(':', R10, I0),
                              i_atom_or_macro(R11, I0);
                          _ ->
                              R10
                      end,
                R1 = i_kind('(', R12, I0),
                I10 = push(parameters, R0, I0),
                I1 = push(paren, R12, I10),
                {R1, I1}
        end,
    {R3, _I2} = i_spec_expr(R2, I2, top(I2)),
    R4 = i_kind(')', R3, push(none, I2)),
    case i_sniff(R4) of
        '->' ->
            I3 = pop(I2),
            R5 = i_kind('->', R4, I3),
            I4 = push(after_arrow, I3),
            {R6, _} = i_spec_expr(R5, I4, top(I4)),
            {R6, I2};
        _ ->
            {R4, I2}
    end.

i_spec_list(R0, I0, A0) ->
    {R1,I1} = i_spec_aux(R0, I0),
    case i_sniff(R1) of
        ';' ->
            R2 = i_kind(';', R1, push(delimiter_spec, I1)),
            i_spec_list(R2, I1, A0);
        _ ->
            {R1,pop(I1)}
    end.

i_spec(R0, I) ->
    {R,I2} = case i_sniff(R0) of
                 '(' -> %% old style as in -spec(funcname(a1,a2) -> type()).
                     R1 = i_kind('(', R0, I),
                     I1 = push(spec, R0, I),
                     {R2,I10} = i_spec_list(R1, I1, top(I1)),
                     R3 = i_kind(')', R2, I1),
                     {R3, I10};
                 _ ->
                     i_spec_list(R0, I, top(I))
             end,
    i_dot(R, push(none, I2)).

i_macro_def(R0, I0) ->
    R1 = i_kind('(', R0, I0),
    I10 = push(parameters, I0),
    I1  = push('paren', R0, I10),
    {R2, I2} = i_expr(R1, I1, top(I1)),
    R3 = i_kind(',', R2, I1),
    {R4, I3} = i_macro_exp(R3, I2, top(I2)),
    R5 = i_end_paren(R4, I3, top(I1)),
    i_dot(R5, I0).

i_macro_exp(R0, I0, A0) ->
    {R1, I1} = i_expr(R0, I0, A0),
    case i_sniff(R1) of
        'when' ->
            I2 = push('before_arrow', I0),
            R2 = i_kind('when', R1, I2),
            I3 = push('when', I2),
            {R3, _} = i_predicate_list(R2, I3),
            i_macro_exp(R3, I1, A0);
        '->' ->
            R2 = i_kind('->', R1, I1),
	    I2 = push(after_arrow, I0),
	    R3 = i_expr_list(R2, I2),
            i_macro_exp(R3, I1, A0);
        ';' ->
            R2 = i_kind(';', R1, I0),
            I2 = keep_one(A0, I1),
            i_macro_exp(R2, I2, A0);
        ',' ->
            R2 = i_kind(',', R1, I0),
            I2 = keep_one(A0, I1),
            i_macro_exp(R2, I2, A0);
        _ ->
            {R1, I1}
    end.

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
    case i_sniff(R3) of
        '->' ->
            R4 = i_kind('->', R3, I1),
            I2 = push(fun_body, pop_until(A0, I0)),
            {i_expr_list(R4, I2), I10};
        _ ->
            {R3, I1}
    end.

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

i_clause(R0, I, A0, Tag) ->
    I0 = pop_until(A0, I),
    {R1, I10} = i_expr(R0, I, top(I)),
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
    case i_sniff(R4) of
	'->' ->
	    R5 = i_kind('->', R4, I3),
	    I5 = push(Tag, I0),
	    {i_expr_list(R5, I5), I10};
	_ ->
	    {R4, I3}
    end.

i_clause_list(R0, I, Tag) ->
    i_clause_list(R0, I, top(I), Tag).

i_clause_list(R0, I0, A0, Tag) ->
    {R1, I1} = i_clause(R0, I0, A0, Tag),
    case i_sniff(R1) of
        ';' ->
            I = push(none, keep_one(A0, I1)),
            R2 = i_kind(';', R1, push(delimiter_clause, pop_until(A0, I))),
            i_clause_list(R2, I, A0, Tag);
        _ ->
            R1
    end.

i_if_clause(R0, I0, A0) ->
    {R1, I1} = i_predicate_list(R0, I0, A0),
    R2 = i_kind('->', R1, I1),
    I2 = push(icr, pop_until(A0, I1)),
    R = i_expr_list(R2, I2),
    {R, I1}.

i_if_clause_list(R0, I0, A0) ->
    {R1, I1} = i_if_clause(R0, I0, A0),
    I2 = keep_one(A0, I1),
    case i_sniff(R1) of
        ';' ->
            R2 = i_kind(';', R1, push(delimiter_clause, pop_until(A0, I2))),
            i_if_clause_list(R2, I2, A0);
        _ ->
            R1
    end.

i_sniff(L) ->
    case skip_comments(L) of
        [] ->
            eof;
        [?k(Kind) | _] ->
            Kind
    end.
