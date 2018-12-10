-module(sourcer_scan).

%%-define(DEBUG, true).
-include("debug.hrl").

-export([line_info/1, split_lines/1,
         string/1, string/2, string/3,
         tokens/1, tokens/3,
         filter_ws_tokens/1,
         filter_comment_tokens/1,
         filter_ws_comment_tokens/1,
         filter_tokens/2,
         to_string/1]).

-export([convert_token/1]).

-type token() ::  {
        Kind :: atom(),
        Location :: {integer(), integer()},
        Text :: string(),
        Value :: term()
    }.

-type line_info() :: {
        Index :: integer(),
        Offset :: integer(),
        Length :: integer(),
        Indent :: string()
    }.

-export_type([token/0, line_info/0]).

-spec line_info(string()) -> [line_info()].
line_info(String) ->
    Lines = re:split(String, "(\r\n|\r|\n)", [{return, list}]),
    {Result, _} = lists:mapfoldl(fun create_line_info/2, {0, 0}, Lines),
    lists:flatten(Result).

create_line_info("\r\n", {LineNo, Offset}) ->
    {[], {LineNo+1, Offset+2}};
create_line_info("\n", {LineNo, Offset}) ->
    {[], {LineNo+1, Offset+1}};
create_line_info("\r", {LineNo, Offset}) ->
    {[], {LineNo+1, Offset+1}};
create_line_info(Line, {LineNo, Offset}) ->
    Len = length(Line),
    {[{LineNo, Offset, Len, get_indent(Line)}], {LineNo, Offset+Len}}.

get_indent(String) ->
    lists:takewhile(fun is_white_space/1, String).

-define(WHITE_SPACE(C),
        is_integer(C) andalso
         (C >= $\000 andalso C =< $\s orelse C >= $\200 andalso C =< $\240)).
is_white_space(C) -> ?WHITE_SPACE(C).

-spec split_lines(string()|binary()) -> [string()].
split_lines(Str) when is_binary(Str) ->
    split_lines(unicode:characters_to_list(Str));
split_lines(Str) ->
    split_lines(Str, []).

split_lines("\n" ++ Rest, Line) ->
    [lists:reverse(Line, "\n") | split_lines(Rest,[])];
split_lines([C|Rest], Line) ->
    split_lines(Rest, [C|Line]);
split_lines([], Line) ->
    [lists:reverse(Line)].


-spec string(string()) -> {ok, [token()], any()} | {error, any()}.
string(String) ->
    string(String, {0, 1}).

string(String, {_L, _C}=Pos) ->
    string(String, Pos, []);
string(String, Opts) when is_list(Opts) ->
    string(String, {0, 1}, Opts).

string(String, {L, C}, Opts) ->
    case string2(String, {L, C}, Opts) of
        {ok, _, _}=R ->
            R;
        {error, {_, _, {_, Quote, _}}, _} ->
            case string2(String++[Quote], {L, C}, Opts) of
                {ok, _, _}=R1 ->
                    %% TODO should remove the syntetic quote from last token's text
                    R1;
                Err ->
                    Err
            end;
        Err2 ->
            Err2
    end.

string2(String, {L, C}, Opts) ->
    case erl_scan:string(String, {L, C}, [text, return |Opts]) of
        {ok, Tokens, {L1, C1}} ->
            Result = convert_tokens(Tokens),
            {ok, Result, {L1, C1}};
        Err ->
            Err
    end.

tokens(Str) ->
    tokens_1({[],""}, Str, {0, 1}, all).

tokens(Str, Loc, Last) ->
    tokens_1({[],""}, Str, Loc, Last).

%% Scans Str after tokens until line
tokens_1({Cont, Orig}, Str, {_,_}=Start, Last) ->
    case erl_scan:tokens(Cont, Str, Start, [return_comments, text]) of
        {done, {ok, Tokens0, {Next,_} = EndLoc}, Rest} when Next < Last ->
            Tokens = convert_tokens(Tokens0),
            [{Tokens, Start, EndLoc} |
             tokens_1({[],""}, Rest, EndLoc, Last)];
        {done, {ok, Tokens0, EndLoc}, _Rest} ->
            Tokens = convert_tokens(Tokens0),
            [{Tokens, Start, EndLoc}];
        {done, {eof, _EndLoc}, _Rest} ->
            [];
        {done, {error, {ErrorLoc, _, _}=_Info, EndLoc}, Rest} ->
            Until = case Orig of
                        "" -> until_error(Str, Start, ErrorLoc);
                        _  -> until_error(Orig, Start, ErrorLoc)
                    end,
            [{BefTokens,SLoc,_}] = tokens_1({[],""}, Until, Start, Last),
            ErrToken = {scan_error, ErrorLoc, "", undefined},
            case tokens_1({[],""}, Rest, EndLoc, Last) of
                [] ->
                    [{BefTokens++[ErrToken], SLoc, EndLoc}];
                [{AfterTokens,_, ELoc}|RestForms] ->
                    [{BefTokens++[ErrToken|AfterTokens], SLoc, ELoc}|RestForms]
            end;
        {more, More} ->
            tokens_1({More, Str}, eof, Start, Last)
    end.

until_error(Str, {SL,SC}, {EL,EC}) ->
    until_error(Str, SL, SC, EL, EC).

until_error([Char|Str], SL, SC, EL, EC) ->
    if SL < EL ->
            case Char =:= $\n of
                true -> [$\n|until_error(Str, SL+1, 1, EL, EC)];
                false -> [Char|until_error(Str,SL,SC,EL,EC)]
            end;
       SC < EC ->
            [Char|until_error(Str, SL, SC+1, EL, EC)];
       true ->
            []
    end.

filter_tokens_by_kinds(Tokens, Kinds) ->
    lists:filter(fun({Kind,_, _, _}) -> not lists:member(Kind, Kinds) end, Tokens).

filter_ws_tokens(L) ->
    filter_tokens_by_kinds(L, [white_space]).

filter_comment_tokens(L) ->
    filter_tokens_by_kinds(L, [comment]).

filter_ws_comment_tokens(L) ->
    filter_tokens_by_kinds(L, [white_space, comment]).

filter_tokens(Tokens, Opts) ->
    case {lists:member(return, Opts),
          lists:member(return_comments, Opts),
          lists:member(return_white_space, Opts)} of
        {true, _, _} ->
            Tokens;
        {_, true, true} ->
            Tokens;
        {_, true, false} ->
            filter_ws_tokens(Tokens);
        {_, false, true} ->
            filter_comment_tokens(Tokens);
        {_, false, false} ->
            filter_ws_comment_tokens(Tokens)
    end.

convert_tokens(Tokens) ->
    convert_tokens(Tokens, []).

convert_tokens([], TokensAcc) ->
    lists:reverse(TokensAcc);
convert_tokens([{white_space, Ann, V}=T0 | Rest], TokensAcc) ->
    case V of
        "\n" ->
            convert_tokens(Rest, [convert_token(T0) | TokensAcc]);
        [$\n | Text] ->
            Ann1 = erl_anno:set_text("\n", Ann),
            T1 = {white_space, Ann1, "\n"},
            Ann2 = erl_anno:set_text(Text, Ann),
            L = erl_anno:line(Ann2),
            C = erl_anno:column(Ann2),
            Ann3 = erl_anno:set_location({L+1,C}, Ann2),
            T2 = {white_space, Ann3, Text},
            convert_tokens(Rest, [convert_token(T2), convert_token(T1) | TokensAcc]);
        _ ->
            convert_tokens(Rest, [convert_token(T0) | TokensAcc])
    end;
convert_tokens([{'?', Ann1}, {'?', Ann2}, {atom, Ann3, _} | Rest], TokensAcc) ->
    Txt1 = lists:append([erl_anno:text(Ann1),erl_anno:text(Ann2),erl_anno:text(Ann3)]),
    T = make_macro(Ann1, Txt1),
    convert_tokens(Rest, [convert_token(T) | TokensAcc]);
convert_tokens([{'?', Ann1}, {'?', Ann2}, {var, Ann3, _} | Rest], TokensAcc) ->
    Txt1 = lists:append([erl_anno:text(Ann1),erl_anno:text(Ann2),erl_anno:text(Ann3)]),
    T = make_macro(Ann1, Txt1),
    convert_tokens(Rest, [convert_token(T) | TokensAcc]);
convert_tokens([{'?', Ann1}, {atom, Ann3, _} | Rest], TokensAcc) ->
    Txt1 = lists:append([erl_anno:text(Ann1),erl_anno:text(Ann3)]),
    T = make_macro(Ann1, Txt1),
    convert_tokens(Rest, [convert_token(T) | TokensAcc]);
convert_tokens([{'?', Ann1}, {var, Ann3, _} | Rest], TokensAcc) ->
    Txt1 = lists:append([erl_anno:text(Ann1),erl_anno:text(Ann3)]),
    T = make_macro(Ann1, Txt1),
    convert_tokens(Rest, [convert_token(T) | TokensAcc]);
convert_tokens([{dot, Ann}=T0 | Rest], TokensAcc) ->
    %% erl_scan conflates the dot with following whitespace.
    Txt = erl_anno:text(Ann),
    case Txt of
        "." ->
            convert_tokens(Rest, [convert_token(T0) | TokensAcc]);
        _ ->
            Ann1 = erl_anno:set_text([hd(Txt)], Ann),
            T = {dot, Ann1},
            %Ann5 = erl_anno:set_text(tl(Txt), Ann),
            %L = erl_anno:line(Ann5),
            %C = erl_anno:column(Ann5),
            %Ann6 = erl_anno:set_location({L,C+1}, Ann5),
            %T1 = {white_space, Ann6},
            convert_tokens(Rest, [convert_token(T) | TokensAcc])
    end;
convert_tokens([{comment, _Ann,_}=T | Rest], TokensAcc) ->
    {A,B,C,D} = convert_token(T),
    C0 = lists:reverse(C),
    C1 = case C0 of
            [$\r|_] -> lists:reverse(tl(C0));
            _ -> C
        end,
    D0 = lists:reverse(D),
    D1 = case D0 of
            [$\r|_] -> lists:reverse(tl(D0));
            _ -> D
        end,
    Z = {A, B, C1, D1},
    convert_tokens(Rest, [Z | TokensAcc]);
convert_tokens([T | Rest], TokensAcc) ->
    convert_tokens(Rest, [convert_token(T) | TokensAcc]).

make_macro(Anno, Txt) ->
    V0 = case Txt of [$?, $? | T] -> T; [$? | T] -> T; _ -> Txt end,
    V =  list_to_atom(V0),
    Anno1 = erl_anno:set_text(Txt, Anno),
    {macro, Anno1, V}.

convert_token({K, A}) ->
    {K, erl_anno:location(A), erl_anno:text(A), undefined};
convert_token({K, A, V}) ->
    {K, erl_anno:location(A), erl_anno:text(A), V}.

to_string(L) ->
    lists:flatten([T || {_,_,T,_}<-L]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

create_line_info_test_() ->
    [
        ?_assertEqual({[{0, 0, 3, ""}], {0, 3}}, create_line_info("hej", {0, 0})),
        ?_assertEqual({[{2, 5, 5, "  "}], {2, 10}}, create_line_info("  hej", {2, 5})),
        ?_assertEqual({[{2, 5, 6, " \t "}], {2, 11}}, create_line_info(" \t hej", {2, 5}))
    ].

-endif.
