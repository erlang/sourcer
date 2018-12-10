-module(sourcer_parse_util).

-export([
    get_line_text/2,
    get_line_text/3,

    extract_top_comments/1,

    take_until_token/2,
    take_until_token/3,
    split_at_token/2,
    split_at_token/3,
    take_until_matching_token/2,

    middle/1,

    take_block_list/1
]).

%%-define(DEBUG, true).
-include("debug.hrl").

-define(k(X), {X,_,_,_}).
-define(LPAR, '(').
-define(RPAR, ')').

get_line_text(String, {_, Ofs, Len, _}) ->
    string:substr(String, Ofs+1, Len).

get_line_text(String, Index, LineInfo) ->
    I = lists:keyfind(Index, 1, LineInfo),
    get_line_text(String, I).

extract_top_comments(Toks) ->
    Toks1 = remove_inline_whitespace(Toks),
    % keep comments and whitespace at beginning
    SplitFun = fun(?k(white_space)) -> true;
                    (?k(comment)) -> true;
                    (_) -> false
                end,
    {Toks2, Rest} = lists:splitwith(SplitFun, Toks1),
    % remove last lines of whitespace
    DropFun = fun(?k(white_space))->true;
                 (_) -> false
              end,
    Toks3 = lists:reverse(lists:dropwhile(DropFun, lists:reverse(Toks2))),
    {skip_unrelated_comments(Toks3, []), Rest}.

remove_inline_whitespace(L) ->
    Fun = fun({white_space,_,_,V}) -> V=="\n"; (_)-> true end,
    lists:filter(Fun, L).

%% only the last block of comments before the form is kept
skip_unrelated_comments([], Acc) ->
    compact_comments(lists:reverse(Acc));
skip_unrelated_comments([?k(comment)=_C,{white_space, _, _, "\n"},{white_space, _, _, "\n"}|Toks], _Acc) ->
    skip_unrelated_comments(Toks, []);
skip_unrelated_comments([?k(comment)=C,{white_space, _, _, "\n"}|Toks], Acc) ->
    skip_unrelated_comments(Toks, [C|Acc]);
skip_unrelated_comments([?k(comment)=C|Toks], Acc) ->
    skip_unrelated_comments(Toks, [C|Acc]);
skip_unrelated_comments([?k(white_space)=_C|Toks], Acc) ->
    skip_unrelated_comments(Toks, Acc).

compact_comments([]) ->
    none;
compact_comments(L) ->
    {_,P1,_,_} = hd(L),
    {_,{L2,C2},T,_} = lists:last(L),
    {P1, {L2,C2+length(T)}}.

%% split list at the first occurence of delimiter;
%% if delimiter not found, return whole list as result.
take_until_token(L, Delim) ->
    take_until_token(L, Delim, fun(_)-> true end).

%% split list at the first occurence of delimiter and where
%% Pred(Rest) == true; if Pred is never true, return whole list as result;
%% if encountering blocks, handle them properly.
take_until_token(L, Delim, Pred) ->
    take_until_token(L, Delim, Pred, []).

take_until_token([], _Delim, _Pred, R) ->
    {lists:flatten(lists:reverse(R)), none, []};
take_until_token([?k(Delim)=H|Rest], Delim, Pred, R) ->
    case Pred(Rest) of
        true ->
            {lists:flatten(lists:reverse(R)), H, Rest};
        false ->
            take_until_token(Rest, Delim, Pred, [H|R])
    end;
take_until_token([?k(K)=H|Rest], Delim, Pred, R) ->
    case lists:keyfind(K, 1, get_block_tokens()) of
        false ->
            take_until_token(Rest, Delim, Pred, [H|R]);
        _ ->
            Rest2 = case {K, Rest} of
                {'fun', [?k(?LPAR)=Hh|_]} ->
                    % check if it is a defun or type
                    {_, _, _, Bb} = take_until_matching_token(Hh, tl(Rest)),
                    case Bb of
                        [?k('->')|_] ->
                            false;
                        _ ->
                            Bb
                    end;
                {'fun', [?k(_),?k(':')|_]} ->
                    Rest;
                {'fun', [?k(_),?k('/')|_]} ->
                    Rest;
                _ ->
                    false
            end,
            case Rest2 of
                false ->
                    {HH, LL, TT, RR} = take_until_matching_token(H, Rest),
                    case TT of
                        none ->
                            take_until_token(RR, Delim, Pred, [LL|[HH|R]]);
                        _ ->
                            take_until_token(RR, Delim, Pred, [TT|[LL|[HH|R]]])
                    end;
                _ ->
                    take_until_token(Rest, Delim, Pred, [H|R])
            end
    end.

%% surrounding tokens are included in result
take_until_matching_token(?k(T1)=H, Rest) ->
    case lists:keyfind(T1, 1, get_block_tokens()) of
        false ->
            %% should not happen
            {error, bad_token, T1};
        {T1, T2} ->
            {A, D, B} = take_until_token(Rest, T2),
            case D of
                none ->
                    {H, A, none, B};
                _ ->
                    {H, A, D, B}
            end
    end.

%% the pairs of tokens that build structured code
get_block_tokens() ->
    [
        {'(', ')'},
        {'[', ']'},
        {'{', '}'},
        {'<<', '>>'},
        {'begin', 'end'},
        {'if', 'end'},
        {'case', 'end'},
        {'receive', 'end'},
        {'try', 'end'},
        {'fun', 'end'}
    ].

%% split list at all delimiters; result does not include delimiters
%% returns [{[token], delimiter token}]
split_at_token(L, Delim) ->
    split_at_token(L, Delim, fun(_)-> true end).

split_at_token([], _Delim, _Pred) ->
    [];
split_at_token(L, Delim, Pred) ->
    {H, D, T} = take_until_token(L, Delim, Pred),
    DD = case D of
        none ->
            none;
        _ ->
            D
    end,
    [{H,DD}|split_at_token(T, Delim, Pred)].

middle([]) ->
    [];
middle([_]) ->
    [];
middle([_|T]) ->
    lists:reverse(tl(lists:reverse(T))).

skip_percent("%"++L) ->
    skip_percent(L);
skip_percent(L) ->
    L.

take_block_list(L) ->
    {_, Args0, _, Rest} = take_until_matching_token(hd(L), tl(L)),
    A = split_at_token(Args0, ','),
    {[Ts||{Ts,_}<-A], Rest}.
