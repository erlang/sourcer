-module(sourcer_parse_util).

-export([
    split/2,
    get_line_text/2, 
    get_line_text/3,

    compact_newlines/1,
    group_comments/1,
    comment_level/1,
    skip_percent/1,

    split_at_semicolon_paren/1,
    split_at_semicolon_name/1,
    split_at_token/2,
    middle/1
]).

-include("debug.hrl").

%% a string split that keeps the separators at the end of the parts
split(String, Sep) ->
	split(String, Sep, []).

split(String, Sep, R) ->
	case string:split(String, Sep) of	
		[H] ->
			lists:reverse([H|R]);
		[H, []] ->
			lists:reverse([H++Sep|R]);
		[H, T] ->
			split(T, Sep, [H++Sep|R])
	end.

get_line_text(String, {_, Ofs, Len, _}) ->
    string:substr(String, Ofs+1, Len).

get_line_text(String, Index, LineInfo) ->
    I = lists:keyfind(Index, 1, LineInfo),
    get_line_text(String, I).

compact_newlines(L) ->
compact_newlines(L, []).

compact_newlines([{white_space,_,_,_}=C,{white_space,_,_,_}|Rest], Acc) ->
    compact_newlines(Rest, [C|Acc]);
compact_newlines([{white_space,_,_,_}|Rest], Acc) ->
    compact_newlines(Rest, Acc);
compact_newlines([T|Rest], Acc) ->
    compact_newlines(Rest, [T|Acc]);
compact_newlines([], Acc) ->
    lists:reverse(Acc).

%% Split token list at "; Name (", where Name is the same
%% as the first token which must be an atom.
%% Semicolon is not included in result.
split_at_semicolon_name([H|_]=L) ->
    {atom, _, _, Name} = H,
    split_at_semicolon_name(L , Name, [], []).

split_at_semicolon_name([], _, R, []) ->
    lists:reverse(R);
split_at_semicolon_name([], _, Acc, Crt) ->
    lists:reverse(Acc, [lists:reverse(Crt)]);
split_at_semicolon_name([{';', _, _, _}, {atom, _, _, Name}=H1, {'(', _, _, _}=H2|T],
                        Name, Acc, Crt) ->
    split_at_semicolon_name([H1, H2|T], Name, [lists:reverse(Crt)|Acc], []);
split_at_semicolon_name([H|T], Name, Acc, Crt) ->
    split_at_semicolon_name(T, Name, Acc, [H|Crt]).

%% Split token list at "; ("
split_at_semicolon_paren(L) ->
    split_at_semicolon_paren(L , [], []).

split_at_semicolon_paren([], R, []) ->
    lists:reverse(R);
split_at_semicolon_paren([], Acc, Crt) ->
    lists:reverse(Acc, [lists:reverse(Crt)]);
split_at_semicolon_paren([{';', _, _, _}, {'(', _, _, _}=H2|T], Acc, Crt) ->
    split_at_semicolon_paren([H2|T], [lists:reverse(Crt)|Acc], []);
split_at_semicolon_paren([H|T], Acc, Crt) ->
    split_at_semicolon_paren(T, Acc, [H|Crt]).

split_at_token(L, Delim) ->
    {X, Y} = lists:splitwith(fun({K,_,_,_}) -> K=/=Delim
                            end, 
                            L),
    case Y of
        [] ->
            {X, Y};
        _ ->
            {X, tl(Y)}
    end.

%% split the list once at token kind; don't keep the delimiter
split_at_token([], _, R) ->
    {lists:reverse(R), []};
split_at_token([{Delim, _, _, _}|Rest], Delim, R) ->
    {lists:reverse(R), Rest};
split_at_token([H|Rest], Delim, R) ->
    split_at_token(Rest, Delim, [H|R]).

% split the list at all Delims
split_all_at_token(L, Delim) ->
    %% TODO
    [L].

middle([]) ->
    [];
middle([H|T]) ->
    lists:reverse(tl(lists:reverse(T))).


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

comment_level({comment, _, "%%%%"++_, _}) -> 4;
comment_level({comment, _, "%%%"++_, _}) -> 3;
comment_level({comment, _, "%%"++_, _}) -> 2;
comment_level({comment, _, "%"++_, _}) -> 1.

skip_percent("%"++L) ->
    skip_percent(L);
skip_percent(L) ->
    L.

