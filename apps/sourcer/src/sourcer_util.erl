-module(sourcer_util).

-export([
    split/2
]).
-export([pack/1, unpack/1, join/2]).
-export([load/2]).
-export([reverse2/1]).
-export([get_between_strs/3, get_all_between_strs/3, get_from_str/2,
		 get_upto_str/2 ,split_lines/1]).
-export([get_auto_imported/1, add_auto_imported/1]).

-include("dbglog.hrl").

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

load(Mod, All) ->
    case code:is_sticky(Mod) of
        true ->
            ok;
        false ->
            if All ->
                   c:nl(Mod);
               true ->
                   c:l(Mod)
            end
    end.

-define(SEP, ";").

unpack(F) ->
    string:tokens(F, ?SEP).

pack(L) ->
    join(L, ?SEP).

reverse2(L) when is_list(L) ->
    lists:reverse([lists:reverse(A) || A <- L]).

get_from_str(Text, Start) ->
    case string:str(Text, Start) of
        0 ->
            Text;
        N ->
            string:substr(Text, N + length(Start))
    end.

get_between_strs(Text, Start, End) ->
    get_upto_str(get_from_str(Text, Start), End).

get_all_between_strs(Text, Start, End) ->
    {One, Next} = split_at(get_from_str(Text, Start), End),
    case Next of
        "" ->
            [One];
        _ ->
            [One | get_all_between_strs(Next, Start, End)]
    end.

get_upto_str(Text, End) ->
    case string:rstr(Text, End) of
        0 ->
            Text;
        N ->
            string:substr(Text, 1, N-1)
    end.

split_at(Text, End) ->
    case string:str(Text, End) of
        0 ->
            {Text, ""};
        N ->
            {string:substr(Text, 1, N-1), string:substr(Text, N+length(End))}
    end.

split_lines(<<B/binary>>) ->
    split_lines(unicode:characters_to_list(B));
split_lines(L) when is_list(L) ->
    split_lines(L, [], []).

split_lines([], [], Acc) ->
    lists:reverse(Acc);
split_lines([], LineAcc, Acc) ->
    split_lines([], [], [lists:reverse(LineAcc) | Acc]);
split_lines([$\r, $\n | Rest], LineAcc, Acc) ->
    split_lines(Rest, [], [lists:reverse(LineAcc) | Acc]);
split_lines([$\n | Rest], LineAcc, Acc) ->
    split_lines(Rest, [], [lists:reverse(LineAcc) | Acc]);
split_lines([$\r | Rest], LineAcc, Acc) ->
    split_lines(Rest, [], [lists:reverse(LineAcc) | Acc]);
split_lines([C | Rest], LineAcc, Acc) ->
    split_lines(Rest, [C | LineAcc], Acc).

join([], Sep) when is_list(Sep) ->
    [];
join([H|T], Sep) ->
    H ++ lists:append([Sep ++ X || X <- T]).

add_auto_imported(Imports) ->
    [{erlang, get_auto_imported("")} | Imports].

get_auto_imported(Prefix) when is_list(Prefix) ->
    case catch erlang:module_info(exports) of
        Val when is_list(Val) ->
            lists:filter(fun({N, A}) ->
                                 lists:prefix(Prefix, atom_to_list(N)) andalso
                                     erl_internal:bif(N, A)
                         end, Val);
        _Error ->
            ?D(_Error),
            error
    end.

%%
%% Local Functions
%%

