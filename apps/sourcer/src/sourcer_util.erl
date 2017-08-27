-module(sourcer_util).

-export([pack/1, unpack/1, join/2]).
-export([reverse2/1]).

-export([get_auto_imported/1, add_auto_imported/1]).

-include("debug.hrl").

-define(SEP, ";").

unpack(F) ->
    string:tokens(F, ?SEP).

pack(L) ->
    join(L, ?SEP).

reverse2(L) when is_list(L) ->
    lists:reverse([lists:reverse(A) || A <- L]).

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

