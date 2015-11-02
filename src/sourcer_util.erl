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

%% @doc Utilities used around the code.

-module(sourcer_util).

-export([
         middle/1,
         split_at_brace/1,
         split_at_comma/1,
         filter_tokens/1
        ]).

-include("sourcer.hrl").

-spec middle([any()]) -> [any()].
middle([]) ->
    [];
middle([_]) ->
    [];
middle([_,_]) ->
    [];
middle([_|T]) ->
    lists:reverse(tl(lists:reverse(T))).

%% @doc Split L at the first matching brace, keeping track of brace levels.
%% We assume that the start brace is the first element of the list.
-spec split_at_brace(sourcer:tokens()) -> {sourcer:tokens(),sourcer:tokens()}.
%%
split_at_brace([]) ->
    {[], []};
split_at_brace([H|T]=L) when is_list(T) ->
    case token_pair(element(1, H)) of
        none ->
            {[], L};
        End ->
            split_at_brace(T, End, [H])
    end.

-spec split_at_brace(sourcer:tokens(),sourcer:close_brace(),sourcer:tokens()) -> {sourcer:tokens(),sourcer:tokens()}.
%%
split_at_brace([], _, Acc) ->
    {lists:reverse(Acc), []};
split_at_brace([{End,_}=H|T], End, Acc) ->
    {lists:reverse([H|Acc]), T};
split_at_brace([H|T], End, Acc) ->
    case token_pair(element(1, H)) of
        none ->
            split_at_brace(T, End, [H|Acc]);
        Other ->
            {L1, L2} = split_at_brace(T, Other, [H]),
            split_at_brace(L2, End, lists:reverse(L1)++Acc)
    end.

%% @doc Split token list at top-level commas (not included in result),
%% while keeping track of brace levels.
-spec split_at_comma(sourcer:tokens()) -> [sourcer:tokens()].
%%
split_at_comma([])->
    [];
split_at_comma(L)->
    split_at_comma(L, [], []).

-spec split_at_comma(sourcer:tokens(),[sourcer:tokens()],sourcer:tokens()) -> [sourcer:tokens()].
split_at_comma([], Acc, Crt) ->
    lists:reverse([lists:reverse(Crt) | Acc]);
split_at_comma([{',', _}|T], Acc, Crt) ->
    split_at_comma(T, [lists:reverse(Crt)|Acc], []);
split_at_comma([{B, _}=H|T], Acc, Crt) ->
    case token_pair(B) of
        none ->
            split_at_comma(T, Acc, [H|Crt]);
        End ->
            {L1, L2} = split_at_brace(T, End, [H]),
            split_at_comma(L2, Acc, lists:reverse(L1)++Crt)
    end.

-spec token_pair(atom()) -> sourcer:close_brace() | 'none'.
token_pair('(') ->
    ')';
token_pair('[') ->
    ']';
token_pair('{') ->
    '}';
token_pair('<<') ->
    '>>';
token_pair(X) when is_atom(X) ->
    none.

-spec filter_tokens(sourcer:tokens()) -> sourcer:tokens().
filter_tokens(Toks) ->
    lists:filter(fun filter_token/1, Toks).

-spec filter_token(_) -> boolean().
filter_token({white_space, _}) ->
    false;
filter_token({comment, _}) ->
    false;
filter_token(_) ->
    true.

