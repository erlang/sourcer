%%% Copyright 2015-2016 Vlad Dumitrescu
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

-module(sourcer_iterator).

-export([
         iterate_tokens_source/2,
         iterate_tokens_expanded/2
        ]).

iterate_tokens_source(L, Fun) when is_function(Fun, 1) ->
    iterate(L, fun next_token_source/2, Fun, []).

iterate_tokens_expanded(L, Fun) when is_function(Fun, 1) ->
    iterate(L, fun next_token_expanded/2, Fun, []).

%% TODO iterate_fold

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iterate(L, Iter, Fun, State0)
  when is_function(Iter, 2), is_function(Fun, 1) ->
    case Iter(L, State0) of
        {H, T, State} ->
            [Fun(H) | iterate(T, Iter, Fun, State)];
        none ->
            [];
        error ->
            [T | State] = State0,
            iterate(T, Iter, Fun, State)
    end.

next_token_source(T, S) ->
    next_token(T, source, S).

next_token_expanded(T, S) ->
    next_token(T, expanded, S).

%% next_token(Tokens, Path, State)
next_token([], _, []) ->
    none;
next_token([], Path, [H|State]) ->
    next_token(H, Path, State);
next_token([{fork, [], _}|_T], source, _State) ->
    error;
next_token([{fork, A, _}|T], source, State) ->
    {hd(A), tl(A), [T|State]};
next_token([{fork, _, []}|_T], expanded, _State) ->
    error;
next_token([{fork, _, A}|T], expanded, State) ->
    {hd(A), tl(A), [T|State]};
next_token([H|T], _Path, State) ->
    {H, T, State}.

