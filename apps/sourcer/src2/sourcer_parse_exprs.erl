-module(sourcer_parse_exprs).

%% API
-export([consult/1]).

consult(B) when is_binary(B) ->
    consult(unicode:characters_to_list(B));
consult(L) when is_list(L) ->
    {ok, Toks, _} = erl_scan:string(L),
    {FormToks, _} = sourcer_parse_util:split_at_dot(Toks),
    lists:map(fun parse1/1, FormToks).

parse1(Toks) ->
    R = erl_parse:parse_exprs(Toks),
    case R of
        {ok, [Fs]} ->
            erl_parse:normalise(Fs);
        Err ->
            throw(Err)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

consult_test_() ->
    [
        ?_assertMatch([], consult("2"))
    ].

-endif.