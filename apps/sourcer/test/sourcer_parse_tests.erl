-module(sourcer_parse_tests).

-include_lib("eunit/include/eunit.hrl").

-include("sourcer_parse.hrl").
-include("sourcer_db.hrl").

-define(DEBUG, true).
-include("debug.hrl").

-define(e(R, S),
        ?_assertEqual(R, sourcer_parse:parse(scan(S)))
    ).

assert(Exp, Val) ->
    Expected = Exp,
    Value = sourcer_parse:parse(scan(Val)),
    {Val, ?_assertEqual(Expected, Value)}.

parse_test_() ->
    {ok, Terms} = file:consult("apps/sourcer/test/parser_db_tests_data"),
    [assert(Y, X) || {X,Y,_}<-(Terms)].

scan(D) ->
    {ok, Ts, _} = sourcer_scan:string(D),
    sourcer_scan:filter_ws_tokens(Ts).

scan(D, P0) ->
    {ok, Ts, _} = sourcer_scan:string(D, P0),
    sourcer_scan:filter_ws_tokens(Ts).
