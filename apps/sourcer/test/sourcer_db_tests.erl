-module(sourcer_db_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DEBUG, true).
-include("debug.hrl").

-include("sourcer_db.hrl").

merge_test_() ->
    M =#model{defs=[{[a1],1},{[b1], 1}],
                refs=[{[c1],1},{[d1], 1}]},
    [
        ?_assertEqual(
            M,
            sourcer_db:merge([
                M
            ])
        ),
        ?_assertEqual(
            M,
            sourcer_db:merge([
                #model{},
                M
            ])
        ),
        ?_assertEqual(
            M,
            sourcer_db:merge([
                M,
                #model{}
            ])
        ),
        ?_assertEqual(
            #model{defs=[{[a1],1},{[a2],1},{[b1],1},{[b2],1}],
                refs=[{[c1],1},{[c2],1},{[d1],1},{[d2],1}]},
            sourcer_db:merge([
                #model{defs=[{[a1],1},{[b1], 1}],
                    refs=[{[c1],1},{[d1], 1}]},
                #model{defs=[{[a2],1},{[b2],1}],
                    refs=[{[c2],1},{[d2],1}]}])
        ),
        ?_assertEqual(
            #model{defs=[{[{a1}],1},{[{macro,1,1}],1},{[{macro,1,1}],2}],
                refs=[{[c1],1},{[c1],2},{[d1],1},{[d2],1}]},
            sourcer_db:merge([
                #model{defs=[{[{a1}],2},{[{macro,1,1}], 2}],
                    refs=[{[c1],2},{[d1], 1}]},
                #model{defs=[{[{a1}],1},{[{macro,1,1}],1}],
                    refs=[{[c1],1},{[d2],1}]}])
        ),
        ?_assertEqual(
            #model{},
            sourcer_db:merge([#model{}, #model{}])
        )
    ].

persistence_test_() ->
    F = "/tmp/f",
    [
        {foreach,
            fun() -> file:delete(F), ok end,
            fun(_) -> file:delete(F), ok end,
            [
                % TODO persist(F, "-module(mmm). -ifdef(X). foo()->ok. "),
                persist(F, "")
            ]
        }
    ].

persist(F, S) ->
    {ok, Ts, _} = sourcer_scan:string(S),
    Fs = sourcer_parse:parse(sourcer_scan:filter_ws_tokens(Ts)),
    E = sourcer_db:save(F, Fs),
    A = sourcer_db:load(F),
    ?_assertEqual(E, A).

assert(Exp, Val) ->
    Expected = model(Exp),
    Value = sourcer_db:analyse(sourcer_parse:parse(scan(Val))),
    {Val, ?_assertEqual(Expected, Value)}.

analyze_test_() ->
    {ok, Terms} = file:consult("apps/sourcer/test/parser_db_tests_data"),
    [assert(Y, X) || {X,_,Y}<-lists:reverse(Terms)].

scan(D) ->
    {ok, Ts, _} = sourcer_scan:string(D),
    sourcer_scan:filter_ws_tokens(Ts).

scan(D, P0) ->
    {ok, Ts, _} = sourcer_scan:string(D, P0),
    sourcer_scan:filter_ws_tokens(Ts).

model({D, R, _}) ->
    #model{refs=lists:sort(R), defs=lists:sort(D)};
model({D, R}) ->
    #model{refs=lists:sort(R), defs=lists:sort(D)}.

