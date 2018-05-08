-module(sourcer_model_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DEBUG, true).
-include("debug.hrl").

-include("sourcer_model.hrl").

persistence_test_() ->
    F = case os:type() of
            {win32,_} -> "tmp/f";
            _ -> "/tmp/f"
        end,
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

get_elements_at_pos_test_() ->
    Text = "
-module(foo).
bar() ->
    quz(1).
quz(1) ->
    ok;
quz(_) ->
    ok.
    ",
    Model = sourcer_analyse:analyse_text(Text),
    [
        ?_assertMatch({[{def,[{module,foo},{function,bar,0}],
                            {{2,1},{2,4}},
                            #{}}],
                        []},
                        sourcer_model:get_elements_at_pos(Model, {3, 4})
                    ),
        ?_assertMatch({[{def,[{module,foo},{function,bar,0}],
                            {{2,1},{2,4}},
                            #{}}],
                        [{ref,[{module,foo},{function,quz,1}],{{3,5},{3,8}}}]},
                        sourcer_model:get_elements_at_pos(Model, {3, 5})
                    ),
        ?_assertMatch({[{def,[{module,foo},{function,bar,0}],
                            {{2,1},{2,4}},
                            #{}}],
                        [{ref,[{module,foo},{function,quz,1}],{{3,5},{3,8}}}]},
                        sourcer_model:get_elements_at_pos(Model, {3, 6})
                    ),
        ?_assertMatch({[{def,[{module,foo},{function,bar,0}],
                            {{2,1},{2,4}},
                            #{}}],
                        []},
                        sourcer_model:get_elements_at_pos(Model, {3, 8})
                    ),
        ?_assertMatch({[{def,[{module,foo},{function,bar,0}],
                            {{2,1},{2,4}},
                            #{}}],
                        []},
                        sourcer_model:get_elements_at_pos(Model, {3, 9})
                    )
    ].

persist(F, S) ->
    {ok, Ts, _} = sourcer_scan:string(S),
    Fs = sourcer_parse:parse(sourcer_scan:filter_ws_tokens(Ts)),
    E = sourcer_model:save_model(F, Fs),
    A = sourcer_model:load_model(F),
    ?_assertEqual(E, A).

assert(Exp, Val) ->
    Expected = model(Exp),
    Value = sourcer_analyse:analyse_text(Val),
    {Val, ?_assertEqual(Expected, Value)}.

scan(D) ->
    {ok, Ts, _} = sourcer_scan:string(D),
    sourcer_scan:filter_ws_tokens(Ts).

scan(D, P0) ->
    {ok, Ts, _} = sourcer_scan:string(D, P0),
    sourcer_scan:filter_ws_tokens(Ts).

model({D, R}) ->
    #model{refs=lists:sort(R), defs=lists:sort(D)}.

print_key_test_() ->
    [
        ?_assertEqual(<<"hej:">>, sourcer_model:print_key({module, hej})),
        ?_assertEqual(<<"\"hej\"">>, sourcer_model:print_key({include, "hej"})),
        ?_assertEqual(<<"\"hej\"">>, sourcer_model:print_key({include_lib, "hej"})),
        ?_assertEqual(<<"hej/3">>, sourcer_model:print_key({function, hej, 3})),
        ?_assertEqual(<<"@2">>, sourcer_model:print_key({clause, 2})),
        ?_assertEqual(<<"hej">>, sourcer_model:print_key({var, 'hej'})),
        ?_assertEqual(<<"'Hej'">>, sourcer_model:print_key({var, 'Hej'})),
        ?_assertEqual(<<"#hej">>, sourcer_model:print_key({record, hej})),
        ?_assertEqual(<<".hej">>, sourcer_model:print_key({field, hej})),
        ?_assertEqual(<<"?hej/2">>, sourcer_model:print_key({macro, hej, 2})),
        ?_assertEqual(<<"?hej">>, sourcer_model:print_key({macro, hej, -1})),
        ?_assertEqual(<<"?Hej/0">>, sourcer_model:print_key({macro, 'Hej', 0})),
        ?_assertEqual(<<"was()/3">>, sourcer_model:print_key({type, was, 3})),
        ?_assertEqual(<<"hej:ff/2">>, sourcer_model:print_key([{module, hej},{function, ff, 2}])),
        ?_assertEqual(<<"hej:">>, sourcer_model:print_key([{module, hej}])),
        ?_assertEqual(<<"{asdd,hej}">>, sourcer_model:print_key([{asdd, hej}])),
        ?_assertEqual(<<"">>, sourcer_model:print_key([]))
    ].


