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
            #model{defs=[{[a1],1,#{}},{[a2],1,#{}},{[b1],1,#{}},{[b2],1,#{}}],
                refs=[{[c1],1},{[c2],1},{[d1],1},{[d2],1}]},
            sourcer_db:merge([
                #model{defs=[{[a1],1,#{}},{[b1], 1,#{}}],
                    refs=[{[c1],1},{[d1], 1}]},
                #model{defs=[{[a2],1,#{}},{[b2],1,#{}}],
                    refs=[{[c2],1},{[d2],1}]}])
        ),
        ?_assertEqual(
            #model{defs=[{[{a1}],1,#{}},{[{macro,1,1}],1,#{}},{[{macro,1,1}],2,#{}}],
                refs=[{[c1],1},{[c1],2},{[d1],1},{[d2],1}]},
            sourcer_db:merge([
                #model{defs=[{[{a1}],2,#{}},{[{macro,1,1}], 2,#{}}],
                    refs=[{[c1],2},{[d1], 1}]},
                #model{defs=[{[{a1}],1,#{}},{[{macro,1,1}],1,#{}}],
                    refs=[{[c1],1},{[d2],1}]}])
        ),
        ?_assertEqual(
            #model{},
            sourcer_db:merge([#model{}, #model{}])
        )
    ].

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

get_element_at_pos_test_() ->
    Text = "
-module(foo).
bar() ->
    quz(1).
quz(1) ->
    ok;
quz(_) ->
    ok.
    ",
    Model = sourcer_db:analyse_text(Text),
    [
        ?_assertMatch({[{[{module,foo},{function,bar,0}],
                            {{2,1},{2,4}},
                            #{}}],
                        []},
                        sourcer_db:get_element_at_pos(Model, {3, 4})
                    ),
        ?_assertMatch({[{[{module,foo},{function,bar,0}],
                            {{2,1},{2,4}},
                            #{}}],
                        [{[{module,foo},{function,quz,1}],{{3,5},{3,8}}}]},
                        sourcer_db:get_element_at_pos(Model, {3, 5})
                    ),
        ?_assertMatch({[{[{module,foo},{function,bar,0}],
                            {{2,1},{2,4}},
                            #{}}],
                        [{[{module,foo},{function,quz,1}],{{3,5},{3,8}}}]},
                        sourcer_db:get_element_at_pos(Model, {3, 6})
                    ),
        ?_assertMatch({[{[{module,foo},{function,bar,0}],
                            {{2,1},{2,4}},
                            #{}}],
                        []},
                        sourcer_db:get_element_at_pos(Model, {3, 8})
                    ),
        ?_assertMatch({[{[{module,foo},{function,bar,0}],
                            {{2,1},{2,4}},
                            #{}}],
                        []},
                        sourcer_db:get_element_at_pos(Model, {3, 9})
                    )
    ].

persist(F, S) ->
    {ok, Ts, _} = sourcer_scan:string(S),
    Fs = sourcer_parse:parse(sourcer_scan:filter_ws_tokens(Ts)),
    E = sourcer_db:save_model(F, Fs),
    A = sourcer_db:load_model(F),
    ?_assertEqual(E, A).

assert(Exp, Val) ->
    Expected = model(Exp),
    Value = sourcer_db:analyse_text(Val),
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

model({D, R}) ->
    #model{refs=lists:sort(R), defs=lists:sort(D)}.

print_key_test_() ->
    [
        ?_assertEqual(<<"hej:">>, sourcer_db:print_key({module, hej})),
        ?_assertEqual(<<"\"hej\"">>, sourcer_db:print_key({include, "hej"})),
        ?_assertEqual(<<"\"hej\"">>, sourcer_db:print_key({include_lib, "hej"})),
        ?_assertEqual(<<"hej/3">>, sourcer_db:print_key({function, hej, 3})),
        ?_assertEqual(<<"@2">>, sourcer_db:print_key({clause, 2})),
        ?_assertEqual(<<"hej">>, sourcer_db:print_key({var, 'hej'})),
        ?_assertEqual(<<"'Hej'">>, sourcer_db:print_key({var, 'Hej'})),
        ?_assertEqual(<<"#hej">>, sourcer_db:print_key({record, hej})),
        ?_assertEqual(<<".hej">>, sourcer_db:print_key({field, hej})),
        ?_assertEqual(<<"?hej/2">>, sourcer_db:print_key({macro, hej, 2})),
        ?_assertEqual(<<"?hej">>, sourcer_db:print_key({macro, hej, -1})),
        ?_assertEqual(<<"?Hej/0">>, sourcer_db:print_key({macro, 'Hej', 0})),
        ?_assertEqual(<<"was()/3">>, sourcer_db:print_key({type, was, 3})),
        ?_assertEqual(<<"hej:ff/2">>, sourcer_db:print_key([{module, hej},{function, ff, 2}])),
        ?_assertEqual(<<"hej:">>, sourcer_db:print_key([{module, hej}])),
        ?_assertEqual(<<"{asdd,hej}">>, sourcer_db:print_key([{asdd, hej}])),
        ?_assertEqual(<<"">>, sourcer_db:print_key([]))
    ].


