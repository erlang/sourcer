-module(sourcer_analyse_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DEBUG, true).
-include("debug.hrl").

-include("sourcer_model.hrl").

merge_test_() ->
    M =#model{defs=[{def,[a1],1,0},{def,[b1],1,0}],
                refs=[{ref,[c1],1},{ref,[d1],1}]},
    [
        ?_assertEqual(
            M,
            sourcer_analyse:merge([
                M
            ])
        ),
        ?_assertEqual(
            M,
            sourcer_analyse:merge([
                #model{},
                M
            ])
        ),
        ?_assertEqual(
            M,
            sourcer_analyse:merge([
                M,
                #model{}
            ])
        ),
        ?_assertEqual(
            #model{defs=[
                    {def,[a1],1,#{}},
                    {def,[a2],1,#{}},
                    {def,[b1],1,#{}},
                    {def,[b2],1,#{}}
                ],
                refs=[
                    {ref,[c1],1},
                    {ref,[c2],1},
                    {ref,[d1],1},
                    {ref,[d2],1}
                ]},
            sourcer_analyse:merge([
                #model{defs=[
                        {def,[a1],1,#{}},
                        {def,[b1],1,#{}}
                    ],
                    refs=[
                        {ref,[c1],1},
                        {ref,[d1],1}
                    ]},
                #model{defs=[
                        {def,[a2],1,#{}},
                        {def,[b2],1,#{}}
                    ],
                    refs=[
                        {ref,[c2],1},
                        {ref,[d2],1}
                    ]}])
        ),
        ?_assertEqual(
            #model{defs=[
                    {def,[{a1}],1,#{}},
                    {def,[{macro,1,1}],1,#{}},
                    {def,[{macro,1,1}],2,#{}}
                ],
                refs=[
                    {ref,[c1],1},
                    {ref,[c1],2},
                    {ref,[d1],1},
                    {ref,[d2],1}
                ]},
            sourcer_analyse:merge([
                #model{defs=[
                        {def,[{a1}],2,#{}},
                        {def,[{macro,1,1}], 2,#{}}
                    ],
                    refs=[
                        {ref,[c1],2},
                        {ref,[d1], 1}
                    ]},
                #model{defs=[
                        {def,[{a1}],1,#{}},
                        {def,[{macro,1,1}],1,#{}}
                    ],
                    refs=[
                        {ref,[c1],1},
                        {ref,[d2],1}
                    ]}])
        ),
        ?_assertEqual(
            #model{},
            sourcer_analyse:merge([#model{}, #model{}])
        )
    ].

assert(Exp, Val) ->
    Expected = model(Exp),
    Value = sourcer_analyse:analyse_text(Val),
    {Val, ?_assertEqual(Expected, Value)}.

analyze_test_() ->
    {ok, Terms} = file:consult("apps/sourcer/test/parser_model_tests_data"),
    [assert(Y, X) || {X,_,Y}<-lists:reverse(Terms)].

scan(D) ->
    {ok, Ts, _} = sourcer_scan:string(D),
    sourcer_scan:filter_ws_tokens(Ts).

scan(D, P0) ->
    {ok, Ts, _} = sourcer_scan:string(D, P0),
    sourcer_scan:filter_ws_tokens(Ts).

model({D, R}) ->
    #model{refs=lists:sort(R), defs=lists:sort(D)}.

