-module(sourcer_analyze_tests).

-include_lib("eunit/include/eunit.hrl").
-include("sourcer_parse.hrl").
-include("sourcer_analyze.hrl").

analyze_module_test_() ->
    {Mod, _} = parse(""
                     "-module(fox).\n"
                     "-export([a/2]).\n"
                     "-export([b/3]).\n"
                     "-include(\"bar.hrl\").\n"
                     "fow(A, [X|Y]) -> {X, A+Y}.\n"
                    ),
    [
     ?_assertMatch([{module, _, fox}],
                   find_forms(module, Mod)),
     ?_assertMatch([{export,_,[[{atom,#{value:=a}},{'/',_},{integer,#{value:=2}}]]},
                    {export,_,[[{atom,#{value:=b}},{'/',_},{integer,#{value:=3}}]]}],
                   find_forms(export, Mod)),
     ?_assertMatch([{include, _, "bar.hrl"}],
                   find_forms(include, Mod)),
     ?_assertMatch([{function,_,fow,2,
                     [{clause,_,
                       [[{var,#{value:='A'}}],[{'[',_},{var,#{value:='X'}},{'|',_},{var,#{value:='Y'}},{']',_}]],
                       [],
                       [{'{',_},{var,#{value:='X'}},{',',_},{var,#{value:='A'}},{'+',_},{var,#{value:='Y'}},{'}',_}]}]}],
                   find_forms(function, Mod)),
     %% TODO match something relevant
     ?_assertMatch(#module{},
                   sourcer_analyze:analyze_module(Mod))
    ].

analyze_references_test_() ->
    [
     ?_assertMatch([{macroref,#{line:=0,column:=10,offset:=9,length:=3},'Z4'},
                    {macroref,#{line:=0,column:=22,offset:=21,length:=2},'M'},
                    {functionref,#{line:=0,column:=4,offset:=3,length:=3},{function_id,a,b,2}},
                    {recordref,#{line:=0,column:=16,offset:=15,length:=2},r},
                    {functionref,#{line:=0,column:=22,offset:=21,length:=4},{function_id,'M',f,0}}],
                   sourcer_analyze:analyze_references(scan2("3, a:b(3,?Z4), #r{}, ?M:f()"))),
     ?_assertMatch([{macroref,#{line:=0,column:=2,offset:=1,length:=4},'REC'},
                    {recordref,#{line:=0,column:=1,offset:=0,length:=5},'REC'},
                    {functionref,#{line:=0,column:=10,offset:=9,length:=2},{function_id,'$this$',ff,3}},
                    {functionref,#{line:=0,column:=13,offset:=12,length:=1},{function_id,'$this$',t,1}}],
                   sourcer_analyze:analyze_references(scan2("#?REC.f, ff(t(4),5,6)"))),
     ?_assertMatch([{functionref,#{line:=0,column:=8,offset:=7,length:=2},{function_id,'$this$',ff,0}},
                    {functionref,#{line:=0,column:=14,offset:=13,length:=2},{function_id,'$this$',gg,1}}],
                   sourcer_analyze:analyze_references(scan2("2, ff, ff(), gg(1)"))),
     ?_assertMatch([],
                   sourcer_analyze:analyze_references(scan2("a, 4")))
    ].

%%%%%%%%%%%%%%%%

parse(S) ->
    {ok, R} = sourcer_parse:string(S, #context{}),
    R.

scan2(S) ->
    {ok, Mod0, _} = sourcer_scan:string(S),
    sourcer_util:filter_tokens(Mod0).

find_forms(Key, #{forms:=Forms}) when is_atom(Key) ->
    Fun = fun(X) ->
                  element(1, X) == Key
          end,
    lists:filter(Fun, Forms).
