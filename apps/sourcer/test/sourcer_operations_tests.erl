-module(sourcer_operations_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DEBUG, true).
-include("debug.hrl").

-include("sourcer_model.hrl").

general_test_() ->
    DB = load_db(),
    [
        ?_assertEqual(3, dict:size(DB#db.models))
    ].

symbols_test_() ->
    DB = load_db(),
    [
        ?_assertEqual([], 
                sourcer_operations:symbols(<<"">>, DB)),

        %% TODO how to get the real file URI?
        ?_assertMatch([
                    {_, 
                        {def,[{module,aaa},{function,mmm,0}],
                            {{11,1},{11,4}},
                            #{body := {{11,1},{12,7}}}}
                    }
                ],
                sourcer_operations:symbols(<<"mm">>, DB))
    ].

document_symbols_test_() ->
    DB = load_db(),
    [
        ?_assertEqual([
                [{module,aaa}],
                [{module,aaa},{function,baz,0}],
                [{module,aaa},{function,foo,0}],
                [{module,aaa},{function,mmm,0}]
            ], 
            dkeys(sourcer_operations:document_symbols(data_file("aaa.erl"), DB))
        )
    ].

hover_test_() ->
    DB = load_db(),
    [
        ?_assertEqual(<<"">>, 
            unicode:characters_to_binary(sourcer_operations:hover(data_file("aaa.erl"), {2, 3}, DB))),
        ?_assertEqual(<<"### aaa:foo/0\n\n\n\n```\n```\n\n\n\n">>, 
            unicode:characters_to_binary(sourcer_operations:hover(data_file("aaa.erl"), {3, 2}, DB))),
        ?_assertEqual(<<"### aaa:baz/0\n\n\n\n```\n```\n\n\n%% extra\n\n">>, 
            unicode:characters_to_binary(sourcer_operations:hover(data_file("aaa.erl"), {7, 2}, DB)))
    ].

definition_test_() ->
    DB = load_db(),
    [
        ?_assertEqual([], sourcer_operations:definition(data_file("aaa.erl"), {2, 1}, DB)),
        ?_assertMatch([
            {
                _,
                {def,[{module,bbb},{function,bar,0}],
                    {{3,1},{3,4}},
                    #{body := {{3,1},{5,14}}}}
            }], 
            sourcer_operations:definition(data_file("aaa.erl"), {4, 10}, DB))
    ].

references_test_() ->
    DB = load_db(),
    [
        ?_assertEqual([], sourcer_operations:references(data_file("aaa.erl"), 
            {2, 1}, #{}, DB)),
        ?_assertMatch([
            {
                _,
                {ref,[{module,bbb},{function,bar,0}],{{4,9},{4,12}}}
            }], 
            sourcer_operations:references(data_file("aaa.erl"), 
                {4, 10}, #{includeDeclaration=>false}, DB)),
        ?_assertMatch([
            {
                _,
                {def,[{module,bbb},{function,bar,0}],
                    {{3,1},{3,4}},
                    #{body := {{3,1},{5,14}}}}
            },
            {
                _, {ref,[{module,bbb},{function,bar,0}],{{4,9},{4,12}}}
            }], 
            sourcer_operations:references(data_file("aaa.erl"), 
                {4, 10}, #{includeDeclaration=>true}, DB))
    ].

completion_test_() ->
    DB = load_db(),
    [
    ].

highlight_test_() ->
    DB = load_db(),
    [
        ?_assertEqual([],
            sourcer_operations:highlight(data_file("aaa.erl"), 
                {2, 1}, DB)),
        ?_assertEqual([
                {ref,[{module,bbb},{function,bar,0}],{{4,9},{4,12}}},
                {def,[{module,bbb},{function,bar,0}],
                    {{3,1},{3,4}},
                    #{body => {{3,1},{5,14}}}}
            ],
            sourcer_operations:highlight(data_file("aaa.erl"), 
                {4, 10}, DB))
    ].

load_db() ->
    Dir = data_dir(),
    Files0 = filelib:wildcard(Dir ++ "/*"),
    Files = [sourcer_util:path_to_uri(F) || F<-Files0],
    DB = sourcer_db:new(),
    sourcer_db:add_files(Files, DB).

data_dir() ->
    filename:dirname(code:which(?MODULE)) ++ "/operations_data".

data_file(Name) ->
    sourcer_util:path_to_uri(filename:join(data_dir(), Name)).

dkeys(Defs) ->
    [X || #def{ctx=X} <- Defs].

rkeys(Defs) ->
    [X || #ref{ctx=X} <- Defs].