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
                            {{10,1},{10,4}},
                            #{body := {{10,1},{11,7}}}}
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
    ].

definition_test_() ->
    DB = load_db(),
    [
    ].

references_test_() ->
    DB = load_db(),
    [
    ].

completion_test_() ->
    DB = load_db(),
    [
    ].

highlight_test_() ->
    DB = load_db(),
    [
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