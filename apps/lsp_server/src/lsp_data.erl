-module(lsp_data).

-export([
    encode/2,
    decode/2,
    get_data/1,
    get_value/2
]).

-export_type([
    initialize_params/0
]).

-type initialize_params() :: #{
    'processId' := integer()|'null',
    'rootPath' => string(),
    'rootUri' := string()|'null',
    'initializationOptions' => any(),
    'capabilities' => any(),
    'trace' => 'off'|'messages'|'verbose'
}.

encode(Key, Data) ->
    {Key, Value} = lists:keyfind(Key, 1, get_data(Data)),
    Value.

decode(Key, Data) ->
    {Value, Key} = lists:keyfind(Key, 2, get_data(Data)),
    Value.

encode_server_capabilities(Input) ->
    try
        Caps = maps:get(capabilities, Input),
        Sync = maps:get(textDocumentSync, Caps, none),
        Input#{capabilities=>Caps#{textDocumentSync=>lsp_data:encode(Sync, sync)}}
    catch _:_ ->
        Input
    end.

get_data(sync) ->
    sync_data();
get_data(symbol) ->
    symbol_data();
get_data(completion) ->
    completion_data().

sync_data() ->
    [
        {none, 0},
        {full, 1},
        {incremental, 2}
    ].

symbol_data() ->
    [
        {file, 1},
        {module, 2},
        %{namespace, 3},
        %{package, 4},
        {type, 5}, % class
        %{method, 6},
        {macro, 7}, % property
        {field, 8},
        %{constructor, 9},
        %{enum, 10},
        %{interface, 11},
        {function, 12},
        {variable, 13},
        {constant, 14},
        {string, 15},
        {number, 16},
        {boolean, 17}
        %{array, 18}
    ].

completion_data() ->
    [
        {text, 1},
        {macro, 2}, % method
        {function, 3},
        %{constructor, 4},
        {field, 5},
        {variable, 6},
        {type, 7}, % class
        %{interface, 8},
        {module, 9},
        %{property, 10},
        %{unit, 11},
        {value, 12},
        %{enum, 13},
        {keyword, 14},
        {snippet, 15},
        %{color, 16},
        {file, 17}
        %{reference, 18}
    ].

get_value(Data, Key) when is_map(Data), is_atom(Key) ->
    maps:get(Key, Data, null);
get_value(Data, []) when is_map(Data) ->
    null;
get_value(Data, [Key]) when is_map(Data) ->
    get_value(Data, Key);
get_value(Data, [Key|T]) when is_map(Data) ->
    case get_value(Data, Key) of
        null ->
            null;
        MoreData ->
            get_value(MoreData, T)
    end;
get_value(_Data, _Key) ->
    null.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_value_test_() ->
    [
        ?_assertEqual(null, get_value(#{y=>1}, x)),
        ?_assertEqual(1, get_value(#{x=>1}, x)),

        ?_assertEqual(null, get_value(#{y=>1}, [])),

        ?_assertEqual(null, get_value(#{y=>1}, [x])),
        ?_assertEqual(1, get_value(#{x=>1}, [x])),

        ?_assertEqual(null, get_value(#{y=>1}, [x, y])),
        ?_assertEqual(null, get_value(#{x=>1}, [x, y])),
        ?_assertEqual(null, get_value(#{x=>#{z=>1}}, [x, y])),
        ?_assertEqual(1, get_value(#{x=>#{y=>1}}, [x, y])),

        ?_assertEqual(null, get_value(#{}, x))
    ].

encode_test_() ->
    [
        ?_assertEqual(0, encode(none, sync)),
        ?_assertEqual(1, encode(full, sync)),
        ?_assertEqual(17, encode(file, completion)),
        ?_assertEqual(17, encode(boolean, symbol))
    ].

decode_test_() ->
    [
        ?_assertEqual(none, decode(0, sync)),
        ?_assertEqual(full, decode(1, sync)),
        ?_assertEqual(file, decode(17, completion)),
        ?_assertEqual(boolean, decode(17, symbol))
    ].

encode_server_capabilities_test_() ->
    [
        ?_assertEqual(#{}, encode_server_capabilities(#{})),
        ?_assertEqual(#{capabilities=>#{textDocumentSync=>1}}, encode_server_capabilities(#{capabilities=>#{textDocumentSync=>full}}))
    ].

-endif.
