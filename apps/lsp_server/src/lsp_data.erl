-module(lsp_data).

-export([
    encode/2,
    decode/2
]).

encode(Key, Data) ->
    {Key, Value} = lists:keyfind(Key, 1, get_data(Data)),
    Value.

decode(Key, Data) ->
    {Value, Key} = lists:keyfind(Key, 2, get_data(Data)),
    Value.
        
encode_server_capabilities(Input) ->
	Caps = maps:get(capabilities, Input),
	Sync = maps:get(textDocumentSync, Caps, none),
	Input#{capabilities=>Caps#{textDocumentSync=>lsp_data:encode(Sync, sync)}}.

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
		{macro, 6}, % method
		%{property, 7},
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
