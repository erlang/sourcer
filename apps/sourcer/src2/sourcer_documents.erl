-module(sourcer_documents).

-export([
    get_sync_value/0,
	open_file/3,
    update_file/3,
    get_text/2,
    get_refs/2,
    parse_file/2,
    process_file/2,
    process_watched/2,
	get_element/3,
	get_line_at_offset/2,
	get_line_info/2
]).

-define(DEBUG, true).

-ifdef(DEBUG).
-define(DEBUG(F, A), io:format(F, A)).
-else.
-define(DEBUG(F, A), ok).
-endif.

get_sync_value() ->
    1.

open_file(Open, URI, Text) ->
	NewOpen = [{URI, Text, sourcer_documents:process_file(URI, Text)}|Open],
	?DEBUG("***** OPEN---+++ ~p~n", [URI]),
	NewOpen.

update_file(Open, URI, [#{text:=Text}]) ->
	NewOpen = lists:keyreplace(URI, 1, Open, {URI, Text, process_file(URI, Text)}),
	?DEBUG("***** CHANGE---+++ ~p~n", [URI]),
	NewOpen;
update_file(Open, _URI, _Changes) ->
	Open.

get_text(State, URI) ->
	{URI, Text, _Data} = lists:keyfind(URI, 1, State),
	Text.

get_refs(State, URI) ->
?DEBUG("URI:: ~p~nState--->>>> ~p~n", [URI, State]),
	{URI, _Text, Data} = lists:keyfind(URI, 1, State),
	Data.

parse_file(File, Text) ->
	TText = unicode:characters_to_list(Text),
	case sourcer_noparse:initial_parse(list_to_atom(unicode:characters_to_list(File)), 
			unicode:characters_to_list(File), 
			TText,
			".", false) of
		{ok, {model, AST, _}, Refs} ->
			%io:format("00* ~p~n", [Refs]),
			{Lines, _} = get_line_info(TText),
			%io:format("000 ~p~n", [Lines]),
			{ok, AST, Refs, Lines};
		Err ->
			Err
	end.

process_file(URI, Text) ->
	case parse_file(URI, Text) of
		{ok, AST, Refs, Lines} ->
			%io:format("Parsed: ~p~n   ~p~n??????~p~n", [URI, Refs, Lines]),
			{AST, Refs, Lines};
		Err ->
			io:format("Parse error: ~p ~p~n", [URI, Err]),
			{undefined, undefined, undefined}
	end.

process_watched(#{uri:=URI, type:=1}, List) ->
	%% TODO: start parsing & processing
	[URI|List];
process_watched(#{uri:=_URI, type:=2}, List) ->
	%% TODO: start parsing & processing
	List;
process_watched(#{uri:=URI, type:=3}, List) ->
	lists:delete(URI, List).

get_element(Data, URI, #{character:=C, line:=L}) ->
	{_, Refs, Lines} = get_refs(Data, URI),
	{Ofs,Len,_} = get_line_info(L, Lines),
	%% FIXME!
	Refs1 = lists:filter(fun filter_defs/1, Refs),
	%io:format("refs1 ~p ~n", [{Refs1, Ofs, Len}]),
	Refs2 = Refs1,
	%io:format("refs2 ~p ~n", [Refs2]),
	case Refs2 of
		[] -> [];
		_ -> lists:last(Refs2)
	end.

filter_defs({ref, {module_def, _}, _, _, _, _, _, _}) ->
	true;
filter_defs({ref, {function_def, _}, _, _, _, _, _, _}) ->
	true;
filter_defs(_) ->
	false.

get_line_info(Text) ->
	Lines = sourcer_util:split(Text, "\n"),
	lists:mapfoldl(
		fun(X, {A, I}) -> L=length(X), {{A, L, I}, {A+L, I+1}} end, 
		{0, 0}, 
		Lines
	).

get_line_at_offset(Offset, Lines) ->
	L = lists:dropwhile(fun({O,_,_})-> O<Offset end, Lines),
	case L of
		[{_,_,I}|_] -> I;
		_ -> none
	end.

get_line_info(none, _) ->
	undefined;
get_line_info(Index, Lines) ->
	lists:nth(Index+1, Lines).
