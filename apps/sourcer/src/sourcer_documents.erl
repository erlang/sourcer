-module(sourcer_documents).

-export([
    get_sync_value/0,
    open_file/3,
    update_file/3,
    get_text/2,
    get_model/2,
    parse_file/2,
    process_file/2,
    process_watched/2,
    get_element/3,
    get_line_at_offset/2,
    get_line_info/2
]).

-define(DEBUG, true).

-include("sourcer_db.hrl").
-include("debug.hrl").

get_sync_value() ->
    1.

open_file(Open, URI, Text) ->
    NewOpen = [{URI, Text, process_file(URI, Text)}|Open],
    ?D({open, URI}),
    NewOpen.

update_file(Open, URI, [#{text:=Text}]) ->
    NewOpen = lists:keyreplace(URI, 1, Open, {URI, Text, process_file(URI, Text)}),
    ?D({change, URI}),
    NewOpen;
update_file(Open, _URI, _Changes) ->
    Open.

get_text(State, URI) ->
    {URI, Text, _Data} = lists:keyfind(URI, 1, State),
    Text.

get_model(State, URI) ->
    case lists:keyfind(URI, 1, State) of
        {URI, _Text, Data} ->
            Data;
        false ->
            []
    end.

parse_file(File, Text) ->
    TText = unicode:characters_to_list(Text),
    Mod = list_to_atom(unicode:characters_to_list(File)),
    {ok, Toks, _} = sourcer_scan:string(TText),
    Forms = sourcer_parse:parse(Toks),
    Db = sourcer_db:analyze(Forms),
    Db.

process_file(URI, Text) ->
    {parse_file(URI, Text), get_line_info(Text)}.

process_watched(#{uri:=URI, type:=1}, List) ->
    %% TODO: start parsing & processing
    [URI|List];
process_watched(#{uri:=_URI, type:=2}, List) ->
    %% TODO: start parsing & processing
    List;
process_watched(#{uri:=URI, type:=3}, List) ->
    lists:delete(URI, List).

get_element(Data, URI, #{character:=C, line:=L}) ->
    Model = get_model(Data, URI),
    
    %Lines = sourcer_scan:line_info(Data#)
    %{Ofs,Len,_} = get_line_info(L, Lines),
    %% FIXME!
    Refs2 = [], 
    %io:format("refs2 ~p ~n", [Refs2]),
    case Refs2 of
        [] -> [];
        _ -> lists:last(Refs2)
    end.

get_line_info(Text) ->
    ?D(Text),
    %% we hope there are no mixed newlines
    Lines = binary:split(Text, [<<"\r\n">>, <<"\r">>, <<"\n">>], [global]),
    lists:mapfoldl(
        fun(X, {A, I}) -> L=size(X), {{A, L, I}, {A+L, I+1}} end, 
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

