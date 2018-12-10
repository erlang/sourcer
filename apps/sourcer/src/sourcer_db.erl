-module(sourcer_db).

-export([
    new/0,
    add_files/2,
    get_model/2,

    get_defs/3,
    get_refs/3,
    get_info/3,
    get_text/3,
    get_completion/3,

    open_file/3,
    update_file/3,
    close_file/2,
    get_text/2,
    process_watched/2,

    resolve_file_reference/3,

    symbols/2,

    get_uri/2
]).

%%-define(DEBUG, true).
-include("debug.hrl").

-include("sourcer_model.hrl").

new() ->
    #db{}.

add_entry(Uri, Text, IsOpen, DB) ->
    %?D({Uri, Text, IsOpen}),
    Text1 = unicode:characters_to_list(Text),
    Model = sourcer_analyse:analyse_text(Text1),
    MyText = case IsOpen of
        true ->
            Text;
        false ->
            Text %sourcer_util:uri_to_path(Uri)
        end,
    Entry = #db_entry{
                model = Model,
                text = MyText
            },
    Models = DB#db.models,
    % TODO retrieved includes
    NewModels = dict:store(Uri, Entry, Models),
    digraph:add_vertex(DB#db.deps, Uri),
    DB#db{models=NewModels}.

remove_entry(Uri, DB) ->
    Models = DB#db.models,
    Entry = dict:fetch(Uri, Models),
    digraph:del_vertex(DB#db.deps, Uri),
    NewModels = dict:erase(Uri, Models),
    DB#db{models=NewModels}.

update_entry(Uri, Text, IsOpen, DB) ->
    add_entry(Uri, Text, IsOpen, DB).

-spec get_model(uri(), db()) -> model().
get_model(Uri, DB) ->
    Models = DB#db.models,
    Entry = dict:fetch(Uri, Models),
    Entry#db_entry.model.

get_defs(_Uri, _Pos, _DB) ->
    [].

get_refs(_Uri, _Pos, _DB) ->
    [].

get_info(_Uri, _Key, _DB) ->
    % (docs, spec, children keys [both defs and refs?])
    [].

get_text(Uri, DB) ->
    Models = DB#db.models,
    case dict:find(Uri, Models) of
        {ok, Entry} ->
            Entry#db_entry.text;
        Error ->
            ?D({uri_not_found, Uri, dict:fetch_keys(Models)}),
            <<>>
    end.

get_text(_Uri, _Range, _DB) ->
    [].

get_completion(_Uri, _Pos, _DB) ->
    % get the crt line's text and scan it, so we don't have to save tokens?
    [].

%% @doc Return the full path of Target when referenced from Source
resolve_file_reference(_Source, Target, _DB) ->

    Target.

%%%%%%%%%%%%%%%%%%

open_file(DB, Uri, Text) ->
    ?D({open, Uri}),
    add_entry(Uri, Text, true, DB).

update_file(DB, Uri, [#{text:=Text}]) ->
    ?D({change, Uri}),
    update_entry(Uri, Text, true, DB);
update_file(DB, _Uri, _Changes) ->
    DB.

close_file(DB, Uri) ->
    ?D({close, Uri}),
    Models = DB#db.models,
    Entry = dict:fetch(Uri, Models),
    NewModels = dict:store(Uri,
        Entry#db_entry{text=sourcer_util:uri_to_path(Uri)},
        Models),
    DB#db{models=NewModels}.

process_watched(DB, Changes) ->
    DB.

process_watched_1(#{uri:=Uri, type:=1}, List) ->
    %% TODO: start parsing & processing
    [Uri|List];
process_watched_1(#{uri:=_Uri, type:=2}, List) ->
    %% TODO: start parsing & processing
    List;
process_watched_1(#{uri:=Uri, type:=3}, List) ->
    lists:delete(Uri, List).

add_files(Files, DB) ->
    lists:foldl(fun(F, Acc)->
            case dict:find(F, Acc#db.models) of
                {ok, _} ->
                    Acc;
                error ->
                    case file:read_file(sourcer_util:uri_to_path(F)) of
                        {ok, Txt} ->
                            add_entry(F, Txt, false, Acc);
                        _Err ->
                            ?D({_Err, F}),
                            Acc
                    end
            end
        end, DB, Files).

symbols(Query, DB) ->
    Models = DB#db.models,
    %?D(Models),
    All = dict:to_list(Models),
    ?D(length(All)),
    Syms = lists:flatten([spread(K,(V#db_entry.model)#model.defs)
                            || {K,V}<-All
                        ]),
    ?D(length(Syms)),
    Z = lists:flatten([symbol(X, Query) || X<-Syms]),
    ?D(length(Z)),
    % TODO too long list crashes something with vscode
    case length(Z)>200 of
        true -> truncate(Z, 200);
        false -> Z
    end.

symbol({Uri, #def{ctx=Ctx,name_range=Range}}=_X, Query) ->
    Name = sourcer_lsp:print_name(Ctx),
    case {Query, string:find(Name, Query)} of
        {<<>>, _} ->
            [{Name, 5, Uri, Range}];
        {_, nomatch} ->
            [];
        _ ->
            [{Name, 5, Uri, Range}]
    end;
symbol(_Y, _) ->
    ?D(_Y),
    {<<"foo">>, 5, <<"Uri">>, none}.

spread(K, L) ->
    [{K, X} || X<-L].

truncate(L, N) ->
    truncate(L, N, []).

truncate(_, 0, R) ->
    R;
truncate([H|T], N, R) ->
    truncate(T, N-1, [H|R]).


get_uri(Key, DB) ->
    <<"hello">>.
    