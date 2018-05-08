-module(sourcer_operations).

-export([
    hover/3,
    definition/3,
    references/4,
    completion/3,
    resolve_completion/2,
    highlight/3,
    symbols/2,
    document_symbols/2
]).

-define(DEBUG, true).
-include("debug.hrl").

-include("sourcer_model.hrl").

-spec hover(uri(), pos(), db()) -> text().
hover(Uri, Position, DB) ->
    Model = sourcer_db:get_model(Uri, DB),
    case sourcer_model:get_elements_at_pos(Model, Position) of
        {_, [#def{}=Def|_]} ->
            hover_for([{Uri,Def}], DB);
        {_, [#ref{ctx=Key}|_]} ->
            %% TODO not variables!
            hover_for(get_def(Key, DB), DB); 
        _Other ->
            ?D({hover, "???", _Other}),
            []
    end.

-spec definition(uri(), pos(), db()) -> [location()].
definition(Uri, Position, DB) ->
    Model = sourcer_db:get_model(Uri, DB),
    case sourcer_model:get_elements_at_pos(Model, Position) of 
        {_, [#def{}=Def|_]} ->
            def_for({Uri, Def});
        {_, [#ref{ctx=Key}|_]} ->
            %% TODO not variables!
            def_for(get_def(Key, DB)); 
        {[#def{}=Def], _} ->
            def_for({Uri, Def});
        _Other ->
            ?D({hover, "???", _Other}),
            []
    end.

-spec references(uri(), pos(), map(), db()) -> [{uri(), location()}].
references(Uri, Position, Context, DB) ->
    Model = sourcer_db:get_model(Uri, DB),
    case sourcer_model:get_elements_at_pos(Model, Position) of
        {_,[Ref]} ->
            Val = get_refs(Ref#ref.ctx, DB),
            ?D(Context),
            case maps:get('includeDeclaration', Context, false) of
                true ->
                    get_def(Ref#ref.ctx, DB) ++ Val;
                false ->
                    Val
            end;
        _ ->
            []
    end.

completion(Uri, Position, DB) ->
    Model = sourcer_db:get_model(Uri, DB),
    {[], true}.

resolve_completion(Params, DB) ->
    %Model = sourcer_db:get_model(Uri, DB),
    [].

document_symbols(Uri, DB) ->
    Model = sourcer_db:get_model(Uri, DB),
    Model#model.defs.

symbols(<<"">>, _DB) ->
    [];
symbols(Query, DB) ->
    Models = dict:to_list(DB#db.models),
    Filter = fun({_, #def{ctx=Key}}) -> 
                Name=sourcer_lsp:print_name(Key), 
                string:find(Name, Query)=/=nomatch
            end,
    lists:filter(Filter, lists:flatten([symbol_list(Uri, M#model.defs) || {Uri, #db_entry{model=M}}<-Models])).

highlight(Uri, Position, DB) ->
    Model = sourcer_db:get_model(Uri, DB),
    [].

%%%%%%%%%%%%%%%%%%%

hover_for([{Uri, #def{ctx=Key,info=Info}}|_], DB) ->
    Text = sourcer_db:get_text(Uri, DB),
    DocRange = maps:get(comments, Info, none),
    Doc = sourcer_util:get_text_range(Text, DocRange),
    SpecRange = maps:get(spec, Info, none),
    Spec = sourcer_util:get_text_range(Text, SpecRange),
    SpecDocRange = maps:get(spec_comments, Info, none),
    SpecDoc = sourcer_util:get_text_range(Text, SpecDocRange),
    hover_content(Key, SpecDoc, Spec, Doc);
hover_for(_Tgt, _) ->
    ?D({hover, '---', _Tgt}),
    [].

hover_content(Key, SpecDoc, Spec, Doc) ->
    Fmt = "### ~s

~s

~s

~s
",
    io_lib:format(Fmt, 
        [sourcer_lsp:print_name(Key), SpecDoc, ["```\n", Spec, "```\n"], Doc]).

def_for([]) ->
    [];
def_for({Uri,#def{name_range=Range}}) ->
    [#{uri=>Uri, range=>Range}].

get_def(Key, DB) ->
    Entries = dict:to_list(DB#db.models),
    Pred = fun({_,#db_entry{model=M}}) -> 
            not lists:keymember(Key, #def.ctx, M#model.defs) 
        end,
    case lists:dropwhile(Pred, Entries) of
        [{Uri,Item}|_] ->
            case lists:keyfind(Key, #def.ctx, Item#db_entry.model#model.defs) of
                false ->
                    [];
                #def{}=Def ->
                    %?D([Uri, Def]),
                    [{Uri, Def}]
            end;
        _X ->
            []
    end.

get_refs(Key, DB) ->
    Entries = dict:to_list(DB#db.models),
    Pred = fun({_,#db_entry{model=M}}) -> 
            lists:keymember(Key, #ref.ctx, M#model.refs) 
        end,
    {Found, _} = lists:partition(Pred, Entries),
    lists:flatten([get_refs_aux(Uri, Key, Item) || {Uri, Item}<-Found]).

get_refs_aux(Uri, Key, Item) ->
    case lists:keyfind(Key, #def.ctx, Item#db_entry.model#model.refs) of
        false ->
            [];
        #def{}=Def ->
            [{Uri, Def}];
        #ref{}=Ref ->
            [{Uri, Ref}]
    end.

symbol_list(Uri, Defs) ->
    [{Uri, X} || X<-Defs].