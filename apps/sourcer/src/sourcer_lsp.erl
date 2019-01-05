%%% @doc Translate to/from sourcer_module and sourcer_db data from/to 
%%% the json expected by LSP.
-module(sourcer_lsp).

-export([
    location/2,
    text_edit/2,
    hover/1,
    hover/2,
    print_name/1,
    markup_content/2,
    completion_item/1,
    completion_list/2,
    symbol_information/1,
    symbol_information/2,
    highlight/1,
    symbols/2,
    references/1,
    definition/1
]).

%%-define(DEBUG, true).
-include("debug.hrl").

-include("sourcer_model.hrl").

range(none) ->
    null;
range({{L1,C1},{L2,C2}}) ->
    #{
        start=>#{line=>L1, character=>C1-1},
        'end'=>#{line=>L2, character=>C2-1}
    }.

location(Uri, Range) ->
    #{uri=>Uri, range=>range(Range)}.

text_edit(NewText, Range) ->
    #{range=>range(Range),
        newText=>unicode:characters_to_binary(NewText)}.

definition(Defs) ->
    [location(Uri, Range) || #{uri:=Uri, range:=Range}<-Defs].

references(L) ->
    Fun = fun({Uri, #ref{range=Y}}) -> location(Uri, Y);
            ({Uri, #def{name_range=Y}}) -> location(Uri, Y)
        end,
    lists:map(Fun, L).

refdef(Uri, #ref{ctx=Key,range=Range}) ->
    #{
        name=>print_name(Key),
        kind=>kind(element(1, hd(Key))),
        location=>#{
            Uri=>Uri,
            range=>range(Range)
        }
    };
refdef(Uri, #def{ctx=Key,name_range=Range}) ->
    #{
        name=>print_name(Key),
        kind=>kind(element(1, hd(Key))),
        location=>#{
            Uri=>Uri,
            range=>range(Range)
        }
    }.

location_refs(Model, Uri) ->
    #model{refs=Refs, defs=Defs} = Model,
    [
        #{
            Uri=>Uri,
            range=>range(Pos)
        }
        || {Key,Pos} <- Refs++Defs].


print_name(List) when is_list(List) ->
    iolist_to_binary([print_name(X)||X<-List]);
print_name(Data) ->
    case Data of
        {module, M} ->
            iolist_to_binary(io_lib:format("~s:", [M]));
        {function, F, A} ->
            iolist_to_binary(io_lib:format("~w/~w", [F, A]));
        {clause, N} ->
            iolist_to_binary(io_lib:format("@~w", [N]));
        {macro, M, A} ->
            case A of
                -1 ->
                    iolist_to_binary(io_lib:format("?~s", [M]));
                _ ->
                    iolist_to_binary(io_lib:format("?~s/~w", [M, A]))
            end;
        {var, N} ->
            iolist_to_binary(io_lib:format("!~s", [N]));
        _ ->
            iolist_to_binary(io_lib:format("~w", [Data]))
    end.

kind(E) ->
    case lists:keyfind(E, 1, lsp_data:get_data(symbol)) of
        false ->
            2;
        {_, N} ->
            N
    end.

hover(Contents) ->
    hover(Contents, null).

hover(Contents, Range) ->
    #{
        contents => markup_content(markdown, Contents), 
        range => Range
    }.

markup_content(Kind, Str) ->
    #{
        kind => unicode:characters_to_binary(atom_to_list(Kind)), 
        value => unicode:characters_to_binary(Str)
    }.

completion_item(Item) ->
    %% TODO
    #{}.

completion_list(complete, Items) ->
    #{
        isIncomplete => false, 
        items => [completion_item(X) || X <-Items]
    };
completion_list(_, Items) ->
    #{
        isIncomplete => true, 
        items => [completion_item(X) || X <-Items]
    }.
    
symbol_information(Uri, #def{ctx=Key, name_range=Range}) ->
    %% TODO symbol kind
    symbol_information({print_name(Key), 2, Uri, Range}).

symbol_information({Name, Kind, Uri, Range}=X) ->
    #{
        name => unicode:characters_to_binary(Name), 
        kind => Kind,
        location => location(Uri, Range)
    };
symbol_information({Name, Kind, Uri, Range, Container}) ->
    #{
        name => unicode:characters_to_binary(Name), 
        kind => Kind,
        location => location(Uri, Range),
        containerName => Container
    }.
    
symbols(Uri, Syms) ->
    [   #{
            name => print_name(Key),
            kind => 1,
            location => location(Uri, Range)
        }
        || #def{ctx=Key,name_range=Range}<-Syms].

highlight(L) when is_list(L) ->
    [highlight(X) || X<-L];
highlight(#ref{range=Range}) ->
    #{range=>range(Range), kind=>2};
highlight(#def{name_range=Range}) ->
    #{range=>range(Range), kind=>3}.

