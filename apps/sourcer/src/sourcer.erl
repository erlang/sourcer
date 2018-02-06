-module(sourcer).

-export([
        'initialize'/1,
        'initialized'/2,

        'workspace/didChangeConfiguration'/2,
        'workspace/didChangeWatchedFiles'/2,
        'workspace/didChangeWorkspaceFolders'/2,
        'workspace/symbol'/3,
        'workspace/executeCommand'/3,

        'textDocument/didChange'/3,
        'textDocument/didOpen'/2,
        'textDocument/didClose'/2,
        'textDocument/willSave'/2,
        'textDocument/willSaveWaitUntil'/3,
        'textDocument/didSave'/2,

        'textDocument/completion'/3,
        'completionItem/resolve'/3,
        'textDocument/hover'/3,
        'textDocument/signatureHelp'/3,
        'textDocument/references'/3,
        'textDocument/documentHighlight'/3,
        'textDocument/documentSymbol'/3,
        'textDocument/formatting'/3,
        'textDocument/rangeFormatting'/3,
        'textDocument/onTypeFormatting'/3,
        'textDocument/definition'/3,
        'textDocument/codeAction'/3,
        'textDocument/codeLens'/3,
        'codeLens/resolve'/3,
        'textDocument/documentLink'/3,
        'documentLink/resolve'/3,
        'textDocument/rename'/3,

        default_answer/1
]).

-record(state, {
        client_capabilities = #{},
        server_capabilities = #{},
        configuration = #{},
        watched_files = [],
        open_files = [],
        initialized = false
        }).

-record(small_state, {
        client_capabilities = #{},
        server_capabilities = #{},
        configuration = #{},
        open_file
        }).

-define(DEBUG, true).

-ifdef(DEBUG).
-define(DEBUG(F, A), io:format(F, A)).
-else.
-define(DEBUG(F, A), ok).
-endif.

-include("sourcer_db.hrl").
-include("debug.hrl").

-define(OPEN_PAREN, <<"(">>).
-define(CLOSE_PAREN, <<")">>).
-define(OPEN_CURLY, <<"{">>).
-define(CLOSE_CURLY, <<"}">>).

'initialize'(InitializeParams) ->
    %% TODO: should use internal format, to be converted by LSP layer
    Capabilities = #{
        textDocumentSync => sourcer_documents:get_sync_value(),
        hoverProvider => true,
        completionProvider => #{
            resolveProvider => true,
            triggerCharacters => [<<":">>, <<"?">>, <<"#">>]
            },
        signatureHelpProvider => #{
            triggerCharacters => [?OPEN_PAREN]
            },
        definitionProvider => true,
        referencesProvider => true,
        documentHighlightProvider => true,
        documentSymbolProvider => true,
        workspaceSymbolProvider => true,
        codeActionProvider => true,
        codeLensProvider => #{
            resolveProvider => true
            },
        documentFormattingProvider => true,
        documentRangeFormattingProvider => true,
        documentOnTypeFormattingProvider => #{
            firstTriggerCharacter => ?CLOSE_CURLY,
            moreTriggerCharacters => [?CLOSE_CURLY,<<";">>,<<".">>]
            },
        renameProvider => true,
        documentLinkProvider => #{resolveProvider => false},
        executeCommandProvider => #{commands => []},
        experimental => [],
        workspace => #{workspaceFolders => #{
                        supported => true,
                        changeNotifications => true}
                    }
        },
    Server = #{capabilities => Capabilities},
    {Server, #state{client_capabilities=InitializeParams, server_capabilities=Server}}.

'initialized'(State, #{}) ->
    State#state{initialized=true}.

'workspace/didChangeConfiguration'(State, #{settings:=Settings}) ->
    %lsp_client:workspaceFolders(),
    %% TODO: recompute code path (for db)
    %% TODO: start reparsing & processing
    %% TODO: start compile
    State#state{configuration=Settings}.

'workspace/didChangeWatchedFiles'(State, _Changes) ->
    Watched = State#state.watched_files,
    NewWatched = Watched,
    % TODO: lists:foldl(fun sourcer_documents:process_watched/2, [], Watched),
    %% TODO: start compile
    State#state{watched_files=NewWatched}.

'workspace/didChangeWorkspaceFolders'(State, _Changes) ->
    ?DEBUG("!!!!!!!!!!!!!!!!!!!! workspaces ~p", [_Changes]),
    State.

'textDocument/didOpen'(State, #{textDocument:=Item}) ->
    ?DEBUG("OPEN::~p~n", [Item]),
    #{uri:=URI, text:=Text}=Item,
    Open = State#state.open_files,
    NewOpen = sourcer_documents:open_file(State#state.open_files, URI, Text),
    State#state{open_files=NewOpen}.

%% TODO: this is for full sync, handle incremental changes too
'textDocument/didChange'(State, #{textDocument:=Item}, Changes) ->
    ?DEBUG("CHANGE::~p -- ~p~n", [Item, Changes]),
    #{uri:=URI} = Item,
    %% TODO: start parsing & processing
    NewOpen = sourcer_documents:update_file(State#state.open_files, URI, Changes),
    %% TODO: start compile
    State#state{open_files=NewOpen}.

'textDocument/didSave'(State, #{textDocument:=#{uri:=_URI}}) ->
    State.

'textDocument/willSave'(State, #{textDocument:=#{uri:=_URI}}) ->
    State.

'textDocument/willSaveWaitUntil'(State,  #{textDocument:=#{uri:=_URI}}, Reporter) ->
    Reporter([]),
    State.

'textDocument/didClose'(State,  #{textDocument:=#{uri:=URI}}) ->
    Open = State#state.open_files,
    NewOpen = lists:keydelete(URI, 1, Open),
    State#state{open_files=NewOpen}.

'workspace/symbol'(_State, _Query, Reporter) ->
    %% symbol = #{name, kind, location, containerName?}}
    Res = [],
    Reporter({value, Res}).

'workspace/executeCommand'(_State, _Query, Reporter) ->
    %% symbol = #{name, kind, location, containerName?}}
    Res = [],
    Reporter({value, Res}).

%% completion_item() :: label, kind?, detail?, documentation?, sortText?, filterText?,
%% insertText?, textEdit? additionalTextEdits?, command? data?

'textDocument/completion'(_State, #{textDocument:=#{uri:=URI}, position:=Position}, Reporter) ->
    Res = #{
      isIncomplete => false,
      items => []
     },
    Reporter({value, Res}).

'completionItem/resolve'(_State, Item, Reporter) ->
    Res = Item,
    Reporter({value, Res}).

'textDocument/hover'(State, #{textDocument:=#{uri:=URI}, position:=Position}, Reporter) ->
    Source = sourcer_documents:get_element(State#state.open_files, URI, Position),
    %% [markedstring()]:: String (=markdown)
    Res = #{
      contents => []
     %%, range => lsp_utils:range(_Position, _Position)
     },
    Reporter({value, Res}).

'textDocument/references'(State, #{textDocument:=#{uri:=URI}, position:=Position, context:=Context}, Reporter) ->
    Source = sourcer_documents:get_element(State#state.open_files, URI, Position),
    {Refs, _} = sourcer_documents:get_model(State#state.open_files, URI),
    ?DEBUG("SOURCE=~p~nRREEFF: ~p~n", [Source, Refs]),
    Res = convert_refs(Refs, URI),
    Reporter({value, Res}).

'textDocument/documentHighlight'(_State, _Args, Reporter) ->
    Res = [],
    Reporter({value, Res}).

'textDocument/documentSymbol'(State, #{textDocument:=#{uri:=URI}}, Reporter) ->
    ?D(State),
    XX = sourcer_documents:get_model(State#state.open_files, URI),
    ?DEBUG("SYM URI=~p~nSTATE=~p~n", [URI, XX]),
    {Refs, _} = XX,
    Res = convert_refs(Refs, URI),
    Reporter({value, Res}).

'textDocument/definition'(State, #{textDocument:=#{uri:=URI}, position:=Position}, Reporter) ->
    Source = sourcer_documents:get_element(State#state.open_files, URI, Position),
    ?D(Source),
    {Model, _} = sourcer_documents:get_model(State#state.open_files, URI),
    D = find_def(Source, Model#model.defs),
    Res = #{uri=>URI, range=>range(element(2, D))},
    Reporter({value, Res}).

'textDocument/signatureHelp'(_State, _Args, Reporter) ->
    Res = #{
      signatures => [],
      activeSignature => null,
      activeParameter => null
      },
    Reporter({value, Res}).

'textDocument/rename'(_State, _Args, Reporter) ->
    %% #{URI: [edits]}
    Res = #{changes => []},
    Reporter({value, Res}).

'textDocument/formatting'(State, #{textDocument:=#{uri:=URI}, options:=Options}, Reporter) ->
    #{tabSize:=TabSize, insertSpaces:=InsertSpaces} = Options,
    Text = sourcer_documents:get_text(State#state.open_files, URI),
    NewText = sourcer_indent:lines(unicode:characters_to_list(Text)),
    Res = [#{range=>range({{0,1},{9991, 1}}),
            newText=>unicode:characters_to_binary(NewText)}],
    Reporter({value, Res}).

'textDocument/rangeFormatting'(_State, _Args, Reporter) ->
    Res = [],
    Reporter({value, Res}).

'textDocument/onTypeFormatting'(_State, _Args, Reporter) ->
    Res = [],
    Reporter({value, Res}).

'textDocument/codeAction'(_State, _Args, Reporter) ->
    Res = [],
    Reporter({value, Res}).

'textDocument/codeLens'(_State, _Args, Reporter) ->
    Res = [],
    Reporter({value, Res}).

'codeLens/resolve'(_State, _Args, Reporter) ->
    Res = #{},
    Reporter({value, Res}).

'textDocument/documentLink'(_State, _Args, Reporter) ->
    Res = [],
    Reporter({value, Res}).

'documentLink/resolve'(_State, _Args, Reporter) ->
    Res = #{},
    Reporter({value, Res}).

%%%%%%%%%%%%%%%%%

find_def(Src, L) ->
    hd(L).

default_answer(completion) ->
    null;
default_answer(completion_resolve) ->
    null;
default_answer(hover) ->
    null;
default_answer(signature_help) ->
    null;
default_answer(_) ->
    [].

range({{L1,C1},{L2,C2}}) ->
    ?D({{L1,C1},{L2,C2}}),
    #{
        start=>#{line=>L1, character=>C1-1},
        'end'=>#{line=>L2, character=>C2-1}
    }.

convert_refs(Model, URI) ->
    #model{refs=Refs, defs=Defs} = Model,
    ?DEBUG("REFS===~p~n----~n", [Refs]),
    [
        begin
            #{
            name=>print_name(Key),
            kind=>kind(element(1, Key)),
            location=>#{
                uri=>URI,
                range=>range(Pos)
                }
            }
        end ||
        {Key,Pos} <- Refs++Defs].

print_name(Data) ->
    case Data of
        {function, _, F, A} ->
            iolist_to_binary(io_lib:format("~s/~w", [F, A]));
        {macro, _, M, A} ->
            case A of
                -1 ->
                    iolist_to_binary(io_lib:format("?~s", [M]));
                _ ->
                    iolist_to_binary(io_lib:format("?~s/~w", [M, A]))
            end;
        {var, _, N} ->
            iolist_to_binary(io_lib:format("~s", [N]));
        _ ->
            iolist_to_binary(io_lib:format("~p", [Data]))
    end.

kind(E) ->
    case lists:keyfind(E, 1, lsp_data:get_data(symbol)) of
        false ->
            2;
        {_, N} ->
            N
    end.

encode_file_changes(Changes) ->
    [encode_file_change(X) || X<-Changes].

encode_file_change(#{type:=1}=Change) ->
    Change#{type=>created};
encode_file_change(#{type:=2}=Change) ->
    Change#{type=>changed};
encode_file_change(#{type:=3}=Change) ->
    Change#{type=>deleted}.

