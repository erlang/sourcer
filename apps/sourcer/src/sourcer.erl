-module(sourcer).

-export([
        'initialize'/1,
        'initialized'/2,

        'workspace/didChangeConfiguration'/2,
        'workspace/didChangeWatchedFiles'/2,
        'workspace/didChangeWorkspaceFolders'/2,
        'workspace/symbol'/3,
        %'workspace/executeCommand'/3,

        'textDocument/didChange'/3,
        'textDocument/didOpen'/2,
        'textDocument/didClose'/2,
        'textDocument/willSave'/2,
        'textDocument/willSaveWaitUntil'/3,
        'textDocument/didSave'/2,

        'textDocument/completion'/3,
        'completionItem/resolve'/3,
        'textDocument/hover'/3,
        %'textDocument/signatureHelp'/3,
        'textDocument/references'/3,
        'textDocument/documentHighlight'/3,
        'textDocument/documentSymbol'/3,
        'textDocument/formatting'/3,
        'textDocument/rangeFormatting'/3,
        %'textDocument/onTypeFormatting'/3,
        'textDocument/definition'/3
        %'textDocument/codeAction'/3,
        %'textDocument/codeLens'/3,
        %'codeLens/resolve'/3,
        %'textDocument/documentLink'/3,
        %'documentLink/resolve'/3,
        %'textDocument/rename'/3,
]).

%%-define(DEBUG, true).
-include("debug.hrl").

-include("sourcer_model.hrl").

-record(state, {
        client_capabilities = #{},
        server_capabilities = #{},
        configuration = #{},
        watched_files = [],
        workspace = [],
        db = sourcer_db:new() :: db(),
        initialized = false
        }).

-record(small_state, {
        client_capabilities = #{},
        server_capabilities = #{},
        configuration = #{},
        open_file
        }).

-define(OPEN_PAREN, <<"(">>).
-define(CLOSE_PAREN, <<")">>).
-define(OPEN_CURLY, <<"{">>).
-define(CLOSE_CURLY, <<"}">>).

'initialize'(InitializeParams) ->
    %% TODO: should use internal format, to be converted by LSP layer
    Capabilities = #{
        textDocumentSync => lsp_data:get_value(sync, full),
        hoverProvider => true,
        completionProvider => #{
            resolveProvider => true,
            triggerCharacters => [<<":">>, <<"?">>, <<"#">>]
            },
        definitionProvider => true,
        referencesProvider => true,
        documentHighlightProvider => true,
        documentSymbolProvider => true,
        workspaceSymbolProvider => true,
        documentFormattingProvider => true,
        documentRangeFormattingProvider => true,
        %signatureHelpProvider => #{
        %    triggerCharacters => [?OPEN_PAREN]
        %    },
        %codeActionProvider => true,
        %codeLensProvider => #{
        %    resolveProvider => true
        %    },
        %documentOnTypeFormattingProvider => #{
        %    firstTriggerCharacter => ?CLOSE_CURLY,
        %    moreTriggerCharacters => [?CLOSE_CURLY,<<";">>,<<".">>]
        %    },
        %renameProvider => true,
        %documentLinkProvider => #{resolveProvider => false},
        %executeCommandProvider => #{commands => [<<"demoz">>]},
        experimental => [
                        ],
        workspace => #{workspaceFolders => #{
                        supported => true,
                        changeNotifications => true}
                    }
        },
    Server = #{capabilities => Capabilities},
    {Server, #state{client_capabilities=InitializeParams, server_capabilities=Server}}.

'initialized'(State, #{}) ->
    R1 = maps:get(rootUri, State#state.client_capabilities),
    R2 = sets:from_list([maps:get(uri, M) || 
            M <- maps:get(workspaceFolders, State#state.client_capabilities)]),
    Roots = sets:add_element(R1, R2),
    Wspace = sourcer_layout:detect_layouts(sets:to_list(Roots)),

    %% TODO make async
    DB = State#state.db,
    AllFiles = get_all_files(Wspace),
    NewDB = sourcer_db:add_files(AllFiles, DB),

    State#state{workspace=Wspace, db = NewDB, initialized=true}.

'workspace/didChangeConfiguration'(State, #{settings:=Settings}) ->
    ErlSettings = maps:get(erlang, Settings),
    Runtimes = [{K,V} || #{name:=K, path:=V} <- maps:get(runtimes, ErlSettings)],
    Runtime = maps:get(runtime, ErlSettings),
    {_ , RuntimePath} = lists:keyfind(Runtime, 1, Runtimes),
    ?D(Runtimes),
    ?D(Runtime),
    %% TODO error checking
    %?D(Runtimes),
    %?D(Runtime),
    %?D(RuntimePath),
    %% TODO: recompute code path (for db)
    %% TODO: start reparsing & processing
    %% TODO: start compile
    State#state{configuration=Settings}.

'workspace/didChangeWatchedFiles'(State, Changes) ->
    DB = State#state.db,
    NewDB = sourcer_db:process_watched(DB, Changes),
    State#state{db=NewDB}.

'workspace/didChangeWorkspaceFolders'(State, _Changes) ->
    ?D("!!!!!!!!!!!!!!!!!!!! workspaces ~p", [_Changes]),
    State.

'textDocument/didOpen'(State, #{textDocument:=#{uri:=Uri, text:=Text}}) ->
    ?D("OPEN::~p~n", [Uri]),
    DB = State#state.db,
    NewDB = sourcer_db:open_file(DB, Uri, Text),
    State#state{db=NewDB}.

%% TODO: this is for full sync, handle incremental changes too
'textDocument/didChange'(State, #{textDocument:=#{uri:=Uri}}, Changes) ->
    ?D("CHANGE::~p -- ~p~n", [Uri, Changes]),
    NewOpen = sourcer_db:update_file(State#state.db, Uri, Changes),
    State#state{db=NewOpen}.

'textDocument/didSave'(State, #{textDocument:=#{uri:=Uri}}) ->
    ?D("SAVE::~p~n", [Uri]),
    State.

'textDocument/willSave'(State, #{textDocument:=#{uri:=_Uri}}) ->
    State.

'textDocument/willSaveWaitUntil'(State, #{textDocument:=#{uri:=_Uri}}, Reporter) ->
    Reporter([]),
    State.

'textDocument/didClose'(State, #{textDocument:=#{uri:=Uri}}) ->
    DB = State#state.db,
    NewOpen = sourcer_db:close_file(DB, Uri),
    State#state{db=NewOpen}.

'workspace/symbol'(State, #{query:=Query}, Reporter) ->
    DB = State#state.db,
    Syms = sourcer_operations:symbols(Query, DB),
    ?D(Syms),
    %% TODO fix me
    Res = [sourcer_lsp:symbol_information(Uri, Def) 
            || {Uri, Def} <- Syms
        ],
    %?D(Res),
    Reporter({value, Res}).

%%'workspace/executeCommand'(_State, #{command:=Cmd}, Reporter) ->
%%    ?D("EXECUTE: ~p~n", [Cmd]),
%%    Res = [],
%%    Reporter({value, Res}).

'textDocument/completion'(State, #{textDocument:=#{uri:=Uri}, position:=Position}, Reporter) ->
    DB = State#state.db,
    {Items, Complete} = sourcer_operations:completion(Uri, Position, DB),
    Res = sourcer_lsp:completion_list(Complete, Items),
    Reporter({value, Res}).

'completionItem/resolve'(State, Item, Reporter) ->
    DB = State#state.db,
    Item2 = sourcer_operations:resolve_completion(Item, DB),
    Res = sourcer_lsp:completion_item(Item2),
    Reporter({value, Res}).

'textDocument/hover'(State, #{textDocument:=#{uri:=Uri}, position:=Position}, Reporter) ->
    DB = State#state.db,
    Hover = sourcer_operations:hover(Uri, Position, DB),
    Res = sourcer_lsp:hover(Hover),
    Reporter({value, Res}).

'textDocument/references'(State, #{textDocument:=#{uri:=Uri}, position:=Position, context:=Context}, Reporter) ->
    DB = State#state.db,
    Refs = sourcer_operations:references(Uri, Position, Context, DB),
    ?D({myrefs, Refs}),
    Res = sourcer_lsp:references(Refs),
    Reporter({value, Res}).

'textDocument/documentHighlight'(State, #{textDocument:=#{uri:=Uri}, position:=Position}, Reporter) ->
    DB = State#state.db,
    Highlight = sourcer_operations:highlight(Uri, Position, DB),
    Res = sourcer_lsp:highlight(Highlight),
    Reporter({value, Res}).

'textDocument/documentSymbol'(State, #{textDocument:=#{uri:=Uri}}, Reporter) ->
    DB = State#state.db,
    Defs = sourcer_operations:document_symbols(Uri, DB),
    Res = sourcer_lsp:symbols(Uri, Defs),
    Reporter({value, Res}).

'textDocument/definition'(State, #{textDocument:=#{uri:=Uri}, position:=Position}, Reporter) ->
    DB = State#state.db,
    Defs = sourcer_operations:definition(Uri, Position, DB),
    ?D(Defs),
    Res = sourcer_lsp:definition(Defs),
    Reporter({value, Res}).

%%'textDocument/signatureHelp'(State, _Args, Reporter) ->
%%    DB = State#state.db,
%%    Res = #{
%%      signatures => [],
%%      activeSignature => null,
%%      activeParameter => null
%%      },
%%    Reporter({value, Res}).

%%'textDocument/rename'(_State, _Args, Reporter) ->
%%    Res = #{changes => []},
%%    Reporter({value, Res}).

'textDocument/formatting'(State, #{textDocument:=#{uri:=Uri}, options:=Options}, Reporter) ->
    #{tabSize:=TabSize, insertSpaces:=InsertSpaces} = Options,
    Text = sourcer_db:get_text(Uri, State#state.db),
    NewText = sourcer_indent:all(unicode:characters_to_list(Text),
                                 [{tab_len,TabSize},
                                  {use_tabs,not InsertSpaces}]),
    Res = [sourcer_lsp:text_edit(NewText, {{0,1},{9991, 1}})],
    Reporter({value, Res}).

'textDocument/rangeFormatting'(_State, _Args, Reporter) ->
    Res = [],
    Reporter({value, Res}).

%%'textDocument/onTypeFormatting'(_State, _Args, Reporter) ->
%%    Res = [],
%%    Reporter({value, Res}).

%%'textDocument/codeAction'(_State, _Args, Reporter) ->
%%    Res = [],
%%    Reporter({value, Res}).

%%'textDocument/codeLens'(_State, _Args, Reporter) ->
%%    Res = [],
%%    Reporter({value, Res}).

%%'codeLens/resolve'(_State, _Args, Reporter) ->
%%    Res = #{},
%%    Reporter({value, Res}).

%%'textDocument/documentLink'(_State, _Args, Reporter) ->
%%    Res = [],
%%    Reporter({value, Res}).

%%'documentLink/resolve'(_State, _Args, Reporter) ->
%%    Res = #{},
%%    Reporter({value, Res}).

%%%%%%%%%%%%%%%%%

get_all_files(#project{location=Loc, sources=Srcs, includes=Incs}) ->
    Root = sourcer_util:uri_to_path(Loc),
    L1 = [sourcer_util:path_to_uri(filename:join(Root, X)) 
            || X <- filelib:wildcard("rebar.config", Root)
        ],
    L2 = [
            [sourcer_util:path_to_uri(filename:join([Root, D, X])) 
                || X <- filelib:wildcard("*.{erl,hrl}", filename:join(Root, D))
            ] 
            || D <- Srcs++Incs
        ],
    %% TODO app.src
    L1++L2;
get_all_files(Wspace) when is_list(Wspace) ->
    lists:flatten([get_all_files(Prj) || Prj<-Wspace]).

