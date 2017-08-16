-module(sourcer).

-export([
        'initialize'/1,

        'workspace/didChangeConfiguration'/2,
        'workspace/didChangeWatchedFiles'/2,
        'workspace/symbol'/3,
        'workspace/executeCommand'/3,

        'textDocument/didChange'/3,
        'textDocument/didOpen'/2,
        'textDocument/didClose'/2,
        'textDocument/didSave'/2,
        'textDocument/willSave'/2,
        'textDocument/willSaveWaitUntil'/3,

        'textDocument/completion'/3,
        'completionItem/resolve'/3,
        'textDocument/hover'/3,
        'textDocument/references'/3,
        'textDocument/documentHighlight'/3,
        'textDocument/documentSymbol'/3,
        %'textDocument/formatting'/3,
        %'textDocument/rangeFormatting'/3,
        %'textDocument/onTypeFormatting'/3,
        'textDocument/definition'/3,
        'textDocument/signatureHelp'/3,
        %'textDocument/codeAction'/3,
        %'textDocument/codeLens'/3,
        %'codeLens/resolve'/3,
        'textDocument/rename'/3,
        %'textDocument/documentLink'/3,
        %'documentLink/resolve'/3,

        default_answer/1
]).

-record(state, {
        client_capabilities = #{},
        server_capabilities = #{},
        configuration = #{},
        watched_files = [],
        open_files = []
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
									triggerCharacters => [<<"(">>]
									},
			definitionProvider => true,
			referencesProvider => true,
			documentHighlightProvider => true,
			documentSymbolProvider => true,
			workspaceSymbolProvider => true,
			%codeActionProvider => true,
			%codeLensProvider => #{
			%					resolveProvider => true
			%					},
			%documentFormattingProvider => true,
			%documentRangeFormattingProvider => true,
			%documentOnTypeFormattingProvider => #{
			%									firstTriggerCharacter => <<"}">>,
			%									moreTriggerCharacters => [<<"}">>,<<";">>,<<".">>]
			%									},
			renameProvider => true
		},
	Server = #{capabilities => Capabilities},
	{Server, #state{client_capabilities=InitializeParams, server_capabilities=Server}}.

'workspace/didChangeConfiguration'(State, Settings) ->
	%#{erlang:=ErlSettings} = Settings,
	%% io:format("cfg: ~p~n", [ErlSettings]),
	%% TODO: start reparsing & processing 
	%% TODO: start compile
	State#state{configuration=Settings}.

'workspace/didChangeWatchedFiles'(State, _Changes) ->
	Watched = State#state.watched_files,
	NewWatched = Watched, % TODO: lists:foldl(fun sourcer_documents:process_watched/2, [], Watched),
	%% TODO: start compile
	State#state{watched_files=NewWatched}.

'textDocument/didOpen'(State, #{textDocument:=Item}) ->
	?DEBUG("OPEN::~p~n", [Item]),
	#{uri:=URI, text:=Text}=Item,
	Open = State#state.open_files,
	NewOpen = [{URI, Text, sourcer_documents:process_file(URI, Text)}|Open],
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

'textDocument/completion'(_State, _, Reporter) ->
	Res = #{
	  isIncomplete => false,
	  items => []
	 },
	Reporter({value, Res}).

'completionItem/resolve'(_State, Item, Reporter) ->
	Res = Item,
	Reporter({value, Res}).

'textDocument/hover'(_State, _, Reporter) ->
	%% [markedstring()]:: String (=markdown)
	Res = #{
	  contents => []
	 %%, range => lsp_utils:range(_Position, _Position)
	 },
	Reporter({value, Res}).

'textDocument/references'(State, #{textDocument:=#{uri:=URI}, position:=Position, context:=Context}, Reporter) ->
	Source = sourcer_documents:get_element(State#state.open_files, URI, Position),
	{_, Refs} = sourcer_documents:get_refs(State#state.open_files, URI),
	?DEBUG("RREEFF: ~p~n", [Source]),
	Res = [],
	Reporter({value, Res}).

'textDocument/documentHighlight'(_State, _Args, Reporter) ->
	Res = [],
	Reporter({value, Res}).

'textDocument/documentSymbol'(State, URI, Reporter) ->
	?DEBUG("STATE=~p~n", [State#state.open_files]),
	{_, Refs} = sourcer_documents:get_refs(State#state.open_files, URI),
	Res = convert_refs(Refs, URI),
	Reporter({value, Res}).

'textDocument/definition'(_State, _Args, Reporter) ->
	Res = [],
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

%%%%%%%%%%%%%%%%%

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

convert_refs(Refs, URI) ->
	%%io:format("REFS===~p~n----~n", [Refs]),
	[
		begin
			#{
			name=>print_name(Data), 
			kind=>3, 
			location=>#{
				uri=>URI, 
				range=>#{
					start=>#{line=>1, character=>0}, 
					'end'=>#{line=>1, character=>1}
					}
				}
			}
		end || 
	
	%{ref,{module_def,"test1"},0,15,module,-3,[],false}
	
	{ref,Data,Offset,Length,_Function,_Arity,_Clause,_SubClause} <- Refs].

print_name(Data) ->
	{Kind, Name, Key} = case Data of 
		{KK, NN} ->
			{KK, NN, ''};
		E ->
			E
	end,
	case  Key of
		'' ->	
			iolist_to_binary(io_lib:format("~s", [Name]));
		_ ->
			iolist_to_binary(io_lib:format("~s:~w", [Name, Key]))
	end.
	

encode_file_changes(Changes) ->
	[encode_file_change(X) || X<-Changes].

encode_file_change(#{type:=1}=Change) ->
	Change#{type=>created};
encode_file_change(#{type:=2}=Change) ->
	Change#{type=>changed};
encode_file_change(#{type:=3}=Change) ->
	Change#{type=>deleted}.
