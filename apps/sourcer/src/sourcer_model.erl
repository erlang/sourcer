-module(sourcer_model).

-export([
		 init/2,
		 change_configuration/2,
		 open_document/5,
		 change_document/4,
		 save_document/2,
		 close_document/2,
		 change_watched_files/2
		]).

-type path_item() :: {'lib'|'app'|'file', string()}.

-type state() :: #{
				   'workspace' => string(),
				   'open_documents' => [any()],
				   'code_path' => [path_item()],
				   'logger' => atom(),
				   'files' => [{string(), boolean()}],
				   'parser' => fun((string()) -> any())
				  }.

%-define(LOG(X), State#state.logger:log(X)).
-define(LOG(X), io:format("~p~n", [X])).

-spec init(string(), atom()) -> {'ok', state()} | {'error', any()}.
init(Workspace, Logger) ->
	State = #{
			  files => [],
			  parser => fun parse_file/1
			 },
	change_configuration(State, [
								 {workspace, Workspace},
								 {logger, Logger}
								]).

-spec change_configuration(state(), [{atom(), any()}]) ->
		  {'ok', state()} | {'error', any()}.
change_configuration(State, Settings) ->
	NewState = maps:merge(State, maps:from_list(Settings)),
	?LOG({change_configuration, State, NewState}),
	{ok, NewState}.

-spec open_document(state(), string(), atom(), integer(), iolist()) ->
		  {'ok', state()} | {'error', any()}.
open_document(State, _Uri, _LangId, _Version, _Text) ->
	NewState = State,
	?LOG({open_document, State, NewState}),
	{ok, NewState}.

-spec change_document(state(), sourcer_lsp_server:text_document_id(), integer(), [sourcer_lsp_server:content_change()]) ->
		  {'ok', state()} | {'error', any()}.
change_document(State, _DocumentId, _Version, _Changes) ->
	NewState = State,
	?LOG({change_document, State, NewState}),
	{ok, State}.

-spec close_document(state(), sourcer_lsp_server:text_document_id()) ->
		  {'ok', state()} | {'error', any()}.
close_document(State, _DocumentId) ->
	NewState = State,
	?LOG({close_document, State, NewState}),
	{ok, State}.

-spec save_document(state(), sourcer_lsp_server:text_document_id()) ->
		  {'ok', state()} | {'error', any()}.
save_document(State, _DocumentId) ->
	NewState = State,
	?LOG({save_document, State, NewState}),
	{ok, State}.

-spec change_watched_files(state(), [sourcer_lsp_server:file_event()]) ->
		  {'ok', state()} | {'error', any()}.
change_watched_files(State=#{files:=Files, parser:=Parser}, FileEvents) ->
	NewFiles = lists:foldl(fun update_file_change/2, Files, FileEvents),
	NewFiles1 = lists:map(parse(Parser), NewFiles),
	NewState = State#{files=>NewFiles1},
	?LOG({change_watched_files, State, NewState}),
	{ok, NewState}.

%%%%%%%%%%%%%%%%%%%

update_file_change({File, created}, Files) ->
	update_file_change({File, changed}, Files);
update_file_change({File, changed}, Files) ->
	NewFiles0 = lists:keydelete(File, 1, Files),
	NewFiles = [{File, true} | NewFiles0],
	NewFiles;
update_file_change({File, deleted}, Files) ->
	NewFiles = lists:keydelete(File, 1, Files),
	NewFiles.

parse(Parser) ->
	fun({File, false}) ->
			{File, false};
	   ({File, true}) ->
			spawn(fun()->Parser(File) end),
			{File, false}
	end.

parse_file(_File) ->
	ok.

