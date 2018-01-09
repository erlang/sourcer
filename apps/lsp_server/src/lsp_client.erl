%% @author vlad
%% @doc Handle outgoing requests from server to client.

-module(lsp_client).

-behaviour(gen_server).

-export([
	start_link/0,

	show_message/2,
	show_message_request/3,
	log_message/2,
	telemetry_event/1,
	publish_diagnostics/2,
	register_capability/1,
	unregister_capability/1,
	apply_edit/1,

	workspaceFolders/0
]).

-export([
	init/1, 
	handle_call/3, 
	handle_cast/2, 
	handle_info/2, 
	terminate/2, 
	code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
		crt_id = 0,
		pending_requests = []
	}).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% client API

show_message(Type, Msg) ->
	gen_server:cast(?SERVER, {show_message, Type, Msg}).

show_message_request(Type, Msg, Actions) ->
	gen_server:call(?SERVER, {show_message_request, Type, Msg, Actions, self()}).

log_message(Type, Msg) ->
	gen_server:cast(?SERVER, {log_message, Type, Msg}).

telemetry_event(Msg) ->
	gen_server:cast(?SERVER, {telemetry_event, Msg}).

publish_diagnostics(URI, Diagnostics) ->
	gen_server:cast(?SERVER, {publish_diagnostics, URI, Diagnostics}).

register_capability(Args) ->
	gen_server:call(?SERVER, {register_capability, Args, self()}).

unregister_capability(Args) ->
	gen_server:call(?SERVER, {unregister_capability, Args, self()}).

apply_edit(Args) ->
	gen_server:call(?SERVER, {apply_edit, Args, self()}).

workspaceFolders() ->
	io:format("FOO\n"),
	gen_server:call(?SERVER, {workspaceFolders, self()}).

%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	process_flag(trap_exit, true),
	State = #state{
		},
	{ok, State}.

handle_call({show_message_request, Type, Msg, Actions, Pid}, _From,
			State = #state{pending_requests=Reqs}) ->
	Id = State#state.crt_id,
	NewState = State#state{
						    pending_requests = [{Id, Pid} | Reqs],
						    crt_id = Id + 1
						},
	jsonrpc ! {request, Id, 'window/showMessageRequest',
			 #{type => Type,
			   message => unicode:characters_to_binary(Msg),
			   actions => Actions}
			},
	%% TODO
	{reply, ok, NewState};
handle_call({register_capability, Args, Pid}, _From, State = #state{pending_requests=Reqs}) ->
	Id = State#state.crt_id,
	NewState = State#state{
						    pending_requests = [{Id, Pid} | Reqs],
						    crt_id = Id + 1
						},
	jsonrpc ! {request, 'client/register_capability', Args, Pid},
	%% TODO
	{reply, ok, NewState};
handle_call({unregister_capability, Args, Pid}, _From, State = #state{pending_requests=Reqs}) ->
	Id = State#state.crt_id,
	NewState = State#state{
						    pending_requests = [{Id, Pid} | Reqs],
						    crt_id = Id + 1
						},
	jsonrpc ! {request, 'client/unregister_capability', Args, Pid},
	%% TODO
	{reply, ok, NewState};
handle_call({apply_edit, Args, Pid}, _From, State = #state{pending_requests=Reqs}) ->
	Id = State#state.crt_id,
	NewState = State#state{
						    pending_requests = [{Id, Pid} | Reqs],
						    crt_id = Id + 1
						},
	jsonrpc ! {request, 'workspace/applyEdit', Args, Pid},
	%% TODO
	{reply, ok, NewState};
handle_call({workspaceFolders, Pid}, _From, State = #state{pending_requests=Reqs}) ->
	Id = State#state.crt_id,
	NewState = State#state{
						    pending_requests = [{Id, Pid} | Reqs],
						    crt_id = Id + 1
						},
	jsonrpc ! {request, 'workspace/workspaceFolders', Pid},
	%% TODO
	{reply, ok, NewState};
handle_call(Request, From, State) ->
	io:format("Unrecognized call from ~w: ~p~n", [From, Request]),
	Reply = {error, {unknown, Request}},
	{reply, Reply, State}.

handle_cast({'shutdown', _Id, _}, State) ->
	{noreply, State};
handle_cast({'exit', _}, State) ->
	{stop, State};
handle_cast({show_message, Type, Msg}, State) ->
	jsonrpc ! {notify, 'window/showMessage',
			 #{type => Type,
			   message => unicode:characters_to_binary(Msg)}},
	{noreply, State};
handle_cast({log_message, Type, Msg}, State) ->
	jsonrpc ! {notify, 'window/logMessage',
			 #{type => Type,
			   message => unicode:characters_to_binary(Msg)}},
	{noreply, State};
handle_cast({telemetry_event, Msg}, State) ->
	jsonrpc ! {notify, 'telemetry/event', Msg},
	{noreply, State};
handle_cast({publish_diagnostics, URI, Diagnostics}, State) ->
	jsonrpc ! {notify, 'textDocument/publishDiagnostics',
			 #{uri => URI,
			   diagnostics => Diagnostics}},
	{noreply, State};
handle_cast(Other, State) ->
	io:format("Unrecognized cast: ~p~n", [Other]),
	{noreply, State}.

handle_info({'$reply', Id, Msg}, State) ->
	case lists:keytake(Id, 1, State#state.pending_requests) of
		false ->
			{noreply, State};
		{value, {Id, Pid}, Rest} ->
			Pid ! Msg,
			{noreply, State#state{pending_requests=Rest}}
	end;
handle_info(Info, State) ->
	io:format("Unrecognized message: ~p~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

reply(Connection, Id, Answer) ->
	Connection ! {reply, Id, Answer}.

