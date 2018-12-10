%%% @author vlad
%%% @doc Handle LSP messages by dispatching (asynchronously)
%%% to a provided handler module.

-module(lsp_server).

-behaviour(gen_server).

-export([
		 start_link/1
		]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link(Mod) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Mod], []).

%%%%%%%%%%%%%%%%%%%%%

-record(state, {
				stopped = true,
				pending_requests = [],
				user_module = sourcer,
				user_state
				%% TODO: user_state can be big, do we want to keep send it back and forth
				%% for each request?? If we want async workers, it can't be completely
				%% avoided, I think
				%% -->> workers should get only parts of the state that are relevant for
				%% the operation, for example only the current document data
				%% (unless it's a worskpace operation)
			   }).

%%-define(DEBUG, true).

-ifdef(DEBUG).
-define(DEBUG(F, A), io:format(F, A)).
-else.
-define(DEBUG(F, A), ok).
-endif.

init([Mod]) ->
	State = #state{
			user_module = Mod
		},
	{ok, State}.

handle_call({'initialize', Id, Params},
		_From, State=#state{user_module = Mod}) ->
	?DEBUG("REQ ~p: ~p:: ~p~n", [Id, 'initialize', Params]),
	{Reply, NewUserState} = Mod:initialize(Params),
	reply(Id, Reply),
	{reply, error, State#state{user_state=NewUserState, stopped=false}};
handle_call({_Method, Id, _Params},
		_From, State=#state{user_module = Mod, stopped=true}) ->
	reply(Id, Mod:error(server_not_initialized, "Server was stopped")),
	{reply, error, State};
handle_call({Method, Id, Params},
		_From, State=#state{user_module = Mod, pending_requests=Reqs}) ->
	?DEBUG("REQ ~p: ~p:: ~p~n", [Id, Method, Params]),
	Pid = start_worker(Id, Method, Params, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_requests=NewReqs}};
handle_call(Request, _From, State) ->
	?DEBUG("Unrecognized request: ~p~n", [Request]),
	Reply = {error, {unknown, Request}},
	{reply, Reply, State}.

handle_cast({'exit', _}, State) ->
	?DEBUG("NTF: ~p::~n", ['exit']),
	{stop, State};
handle_cast({Method, Params}, State=#state{stopped=true}) ->
	?DEBUG("NTF: ignored ~p:: ~p~n", [Method, Params]),
	{noreply, State};
handle_cast({'$/cancelRequest', #{id := Id}}, State) ->
	?DEBUG("NTF: ~p:: ~p~n", ['$/cancelRequest', Id]),
	NewState = cancel_worker(Id, State),
	{noreply, NewState};
handle_cast({Method, Params}, State=#state{user_module=Mod}) ->
	?DEBUG("NTF: ~p:: ~p~n", [Method, Params]),
	%% run in-process to keep the ordering of received messages
	Exps = Mod:module_info(exports),
	NewState = case lists:member({Method, 2}, Exps) of
		true ->
			try
				Mod:Method(State#state.user_state, Params)
			catch _:E ->
				?DEBUG("####################~nERROR: ~p~n(~p:~p ~p) ~n-- ~p~n", [E, Mod, Method, Params, erlang:get_stacktrace()]),
				State#state.user_state
			end;
		false ->
			io:format("Unsupported notification: ~p~n", [Method]),
			State#state.user_state
	end,
	{noreply, State#state{user_state=NewState}};
handle_cast(Other, State) ->
	io:format("Unrecognized notification: ~p~n", [Other]),
	{noreply, State}.

handle_info(_Info, State) ->
	io:format("Unrecognized message: ~p~n", [_Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

reply(Id, Answer) ->
	?DEBUG("ANS ~p:: ~p~n", [Id, Answer]),
	jsonrpc ! {reply, Id, Answer},
	ok.

cancel_worker(Id, #state{pending_requests=Reqs}=State) ->
	case lists:keytake(Id, 1, Reqs) of
		{value, {Id, Pid}, NewReqs} ->
			cancellable_worker:cancel(Pid),
			State#state{pending_requests=NewReqs};
		false ->
			State
	end.

start_worker(Id, Method, Params, State) ->
	UserState = State#state.user_state,
	Mod = State#state.user_module,
	Work = fun(Reporter) ->
				Exps = Mod:module_info(exports),
				case lists:member({Method, 3}, Exps) of
					true ->
						try
							sourcer:Method(UserState, Params, Reporter)
						catch _:E ->
							?DEBUG("####################~nERROR: ~p~n(~p:~p ~p) ~n-- ~p~n", [E, Mod, Method, Params, erlang:get_stacktrace()]),
							% Mod:default_answer(Method)
							null
						end;
					false ->
						io:format("Unsupported request: ~p~n", [Method])
						%% ??
				end
		end,
	Replier = fun({_, nothing}) ->
				Answer = null, %Mod:default_answer(Method),
				reply(Id, Answer);
			({_, Answer}) ->
				reply(Id, Answer)
		end,
	{ok, Pid} = cancellable_worker:start(Id, Work, Replier),
	Pid.

