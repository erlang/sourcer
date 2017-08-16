%%%

-module(sourcer_doc_server).

-export([
		 get_raw_documentation/3,
		 get_documentation/3
		]).

-type root() ::
		  {'lib', string()} |
		  {'app', string()}.
-type configuration() ::
		  #{
			'roots' => [root()]
		   }.

-type doc_ref() ::
		  {'application', atom()} |
		  {'module', atom()} |
		  {'function', {atom(), atom(), integer()}} |
		  {'macro', {atom(), atom(), integer()}} |
		  {'record', {atom(), atom(), integer()}} |
		  {'behaviour', atom()} |
		  {'type', {atom(), atom(), integer()}}.

-type doc_tree() :: any().

-type doc_result() :: {'ok', [{atom(), doc_tree()}]} | {'error', any()}.
-type raw_doc_result() :: {'ok', [{atom(), iolist()}]} | {'error', any()}.

-type provider() :: fun((doc_ref()) -> {'ok', {atom(), iolist()}} | {'error', any()}).

-spec get_documentation(configuration(), doc_ref(), [provider()]) -> doc_result().
get_documentation(Config, Ref, Providers) ->
	RawDocs = get_raw_documentation(Config, Ref, Providers),
	convert(Config, Providers, RawDocs).

-spec get_raw_documentation(configuration(), doc_ref(), [provider()]) -> raw_doc_result().
get_raw_documentation(Config, Ref, Providers) ->
	traverse(Config, Providers, Ref).

%% ====================================================================
%% Internal functions
%% ====================================================================

traverse(Config, L, Args) ->
	traverse(Config, L, Args, [], []).

traverse(_Config, [], _Args, Result, []) ->
	{ok, lists:reverse(Result)};
traverse(_Config, [], _Args, Result, Errs) ->
	{error, {lists:reverse(Errs), lists:reverse(Result)}};
traverse(Config, [M|T], Args, Result, Err) ->
	case catch apply(M, Args) of
		{error, E} ->
			traverse(Config, T, Args, Result, [E|Err]);
		{'EXIT', E} ->
			traverse(Config, T, Args, Result, [E|Err]);
		{ok, V} ->
			traverse(Config, T, Args, [V|Result], Err);
		V ->
			traverse(Config, T, Args, [V|Result], Err)
	end.

convert(Config, Providers, {ok, Docs}) ->
	{ok, convert1(Config, Providers, Docs, [])};
convert(Config, Providers, {error, Errors, Docs}) ->
	{error, Errors, convert1(Config, Providers, Docs, [])}.

convert1(_Config, _Providers, [], Result) ->
	lists:reverse(Result);
convert1(Config, Providers, [{M, D}|T], Result) ->
	case lists:keyfind(M, 1, Providers) of
		{M, P} ->
			V = P:convert(D),
			convert1(Config, Providers, T, [V|Result]);
		false ->
			convert1(Config, Providers, T, Result)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

traverse_test_() ->
	[
	 ?_assertEqual({ok, [[1,2]]},traverse(#{}, [fun lists:seq/2], [1,2])),
	 ?_assertEqual({ok, []},traverse(#{}, [], []))
	].
