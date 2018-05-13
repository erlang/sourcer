%%% TCP server listening for JSON RPC messages, handles encoding and decoding 
%%% to Erlang terms and dispatching to processes (server and client). 

-module(jsonrpc).

-export([
		start_link/3,
		start_link/4,
		send_notification/2,
		send_request/3,
		send_request/2,
		send_reply/2
	]).

-define(TRACE, true).
-define(DEBUG, true).

-ifdef(TRACE).
-define(TRACE(F, A), io:format(F, A)).
-else.
-define(TRACE(F, A), ok).
-endif.
-ifdef(DEBUG).
-define(DEBUG(F, A), io:format(F, A)).
-else.
-define(DEBUG(F, A), ok).
-endif.

start_link(Port, Server, Client) ->
	start_link(Port, Server, Client, []).

start_link(Port, Server, Client, Options) ->
	Pid = proc_lib:spawn_link(fun() ->
				start(Port, Server, Client, Options)
			end),
	register(?MODULE, Pid),
	{ok, Pid}.

send_notification(Method, Params) ->
	?MODULE ! {notify, Method, Params}.

send_request(Id, Method, Params) ->
	?MODULE ! {request, Id, Method, Params}.

send_request(Id, Method) ->
	?MODULE ! {request, Id, Method}.

send_reply(Id, Answer) ->
	?MODULE ! {reply, Id, Answer}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Port, Server, Client, Options) ->
	?TRACE("LSP: Start connection on port ~w~n", [Port]),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, true}]),
	case gen_tcp:accept(LSock) of
		{ok, Socket} ->
			?TRACE("LSP: Listening on: ~p~n", [Socket]),
			loop(Socket, Server, Client, Options, <<"">>, queue:new(), []);
		Err ->
			?TRACE("LSP: Connection error: ~p~n", [Err]),
		ok
	end.

loop(Socket, Server, Client, Options, Buf, Pending, Results) ->
	receive
		{notify, Method, Params} ->
			%?TRACE("Notify ~p", [Method]),
			notify(Socket, Method, Params),
			loop(Socket, Server, Client, Options, Buf, Pending, Results);
		{request, Id, Method, Params} ->
			%?TRACE("Request ~w ~p", [Id, Method]),
			request(Socket, Id, Method, Params),
			loop(Socket, Server, Client, Options, Buf, Pending, Results);
		{request, Id, Method} ->
			%?TRACE("Request ~w ~p", [Id, Method]),
			request(Socket, Id, Method),
			loop(Socket, Server, Client, Options, Buf, Pending, Results);
		{reply, Id, Answer} ->
			%?TRACE("Reply ~w ~p~n", [Id, Answer]),
			Results1 = [{Id, Answer} | Results],
			%?TRACE("Reply queue:: ~p~n", [Results1]),
			{NewPending, NewResults} = send_replies(Socket, Pending, Options, Results1),
			%?TRACE("Sent!?:: ~p~n", [{NewPending, NewResults}]),
			loop(Socket, Server, Client, Options, Buf, NewPending, NewResults);
		{tcp, Socket, Data} ->
			%?TRACE("TCP", []),
			Buf2 = <<Buf/binary, Data/binary>>,
			{ok, Msgs, Buf3} = try_decode(Buf2),
			NewPending = process_messages(Msgs, Pending, Server, Client),
			loop(Socket, Server, Client, Options, Buf3, NewPending, Results);
		{tcp_error, Socket, _Error} ->
			?TRACE("LSP: TCP error, exiting: ~p~n", [_Error]),
			ok;
		{tcp_closed, Socket} ->
			?TRACE("LSP: closing connection~n", []),
			ok;
		_Other ->
			?TRACE("LSP: unknown message: ~p~n", [_Other]),
			loop(Socket, Server, Client, Options, Buf, Pending, Results)
	end.

try_decode(Buf) ->
	try_decode(Buf, []).

try_decode(Buf, Result) ->
	case try_decode_1(Buf, 0) of
		{ok, Msg, Rest} ->
			try_decode(Rest, [Msg|Result]);
		more ->
			{ok, lists:reverse(Result), Buf}
	end.

try_decode_1(Buf, N) ->
	case erlang:decode_packet(httph, Buf, []) of
		{ok, {http_header, _, 'Content-Length', undefined, Len}, R} ->
			try_decode_1(R, list_to_integer(Len));
		{ok, {http_header, _, _, _, _}, R} ->
			try_decode_1(R, N);
		{ok, http_eoh, R} ->
			if size(R) < N ->
				    more;
			    true ->
				    <<D:N/binary, Rest/binary>> = R,
				    {ok, D, Rest}
			end;
		{more, _} ->
			more
	end.

process_messages(Msgs, Pending, Server, Client) ->
	Fun = fun(Msg, Acc) ->
		M = parse(Msg),
		spawn(fun() -> dispatch(M, Server, Client) end),
		case maps:is_key(id, M) of
			true ->
				#{id:=Id} = M,
				queue:in(Id, Acc);
			false ->
				Acc
		end
	end,
	lists:foldl(Fun, Pending, Msgs).

parse(Data) ->
	Req = decode(jsx:decode(Data, [return_maps])),
	Req.

dispatch(#{jsonrpc := <<"2.0">>,
				    id := Id,
				    result := Result
				}, _Server, Client) ->
	?DEBUG("<# RECV: REPLY ~p: ~tp~n", [Id, Result]),
	Client ! {'$reply', Id, Result};
dispatch(#{jsonrpc := <<"2.0">>,
				    id := Id,
				    error := Error
				}, _Server, Client) ->
	ErrCode = error_code_dec(Error),
	?DEBUG("<# RECV: REPLY ~p: ~tp~n", [Id, ErrCode]),
	Client ! {'$reply', Id, ErrCode};
dispatch(#{jsonrpc := <<"2.0">>,
				    id := Id,
				    method := Method0,
				    params := Params
				}, Server, _Client) ->
	Method = binary_to_atom(Method0, unicode),
	?DEBUG("<# RECV: REQUEST ~p: ~tp ~tp~n", [Id, Method, Params]),
	gen_server:call(Server, {Method, Id, Params});
dispatch(#{jsonrpc := <<"2.0">>,
				    id := Id,
				    method := Method0
				}, Server, _Client) ->
	Method = binary_to_atom(Method0, unicode),
	?DEBUG("<# RECV: REQUEST ~p: ~tp~n", [Id, Method]),
	gen_server:call(Server, {Method, Id, none});
dispatch(#{jsonrpc := <<"2.0">>,
				    method := Method0,
				    params := Params
				}, Server, Client) ->
	Method = binary_to_atom(Method0, unicode),
	?DEBUG("<# RECV: NOTIFICATION ~tp ~tp~n", [Method, Params]),
	gen_server:cast(Server, {Method, Params});
dispatch(#{jsonrpc := <<"2.0">>,
				    method := Method0
				}, Server, _Client) ->
	Method = binary_to_atom(Method0, unicode),
	?DEBUG("<# RECV: NOTIFICATION ~tp~n", [Method]),
	gen_server:cast(Server, {Method, none}).

notify(Socket, Method, Params) ->
	?DEBUG("#> SEND: NOTIFY ~tp ~tp~n", [Method, Params]),
	Ans = #{jsonrpc => <<"2.0">>,
			method => Method,
			params => Params
		},
	send_tcp(Socket, Ans).

request(Socket, Id, Method, Params) ->
	?DEBUG("#> SEND: REQUEST ~p: ~tp ~tp~n", [Id, Method, Params]),
	Ans = #{jsonrpc => <<"2.0">>,
			id => Id,
			method => Method,
			params => Params
		},
	send_tcp(Socket, Ans).

request(Socket, Id, Method) ->
	?DEBUG("#> SEND: REQUEST ~p: ~tp ~n", [Id, Method]),
	Ans = #{jsonrpc => <<"2.0">>,
			id => Id,
			method => Method
		},
	send_tcp(Socket, Ans).

reply(Socket, Id, {error, Code0, Msg})  ->
	Code = error_code_enc(Code0),
	?DEBUG("#> SEND: REPLY ~p: ~tp~n", [Id, Msg]),
	Ans = #{jsonrpc => <<"2.0">>,
			id => Id,
			error => #{
					    code => Code,
					    message => Msg
					}
		},
	send_tcp(Socket, Ans);
reply(Socket, Id, Msg) when is_map(Msg); is_list(Msg); Msg==null ->
	?DEBUG("#> SEND: REPLY ~p: ~tp~n", [Id, Msg]),
	Ans = #{jsonrpc => <<"2.0">>,
			id => Id,
			result => Msg
		},
	send_tcp(Socket, Ans);
reply(_Socket, Id, Msg) ->
	?TRACE("Erroneous reply: ~tp~n",[{Id, Msg}]),
	ok.

send_replies(Socket, Pending, Options, Results) -> 
	case proplists:get_value(ordered_replies, Options, true) of
		true ->
			%?TRACE("ORDERED...",[]),
			send_ordered_replies(Socket, Pending, Results);
		false ->
			[reply(Socket, Id, Answer) || {Id, Answer} <- Results]
	end.

send_ordered_replies(Socket, Pending, Results) ->
	%?TRACE(" -- ~p", [queue:peek(Pending)]),
	case queue:peek(Pending) of	
		empty ->
			%?TRACE("  -- empty ", []), 
			{Pending, Results};
		{value, Id} ->
			case lists:keyfind(Id, 1, Results) of
				false ->
					%?TRACE("  -- not found ", []), 
					{Pending, Results};
				{Id, Answer} ->
					%?TRACE("  -- R:: ~p", [Id]), 
					reply(Socket, Id, Answer), 
					{_, Rest} = queue:out(Pending),
					send_ordered_replies(Socket, Rest, lists:keydelete(Id, 1, Results)) 
			end
	end.

send_tcp(Socket, Ans) ->
	Json = try  
		jsx:encode(encode(Ans))  
		catch _:E ->  
			?DEBUG("json error: ~p: ~p~n", [E, Ans]), 
		[] 
	end, 
	Hdr = io_lib:format("Content-Length: ~w\r\n\r\n", [size(Json)]),
	%?DEBUG("TCP:: ~s~n", [[Hdr, Json]]),
	_ = gen_tcp:send(Socket, Hdr),
	_ = gen_tcp:send(Socket, Json),
	ok.

decode(Json) when is_map(Json) ->
	L1 = maps:to_list(Json),
	L2 = [{erlang:binary_to_atom(K, unicode), decode(V)} || {K, V} <- L1],
	maps:from_list(L2);
decode(Json) when is_list(Json) ->
	[decode(V) || V <- Json];
decode(V) ->
	V.

encode(Json) when is_map(Json) ->
	L1 = maps:to_list(Json),
	L2 = [{erlang:atom_to_binary(K, unicode), encode(V)} || {K, V} <- L1],
	maps:from_list(L2);
encode(Json) when is_list(Json) ->
	[encode(V) || V <- Json];
encode(V) ->
	V.

error_code_enc(parse_error) -> -32700;
error_code_enc(invalid_request) -> -32600;
error_code_enc(method_not_found) -> -32601;
error_code_enc(invalid_params) -> -32602;
error_code_enc(internal_error) -> -32603;
error_code_enc(server_error_start) -> -32099;
error_code_enc(server_error_end) -> -32000;
error_code_enc(server_not_initialized) -> -32002;
error_code_enc(unknown_error) -> -32001;
error_code_enc(N) when is_integer(N) -> N.

error_code_dec(-32700) -> parse_error;
error_code_dec(-32600) -> invalid_request;
error_code_dec(-32601) -> method_not_found;
error_code_dec(-32602) -> invalid_params;
error_code_dec(-32603) -> internal_error;
error_code_dec(-32099) -> server_error_start;
error_code_dec(-32000) -> server_error_end;
error_code_dec(-32001) -> unknown_error;
error_code_dec(-32002) -> server_not_initialized;
error_code_dec(N) when is_integer(N) -> N.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

msg(Str) ->
	unicode:characters_to_binary(
		io_lib:format("Content-Length: ~w\r\n\r\n~ts", 
			[size(Str),Str]
		)
	).

assertMsg(Msg, Rest) ->
	M = msg(Msg),
	?_assertEqual({ok, [unicode:characters_to_binary(Msg)], Rest}, 
		try_decode(<<M/binary, Rest/binary>>)
	).

assertMsgs(Msgs, Rest) ->
	L = [msg(M) || M<-Msgs],
	?_assertEqual({ok, [unicode:characters_to_binary(M) || M<-Msgs], Rest}, 
		try_decode(iolist_to_binary(L++[Rest]))
	).

try_decode_test_() ->
	[
		assertMsg(<<"hej">>, <<>>),
		assertMsg(<<"unŷcö汉"/utf8>>, <<>>),
		assertMsg(<<"hej">>, <<"Cha">>),
		assertMsgs([<<"hej">>], <<>>),
		assertMsgs([<<"hej">>, <<"Cha">>], <<>>),
		assertMsgs([<<"hej">>, <<"Cha">>], <<"for">>)
	].

-endif.
