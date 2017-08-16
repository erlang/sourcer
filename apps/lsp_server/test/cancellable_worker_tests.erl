-module(cancellable_worker_tests).

-include_lib("eunit/include/eunit.hrl").

nothing_test() ->
  W = fun(_Rp) -> 
        receive after 500 -> ok end 
      end,
  S = self(),
  R = fun(A) -> S ! A end,
  {ok, _P} = cancellable_worker:start(1, W, R),
  Value = receive X -> X after 4000 -> timeout end,
  Expect = {done, nothing},
  ?assertEqual(Expect, Value).

nothing_cancel_test() ->
  W = fun(_Rp) -> 
        idle_worker(10) 
      end,
  S = self(),
  R = fun(A) -> S ! A end,
  {ok, P} = cancellable_worker:start(1, W, R),
  receive after 1000 -> ok end,
  cancellable_worker:cancel(P),
  Value = receive X -> X after 4000 -> timeout end,
  Expect = {canceled, nothing},
  ?assertEqual(Expect, Value).

idle_worker(N) when N=<0 ->
  ok;
idle_worker(N) ->
  receive 
    after 300 -> 
      idle_worker(N-1) 
  end.
  
something_test() ->
  W = fun(Rp) -> 
        busy_worker(4, Rp) 
      end,
  S = self(),
  R = fun(A) -> S ! A end,
  {ok, _P} = cancellable_worker:start(1, W, R),
  Value = receive X -> X after 4000 -> timeout end,
  Expect = {done, [{4},{3},{2},{1}]},
  ?assertEqual(Expect, Value).

something_cancel_test() ->
  W = fun(Rp) -> 
        busy_worker(10, Rp) 
      end,
  S = self(),
  R = fun(A) -> S ! A end,
  {ok, P} = cancellable_worker:start(1, W, R),
  receive after 1000 -> ok end,
  cancellable_worker:cancel(P),
  Value = receive X -> X after 4000 -> timeout end,
  Expect = {canceled, [{10},{9},{8}]},
  ?assertEqual(Expect, Value).

busy_worker(N, _R) when N=<0 ->
  ok;
busy_worker(N, R) ->
  receive 
    after 300 -> 
      R({chunk, {N}}),
      busy_worker(N-1, R) 
  end.

check_test() ->
  W = fun(Rp) -> 
        busy_worker(10, Rp) 
      end,
  S = self(),
  R = fun(A) -> S ! A end,
  {ok, P} = cancellable_worker:start(1, W, R),
  {ok, X0} = cancellable_worker:get_current_results(P),
  receive after 1000 -> ok end,
  {ok, X1} = cancellable_worker:get_current_results(P),
  receive after 1000 -> ok end,
  {ok, X2} = cancellable_worker:get_current_results(P),
  Expect = {nothing, [{10},{9},{8}],[{10},{9},{8},{7},{6},{5}]},
  ?assertEqual(Expect, {X0, X1, X2}).

value_test() ->
  W = fun(Rp) -> 
        receive after 10 -> ok end, 
        Rp({value, overwritten}),
        Rp({value, excellent}),
        Rp({chunk, ignored})
      end,
  S = self(),
  R = fun(A) -> S ! A end,
  {ok, _P} = cancellable_worker:start(1, W, R),
  Value = receive X -> X after 4000 -> timeout end,
  Expect = {done, excellent},
  ?assertEqual(Expect, Value).

