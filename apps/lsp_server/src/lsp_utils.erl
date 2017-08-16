-module(lsp_utils).

-export([
		 position/2,
		 range/2,
		 range/4
		 ]).

position(L, C) ->
	#{line=>L, character=>C}.

range(P1, P2) ->
	#{start=>P1, 'end'=>P2}.

range(L1, C1, L2, C2) ->
	range(position(L1, C1), position(L2, C2)).

