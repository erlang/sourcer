-module(erlide_util_tests).

-include_lib("eunit/include/eunit.hrl").

middle_test_() ->
	[
		?_assertEqual([], erlide_util:middle([])), 
		?_assertEqual([], erlide_util:middle([1])), 
		?_assertEqual([], erlide_util:middle([1,2])), 
		?_assertEqual([2], erlide_util:middle([1,2,3])), 
		?_assertEqual([2,3], erlide_util:middle([1,2,3,4]))
	].