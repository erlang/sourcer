-module(sourcer_util_tests).

-include_lib("eunit/include/eunit.hrl").

middle_test_() ->
	[
		?_assertEqual([], sourcer_util:middle([])), 
		?_assertEqual([], sourcer_util:middle([1])), 
		?_assertEqual([], sourcer_util:middle([1,2])), 
		?_assertEqual([2], sourcer_util:middle([1,2,3])), 
		?_assertEqual([2,3], sourcer_util:middle([1,2,3,4]))
	].