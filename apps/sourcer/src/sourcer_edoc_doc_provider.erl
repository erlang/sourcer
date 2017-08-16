-module(sourcer_edoc_doc_provider).

-export([
		 get_documentation/1
		]).

get_documentation({Kind, Ref}) ->
	R = lists:flatten(io_lib:format("EDOC:: ~p ~p", [Kind, Ref])),
	{ok, {edoc, R}};
get_documentation(Arg) ->
	{error, {badarg, Arg}}.

