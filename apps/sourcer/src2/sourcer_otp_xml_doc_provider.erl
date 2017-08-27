-module(sourcer_otp_xml_doc_provider).

-export([
		 get_documentation/1
		]).

get_documentation({Kind, Ref}) ->
	R = lists:flatten(io_lib:format("OTP XML:: ~p ~p", [Kind, Ref])),
	{ok, {otp_xml, R}};
get_documentation(Arg) ->
	{error, {badarg, Arg}}.
