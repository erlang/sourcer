%% @author jakob
%% @doc @todo Add description to sourcer_scan_util.

-module(sourcer_scan_util).

-export([
    split_lines_w_lengths/1, 
    find_line_w_offset/2, 
    get_lines_info/1
]).

%% [{Length, TextIncNL}...]
split_lines_w_lengths(Text) ->
    split_lines_w_lengths(Text, 0, [], []).

split_lines_w_lengths("", _Length, [], Acc) ->
    lists:reverse(Acc);
split_lines_w_lengths("", Length, LineAcc, Acc) ->
    lists:reverse(Acc, [{Length, lists:reverse(LineAcc)}]);
split_lines_w_lengths("\r\n" ++ Text, Length, LineAcc, Acc) ->
    split_lines_w_lengths(Text, 0, [],
                          [{Length+2, lists:reverse(LineAcc, "\r\n")} | Acc]);
split_lines_w_lengths("\n" ++ Text, Length, LineAcc, Acc) ->
    split_lines_w_lengths(Text, 0, [],
                          [{Length+1, lists:reverse(LineAcc, "\n")} | Acc]);
split_lines_w_lengths("\r" ++ Text, Length, LineAcc, Acc) ->
    split_lines_w_lengths(Text, 0, [],
                          [{Length+1, lists:reverse(LineAcc, "\r")} | Acc]);
split_lines_w_lengths([C | Text], Length, LineAcc, Acc) ->
    split_lines_w_lengths(Text, Length+1, [C | LineAcc], Acc).

get_lines_info(Binary) when is_binary(Binary) ->
    _Lines = binary:split(Binary, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global]).


%% Find a line from [{Length, Line

find_line_w_offset(Offset, Lines) ->
    find_line_w_offset(Offset, 0, 0, Lines).

find_line_w_offset(0, _Pos, _N, []) ->
    {0, 0, 0, "", on_eof};
find_line_w_offset(_Offset, _Pos, _N, []) ->
    not_found;
find_line_w_offset(Offset, Pos, N, [{Length, _Line} | Lines]) when Offset >= Pos+Length, Lines =/= [] ->
    find_line_w_offset(Offset, Pos+Length, N+1, Lines);
find_line_w_offset(Offset, Pos, N, [{Length, Line} |_]) when Pos =< Offset, Offset < Pos + Length ->
    {N, Pos, Length, Line, false};
find_line_w_offset(Offset, Pos, N, [{Length, Line}]) ->
    case ends_with_newline(Line) orelse Offset > Pos + Length of
        true ->
            {N+1, Pos+Length, 0, "", beyond_eof};
        false ->
            {N, Pos+Length, Length, Line, on_eof}
    end.

ends_with_newline("") -> false;
ends_with_newline("\n") -> true;
ends_with_newline("\r") -> true;
ends_with_newline("\r\n") -> true;
ends_with_newline([_C | R]) ->
    ends_with_newline(R).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

lines_info_test_() ->
    [
        ?_assertEqual([<<"">>], get_lines_info(<<"">>)),
        ?_assertEqual([<<"abc">>], get_lines_info(<<"abc">>)),
        ?_assertEqual([<<"a">>,<<"b">>], get_lines_info(<<"a\nb">>)),
        ?_assertEqual([<<"a">>,<<"b">>,<<"c">>,<<"d">>], get_lines_info(<<"a\nb\rc\r\nd">>)),
        ?_assertEqual([<<"a">>,<<"">>,<<"b">>], get_lines_info(<<"a\n\rb">>)),
        ?_assertEqual([<<"a">>,<<"b">>], get_lines_info(<<"a\nb">>))
    ].

-endif.
