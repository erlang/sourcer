%%
%%
-module(sourcer_indent_tests).

-include_lib("eunit/include/eunit.hrl").

indent_test_() ->
    Dir = filename:dirname(code:which(?MODULE)) ++ "/" ++ ?MODULE_STRING ++ "_data",
    OrigFs = filelib:wildcard(Dir ++ "/*"),
    io:format("Dir: ~s~nFs: ~p~n", [Dir, OrigFs]),
    Fs = [{File, unindent(File)} || File <- OrigFs,
                                    filename:extension(File) =:= ""],
    %% Indent = fun emacs/1,
    Indent = fun sourcerer/1,
    [Indent(File) || {_, File} <- Fs],
    Res = [diff(Orig, File) || {Orig, File} <- Fs],
    [file:delete(File) || {ok, File} <- Res],       %% Cleanup
    Failed = [Fail || {fail, Fail} <- Res],
    ?_assertEqual([],Failed).

unindent(Input) ->
    Output = Input ++ ".erl",
    {ok, Bin} = file:read_file(Input),
    Lines0 = string:split(Bin, "\n", all),
    Lines = [string:trim(Line, leading, [$\s,$\t]) || Line <- Lines0],
    %% io:format("File: ~s lines: ~w~n", [Input, length(Lines0)]),
    %% [io:format("~s~n", [L]) || L <- Lines],
    ok = file:write_file(Output, lists:join("\n", Lines)),
    Output.

diff(Orig, File) ->
    case os:cmd(["diff ", Orig, " ", File]) of
        "" -> {ok, File};
        Diff ->
            io:format(user, "Fail: ~s vs ~s~n~s~n~n",[Orig, File, Diff]),
            {fail, File}
    end.

sourcerer(File) ->
    io:format("* Indenting: ~s *~n",[File]),
    {ok, Bin} = file:read_file(File),
    Src = unicode:characters_to_list(Bin),
    Indented = sourcer_indent:lines(Src),
    file:write_file(File, unicode:characters_to_binary(Indented)).


