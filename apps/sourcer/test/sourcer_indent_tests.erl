%%
%%
-module(sourcer_indent_tests).
-export([sourcer/1]).
-include_lib("eunit/include/eunit.hrl").

indent_test_() ->
    Dir = filename:dirname(code:which(?MODULE)) ++ "/" ++ ?MODULE_STRING ++ "_data",
    OrigFs = filelib:wildcard(Dir ++ "/*"),
    io:format("Dir: ~s~nFs: ~p~n", [Dir, OrigFs]),
    Fs = [{File, unindent(File)} || File <- OrigFs,
                                    filename:extension(File) =:= ""],
    Indent = fun sourcer/1,
    [Indent(File) || {_, File} <- Fs],
    Res = [diff(Orig, File) || {Orig, File} <- Fs],
    {setup, 
        fun()-> ok end,
        fun(_)-> 
            [file:delete(File) || {_, File} <- Res],
            ok
        end,
        [?_assertMatch({ok, _}, Result) || Result <- Res]
    }.

unindent(Input) ->
    Output = Input ++ ".actual",
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

sourcer(File) ->
    io:format("* Indenting: ~s *~n",[File]),
    {ok, Bin} = file:read_file(File),
    Src = unicode:characters_to_list(Bin),
    Indented = sourcer_indent:lines(Src),
    file:write_file(File, unicode:characters_to_binary(Indented)).


