%%
%%
-module(sourcer_indent_tests).
-export([sourcer/1]).
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    Dir = filename:dirname(code:which(?MODULE)) ++ "/" ++ ?MODULE_STRING ++ "_data",
    OrigFs = filelib:wildcard(Dir ++ "/*"),
    io:format("Dir: ~s~nFs: ~p~n", [Dir, OrigFs]),
    Fs = [{File, unindent(File)} || File <- OrigFs,
                                    filename:extension(File) =:= ""],
    Indent = fun sourcer/1,
    [Indent(File) || {_, File} <- Fs],
    Res = [diff(Orig, File, 1) || {Orig, File} <- Fs],
    %% And do the indentation again to see that nothing have changed
    [Indent(File) || {_, File} <- Fs],
    Res2 = [diff(Orig, File, 2) || {Orig, File} <- Fs],
    {setup,
     fun()-> ok end,
     fun(_)->  %% Keep failed files to ease debugging
             [file:delete(File) || {ok, File} <- Res],
             ok
     end,
     [?_assertMatch({ok, _}, Result) || Result <- Res] ++
         [?_assertMatch({ok, _}, Result) || Result <- Res2]
    }.

lines_test_() ->
    Basic =
"foo() ->
    line1,
  line2,
    case X of
    clause1 ->
            ok
  end,
  line7,
    ok.",
    Line2 = {2,"  line2,\n","    line2,\n"},
    Line4 = {4,"    clause1 ->\n","        clause1 ->\n"},
    Line6 = {6,"  end,\n",   "    end,\n"},
    Line7 = {7,"  line7,\n", "    line7,\n"},
    [ ?_assertMatch([], sourcer_indent:lines(0, 0, Basic))
    , ?_assertMatch([Line2], sourcer_indent:lines(2, 2, Basic))
    , ?_assertMatch([Line2], sourcer_indent:lines(0, 3, Basic))
    , ?_assertMatch([Line4], sourcer_indent:lines(4, 5, Basic))
    , ?_assertMatch([Line6,Line7], sourcer_indent:lines(6, 7, Basic))
    ].

line_test_() ->
    Str = ["%% Comments\n",                      % line 0
           "\n",
           "%%\n",
           "\n",
           "line_test(Arg1,\n",                  % line 4
           "          Arg2,\n",
           "          \n",
           "          Arg4)\n",                  % line 7
           "  \n",
           "  when Arg1 =:= Arg2,\n",
           "       \n",
           "       Arg4 > 10 ->\n",              % line 11
           "    line1,\n",
           "    case Arg1 of\n",
           "        true ->\n",
           "            case Arg2 of\n",         % line 15
           "                true ->\n",
           "                    ok\n",
           "            \n",
           "            end;\n",
           "        \n",                         % line 20
           "        false ->\n",
           "            ok\n",
           "    end,\n",
           "    \n",
           "    ok.\n"
          ],
    [?_test(do_test_line(N, Str)) || N <- lists:seq(0, length(Str)-1)].

do_test_line(N, Str) ->
    SrcLists = lists:sublist(Str, N),
    Src = unicode:characters_to_list([SrcLists,"\n"]),
    Indented = sourcer_indent:line(N, Src),
    Next = lists:nth(N+1, Str),
    %% io:format(user, "~.3w: ~s~n", [N, Next]),
    ?assertEqual({N,string:span(Next, " ")}, {N,string:span(Indented, " ")}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers

unindent(Input) ->
    Output = Input ++ ".actual",
    {ok, Bin} = file:read_file(Input),
    Lines0 = string:split(Bin, "\n", all),
    %% We leave one space, so we can show errors that should indent to col 0
    AddOne = fun(<<>>) -> <<>>;
                (<<"row", _/binary>>=Row) -> Row;
                (Str) -> [$\s|Str]
             end,
    Lines = [AddOne(string:trim(Line, leading, [$\s,$\t])) || Line <- Lines0],
    %% io:format("File: ~s lines: ~w~n", [Input, length(Lines0)]),
    %% [io:format("~s~n", [L]) || L <- Lines],
    ok = file:write_file(Output, lists:join("\n", Lines)),
    Output.

diff(Orig, File, Pass) ->
    case os:cmd(["diff ", Orig, " ", File]) of
        "" -> {ok, File};
        Diff ->
            io:format(user, "Fail: ~s vs ~s~n~s~n~n",[Orig, File, Diff]),
            {{fail, Pass}, File}
    end.

sourcer(File) ->
    io:format("* Indenting: ~s *~n",[File]),
    {ok, Bin} = file:read_file(File),
    Src = unicode:characters_to_list(Bin),
    Indented = sourcer_indent:all(Src),
    file:write_file(File, unicode:characters_to_binary(Indented)).


