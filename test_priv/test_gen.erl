-module(test_gen).

-export([epp/1]).

epp(File) ->
    FN = ok(file:get_cwd())++"/../test_priv/"++File,
    {ok, Epp} = epp:open(FN, [], [{bax, 0}]),
    Res = epp_scan_file(Epp),
    epp:close(Epp),
    {ok, AFile} = file:open(FN, [read]),
    Ls = read(AFile),
    Z = split_lines(Res),
    Data = [as_test(X) || X<-lists:zip(Z, Ls)],
    file:write_file(FN++".out", iolist_to_binary(io_lib:format("~s~n", [Data]))).

epp_scan_file(Epp) ->
    case epp:scan_erl_form(Epp) of
        {ok, [{'-',_},{atom,_,file}|_]} ->
            epp_scan_file(Epp);
        {ok, Toks} ->
            Toks++epp_scan_file(Epp);
        {eof, _L} ->
            [];
        {error, E} ->
            [{error, E}]++epp_scan_file(Epp)
    end.

as_test({Ts, S}) ->
    io_lib:format("?_assertMatch(~p, expand(~p)),~n",[pattern(Ts), S]).

pattern(Ts) ->
    [noline(X) || X<-Ts].

noline({error, {_, A,B}}) -> {error, {1, A,B}};
noline({A, _}) -> {A, 1};
noline({A, _, B}) -> {A, 1, B}.

read(File) ->
    case file:read_line(File) of
        {ok, Data} -> [Data | read(File)];
        eof        -> [];
        {error, E} -> [E | read(File)]
    end.

split_lines(L) ->
    split_lines(L, [], []).

split_lines([], LC, LS) ->
    lists:reverse([LC|LS]);
split_lines([H|T], [], LS) ->
    split_lines(T, [H], LS);
split_lines([H|T], [C|_]=LC, LS) ->
    case line(H)==line(C) of
        true ->
            split_lines(T, [H|LC], LS);
        false ->
            split_lines(T, [H], [lists:reverse(LC)|LS])
    end.

line({error, {N, _, _}}) -> N;
line({_, N}) -> N;
line({_, N, _}) -> N.

ok({ok, R, _}) ->
    R;
ok({ok, R}) ->
    R.

