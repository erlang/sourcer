-module(erlang_ls).

-export([main/1]).

-define(DEFAULT_PORT, 9000).

main(Args) ->
    case getopt:parse(cli_options(), Args) of
        {ok, {Opts, _Other}} ->
            case proplists:get_value(dump, Opts) of
                undefined ->
                    start_server(Opts);
                File ->
                    dump_file(File)
            end;
        _Err ->
            io:format("Error: ~p~n", [_Err]),
            getopt:usage(cli_options(), "lsp_server")
    end,
    ok.

cli_options() ->
    [
     {dump,    $d,        "dump",     string,      "Dump sourcer db for file"},
     {port,    $p,        "port",    integer,               "LSP server port"},
     {verbose, $v,        "verbose", integer,               "Verbosity level"}
    ].

start_server(Opts) ->
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),

    ok = application:load(lsp_server),
    ok = application:set_env(lsp_server, port, Port),
    ok = application:set_env(lsp_server, implementor, sourcer),

    case application:ensure_all_started(lsp_server, permanent) of
        {ok, _R} ->
            receive stop -> ok end,
            ok;
        _Err ->
            io:format("Startup error: ~p~n", [_Err]),
            ok
    end.

scan(D) ->
    T = unicode:characters_to_list(D),
    {ok, Ts, _} = sourcer_scan:string(T),
    sourcer_scan:filter_ws_tokens(Ts).

dump_file(File) ->
    io:format("Dump: ~tp~n", [File]),
    {ok, Content} = file:read_file(File),
    {model,_,D,R} = sourcer_db:analyse(sourcer_parse:parse(scan(Content))),
    C = case io:columns() of {ok, Cc} -> Cc; _ -> 80 end,
    io:format("Definitions:::~n~*p~n-----~n", [C, lists:sort(D)]),
    io:format("References:::~n~*p~n-----~n", [C, lists:sort(R)]),
    ok.
