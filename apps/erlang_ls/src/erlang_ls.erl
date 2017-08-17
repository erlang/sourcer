-module(erlang_ls).

-export([main/1]).

-define(DEFAULT_PORT, 9000).

main(Args) ->
    case getopt:parse(cli_options(), Args) of
        {ok, {Opts, _Other}} ->
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
            end;
        _Err ->
            io:format("Error: ~p~n", [_Err]),
            getopt:usage(cli_options(), "lsp_server")
    end,
    ok.

cli_options() ->
    [
     {port,    $p,        "port",    integer,               "LSP server port"},
     {verbose, $v,        "verbose", integer,               "Verbosity level"}
    ].