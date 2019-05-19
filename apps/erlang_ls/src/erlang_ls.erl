-module(erlang_ls).

-export([main/1]).

-define(DEFAULT_TRANSPORT, tcp).
-define(DEFAULT_PORT, 9000).

main(Args) ->
    case getopt:parse(cli_options(), Args) of
        {ok, {Opts, Other}} ->
            OptsMap = maps:from_list(proplists:unfold(Opts)),
            run(OptsMap, Other);
        _Err ->
            io:format("Error: ~p~n", [_Err]),
            getopt:usage(cli_options(), "lsp_server")
    end.

cli_options() ->
    [
     {help,    $h,        "help",    undefined,    "Show this help"},
     {dump,    $d,        "dump",    string,       "Dump sourcer db for file or project"},
     {format,  undefined, "fmt",     {atom, raw},  "Format for the dump (default: raw)"},
     {out,     undefined, "out",     {string, standard_io},    "Destination file for the dump (default: standard_io)"},
     {transport,$t,       "transport",{atom, tcp}, "Transport layer for communication (default: tcp, stdio)"},
     {port,    $p,        "port",    integer,      "LSP server port"},
     {verbose, $v,        "verbose", integer,      "Verbosity level"},
     {indent,  $i,        "indent",  string,       "Indent file(s) and exit"},
     {stdout,  undefined, "stdout",  {boolean, false},       "Output to stdout instead of in place"},
     {config,  undefined, "config",  string,       "Configuration file"}
    ].

run(Opts, Other) ->
    Verbose = maps:get(verbose, Opts, 0),
    Config = maybe_load_config(maps:get(config, Opts, undefined), Verbose),
    case Opts of
        #{help := _} ->
            getopt:usage(cli_options(), "erlang_ls", "", [
                    {"", ""},
                    {"Start LS:", "'erlang_ls -P <nnnn>'"},
                    {"Indent  :", "'erlang_ls -i <files>'"},
                    {"Dump    :", "'erlang_ls -d <files> -fmt <fmt> -out <file>'"}
                ]),
            erlang:halt(0);
        #{dump:=DumpFile, format:=Fmt, out:=Out} ->
            Out1 = case Out of
                    "standard_io" ->
                        standard_io;
                    _ ->
                        Out
                end,
            sourcer_dump:dump(DumpFile, Fmt, Out1);
        #{indent := Indent, stdout := Stdout} ->
            IndentConfig = proplists:get_value(indent, Config, []),
            indent([Indent|Other], IndentConfig, Stdout, Verbose);
        _ ->
            ServerConfig = proplists:get_value(server, Config, []),
            start_server(Opts, ServerConfig)
    end.

start_server(Opts, Config) ->
    Transport = maps:get(transport, Opts, proplists:get_value(transport, Config, ?DEFAULT_TRANSPORT)),

    ok = application:load(lsp_server),
    ok = application:set_env(lsp_server, transport, Transport),
    case Transport of
        tcp ->
            Port = maps:get(port, Opts, proplists:get_value(port, Config, ?DEFAULT_PORT)),
            ok = application:set_env(lsp_server, port, Port);
        _ ->
            ok
    end,
    ok = application:set_env(lsp_server, backend, sourcer),

    case application:ensure_all_started(lsp_server, permanent) of
        {ok, _R} ->
            receive stop -> ok end,
            ok;
        _Err ->
            io:format("Startup error: ~p~n", [_Err]),
            ok
    end.

maybe_load_config(undefined, _Verbose) ->
    [];
maybe_load_config(File, Verbose) ->
    case file:consult(File) of
        {ok, Config} ->
            Config;
        {error, Reason} ->
            io:format("Error loading config file: ~ts~n", [File]),
            Verbose > 0 andalso io:format("Reason ~p~n", [Reason]),
            erlang:halt(1)
    end.

indent([File|Files], Config, Stdout, Verbose) ->
    Output = output(Stdout, File),
    try case file:read_file(File) of
            {ok, BinSrc} ->
                Enc = encoding(BinSrc),
                Src = unicode:characters_to_list(BinSrc, Enc),
                {ST,Indented} = timer:tc(fun() -> sourcer_indent:all(Src, Config) end),
                ok = Output(unicode:characters_to_binary(Indented, utf8, Enc)),
                Verbose > 0 andalso io:format(standard_error, "Indent: ~.6wms ~s~n", [ST div 1000, File]),
                indent(Files, Config, Stdout, Verbose);
            {error, Error} ->
                Str = io_lib:format("Could not read file: ~ts\n Reason: ~p~n", [File, Error]),
                throw({error,Str})
        end
    catch throw:{error, Desc} ->
            io:format(standard_error, "~ts", [Desc]),
            erlang:halt(1);
          error:What ->
            io:format(standard_error, "Error could not indent file: ~ts\n", [File]),
            Verbose > 0 andalso io:format(standard_error, "Error ~p~n", [What]),
            Verbose > 1 andalso io:format(standard_error, "Stacktrace ~p~n", [erlang:get_stacktrace()]),
            erlang:halt(1)
    end;
indent([], _, _, _) ->
    ok.

output(false, File) ->
    fun(C) ->
            file:write_file(File, C)
    end;
output(true, _) ->
    fun(C) ->
            io:format(standard_io, "~s", [C])
    end.

encoding(Bin) ->
    case epp:read_encoding_from_binary(Bin) of
        latin1 -> latin1;
        _ -> utf8
    end.
