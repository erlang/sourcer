-module(erlang_ls).

-export([main/1]).

-define(DEFAULT_PORT, 9000).

main(Args) ->
    case getopt:parse(cli_options(), Args) of
        {ok, {Opts, Other}} ->
            Dump = proplists:get_value(dump, Opts),
            Indent = proplists:get_value(indent, Opts),
            Verbose = proplists:get_value(verbose, Opts, 0),
            Config = maybe_load_config(proplists:get_value(config, Opts), Verbose),
            if Dump =:= undefined, Indent =:= undefined ->
                    start_server(Opts);
               is_list(Indent) ->
                    IndentConfig = proplists:get_value(indent, Config, []),
                    indent([Indent|Other], IndentConfig, Verbose);
               is_list(Dump) ->
                    dump_file(Dump)
            end;
        _Err ->
            io:format("Error: ~p~n", [_Err]),
            getopt:usage(cli_options(), "lsp_server")
    end.

cli_options() ->
    [
     {dump,    $d,        "dump",    string,  "Dump sourcer db for file"},
     {port,    $p,        "port",    integer, "LSP server port"},
     {verbose, $v,        "verbose", integer, "Verbosity level"},
     {indent,  $i,        "indent",  string,  "Indent file(s) and exit"},
     {config,  undefined, "config",  string,  "Configuration file"}
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

scan(D) ->
    T = unicode:characters_to_list(D),
    {ok, Ts, _} = sourcer_scan:string(T),
    sourcer_scan:filter_ws_tokens(Ts).

dump_file(File) ->
    io:format("Dump: ~tp~n", [File]),
    {ok, Content} = file:read_file(File),
    {ST,Tokens} = timer:tc(fun() -> scan(Content) end),
    {PT,ParseTree} = timer:tc(fun() -> sourcer_parse:parse(Tokens) end),
    {AT, {model,_,D,R}} = timer:tc(fun() -> sourcer_db:analyse(ParseTree) end),
    C = case io:columns() of {ok, Cc} -> Cc; _ -> 80 end,
    io:format("Definitions:::~n~*p~n-----~n", [C, lists:sort(D)]),
    io:format("References:::~n~*p~n-----~n", [C, lists:sort(R)]),
    io:format("Scan:    ~.6wms~n", [ST div 1000]),
    io:format("Parse:   ~.6wms~n", [PT div 1000]),
    io:format("Analyze: ~.6wms~n", [AT div 1000]),
    ok.

indent([File|Files], Config, Verbose) ->
    try case file:read_file(File) of
            {ok, BinSrc} ->
                Enc = encoding(BinSrc),
                Src = unicode:characters_to_list(BinSrc, Enc),
                {ST,Indented} = timer:tc(fun() -> sourcer_indent:all(Src, Config) end),
                ok = file:write_file(File, unicode:characters_to_binary(Indented, utf8, Enc)),
                Verbose > 0 andalso io:format("Indent: ~.6wms ~s~n", [ST div 1000, File]),
                indent(Files, Config, Verbose);
            {error, Error} ->
                Str = io_lib:format("Could not read file: ~ts\n Reason: ~p~n", [File, Error]),
                throw({error,Str})
        end
    catch throw:{error, Desc} ->
            io:format("~ts", [Desc]),
            erlang:halt(1);
          error:What ->
            io:format("Error could not indent file: ~ts\n", [File]),
            Verbose > 0 andalso io:format("Error ~p~n", [What]),
            Verbose > 1 andalso io:format("Stacktrace ~p~n", [erlang:get_stacktrace()]),
            erlang:halt(1)
    end;
indent([], _, _) ->
    ok.

encoding(Bin) ->
    case epp:read_encoding_from_binary(Bin) of
        latin1 -> latin1;
        _ -> utf8
    end.
