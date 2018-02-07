-module(erlang_ls).

-export([main/1]).

-define(DEFAULT_PORT, 9000).

main(Args) ->
    case getopt:parse(cli_options(), Args) of
        {ok, {Opts, Other}} ->
            Dump = proplists:get_value(dump, Opts),
            Indent = proplists:get_value(indent, Opts),
            if Dump =:= undefined, Indent =:= undefined ->
                    start_server(Opts);
               is_list(Indent) ->
                    indent([Indent|Other], proplists:get_value(verbose, Opts, 0));
               is_list(Dump) ->
                    dump_file(Dump)
            end;
        _Err ->
            io:format("Error: ~p~n", [_Err]),
            getopt:usage(cli_options(), "lsp_server")
    end.

cli_options() ->
    [
     {dump,    $d,        "dump",     string,      "Dump sourcer db for file"},
     {port,    $p,        "port",    integer,               "LSP server port"},
     {verbose, $v,        "verbose", integer,               "Verbosity level"},
     {indent,  $i,        "indent",   string,      "Indent file(s) and exit"}
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

indent([File|Files], Verbose) ->
    try case file:read_file(File) of
            {ok, BinSrc} ->
                Enc = encoding(BinSrc),
                Src = unicode:characters_to_list(BinSrc, Enc),
                {ST,Indented} = timer:tc(fun() -> sourcer_indent:lines(Src) end),
                ok = file:write_file(File, unicode:characters_to_binary(Indented, utf8, Enc)),
                Verbose > 0 andalso io:format("Indent: ~.6wms ~s~n", [ST div 1000, File]),
                indent(Files, Verbose);
            {error, Error} ->
                Str = io_lib:format("Could not read file: ~ts\n Reason: ~p~n", [File, Error]),
                throw({error,Str})
        end
    catch throw:{error, Desc} ->
            io:format("~ts", [Desc]),
            erlang:halt(1);
          error:_What ->
            io:format("Error could not indent file: ~ts\n", [File]),
            erlang:halt(1)
    end;
indent([], _) ->
    ok.

encoding(Bin) ->
    case epp:read_encoding_from_binary(Bin) of
        latin1 -> latin1;
        _ -> utf8
    end.
