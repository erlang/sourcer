-module(sourcer_dump).

-export([
    dump/3
]).

%%-define(DEBUG, true).
-include("debug.hrl").

-include("sourcer_model.hrl").
-include_lib("xmerl/include/xmerl.hrl").


dump(FileOrDir, Fmt, Out) ->
    case {filelib:is_dir(FileOrDir),filelib:is_regular(FileOrDir)} of
        {true, _} ->
            dump_project(FileOrDir, Fmt, Out);
        {_, true} ->
            dump_files([FileOrDir], Fmt, Out);
        _ ->
            {error, not_found, FileOrDir}
    end.

dump_project(Dir, Fmt, Out) ->
    ?D(Dir),
    [Layout] = sourcer_layout:detect_layout(Dir),
    ?D(Layout),
    {ok, Files1} = file:list_dir(filename:join(Dir, Layout#project.sources)),
    {ok, Files2} = file:list_dir(filename:join(Dir, Layout#project.includes)),
    AllFiles = [filename:join([Dir, Layout#project.sources, F]) || F<-Files1]
        ++ [filename:join([Dir, Layout#project.includes, F]) || F<-Files2],
    dump_files(AllFiles, Fmt, Out),
    ok.

dump_files(Files, Fmt, Out) ->
    with_file(Out, fun(OutF)->
        case Fmt of
            raw ->
                dump_raw(Files, OutF);
            s101 ->
                dump_s101(Files, OutF);
            _ ->
                ok
        end
    end).

with_file(Out, Body) ->
    case Out of
        standard_io ->
            Body(Out);
        _ ->
            case file:open(Out, [write]) of
                {ok, OutF} ->
                    try
                        Body(OutF)
                    after 
                        case Out of
                            standard_io ->
                                ok;
                            _ ->
                                file:close(OutF)
                        end
                    end;
                Err ->
                    Err
            end
    end.

dump_raw(Files, Out) ->
    DB = sourcer_analyse:merge([analyse_file(F) || F<- Files]), 
    io:format(Out, "Dump: ~n", []),
    case DB of
        Model=#model{defs=D, refs=R} ->
            C = case io:columns() of {ok, Cc} -> Cc; _ -> 80 end,
            io:format(Out, "Definitions:::~n~*p~n-----~n", [C, lists:sort(D)]),
            io:format(Out, "References:::~n~*p~n-----~n", [C, lists:sort(R)]),
            ok;
        Err ->
            io:format("Error:: ~p~n", [Err])
    end.

-define(NL, #xmlText{value=[10]}).

dump_s101(Files, Out) ->
    DB = sourcer_analyse:merge([analyse_file(F) || F<- Files]), 
    case DB of
        Model=#model{defs=D, refs=R} ->
            M = get_modules(D),
            Data = {data, [{flavor, "org.erlang.erlang"}], [
                ?NL,
                {modules, M},
                ?NL,
                {dependencies, get_dependencies(M, R)},
                ?NL
            ]},
            XML = xmerl:export_simple([?NL,Data], xmerl_xml),
            io:format("~s~n", [lists:flatten(XML)]);
        Err ->
            io:format("~p~n", [Err])
    end.

get_modules(Defs) ->
    lists:flatten([[{module, [{name, io_lib:format("~w", [D#def.ctx])}, {id, "?"},{type, "module"}], []},?NL] || D<-Defs]).

get_dependencies(Mods, Refs) ->
    lists:flatten([[{dependency, [{from, "?"}, {to, io_lib:format("~w", [R#ref.ctx])}, {type, "calls"}], []},?NL] || R<-Refs]).

scan(D) -> 
    T = unicode:characters_to_list(D), 
    {ok, Ts, _} = sourcer_scan:string(T), 
    sourcer_scan:filter_ws_tokens(Ts). 


analyse_file(FileName) -> 
    case file:read_file(FileName) of
        {ok, Content} -> 
            {ST,Tokens} = timer:tc(fun() -> scan(Content) end), 
            {PT,ParseTree} = timer:tc(fun() -> sourcer_parse:parse(Tokens) end), 
            {AT, M} = timer:tc(fun() -> sourcer_analyse:analyse(ParseTree) end), 
            M;
        _ ->
            {error, FileName}
    end.
