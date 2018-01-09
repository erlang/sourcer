%%% Read and process external module definitions.
%% TODO describe format
%% TODO simplify code
%% TODO remove eclipse-y details

-module(sourcer_externals).

-export([
        get_external_modules/2,
        get_external_module/2,
        get_external_module_tree/1,
        get_external_include/2,
        get_external_1/3,
        get_otp_lib_structure/1,
        get_lib_files/1,
        get_includes_in_dir/1
        ]).

-include("debug.hrl").
-include("sourcer_open.hrl").

%%
%% API Functions
%%

get_external_include(FilePath, #open_context{externalIncludes=ExternalIncludes,
                                             pathVars=PathVars}) ->
    ?D(FilePath),
    ExtIncPaths = get_external_modules_files(ExternalIncludes, PathVars),
    get_ext_inc(ExtIncPaths, FilePath).

get_otp_lib_structure(_StateDir) ->
    RenewFun = fun(_) ->
                    CodeLibs = code:get_path(),
                    LibDir = code:lib_dir(),
                    Libs = lists:filter(fun(N) -> lists:prefix(LibDir, N) end, CodeLibs),
                    LibDirs = [get_lib_dir(Lib) || Lib <- lists:sort(Libs)],
                    R = lists:map(fun(Dir) ->
                                            SubDirs = ["src", "include"],
                                            Group = get_app_group(Dir),
                                            {Dir, get_dirs(SubDirs, Dir, []), Group}
                                    end, LibDirs),
                    ?D(R),
                    R
            end,
    VersionFileName = filename:join([code:root_dir()]),
    R = RenewFun(VersionFileName),
    {ok, R}.


get_app_group(Dir) ->
    case file:open(filename:join(Dir, "info"), [read]) of
        {ok, F} ->
            case file:read_line(F) of
                {ok, "group:"++Group} ->
                    Val = string:strip(string:strip(Group),right, $\n),
                    case split_at_first_char($\s, Val) of
                        {[], A} ->
                            A;
                        {A, _} ->
                            A
                    end;
                _ ->
                    ""
            end;
        _->
            ""
    end.

split_at_first_char(Char, String) ->
    lists:split(string:chr(String, Char), String).

get_dirs([], _, Acc) ->
    lists:reverse(Acc);
get_dirs([Dir | Rest], Base, Acc) ->
    D = filename:join(Base, Dir),
    case filelib:is_dir(D) of
        true ->
            {ok, Files} = get_lib_files(D),
            get_dirs(Rest, Base, [{D, Files} | Acc]);
        false ->
            get_dirs(Rest, Base, Acc)
    end.

get_lib_files(Dir) ->
    case file:list_dir(Dir) of
        {ok, SrcFiles} ->
            Files = [filename:join(Dir, SrcFile) || SrcFile <- lists:sort(SrcFiles)],
            {ok, lists:filter(fun(F) -> filelib:is_regular(F) end, Files)};
        _ ->
            {ok, []}
    end.

get_includes_in_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            {ok, filter_includes(lists:sort(Files))};
        _ ->
            {ok, []}
    end.

%%
%% Local Functions
%%

filter_includes(Files) ->
    [File || File <- Files, filename:extension(File) == ".hrl"].

get_lib_dir(Dir) ->
    case filename:basename(Dir) of
        "ebin" ->
            filename:dirname(Dir);
        _ ->
            Dir
    end.

has_prefix(Prefix, FileName) ->
    lists:prefix(Prefix, filename:basename(FileName)).

has_name(Name, FileName) ->
    Name == filename:rootname(filename:basename(FileName)).

get_external_modules(Prefix, #open_context{externalModules=ExternalModulesFiles, pathVars=PathVars}) ->
    ExternalModules = get_external_modules_files(ExternalModulesFiles, PathVars),
    {ok, [XM || XM <- ExternalModules, has_prefix(Prefix, XM)]}.

get_external_module_tree(#open_context{externalModules=ExternalModulesFiles, pathVars=PathVars}) ->
    {ok, get_external_module_tree(ExternalModulesFiles, PathVars)}.

get_external_module(Name, #open_context{externalModules=ExternalModulesFiles, pathVars=PathVars}) ->
    ExternalModules = get_external_modules_files(ExternalModulesFiles, PathVars),
    case [XM || XM <- ExternalModules, has_name(Name, XM)] of
        [Path | _] ->
            {ok, Path};
        _ ->
            not_found
    end.

get_external_module_tree(PackedFileNames, PathVars) ->
    Fun = fun(Parent, FileName, Acc) -> [{Parent, replace_path_var(FileName, PathVars), module} | Acc] end,
    Fun2 = fun(Parent, FileName, Acc) -> [{Parent, replace_path_var(FileName, PathVars), entry} | Acc] end,
    FileNames = sourcer_util:unpack(PackedFileNames),
    R = fold_externals(Fun, Fun2, FileNames, PathVars),
    R.

get_external_modules_files(PackedFileNames, PathVars) ->
    ?D(PackedFileNames),
    Fun = fun(_Parent, FileName, Acc) -> [replace_path_var(FileName, PathVars) | Acc] end,
    Fun2 = fun(_Parent, _FileName, Acc) -> Acc end,
    FileNames = sourcer_util:unpack(PackedFileNames),
    R = fold_externals(Fun, Fun2, FileNames, PathVars),
    %%?D(R),
    R.

replace_path_vars(FileNames, PathVars) ->
    [replace_path_var(F, PathVars) || F <- FileNames].

replace_path_var(FileName, PathVars) ->
    case filename:split(FileName) of
        [Var | Rest] ->
            filename:join([replace_path_var_aux(Var, PathVars) | Rest]);
        _ ->
            FileName
    end.

replace_path_var_aux(Var, PathVars) ->
    case lists:keysearch(Var, 1, PathVars) of
        {value, {Var, Value}} ->
            Value;
        _ ->
            Var
    end.

get_external_1(FileName0, PathVars, IsRoot) ->
    FileName = replace_path_var(FileName0, PathVars),
    FileNames = case IsRoot orelse filename:extension(FileName) == ".erlidex" of
                    true ->
                        case file:read_file(FileName) of
                            {ok, B} ->
                                sourcer_util:split_lines(B);
                            _ ->
                                [FileName]
                        end;
                    false ->
                        [FileName]
                end,
    R = replace_path_vars(FileNames, PathVars),
    {ok, R}.

fold_externals(Fun, Fun2, FileNames, PathVars) ->
    {_Done, Acc} = fx(FileNames, Fun, Fun2, PathVars, "root", [], []),
    lists:reverse(Acc).

fx([], _Fun, _Fun2, _PathVars, _Parent, Done, Acc) ->
    {Done, Acc};
fx([FN0 | Rest], Fun, Fun2, PathVars, Parent, Done, Acc) ->
    FN = replace_path_var(FN0, PathVars),
    case lists:member(FN, Done) of
        true ->
            fx(Rest, Fun, Fun2, PathVars, Parent, Done, Acc);
        false ->
            case Parent=:="root" orelse filename:extension(FN) == ".erlidex" of
                true ->
                    {NewDone, NewAcc} = fx2(FN, Fun, Fun2, PathVars, Parent, Done, Acc),
                    fx(Rest, Fun, Fun2, PathVars, Parent, NewDone, NewAcc);
                false ->
                    fx(Rest, Fun, Fun2, PathVars, Parent, [FN | Done], Fun(Parent, FN, Acc))
            end
    end.

fx2(FN, Fun, Fun2, PathVars, Parent, Done, Acc) ->
    NewAcc = Fun2(Parent, FN, Acc),
    case file:read_file(FN) of
        {ok, B} ->
            Lines = sourcer_util:split_lines(B),
            fx(Lines, Fun, Fun2, PathVars, FN, [FN | Done], NewAcc);
        _ ->
            {Done, Acc}
    end.

get_ext_inc([], _) ->
    "";
get_ext_inc([P | Rest], FilePath) ->
    S = filename:join(P, FilePath),
    case filelib:is_regular(S) of
        true ->
            {ok, S};
        false ->
            get_ext_inc(Rest, FilePath)
    end.
