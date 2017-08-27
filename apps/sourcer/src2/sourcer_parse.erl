%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(sourcer_parse).

%% called from Java
-export([initial_parse/5, reparse/2]).

%% called from Erlang
-export([get_module_refs/4]).

%-define(DEBUG, 1).


-define(SERVER, sourcer_noparse).

-include("debug.hrl").
-include("sourcer_parse.hrl").
-include("sourcer_scanner_server.hrl").
-include("sourcer_search.hrl").

%%
%% API Functions
%%

-spec initial_parse(atom(), string(), string(), string(), boolean()) ->
          {ok, #model{}, [#ref{}]}
              | {error, term(), term()}.

initial_parse(ScannerName, ModuleFileName, InitialText, StateDir, 
              UpdateSearchServer) ->
    try
        ?D({ScannerName, ModuleFileName, UpdateSearchServer}),
        RenewFun = fun(_F) ->
                           Tokens = get_tokens(ScannerName, ModuleFileName,
                                               InitialText, StateDir),
                           {Model, Refs} =
                               do_parse(ScannerName, Tokens, UpdateSearchServer),
                           {Model, Refs}
                   end,
        {Model, Refs} = RenewFun(ModuleFileName),
        {ok, Model, Refs}
    catch
        error:Reason ->
            {error, Reason, erlang:get_stacktrace()}
    end.

-spec reparse(atom(), boolean()) ->
          {ok, #model{}}
              | {error, term(), term()}.
reparse(ScannerName, UpdateSearchServer) ->
    try
        Tokens = sourcer_scanner:get_tokens(ScannerName),
        {Model, _Refs} = do_parse(ScannerName, Tokens, UpdateSearchServer),
        {ok, Model}
    catch
        error:Reason ->
            {error, Reason, erlang:get_stacktrace()}
    end.

-spec get_module_refs(atom(), string(), string(), boolean()) -> [#ref{}].
get_module_refs(ScannerName, ModulePath, StateDir, UpdateSearchServer) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    %% TODO: shouldn't we check that .refs is up-to-date? using renew
    %% function etc. would probably be more straight-forward...
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            binary_to_term(Binary);
        _ ->
            InitialText = case file:read_file(ModulePath) of
                              {ok, InitialTextBin} ->
                                  unicode:characters_to_list(InitialTextBin);
                              _ ->
                                  ""
                          end,
            {ok, _, _, Refs} = initial_parse(ScannerName, ModulePath, InitialText,
                                             StateDir, UpdateSearchServer),
            Refs
    end.

%%
%% Internal functions
%%

get_tokens(ScannerName, ModuleFileName, InitialText, StateDir) ->
    case whereis(ScannerName) of
        undefined ->
            Module = sourcer_scanner:initial_scan_0(ScannerName, ModuleFileName, InitialText, StateDir),
            sourcer_scan_model:get_all_tokens(Module);
        _ ->
            sourcer_scanner:get_tokens(ScannerName)
    end.

do_parse(ScannerName, Tokens, UpdateSearchServer) ->
    {Forms, Comments, References} = sourcer_np:parse(Tokens),
    ?D(Forms),
    Model = #model{forms=Forms, comments=Comments},
    CompactModel = sourcer_np_util:compact_model(Model),
    ?D(CompactModel),
    update_search_server(UpdateSearchServer, ScannerName, References),
    {CompactModel, References}.

update_search_server(true, ScannerName, Refs) ->
    sourcer_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

