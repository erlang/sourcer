%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%% - some code removed from the original file; also hrl file is inlined
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(sourcer_rebar_config).

-export([consult_root/0
        ,consult/1
        ,consult_app_file/1
        ,consult_file/1
        ,format_error/1
        ]).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).


-define(DEFAULT_BASE_DIR, "_build").
-define(DEFAULT_ROOT_DIR, ".").
-define(DEFAULT_PROJECT_APP_DIRS, ["apps/*", "lib/*", "deps/*", "libs/*", "."]).
-define(DEFAULT_CHECKOUTS_DIR, "_checkouts").
-define(DEFAULT_DEPS_DIR, "lib").
-define(DEFAULT_PLUGINS_DIR, "plugins").
-define(DEFAULT_TEST_DEPS_DIR, "test/lib").

-define(PRV_ERROR(Reason),
        {error, {?MODULE, Reason}}).

-define(DEFAULT_CONFIG_FILE, "rebar.config").

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc reads the default config file at the top of a full project
-spec consult_root() -> [any()].
consult_root() ->
    consult_file(config_file()).

%% @doc reads the default config file in a given directory.
-spec consult(file:name()) -> [any()].
consult(Dir) ->
    consult_file(filename:join(Dir, ?DEFAULT_CONFIG_FILE)).

%% @doc reads a given app file, including the `.script' variations,
%% if any can be found.
-spec consult_app_file(file:filename()) -> [any()].
consult_app_file(File) ->
    consult_file_(File).

%% @doc reads a given config file, including the `.script' variations,
%% if any can be found, and asserts that the config format is in
%% a key-value format.
-spec consult_file(file:filename()) -> [{_,_}].
consult_file(File) ->
    Terms = consult_file_(File),
    true = verify_config_format(Terms),
    Terms.

%% @private reads a given file; if the file has a `.script'-postfixed
%% counterpart, it is evaluated along with the original file.
-spec consult_file_(file:name()) -> [any()].
consult_file_(File) when is_binary(File) ->
    consult_file_(binary_to_list(File));
consult_file_(File) ->
    case filename:extension(File) of
        ".script" ->
            {ok, Terms} = consult_and_eval(remove_script_ext(File), File),
            Terms;
        _ ->
            Script = File ++ ".script",
            case filelib:is_regular(Script) of
                true ->
                    {ok, Terms} = consult_and_eval(File, Script),
                    Terms;
                false ->
                    try_consult(File)
            end
    end.

%% @private checks that a list is in a key-value format.
%% Raises an exception in any other case.
-spec verify_config_format([{_,_}]) -> true.
verify_config_format([]) ->
    true;
verify_config_format([{_Key, _Value} | T]) ->
    verify_config_format(T);
verify_config_format([Term | _]) ->
    throw(?PRV_ERROR({bad_config_format, Term})).

%% @doc convert a given exception's payload into an io description.
-spec format_error(any()) -> iolist().
format_error({bad_config_format, Term}) ->
    io_lib:format("Unable to parse config. Term is not in {Key, Value} format:~n~p", [Term]);
format_error({bad_dep_name, Dep}) ->
    io_lib:format("Dependency name must be an atom, instead found: ~p", [Dep]).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @private consults a consult file, then executes its related script file
%% with the data returned from the consult.
-spec consult_and_eval(File::file:name_all(), Script::file:name_all()) ->
                              {ok, Terms::[term()]} |
                              {error, Reason::term()}.
consult_and_eval(File, Script) ->
    StateData = try_consult(File),
    %% file:consult/1 always returns the terms as a list, however file:script
    %% can (and will) return any kind of term(), to make consult_and_eval
    %% work the same way as eval we ensure that when no list is returned we
    %% convert it in a list.
    case file:script(Script, bs([{'CONFIG', StateData}, {'SCRIPT', Script}])) of
        {ok, Terms} when is_list(Terms) ->
            {ok, Terms};
        {ok, Term} ->
            {ok, [Term]};
        Error ->
            Error
    end.

%% @private drops the .script extension from a filename.
-spec remove_script_ext(file:filename()) -> file:filename().
remove_script_ext(F) ->
    filename:rootname(F, ".script").

%% @private sets up bindings for evaluations from a KV list.
-spec bs([{_,_}]) -> erl_eval:binding_struct().
bs(Vars) ->
    lists:foldl(fun({K,V}, Bs) ->
                        erl_eval:add_binding(K, V, Bs)
                end, erl_eval:new_bindings(), Vars).

%% @private returns the name/path of the default config file, or its
%% override from the OS ENV var `REBAR_CONFIG'.
-spec config_file() -> file:filename().
config_file() ->
    case os:getenv("REBAR_CONFIG") of
        false ->
            ?DEFAULT_CONFIG_FILE;
        ConfigFile ->
            ConfigFile
    end.

-include_lib("kernel/include/file.hrl").

try_consult(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            Terms;
        {error, enoent} ->
            [];
        {error, Reason} ->
            throw(?PRV_ERROR({bad_term_file, File, Reason}))
    end.



-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

read_config_test_() ->
    [
        ?_assertEqual(
            [{require_otp_vsn,"20.*"},{deps,[foo]},{hej,code:priv_dir(sourcer)++"/test_rebar.config.script"}], 
            consult_file(code:priv_dir(sourcer)++"/test_rebar.config")
        )
    ].

-endif.
