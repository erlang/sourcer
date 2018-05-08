%%% @doc Support the basic discovery of apps
%%% and layout for the project. 
-module(sourcer_layout).

-export([
    detect_layouts/1,
    detect_layout/1
]).

-include("debug.hrl").
-include("sourcer_model.hrl").

-define(DEFAULT_REBAR_BUILD_DIR, "_build").
-define(DEFAULT_PROJECT_APP_DIRS, ["apps", "lib", "deps", "libs"]).
-define(DEFAULT_REBAR_CHECKOUTS_DIR, "_checkouts").
-define(DEFAULT_TEST_DEPS_DIR, "test/lib").

%% Assume Dir is root of a project. 
detect_layouts(Dirs) ->
    lists:flatten([detect_layout(D) || D<-Dirs]).

detect_layout(Dir) ->
    case find_erlide_config(Dir) of
        [] ->
             find_rebar_config(Dir);
        Cfg ->
            Cfg 
    end.

find_erlide_config(_Dir) ->
    [].

find_rebar_config(Dir) ->
    [#project{name=Dir, location=Dir}].

%% TODO implement real detection of layout

