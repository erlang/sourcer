%%% Copyright 2015 Vlad Dumitrescu
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%% 
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% 

-module(erlide_analyze).

-export([
         analyze_module/1,
         analyze_references/1
         ]).

-ifndef(TEST).
-define(TEST, true).
-endif.

%% for tests
-ifdef(TEST).
-compile(export_all).
-endif.

-type attrs() :: [{atom(),term()}].
-type token() :: {atom(), attrs()} | {atom(), attrs(), term()}.
-type location() :: map().

%% TODO will have to think about this
%% this reflects the entities directly defined in this module or header
-record(module, {
                 name :: atom(),
                 file="", % for headers, instead of name
                 includes=[],
                 include_libs=[],
                 records=[],
                 macros=[],
                 functions=[],
                 exports=[],
                 imports=[],
                 types=[],
                 specs=[],
                 type_exports=[],
                 other=[]
                }).

-record(function, {name, arity, pos, clauses=[]}).

-record(type_id, {module::atom(), name::atom(), arity::integer()}).
%-record(module_id, name::atom()).
-record(function_id, {module::atom(), name::atom(), arity::integer()}).
-record(macro_id, {module::atom(), name::atom()}).
%-record(variable_id, name::atom()).
-record(record_id, {module::atom(), name::atom()}).
-record(recordfield_id, {record::#record_id{}, name::atom()}).


-record(typeref, {loc::location(), id::#type_id{}}).
-record(moduleref, {loc::location(), id::atom()}).
-record(functionref, {loc::location(), id::#function_id{}}).
-record(macroref, {loc::location(), id::#macro_id{}}).
-record(variableref, {loc::location(), id::atom()}).
-record(recordref, {loc::location(), id::#record_id{}}).
-record(recordfieldref, {loc::location(), id::#recordfield_id{}}).

-type ref() :: #functionref{} | #recordref{} | #recordfieldref{}
          | #macroref{} | #variableref{} | #moduleref{}.

-type form() :: tuple().

%% XXX structure around ifdefs!?

%% Gather the relevant information about a module, from each form.
-spec analyze_module([form()]) -> #module{}.
analyze_module(Mod) ->
    Forms = group_forms(Mod),

    Name = case proplists:get_value(module, Forms) of
               [{module, _, AName, _}|_] ->
                   AName;
               _ ->
                   %% header
                   ''
           end,
    Includes = proplists:get_value(include, Forms, []),
    IncludeLibs = proplists:get_value(include_lib, Forms, []),
    Records = proplists:get_value(record, Forms, []),
    Macros = proplists:get_value(define, Forms, []),
    Functions = proplists:get_value(function, Forms, []),
    Types = proplists:get_value(type, Forms, []) ++ proplists:get_value(opaque, Forms, []),
    Specs = proplists:get_value(spec, Forms, []),
    Exports = proplists:get_value(export, Forms, []),
    Imports = proplists:get_value(import, Forms, []),
    TypeExports = proplists:get_value(export_type, Forms, []),
    Other = proplists:get_value(other, Forms, []),
    #module{name=Name, includes=Includes, include_libs=IncludeLibs, records=Records,
            macros=Macros, functions=Functions, types=Types, specs=Specs,
            exports=Exports,imports=Imports, type_exports=TypeExports, other=Other}.

%% Given a list of tokens, identify and return references to named entities
%% (modules, functions, macros, variables, records, record fields)
%% We need to scan for macros separately so that we can detect overlapped references
%% like '?M:f'
-spec analyze_references([token()]) -> [ref()].
analyze_references(Toks) when is_list(Toks) ->
    M = analyze_macro_references(Toks, []),
    R = analyze_references(Toks, []),
    M ++ R.

analyze_references([], Acc) ->
    lists:reverse(Acc);
analyze_references([{'fun',Pos1},{atom,_,Name},{'/',_},{integer,Pos2,Arity}|Toks], Acc) ->
    Ref = #functionref{loc=mk_loc(Pos1, Pos2),
                       id=#function_id{module='$this$', name=Name, arity=Arity}},
    analyze_references(Toks, [Ref|Acc]);
analyze_references([{'fun',Pos1},{atom,_,NameM},{':',_},{atom,_,NameF},{'/',_},{integer,Pos2,Arity}|Toks], Acc) ->
    Ref = #functionref{loc=mk_loc(Pos1, Pos2),
                       id=#function_id{module=NameM, name=NameF, arity=Arity}},
    analyze_references(Toks, [Ref|Acc]);
analyze_references([{atom,Pos1,NameF},{'(',_}=T|Toks], Acc) ->
    Arity = get_arity([T|Toks]),
    Ref = #functionref{loc=mk_loc(Pos1, Pos1),
                       id=#function_id{module='$this$', name=NameF, arity=Arity}},
    analyze_references(Toks, [Ref|Acc]);
analyze_references([{atom,Pos1,NameM},{':',_},{atom,Pos2,NameF}|Toks], Acc) ->
    Arity = get_arity(Toks),
    Ref = #functionref{loc=mk_loc(Pos1, Pos2),
                       id=#function_id{module=NameM, name=NameF, arity=Arity}},
    analyze_references(Toks, [Ref|Acc]);
analyze_references([{macro,Pos1,NameM},{':',_},{atom,Pos2,NameF}|Toks], Acc) ->
    Arity = get_arity(Toks),
    Ref = #functionref{loc=mk_loc(Pos1, Pos2),
                       id=#function_id{module=NameM, name=NameF, arity=Arity}},
    analyze_references(Toks, [Ref|Acc]);
analyze_references([{atom,Pos1,NameM},{':',_},{macro,Pos2,NameF}|Toks], Acc) ->
    Arity = get_arity(Toks),
    Ref = #functionref{loc=mk_loc(Pos1, Pos2),
                       id=#function_id{module=NameM, name=NameF, arity=Arity}},
    analyze_references(Toks, [Ref|Acc]);
analyze_references([{'#',Pos1},{atom,_,Name},{'.',_},{atom,Pos2,Field}|Toks], Acc) ->
    %% TODO fill module name
    Ref = #recordfieldref{loc=mk_loc(Pos1, Pos2),
                          id=#recordfield_id{record=#record_id{module='$this$', name=Name}, name=Field}},
    analyze_references(Toks, [Ref|Acc]);
analyze_references([{'#',Pos1},{atom,Pos2,Name}|Toks], Acc) ->
    Ref = #recordref{loc=mk_loc(Pos1, Pos2), id=Name},
    analyze_references(Toks, [Ref|Acc]);
analyze_references([{'#',Pos1},{macro,Pos2,Name}|Toks], Acc) ->
    Ref = #recordref{loc=mk_loc(Pos1, Pos2), id=Name},
    analyze_references(Toks, [Ref|Acc]);
analyze_references([{var,Pos1,Name}|Toks], Acc) ->
    Ref = #variableref{loc=mk_loc(Pos1, Pos1), id=Name},
    analyze_references(Toks, [Ref|Acc]);
analyze_references([_|Toks], Acc) ->
    analyze_references(Toks, Acc).

analyze_macro_references([], Acc) ->
    lists:reverse(Acc);
analyze_macro_references([{macro,Pos1,Name}|Toks], Acc) ->
    Ref = #macroref{loc=mk_loc(Pos1, Pos1), id=Name},
    analyze_macro_references(Toks, [Ref|Acc]);
analyze_macro_references([_|Toks], Acc) ->
    analyze_macro_references(Toks, Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mk_loc(Start, End) ->
    Line = maps:get(line, Start),
    Col = maps:get(column, Start),
    Offset = maps:get(offset, Start),
    Text = maps:get(text, End),
    EndOffset = maps:get(offset, End),
    Length = EndOffset + length(Text) - Offset,
    #{line=>Line, column=>Col, offset=>Offset, length=>Length}.

module_keys() ->
    [
     module, include, include_lib, function, record, type, opaque,
     export, import
    ].

group_forms(Forms)  ->
    group_forms(module_keys(), Forms, []).

group_forms(_, [], Acc) ->
    Acc;
group_forms(Keys, [H|T], Acc) ->
    Key = element(1, H),
    Key1 = case lists:member(Key, Keys) of
               true ->
                   Key;
               false ->
                   other
           end,
    Val = proplists:get_value(Key1, Acc, []),
    Acc1 = lists:keystore(Key1, 1, Acc, {Key1, [H|Val]}),
    group_forms(Keys, T, Acc1).

get_arity([{'(',_}|_]=Ts) ->
    {A,_} = erlide_parse:split_at_brace(Ts),
    length(erlide_parse:split_at_comma(erlide_scan_util:middle(A)));
get_arity(_) ->
    -1.
