%%% ******************************************************************************
%%%  Copyright (c) 2009 Vlad Dumitrescu and others.
%%%  All rights reserved. This program and the accompanying materials
%%%  are made available under the terms of the Eclipse Public License v1.0
%%%  which accompanies this distribution, and is available at
%%%  http://www.eclipse.org/legal/epl-v10.html
%%%
%%%  Contributors:
%%%      Vlad Dumitrescu
%%% ******************************************************************************/
%%%
%%% The DB contains info about the source locations of relevant code constructs.
%%% It is kind of like a ctags DB, but has more information.
%%%
-module(sourcer_types).

-export([
		 definition/2,
		 reference/3
		]).

-export_type([
			  range/0,
			  file_key/0,
			  file_props/0,

			  edefinition/0,
			  ereference/0
			 ]).

%-type kind() :: 'module' | 'include' | 'function' | 'macro' | 'record' | 'field' | 'variable' | 'type' | 'ifdef'.
-type range() :: {Line::integer(), Column::integer(), Offset::integer()} | 'undefined'.

-define(COMMON_PROPS, keyRange := range(), defRange := range()).

-type file_key() :: #{kind:='file', location:=string()}.
-type file_props() :: #{is_library:=boolean(),
						?COMMON_PROPS
					   }.

-type module_key() :: #{kind:='module', name:=atom()}.
-type module_props() :: #{file:=file_key(), includes:=[file_key()],
						  ?COMMON_PROPS
						 }.

-type include_key() :: #{file:=file_key(), target:=file_key()}.
-type include_props() :: #{is_library:=boolean(),
						   ?COMMON_PROPS
						  }.

-type function_key() :: #{kind:='function', module:=module_key(), name:=atom(), arity:=integer()}.
-type function_props() :: #{is_exported:=boolean(),
							?COMMON_PROPS
						   }.

-type type_key() :: #{kind:='type', module:=module_key(), name:=atom(), arity:=integer()}.
-type type_props() :: #{is_exported:=boolean(),
						?COMMON_PROPS
					   }.

-type macro_key() :: #{kind:='macro', file:=file_key(), name:=string(), arity:=integer()}.
-type macro_props() :: #{
						 ?COMMON_PROPS
						}.

-type record_key() :: #{kind:='record', file:=file_key(), name:=string()}.
-type record_props() :: #{
						  ?COMMON_PROPS
						 }.

-type field_key() :: #{kind:='field', record:=record_key(), name:=string()}.
-type field_props() :: #{
						 ?COMMON_PROPS
						}.

-type parent_key() :: function_key() | macro_key().
-type variable_key() :: #{kind:='variable', parent:=parent_key(), name:=string(), index:=integer()}.
-type variable_props() :: #{
							?COMMON_PROPS
						   }.

-type ifdef_key() :: #{kind:='ifdef', file:=file_key(), condition:=macro_key(), index:=integer()}.
-type ifdef_props() :: #{if_range:=range(), else_range:=range(),
						 ?COMMON_PROPS}.


-type key() :: file_key() | module_key() | include_key() | function_key() | type_key()
		  | macro_key() | record_key() | field_key() | variable_key() | ifdef_key().


-type props() :: file_props() | module_props() | include_props() | function_props() | type_props()
		  | macro_props() | record_props() | field_props() | variable_props() | ifdef_props().

-record(definition, {
					 key :: key(),
					 props :: props()
					}).

-record(reference, {
					key :: key(),
					file :: file_key(),
					range :: range()
				   }).

-type edefinition() :: #definition{}.
-type ereference() :: #reference{}.

definition(Key, Props) ->
	#definition{key=Key, props=Props}.

reference(Key, File, Range) ->
	#reference{key=Key, file=File, range=Range}.

