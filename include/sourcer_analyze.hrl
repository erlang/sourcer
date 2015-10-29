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
