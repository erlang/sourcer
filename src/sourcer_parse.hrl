-ifndef(TEST).
-define(TEST, true).
-endif.

-ifdef(TEST).
-compile(export_all).
-endif.

-record(context, {
                  %% location (path) of current file
                  %% TODO what about working copies (i.e.
                  %% code in a buffer not saved yet)?
                  file,

                  %% from compiler options
                  include_dirs=[],

                  %% currently defined macros with arity
                  %% initialized from compiler options
                  macros=[],

                  %% macros defined/undefined at this point
                  defines=sets:new(),

                  %% ifdef/ifndef situation at this point
                  active=[],

                  %% names of files included so far
                  included=[]
                 }).

