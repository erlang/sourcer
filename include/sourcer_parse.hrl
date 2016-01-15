-type source_encoding() :: 'latin1' | 'utf8'.

-record(context, {
                  %% the content provider for retrieving file content
                  provider = undefined :: sourcer_content_provider:maybe_provider(),

                  %% location (path) of current file
                  file_name = "",

                  default_encoding = utf8 :: source_encoding(),

                  %% from compiler options
                  include_dirs = [],

                  %% currently defined macros with arity
                  %% initialized from compiler options
                  macros = [],

                  %% macros defined/undefined at this point
                  defines = sets:new(),

                  %% ifdef/ifndef situation at this point
                  active = [],

                  %% names of files included so far
                  included = []
                 }).

-type context() :: #context{}.

