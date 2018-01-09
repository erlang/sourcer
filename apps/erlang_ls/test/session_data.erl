-module(session_data).

-export([
    all_tests/0
]).

all_tests() ->
    [
        {initialize_request(), initialize_response()}
    ].

initialize_request() ->
    #{capabilities =>
            #{textDocument =>
                #{codeAction => #{dynamicRegistration => true},
                    codeLens => #{dynamicRegistration => true},
                    completion =>
                        #{completionItem =>
                            #{snippetSupport => true},
                        dynamicRegistration => true},
                    definition => #{dynamicRegistration => true},
                    documentHighlight =>
                        #{dynamicRegistration => true},
                    documentLink =>
                        #{dynamicRegistration => true},
                    documentSymbol =>
                        #{dynamicRegistration => true},
                    formatting => #{dynamicRegistration => true},
                    hover => #{dynamicRegistration => true},
                    onTypeFormatting =>
                        #{dynamicRegistration => true},
                    rangeFormatting =>
                        #{dynamicRegistration => true},
                    references => #{dynamicRegistration => true},
                    rename => #{dynamicRegistration => true},
                    signatureHelp =>
                        #{dynamicRegistration => true},
                    synchronization =>
                        #{didSave => true,
                        dynamicRegistration => true,
                        willSave => true,
                        willSaveWaitUntil => true}},
            workspace =>
                #{applyEdit => true,
                    didChangeConfiguration =>
                        #{dynamicRegistration => false},
                    didChangeWatchedFiles =>
                        #{dynamicRegistration => true},
                    executeCommand =>
                        #{dynamicRegistration => true},
                    symbol => #{dynamicRegistration => true},
                    workspaceEdit =>
                        #{documentChanges => true}}},
        processId => 16538,
        rootPath => <<"/home/vlad/projects/rebar3">>,
        rootUri => <<"file:///home/vlad/projects/rebar3">>,
        trace => <<"off">>
    }.

initialize_response() ->
    #{capabilities =>
        #{completionProvider =>
            #{resolveProvider => true,
                triggerCharacters => [<<":">>,<<"?">>,<<"#">>]
            },
            definitionProvider => true,
            documentHighlightProvider => true,
            documentSymbolProvider => true,
            hoverProvider => true,
            referencesProvider => true,
            renameProvider => true,
            signatureHelpProvider => #{triggerCharacters => [<<"(">>]},
            textDocumentSync => 1,
            workspaceSymbolProvider => true
        }
    }.
                
didChangeConfiguration_notification() ->
    #{settings =>
        #{erlang =>
                #{erlangPath => <<>>,
                runtime =>
                    #{location =>
                            <<"/home/vlad/erlide_tools/20.0">>},
                server =>
                    #{debug =>
                            <<"false">>,
                        maxNumberOfProblems =>
                            100}}}
    }.

didOpen_notification() ->
    #{textDocument =>
        #{languageId => <<"erlang">>,
            text =>
                <<"%%% @doc external alias for `rebar_agent' for more convenient\n%%% calls from a shell.\n-module(r3).\n-export([do/1, do/2]).\n-export(['$handle_undefined_function'/2]).\n\n%% @doc alias for `rebar_agent:do/1'\n-spec do(atom()) -> ok | {error, term()}.\ndo(Command) -> rebar_agent:do(Command).\n\n%% @doc alias for `rebar_agent:do/2'\n-spec do(atom(), atom()) -> ok | {error, term()}.\ndo(Namespace, Command) -> rebar_agent:do(Namespace, Command).\n\n%% @private defer to rebar_agent\n'$handle_undefined_function'(Cmd, Args) ->\n    rebar_agent:'$handle_undefined_function'(Cmd, Args).\n">>,
            uri =>
                <<"file:///home/vlad/projects/rebar3/src/r3.erl">>,
            version => 1}
    }.

documentSymbol_request() ->
    #{textDocument =>
        #{uri =>
            <<"file:///home/vlad/projects/rebar3/src/r3.erl">>}
    }.

documentSymbol_response() ->
    #{
    }.

hover_request() ->
    #{
        position => #{character => 9,line => 16},
        textDocument =>
            #{uri =>
                    <<"file:///home/vlad/projects/rebar3/src/r3.erl">>}
    }.

hover_response() ->
    #{contents => []}.

