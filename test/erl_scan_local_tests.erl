% coding: utf-8

-module(erl_scan_local_tests).

-include_lib("eunit/include/eunit.hrl").

-define(Z, erl_scan_local).

%% Dummy tests to fix coverage
dummy_test_() ->
    ?Z:format_error({string, q, ""}),
    ?Z:format_error({illegal, q}),
    ?Z:format_error(char),
    ?Z:format_error({base, q}),
    ?Z:format_error(foo),

    ?Z:string("x"),
    ?Z:string("x", 1),

    ?Z:tokens([], "", 1),
    ?Z:tokens([], "", {1,1}, []),

    ?Z:continuation_location({erl_scan_continuation,1,1,1,1,1,1,1}),
    ?Z:continuation_location({erl_scan_continuation,1,no_col,1,1,1,1,1}),

    ?Z:token_info({atom,[{line,1},{column,1},{text,""}], a}),
    ?Z:token_info({atom,[{line,1},{column,1},{text,""}]}),
    ?Z:attributes_info({1,2}),
    ?Z:attributes_info(1),

    ?Z:set_attribute(line, 1, fun(X)->X end),
    ?Z:set_attribute(line, 1, fun(_X)->true end),
    ?Z:set_attribute(line, {1,2}, fun(X)->X end),
    ?Z:set_attribute(line, {1,2}, fun(_X)->true end),
    ?Z:set_attribute(line, [{line,1},{column,1},{text,""}], fun(X)->X end),
    ?Z:set_attribute(line, [{line,1},{column,1},{text,""}], fun(_X)->true end),

    ?Z:string("a@éÁs. \n X $À _S \ng.%k\n ... .. . \r \t a'd", 1, []),
    ?Z:string("as \n X _S $÷ $×  \n%k\n ... .. . \n\r\n\f \t a'd", 1, [return,text]),
    ?Z:string("<< <- <= < >> >= > -> -- - ++ + =:= =: =/= =/ =< => == = /=", 1, [return]),
    ?Z:string("/ || | := =- :: : * < > ! @ \\ ^ ` ~ &  é ", 1, [return]),
    ?Z:string("'a\n\fb' \"a\nb\" ", 1, [return]),
    ?Z:string("$\\n$\\r$\\t$\\v$\\b$\\f$\\e$\\d $ $
$\\", 1, [return,text]),
    ?Z:string("$\\n$\\r$\\t$\\v$\\b$\\f$\\e$\\d $ $
$\\", 1, [return]),
    ?Z:string("\"\\n\\r\\t\\v\\b\\f\\e\\d\"", 1, [return, text]),
    ?Z:string("\"\\n\\r\\t\\v\\b\\f\\e\\d\"", 1, [return]),
    ?Z:string("x
              y", 1, [return]),
    ?Z:string("2.5 3.0e2 3#1 3#6 $x $\\s $\\^X ${face} $\\1 $\\12 $\\123 $\\x{1234} $\\x23 $\\x2 $\\xg ..", 1, [return]),
    ?Z:string("\"$x \\s \\^X {face} \\1 $\\12 $\\123 $\\x{1234} $\\x23 $\\x2 $\\xg\"", 1, [return]),
    ?Z:string("\r\n  a\n   a\n    a\n\t\t          ", 1, [return]),
    lists:foreach(fun(X)->?Z:string(atom_to_list(X), 1, [return]) end,
                  ['after', 'begin', 'case', 'try', 'cond', 'catch', 'andalso', 'orelse',
                   'end', 'fun', 'if', 'let', 'of', 'receive', 'when', 'bnot', 'not', 'div',
                   'rem', 'band', 'and', 'bor', 'bxor', 'bsl', 'bsr', 'or', 'xor']),
    lists:foreach(fun(X)->?Z:string(X, 1, [return]) end,
                  lists:map(fun(X)->
                                    lists:duplicate(X, $\s)++
                                        "z\n"++
                                        lists:duplicate(X, $\s)++
                                        "x\nx\n"++
                                        lists:duplicate(X, $\t)++
                                        "v"++
                                        lists:duplicate(X, $\t)
                            end,
                            lists:seq(1,17))),
    lists:foreach(fun(X)->?Z:string(X, 1, []) end,
                  lists:map(fun(X)->
                                    lists:duplicate(X, $\s)++
                                        "z\n"++
                                        lists:duplicate(X, $\s)++
                                        "x\nx\n"++
                                        lists:duplicate(X, $\t)++
                                        "v"++
                                        lists:duplicate(X, $\t)
                            end,
                            lists:seq(1,17))),

    [].
