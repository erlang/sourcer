%% Author: jakob
%% Created: 24 apr 2008
%% Description:
-module(sourcer_scanner).

%% -define(DEBUG, 1).

-include("debug.hrl").
-include("sourcer_token.hrl").
-include("sourcer_scanner_server.hrl").

-export([light_scan_string/2, scan_string/1, initial_scan_0/4, initial_scan/4,
         get_token_at/2,
         create/1, addref/1, dispose/1, get_text/1,
         get_tokens/1, get_token_window/4,
         dump_module/1, replace_text/4]).

%%
%% API Functions
%%

light_scan_string(B, latin1) ->
    S = unicode:characters_to_list(B),
    do_light_scan(S);
light_scan_string(B, utf8) ->
    S = unicode:characters_to_list(B),
    do_light_scan(S).

scan_string(B) when is_binary(B) ->
    scan_string(binary_to_list(B));
scan_string(L) when is_list(L) ->
    M = sourcer_scan_model:do_scan('', L),
    sourcer_scan_model:get_all_tokens(M).

initial_scan_0(ScannerName, ModuleFileName, Text, _StateDir) ->
    RenewFun = fun(_F) -> sourcer_scan_model:do_scan(ScannerName, Text) end,
    RenewFun(ModuleFileName).

get_token_at(ScannerName, Offset) when is_atom(ScannerName), is_integer(Offset) ->
    sourcer_scanner_server:server_cmd(ScannerName, get_token_at, Offset).

initial_scan(ScannerName, ModuleFileName, InitialText, StateDir)
  when is_atom(ScannerName), is_list(ModuleFileName), is_list(InitialText), is_list(StateDir) ->
    sourcer_scanner_server:server_cmd(ScannerName, initial_scan,
                                     {ScannerName, ModuleFileName, InitialText, StateDir}).

create(ScannerName) when is_atom(ScannerName) ->
    sourcer_scanner_server:spawn_server(ScannerName).

addref(ScannerName) when is_atom(ScannerName) ->
    sourcer_scanner_server:server_cmd(ScannerName, addref).

dispose(ScannerName) when is_atom(ScannerName) ->
    sourcer_search_server:remove_module(ScannerName),
    sourcer_scanner_server:server_cmd(ScannerName, dispose).

get_text(ScannerName) when is_atom(ScannerName) ->
    sourcer_scanner_server:server_cmd(ScannerName, get_text).

get_tokens(ScannerName) when is_atom(ScannerName) ->
    sourcer_scanner_server:server_cmd(ScannerName, get_tokens).

get_token_window(ScannerName, Offset, Before, After)
  when is_atom(ScannerName), is_integer(Offset), is_integer(Before), is_integer(After) ->
    sourcer_scanner_server:server_cmd(ScannerName, get_token_window, {Offset, Before, After}).

dump_module(ScannerName) when is_atom(ScannerName) ->
    sourcer_scanner_server:server_cmd(ScannerName, dump_module).

replace_text(ScannerName, Offset, RemoveLength, NewText)
  when is_atom(ScannerName), is_integer(Offset), is_integer(RemoveLength), is_list(NewText) ->
    sourcer_scanner_server:server_cmd(ScannerName, replace_text, {Offset, RemoveLength, NewText}).

%%
%% Local Functions
%%

do_light_scan(S) ->
    case sourcer_scan:string(S, {0, 1}, [return]) of
        {ok, T, _} ->
            {ok, convert_tokens(T)};
        {error, _, _} ->
            error
    end.

-define(TOK_OTHER, 0).
-define(TOK_WS, 1).
-define(TOK_STR, 2).
-define(TOK_ATOM, 3).
-define(TOK_VAR, 4).
-define(TOK_CHAR, 5).
-define(TOK_MACRO, 6).
-define(TOK_ARROW, 7).
-define(TOK_INTEGER,8).
-define(TOK_FLOAT, 9).
-define(TOK_COMMENT, 10).
-define(TOK_KEYWORD, 11).

kind_small(ws) -> ?TOK_WS;
kind_small(white_space) -> ?TOK_WS;
kind_small(string) -> ?TOK_STR;
kind_small(atom) -> ?TOK_ATOM;
kind_small(var) -> ?TOK_VAR;
kind_small(macro) -> ?TOK_MACRO;
kind_small(dot) -> ?TOK_OTHER;
kind_small(float) -> ?TOK_FLOAT;
kind_small(integer) -> ?TOK_INTEGER;
kind_small(char) -> ?TOK_CHAR;
kind_small('->') -> ?TOK_ARROW;
kind_small(comment) -> ?TOK_COMMENT;
kind_small(Kind) when is_atom(Kind) ->
    case sourcer_scan:reserved_word(Kind) of
        true ->
            ?TOK_KEYWORD;
        false ->
            case atom_to_list(Kind) of
                [I] when I > ?TOK_KEYWORD -> I;
                _ -> ?TOK_OTHER
            end
    end.

convert_tokens(Tokens) ->
    Fun = fun(#token{kind=Kind, line=L, offset=O, text=Txt}) ->
                  G = case is_list(Txt) of true -> length(Txt); _ -> byte_size(Txt) end,
                  <<(kind_small(Kind)), L:24, O:24, G:24>>
          end,
    list_to_binary(lists:flatten([Fun(X) || X <- Tokens])).

