-module(sourcer_util).

-export([
    pack/1, 
    unpack/1, 
    join/2, 
    path_to_uri/1, 
    uri_to_path/1
]).
-export([
    reverse2/1
]).
-export([
    get_text_range/2
]).
-export([
    get_auto_imported/1, 
    add_auto_imported/1
]).

%%-define(DEBUG, true).
-include("debug.hrl").

-define(SEP, ";").

unpack(F) ->
    string:tokens(F, ?SEP).

pack(L) ->
    join(L, ?SEP).

reverse2(L) when is_list(L) ->
    lists:reverse([lists:reverse(A) || A <- L]).

join([], Sep) when is_list(Sep) ->
    [];
join([H|T], Sep) ->
    H ++ lists:append([Sep ++ X || X <- T]).

add_auto_imported(Imports) ->
    [{erlang, get_auto_imported("")} | Imports].

get_auto_imported(Prefix) when is_list(Prefix) ->
    case catch erlang:module_info(exports) of
        Val when is_list(Val) ->
            lists:filter(fun({N, A}) ->
                                 lists:prefix(Prefix, atom_to_list(N)) andalso
                                     erl_internal:bif(N, A)
                         end, Val);
        _Error ->
            ?D(_Error),
            error
    end.

get_text_range(Text, none) ->
    <<"">>;
get_text_range(Text, Range) ->
    {{L1, C1},{L2,C2}} = Range,
    {ok, RE} = re:compile("\\R", [multiline, {newline, anycrlf}, unicode]),
    case re:run(Text, RE, [global]) of
        {match, Lines} ->
            [{X1, X2}] = lists:nth(L1, Lines),
            [{Y1, Y2}] = lists:nth(L2, Lines),
            From = X1 + X2 + C1 - 1,
            Length = Y1 + Y2 + C2 - From,
            string:slice(Text, From, Length);
        nomatch ->
            ?D("NO LINES????"),
            <<"">>
    end.

%% Transforms URI into file path
uri_to_path(URI) ->
    {ok, {file,_,_,_,Path,_}=Fragments} = http_uri:parse(unicode:characters_to_list(URI),
            [{scheme_defaults,[{file,1}]}]
    ),
    Path1 = http_uri:decode(Path),
    case lists:member($:, Path1) of 
        true ->
            Path2 = case Path1 of
                "/"++Path2 -> Path2;
                Path2 -> Path2
            end,
            lists:flatten(string:replace(Path2, "/", "\\", all));
        false ->
            Path1
    end.

%% Transforms an absolute file path into a URI as used by the language server protocol.
path_to_uri(Path) ->
    Path1 = string:strip(lists:flatten(string:replace(Path, "\\", "/", all)), both, $/),
    [H|T] = Parts = string:split(Path1, "/", all),
    First = case lists:member($:, H) of
        false ->
            H;
        true ->
            http_uri:encode(H)
    end,
    Path2 = string:join([First | [http_uri:encode(X) || X<-T]], "/"),
    unicode:characters_to_binary("file:///"++Path2).

