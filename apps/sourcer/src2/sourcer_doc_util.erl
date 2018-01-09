-module(sourcer_doc_util).

-export([
    get_between_strs/3, 
    get_all_between_strs/3, 
    get_from_str/2,
    get_upto_str/2
]).

-include("debug.hrl").

get_from_str(Text, Start) ->
    case string:str(Text, Start) of
        0 ->
            Text;
        N ->
            string:substr(Text, N + length(Start))
    end.

get_between_strs(Text, Start, End) ->
    get_upto_str(get_from_str(Text, Start), End).

get_all_between_strs(Text, Start, End) ->
    {One, Next} = split_at(get_from_str(Text, Start), End),
    case Next of
        "" ->
            [One];
        _ ->
            [One | get_all_between_strs(Next, Start, End)]
    end.

get_upto_str(Text, End) ->
    case string:rstr(Text, End) of
        0 ->
            Text;
        N ->
            string:substr(Text, 1, N-1)
    end.

split_at(Text, End) ->
    case string:str(Text, End) of
        0 ->
            {Text, ""};
        N ->
            {string:substr(Text, 1, N-1), string:substr(Text, N+length(End))}
    end.

