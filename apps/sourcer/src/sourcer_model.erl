-module(sourcer_model).

-export([
    load_model/1,
    save_model/2,
    get_elements_at_pos/2,
    print_key/1
]).

-include("sourcer_model.hrl").

-define(DEBUG, true).
-include("debug.hrl").

-ifdef(DEBUG).

load_model(File) ->
    {ok, [Model]} = file:consult(File),
    Model.

save_model(File, Model) ->
    ok = file:write_file(File, io_lib:format("~tp.~n", [Model])),
    Model.

-else.

load_model(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_term(Bin).

save_model(File, Model) ->
    ok = file:write_file(File, term_to_binary(Model)),
    Model.

-endif.

get_elements_at_pos(Model, {L, C}) ->
    #model{defs=Defs, refs=Refs} = Model,
    MyPos = {L, C},
    Defs1 = lists:filter(fun(#def{name_range=Y,info=X})->
                case maps:is_key(body, X) of
                    true ->
                        pos_between(MyPos, maps:get(body, X));
                    _ ->
                        false
                end
            end, Defs),
    Defs2 = lists:filter(fun(#def{name_range=X})-> pos_between(MyPos, X) end, Defs),
    Refs1 = lists:filter(fun(#ref{range=X})-> pos_between(MyPos, X) end, Refs),
    {Defs1, Refs1++Defs2};
get_elements_at_pos(Model, Pos) ->
    #{line:=L, character:=C} = Pos, 
    get_elements_at_pos(Model, {L, C}).

print_key(Key) ->
    unicode: characters_to_binary(print_key_aux(Key)).

pos_between(_Crt, none) ->
    false;
pos_between(Crt, {Start, End}) ->
    Start =< Crt andalso Crt < End.

print_key_aux(Key) when is_list(Key) ->
    [print_key_aux(K) || K<-Key];
print_key_aux({module, M}) ->
    io_lib:format("~w:", [M]);
print_key_aux({include, F}) ->
    io_lib:format("\"~s\"", [F]);
print_key_aux({include_lib, F}) ->
    io_lib:format("\"~s\"", [F]);
print_key_aux({function, F, A}) ->
    io_lib:format("~w/~w", [F, A]);
print_key_aux({clause, N}) ->
    io_lib:format("@~w", [N]);
print_key_aux({var, V}) ->
    io_lib:format("~w", [V]);
print_key_aux({record, R}) ->
    io_lib:format("#~w", [R]);
print_key_aux({field, F}) ->
    io_lib:format(".~w", [F]);
print_key_aux({macro, M, -1}) ->
    io_lib:format("?~s", [M]);
print_key_aux({macro, M, A}) ->
    io_lib:format("?~s/~w", [M, A]);
print_key_aux({type, T, A}) ->
    io_lib:format("~w()/~w", [T, A]);
print_key_aux(X) ->
    io_lib:format("~w", [X]).


