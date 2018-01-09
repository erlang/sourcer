-module(sourcer_db).

-export([
    load/1,
    save/2,
    merge/1,
    analyse/1
]).

-include("sourcer_db.hrl").

-define(DEBUG, true).
-include("debug.hrl").

-ifdef(DEBUG).

load(File) ->
    {ok, [Model]} = file:consult(File),
    Model.

save(File, Model) ->
    file:write_file(File, io_lib:format("~tp.~n", [Model])),
    Model.

-else.

load(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_term(Bin).

save(File, Model) ->
    file:write_file(File, term_to_binary(Model)),
    Model.

-endif.

merge([])  ->
    #model{};
merge([H])  ->
    merge(H, #model{});
merge([H|T]) ->
    lists:foldl(fun merge/2, H, T);
merge(M) ->
    merge([M]).

merge(#model{defs=D1, refs=R1, info=I1},
        #model{defs=D2, refs=R2, info=I2}) ->
    D = merge_defs(D1 ++ D2),
    R = merge_refs(R1 ++ R2, D),
    #model{
        defs=D,
        refs=R,
        info=merge_info(I1 ++ I2)
    }.

%% - if a def exists for same Ctx, keep the earliest;
%%      except macros - they can have multiple defs.

merge_defs(D) ->
    D1 = lists:sort(D),
    merge_defs(D1, []).

merge_defs([], R) ->
    lists:reverse(R);
merge_defs([E], R) ->
    lists:reverse([E|R]);
merge_defs([{K,P1,_}=E1,{K,P2,_}=E2|T], R) ->
    case hd(K) of
        {macro,_,_} ->
            if P1 =< P2 ->
                    merge_defs([E1|T], R);
                true ->
                    merge_defs([E2|T], R)
            end;
        _ ->
            merge_defs([E2|T], [E1|R])
    end;
merge_defs([E,E|T], R) ->
    merge_defs([E|T], R);
merge_defs([E|T], R) ->
    merge_defs(T, [E|R]).

%% - remove doubles
%% TODO : if def and ref at same location, remove ref
merge_refs(R, D) ->
    R1 = lists:sort(R),
    merge_refs(R1, D, []).

merge_refs([], _, R) ->
    lists:reverse(R);
merge_refs([E,E|T], D, R) ->
    merge_refs([E|T], D, R);
merge_refs([{K,_}=E|T], D, R) ->
    case lists:keyfind(K, 1, D) of
        false ->
            merge_refs(T, D, [E|R]);
        E ->
            merge_refs(T, D, R);
        _ ->
            merge_refs(T, D, [E|R])
    end.

%% - info with same key: merge in one entry

merge_info(I) ->
    I1 = lists:sort(I),
    merge_info(I1, []).

merge_info([], R) ->
    lists:reverse(R);
merge_info([E], R) ->
    lists:reverse([E|R]);
merge_info([E,E|T], R) ->
    merge_info([E|T], R);
merge_info([E|T], R) ->
    merge_info(T, [E|R]).

range({_,P1,_,_}, {_,P2,T2,_}) ->
    range(P1, P2, T2);
range(P, T) when is_list(T) ->
    range(P, P, T).

range(P1, {L2,C2}, T2) when is_list(T2) ->
    {P1, {L2, C2+length(T2)}};
range(P1, none, _) ->
    {P1, none}.

range({_, P, T, _}) ->
    range(P, P, T).

%%%%%%%%%%%%%%%%%%%%

new_ctx() ->
    queue:new().

new_ctx(Items) when is_list(Items) ->
    queue:from_list(Items);
new_ctx(Item) ->
    new_ctx([Item]).

push_ctx(Ctx, New) ->
    queue:join(Ctx, queue:from_list(New)).

get_ctx(Ctx) ->
    queue:to_list(Ctx).

join_ctx(Ctx1, Ctx2) ->
    queue:join(Ctx1, Ctx2).

%%%%%%%%%%%%%%%%%%%%

analyse(Forms) ->
    merge([analyse_form(X) || X<-Forms]).

analyse_form({define, Name, Arity, Args, Value, Comments, Pos, FullRange}) ->
    Key = [{macro, Name, Arity}],
    Ctx = new_ctx(Key),
    Defs = [{Key, Pos, FullRange}],
    Infos = comments_info(Comments, Key),
    Model0 = #model{defs=Defs, info=Infos},
    Model1 = analyse_exprs_list(Args, Ctx, Model0),
    merge(analyse_exprs(Value, Ctx, Model1));
analyse_form({include, Name, _Comments, Pos}) ->
    %% TODO resolve path?
    Key = [{file, Name}],
    Ctx = new_ctx(Key),
    #model{refs=[{Key, Pos}]};
analyse_form({include_lib, Name, _Comments, Pos}) ->
    %% TODO resolve path?
    Key = [{file, Name}],
    Ctx = new_ctx(Key),
    #model{refs=[{Key, Pos}]};
analyse_form({attribute, _Name, _Args, _Comments}) ->
    #model{};
analyse_form({export_type, Args, _Comments}) ->
    Refs = [{[{type,N,A}],P} || {N,A,P}<-Args],
    #model{refs=Refs};
analyse_form({export, Args, _Comments}) ->
    Refs = [{[{function,N,A}],P} || {N,A,P}<-Args],
    #model{refs=Refs};
analyse_form({callback, _Module, _Name, _Arity, _Args, _Body, _Comments, _FullRange}) ->
    %% TODO
    #model{};
analyse_form({callback, _Name, _Arity, _Args, _Body, _Comments, _FullRange}) ->
    %% TODO
    #model{};
analyse_form({spec, Module, Name, Arity, _Args, _Comments, Pos, FullRange}) ->
    %% TODO
    Key = [{module,Module},{function, Name, Arity}],
    Refs = [{Key, Pos}],
    #model{refs=Refs};
analyse_form({spec, Name, Arity, _Args, _Comments, Pos, FullRange}) ->
    %% TODO
    Key = [{function, Name, Arity}],
    Refs = [{Key, Pos}],
    #model{refs=Refs};
analyse_form({type, Name, Arity, Args, Def, Comments, Pos, FullRange}) ->
    Key = [{type, Name, Arity}],
    Ctx = new_ctx(Key),
    Defs = [{Key, Pos, FullRange}],
    Model0 = #model{defs=Defs},
    analyse_type(Def, Model0);
analyse_form({module, Name, Comments, Pos}) ->
    Key = [{module, Name}],
    Defs = [{Key, Pos, Pos}],
    Infos = comments_info(Comments, Key),
    #model{defs=Defs, info=Infos};
analyse_form({import, Module, Funcs, _Comments}) ->
    Key = {module, Module},
    Refs = [{[Key,{function,F,A}],Pos} || {F,A,Pos}<-Funcs],
    #model{refs=Refs};
analyse_form({record, Name, Comments, Pos, Fields, FullRange}) ->
    Key = [{record, Name}],
    Ctx = new_ctx(Key),
    Defs = [{Key, Pos, FullRange}],
    Model0 = #model{defs=Defs},
    Model1 = analyse_fields(Fields, Ctx),
    merge([Model0, Model1]);
analyse_form({function, Name, Arity, Clauses, Comments, Pos, FullRange}) ->
    analyse_function(Name, Arity, Clauses, Comments, Pos, FullRange);
analyse_form({compile, _Pos, _Args, _Comments}) ->
    #model{};
analyse_form(X) ->
    throw({bad_value, X}),
    #model{}.

analyse_function(Name, Arity, Clauses, Comments, Pos, FullRange) ->
    Key = [{function, Name, Arity}],
    Ctx = new_ctx(Key),
    Defs = [{Key, Pos, FullRange}],
    Infos = comments_info(Comments, Key),
    Model0 = #model{defs=Defs, info=Infos},
    merge([analyse_exprs(X,Ctx,Model0) || {clause,_,_,_,X}<-Clauses]).

analyse_clause(Args, Guards, Body, Ctx) ->
    Models = [analyse_exprs(A, Ctx) || A<-Args],
    M1 = merge(Models),
    M2 = analyse_exprs(Guards, Ctx, M1),
    analyse_exprs(Body, Ctx, M2).

analyse_fields(Fields, Ctx) ->
    merge([analyse_field(F, Ctx) || F<-Fields]).

analyse_field({field, Pos, Name, Type, DefVal}, Ctx) ->
    Key = get_ctx(push_ctx(Ctx,[{field, Name}])),
    M0 = #model{defs=[{Key, Pos}]},
    M1 = analyse_type(Type, M0),
    analyse_exprs(DefVal, Ctx, M1).

analyse_exprs_list(none, Ctx, Model) ->
    Model;
analyse_exprs_list(List, Ctx, Model) ->
    Models = [analyse_exprs(A, Ctx) || A<-List],
    merge([Model|Models]).

analyse_exprs(Exprs, Ctx) ->
    analyse_exprs(Exprs, Ctx, #model{}).

has_type(Ctx) ->
    lists:any(fun(?k(type))->true;(_)->false end, get_ctx(Ctx)).

analyse_exprs([], Ctx, Model) ->
    Model;
analyse_exprs(none, Ctx, Model) ->
    Model;
analyse_exprs([{var,_,_,Name}=H|T], Ctx, Model=#model{defs=Defs, refs=Refs}) ->
    Key = get_ctx(push_ctx(Ctx, [{var, Name}])),
    NewDefs = [{Key, range(H)}|Defs],
    NewRefs = [{Key, range(H)}|Refs],
    analyse_exprs(T, Ctx, Model#model{defs=NewDefs, refs=NewRefs});
analyse_exprs([{call, {_,_,_,Name}=F, Args} | T], Ctx, Model=#model{refs=Refs}) ->
    % TODO
    Arity = length(Args),
    %% TODO not good enough - rec field types, t ex
    case has_type(Ctx) of
        true ->
            NewCtx = push_ctx(Ctx, [{type, Name, Arity}]),
            Key = get_ctx(NewCtx),
            NewRefs = [{Key, range(F)}|Refs],
            merge([analyse_exprs(T, Ctx, Model#model{refs=NewRefs}) |
                [analyse_exprs(A, Ctx, Model) || A<-Args]]);
        false ->
            Key = get_ctx(push_ctx(Ctx, [{function, Name, Arity}])),
            NewRefs = [{Key, range(F)}|Refs],
            analyse_exprs(T, Ctx, Model#model{refs=NewRefs})
    end;
analyse_exprs([{call, M, F, Args} | T], Ctx, Model=#model{defs=Defs, refs=Refs}) ->
    % TODO
    analyse_exprs(T, Ctx, Model);
analyse_exprs([{funref, M, F, Args} | T], Ctx, Model=#model{defs=Defs, refs=Refs}) ->
    % TODO
    analyse_exprs(T, Ctx, Model);
analyse_exprs([{defun, M, F, Args} | T], Ctx, Model=#model{defs=Defs, refs=Refs}) ->
    % TODO
    analyse_exprs(T, Ctx, Model);
analyse_exprs([{record, M, F, Args} | T], Ctx, Model=#model{defs=Defs, refs=Refs}) ->
    % TODO
    analyse_exprs(T, Ctx, Model);
analyse_exprs([{recfield, M, F, Args} | T], Ctx, Model=#model{defs=Defs, refs=Refs}) ->
    % TODO
    analyse_exprs(T, Ctx, Model);
analyse_exprs([_|T], Ctx, Model) ->
    analyse_exprs(T, Ctx, Model).

analyse_type([], Model) ->
    Model;
analyse_type([{call,{_,_,_,Name}=Target,Arity}|T], {Model, Ctx}) ->
    %% TODO is this enough?
    M = #model{
        refs=[
            {push_ctx(Ctx, [{type, Name, Arity}]), range(Target)}
        ]
    },
    merge([M,Model]);
analyse_type([_|T], Model) ->
    Model.

comments_info(Comments, Key) ->
    case Comments of
        [] ->
            [];
        _ ->
            [{Key, [{comments, Comments}]}]
    end.
