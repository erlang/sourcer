-module(sourcer_db).

-export([
    load/1,
    save/2,
    merge/1,
    analyse/1,
    analyse_text/1,
    get_element_at_pos/2
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
merge(L) when is_list(L) ->
    lists:foldl(fun merge/2, #model{}, L);
merge(M) ->
    merge([M]).

merge(#model{defs=D1, refs=R1},
        #model{defs=D2, refs=R2}) ->
    D = merge_defs(D1 ++ D2),
    R = merge_refs(R1 ++ R2, D),
    #model{
        defs=D,
        refs=R
    }.

%% - if a def exists for same Ctx, keep the earliest;
%%      except macros - they can have multiple defs.

merge_defs(D) ->
    Ds = lists:sort(D),
    merge_defs(Ds, []).

merge_defs([], R) ->
    lists:reverse(R);
merge_defs([E], R) ->
    lists:reverse([E|R]);
merge_defs([E,E|T], R) ->
    merge_defs([E|T], R);
merge_defs([E1,E2|T], R) ->
    K1 = element(1, E1),
    K2 = element(1, E2),
    if K1 == K2 ->
        case {element(1, hd(K1)), length(K1)} of
            {macro, 1} ->
                merge_defs([E2|T], [E1|R]);
            _ ->
                P1 = element(2, E1),
                P2 = element(2, E2),
                if P1 =< P2 ->
                        merge_defs([E1|T], R);
                    true ->
                        merge_defs([E2|T], R)
                end
        end;
    true ->
        merge_defs([E2|T], [E1|R])
    end.

%% - remove doubles
%% TODO : if def and ref at same location, remove ref
merge_refs(R, D) ->
    R1 = lists:sort(R),
    merge_refs(R1, D, []).

merge_refs([], _, R) ->
    lists:reverse(R);
merge_refs([E,E|T], D, R) ->
    merge_refs([E|T], D, R);
merge_refs([E|T], D, R) ->
    K = element(1, E),
    P = element(2,E),
    case lists:keyfind(K, 1, D) of
        {K,P,_,_} ->
            merge_refs(T, D, R);
        _ ->
            merge_refs(T, D, [E|R])
    end.

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

analyse_text(Text) ->
    TText = unicode:characters_to_list(Text),
    {ok, Toks, _} = sourcer_scan:string(TText),
    Forms = sourcer_parse:parse(Toks),
    sourcer_db:analyse(Forms).

get_element_at_pos(Model, Pos) ->
    #model{defs=Defs, refs=Refs} = Model,
    Defs1 = lists:filter(fun({_,_,X,_})-> pos_between(Pos, X) end, Defs),
    Defs2 = lists:filter(fun({_,X,_,_})-> pos_between(Pos, X) end, Defs),
    Refs1 = lists:filter(fun({_,X})-> pos_between(Pos, X) end, Refs),
    {Defs1, Refs1++Defs2}.

analyse(Forms) ->
    M = merge([analyse_form(X) || X<-Forms]),
    post_process(M).

analyse_form({define, Name, Arity, Args, Value, Comments, Pos, FullRange}) ->
    Key = [{macro, Name, Arity}],
    Ctx = new_ctx(Key),
    Infos = case Comments of [] -> #{}; _-> #{comments=>Comments} end,
    Defs = [{Key, Pos, FullRange, Infos}],
    Model0 = #model{defs=Defs},
    Model1 = analyse_exprs_list(Args, Ctx, Model0),
    merge(analyse_exprs(Value, Ctx, Model1));
analyse_form({include, Name, _Comments, Pos}) ->
    %% TODO resolve path?
    Key = [{include, Name}],
    Ctx = new_ctx(Key),
    #model{refs=[{Key, Pos}]};
analyse_form({include_lib, Name, _Comments, Pos}) ->
    %% TODO resolve path?
    Key = [{include_lib, Name}],
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
analyse_form({callback, Module, Name, Arity, Args, Body, Comments, FullRange}) ->
    analyse_form({spec, Module, Name, Arity, Args, Body, Comments, FullRange});
analyse_form({callback, Name, Arity, Args, Body, Comments, FullRange}) ->
    analyse_form({spec, Name, Arity, Args, Body, Comments, FullRange});
analyse_form({spec, Module, Name, Arity, Args, _Comments, Pos, FullRange}) ->
    %% TODO
    Key = [{module,Module},{function, Name, Arity}],
    Ctx = new_ctx(),
    Refs = [{Key, Pos}],
    analyse_type_clauses(Args, Ctx, #model{refs=Refs});
analyse_form({spec, Name, Arity, Args, _Comments, Pos, FullRange}) ->
    %% TODO
    Key = [{function, Name, Arity}],
    Ctx = new_ctx(),
    Refs = [{Key, Pos}],
    analyse_type_clauses(Args, Ctx, #model{refs=Refs});
analyse_form({type, Name, Arity, Args, Def, Comments, Pos, FullRange}) ->
    Key = [{type, Name, Arity}],
    Ctx = new_ctx(Key),
    Infos = case Comments of [] -> #{}; _-> #{comments=>Comments} end,
    Defs = [{Key, Pos, FullRange, Infos}],
    Model0 = #model{defs=Defs},
    Model1 = merge([analyse_type(A, Ctx, Model0) || A<-Args]),
    Model2 = analyse_type(Def, Ctx, merge([Model0,Model1])),
    Model2;
analyse_form({module, Name, Comments, Pos}) ->
    Key = [{module, Name}],
    Infos = case Comments of [] -> #{}; _-> #{comments=>Comments} end,
    Defs = [{Key, Pos, none, Infos}],
    #model{defs=Defs};
analyse_form({import, Module, Funcs, _Comments}) ->
    Key = {module, Module},
    Refs = [{[Key,{function,F,A}],Pos} || {F,A,Pos}<-Funcs],
    #model{refs=Refs};
analyse_form({record, Name, Comments, Pos, Fields, FullRange}) ->
    Key = [{record, Name}],
    Ctx = new_ctx(Key),
    Infos = case Comments of [] -> #{}; _-> #{comments=>Comments} end,
    Defs = [{Key, Pos, FullRange, Infos}],
    Model0 = #model{defs=Defs},
    Model1 = analyse_fields(Fields, Ctx),
    merge([Model0, Model1]);
analyse_form({function, Name, Arity, Clauses, Comments, Pos, FullRange}) ->
    Key = [{function, Name, Arity}],
    Ctx = new_ctx(Key),
    Infos = case Comments of [] -> #{}; _-> #{comments=>Comments} end,
    Defs = [{Key, Pos, FullRange, Infos}],
    Model0 = #model{defs=Defs},
    merge([Model0 | [analyse_clause(C,Ctx) || C<-Clauses]]);
analyse_form({compile, _Pos, _Args, _Comments}) ->
    #model{};
analyse_form(X) ->
    throw({bad_value, X}),
    #model{}.

analyse_clause({clause, N, Args, Guards, Body}, Ctx) ->
    Ctx1 = push_ctx(Ctx, [{clause, N}]),
    Models = [analyse_exprs(A, Ctx1) || A<-Args],
    M1 = merge(Models),
    M2 = analyse_exprs(Guards, Ctx1, M1),
    analyse_exprs(Body, Ctx1, M2).

analyse_fields(Fields, Ctx) ->
    merge([analyse_field(F, Ctx) || F<-Fields]).

analyse_field({field, Pos, Name, Type, DefVal}, Ctx) ->
    Key = get_ctx(push_ctx(Ctx,[{field, Name}])),
    M0 = #model{defs=[{Key, Pos, none, #{}}]},
    M1 = analyse_type(Type, Ctx, M0),
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
    NewDefs = [{Key, range(H), none, #{}}|Defs],
    NewRefs = [{Key, range(H)}|Refs],
    analyse_exprs(T, Ctx, Model#model{defs=NewDefs, refs=NewRefs});
analyse_exprs([{macro,_,Name,none}=H|T], Ctx, Model=#model{defs=Defs, refs=Refs}) ->
    Key = [{macro, macro_name(Name), -1}],
    NewRefs = [{Key, range(H)}|Refs],
    analyse_exprs(T, Ctx, Model#model{refs=NewRefs});
analyse_exprs([{macro,_,Name,Args}=H|T], Ctx, Model=#model{defs=Defs, refs=Refs}) ->
    Key = [{macro, macro_name(Name), macro_arity(Args)}],
    NewRefs = [{Key, range(H)}|Refs],
    merge([analyse_exprs(T, Ctx, Model#model{refs=NewRefs}) |
        [analyse_exprs(A, Ctx, Model) || A<-Args]]);
analyse_exprs([{call, ?v(Name)=F, Args} | T], Ctx, Model=#model{refs=Refs}) ->
    Arity = length(Args),
    case has_type(Ctx) of
        true ->
            NewCtx = push_ctx(Ctx, [{type, Name, Arity}]),
            Key = get_ctx(NewCtx),
            NewRefs = [{Key, range(F)}|Refs],
            merge([analyse_exprs(T, Ctx, Model#model{refs=NewRefs}) |
                [analyse_exprs(A, Ctx, Model) || A<-Args]]);
        false ->
            Key = [{function, Name, Arity}],
            NewRefs = [{Key, range(F)}|Refs],
            merge([analyse_exprs(T, Ctx, Model#model{refs=NewRefs}) |
                [analyse_exprs(A, Ctx, Model) || A<-Args]])
    end;
analyse_exprs([{call, ?v(Mod)=M, ?v(Fun)=F, Args} | T], Ctx, Model=#model{refs=Refs}) ->
    Arity = length(Args),
    case has_type(Ctx) of
        true ->
            NewCtx = push_ctx(Ctx, [{type, Fun, Arity}]),
            Key = get_ctx(NewCtx),
            NewRefs = [{Key, range(F)}|Refs],
            merge([analyse_exprs(T, Ctx, Model#model{refs=NewRefs}) |
                [analyse_exprs(A, Ctx, Model) || A<-Args]]);
        false ->
            Key = [{module, Mod}, {function, Fun, Arity}],
            NewRefs = [{Key, range(F)}|Refs],
            merge([analyse_exprs(T, Ctx, Model#model{refs=NewRefs}) |
                [analyse_exprs(A, Ctx, Model) || A<-Args]])
    end;
analyse_exprs([{funref, ?v(Mod)=M, ?v(Fun)=F, ?v(A)} | T], Ctx, Model=#model{defs=Defs, refs=Refs}) ->
    Key = [{module, Mod}, {function, Fun, A}],
    NewRefs = [{Key, range(F)}|Refs],
    analyse_exprs(T, Ctx, Model#model{refs=NewRefs});
analyse_exprs([{defun, '', A, Ix, Clauses, Pos} | T], Ctx, Model) ->
    NewCtx = push_ctx(Ctx, [{function, Ix, A}]),
    merge([analyse_exprs(T, Ctx, Model) |
            [analyse_clause(C, NewCtx) || C<-Clauses]]);
analyse_exprs([{defun, FN, Args, Ix, Clauses, Pos} | T], Ctx, Model=#model{defs=Defs}) ->
    NewCtx = push_ctx(Ctx, [{function, FN, Args}]),
    Key = get_ctx(NewCtx),
    NewDefs = [{Key, Pos}|Defs],
    merge([analyse_exprs(T, Ctx, Model#model{defs=NewDefs}) |
            [analyse_clause(C, NewCtx) || C<-Clauses]]);
analyse_exprs([{record, ?v(N)=R, Fs} | T], Ctx, Model=#model{defs=Defs, refs=Refs}) ->
    Key = [{record, N}],
    NewRefs = [{Key, range(R)}|Refs],
    merge([analyse_exprs(T, Ctx, Model#model{refs=NewRefs}),
        analyse_fields(Fs, Ctx)]);
analyse_exprs([{recfield, ?v(RN)=R, ?v(FN)=F} | T], Ctx, Model=#model{defs=Defs, refs=Refs}) ->
    KeyR = [{record, RN}],
    KeyF = [{record, RN},{field, FN}],

    NewRefs = [{KeyR, range(R)},{KeyF, range(F)}|Refs],
    analyse_exprs(T, Ctx, Model#model{refs=NewRefs});
analyse_exprs([_|T], Ctx, Model) ->
    analyse_exprs(T, Ctx, Model).

analyse_type([], Ctx, Model) ->
    Model;
analyse_type([{call,?v(Name)=Target,Args}|T], Ctx, Model) ->
    case lists:member(Name, predefined_types()) of
        true ->
            Model;
        _ ->
            Key = [{type, Name, length(Args)}],
            M = #model{
                refs=[
                    {Key, range(Target)}
                ]
            },
            Model2 = [analyse_exprs(A, Ctx, Model)||A<-Args],
            merge([M,Model|Model2])
    end;
analyse_type([{call,?v(M),?v(Name)=Target,Args}|T], Ctx, Model) ->
    Key = [{module,M},{type, Name, length(Args)}],
    Model1 = #model{
        refs=[
            {Key, range(Target)}
        ]
    },
    Model2 = [analyse_exprs(A, Ctx, Model)||A<-Args],
    merge([Model1,Model|Model2]);
analyse_type(L, Ctx, Model) ->
    analyse_exprs(L, Ctx, Model).

analyse_type_clauses(Clauses, Ctx, Model) ->
    Fun = fun({Args,Return}) ->
            merge([analyse_type(Args, Ctx, Model), analyse_type(Return, Ctx, Model)])
        end,
    merge([Fun(C) || C<-Clauses]).

comments_info(Comments, Key) ->
    case Comments of
        [] ->
            [];
        _ ->
            [{Key, [{comments, Comments}]}]
    end.

predefined_types() ->
    [
        any,
        none,
        pid,
        port,
        reference,
        float,
        atom,
        integer,
        term,
        binary,
        bitstring,
        boolean,
        byte,
        char,
        nil,
        number,
        list,
        map,
        tuple,
        maybe_improper_list,
        nonempty_list,
        string,
        nonempty_string,
        iodata,
        iolist,
        function,
        module,
        mfa,
        arity,
        identifier,
        node,
        timeout,
        no_return,
        non_neg_integer,
        pos_integer,
        neg_integer,
        nonempty_maybe_improper_list,
        nonempty_improper_list
    ].

macro_arity(none) ->
    -1;
macro_arity(L) ->
    length(L).

macro_name("?"++Name) ->
    list_to_atom(Name);
macro_name(X) ->
    X.

post_process(M) ->
    adjust_keys(M).

adjust_keys(M=#model{defs=D, refs=R}) ->
    Fun = fun({[{module,_}],_,_,_})->false; (_)->true end,
    L = lists:dropwhile(Fun, D),
    case L of
        [{[{module,Mod}=K],_,_,_}|_] ->
            M#model{
                defs=fix_keys(D, K),
                refs=fix_keys(R, K)
            };
        _ ->
            M
    end.

fix_keys(L, K) ->
    [fix_key(E,K) || E<-L].

fix_key(E, K) ->
    KK = element(1, E),
    case hd(KK) of
        K ->
            E;
        _ ->
            setelement(1, E, [K|KK])
    end.

pos_between(_Crt, none) ->
    false;
pos_between(Crt, {Start, End}) ->
    Start =< Crt andalso Crt < End.

