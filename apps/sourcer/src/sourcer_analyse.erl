-module(sourcer_analyse).

-export([
    analyse/1,
    analyse_text/1,
    merge/1
]).

-include("sourcer_model.hrl").

-define(DEBUG, true).
-include("debug.hrl").

merge([])  ->
    #model{};
merge(L) when is_list(L) ->
    lists:foldl(fun merge/2, #model{}, L);
merge(M) ->
    merge([M]).

analyse_text(Text) ->
    TText = unicode:characters_to_list(Text),
    {ok, Toks, _} = sourcer_scan:string(TText),
    Forms = sourcer_parse:parse(Toks),
    analyse(Forms).

analyse(Forms) ->
    Ms = [analyse_form(X) || X<-Forms],
    M0 = merge(Ms),
    M1 = adjust_keys(M0),
    M2 = adjust_defs(M1),
    M2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge(#model{defs=D1, refs=R1},
        #model{defs=D2, refs=R2}) ->
    D = merge_defs(lists:sort(D1 ++ D2)),
    R = merge_refs(lists:sort(R1 ++ R2), D),
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
merge_defs([E1=#def{ctx=K1},E2=#def{ctx=K2}|T], R) ->
    if K1 == K2 ->
        case {element(1, hd(K1)), length(K1)} of
            {macro, 1} ->
                merge_defs([E2|T], [E1|R]);
            _ ->
                merge_defs([merge_def(E1, E2)|T], R)
        end;
    true ->
        merge_defs([E2|T], [E1|R])
    end.

merge_def(#def{ctx=K, name_range=P1, info=M1}, #def{ctx=K, name_range=P2, info=M2}) ->
    P = case {P1, P2} of
            {none, P2} -> P2;
            {P1, none} -> P1;
            _ -> if P1<P2 -> P1; true -> P2 end
        end,
    M = maps:merge(M1, M2),
    #def{ctx=K, name_range=P, info=M}.

%% - remove doubles
%% TODO : if def and ref at same location, remove ref
merge_refs(R, D) ->
    R1 = lists:sort(R),
    merge_refs(R1, D, []).

merge_refs([], _, R) ->
    lists:reverse(R);
merge_refs([E,E|T], D, R) ->
    merge_refs([E|T], D, R);
merge_refs([E=#ref{ctx=K,range=P}|T], D, R) ->
    case lists:keyfind(K, #def.ctx, D) of
        #def{ctx=K,name_range=P} ->
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
    queue:from_list(Items).

push_ctx(Ctx, New) ->
    queue:join(Ctx, queue:from_list(New)).

get_ctx(Ctx) ->
    queue:to_list(Ctx).

%%%%%%%%%%%%%%%%%%

analyse_form({define, Name, Arity, Args, Value, Comments, Pos, FullRange}) ->
    Key = [{macro, Name, Arity, 0}],
    Ctx = new_ctx(Key),
    Defs = [#def{ctx=Key, name_range=Pos, 
            info=#{body=>FullRange, comments=>Comments}}],
    Model0 = #model{defs=Defs},
    Model1 = analyse_exprs_list(Args, Ctx, Model0),
    merge(analyse_exprs(Value, Ctx, Model1));
analyse_form({include, Name, _Comments, Pos}) ->
    %% TODO resolve path?
    Key = [{include, Name}],
    #model{refs=[#ref{ctx=Key, range=Pos}]};
analyse_form({include_lib, Name, _Comments, Pos}) ->
    %% TODO resolve path?
    Key = [{include_lib, Name}],
    #model{refs=[#ref{ctx=Key, range=Pos}]};
analyse_form({attribute, _Name, _Args, _Comments}) ->
    #model{};
analyse_form({export_type, Args, _Comments}) ->
    Refs = [#ref{ctx=[{type,N,A}],range=P} || {N,A,P}<-Args],
    #model{refs=Refs};
analyse_form({export, Args, _Comments}) ->
    Refs = [#ref{ctx=[{function,N,A}],range=P} || {N,A,P}<-Args],
    #model{refs=Refs};
analyse_form({callback, Module, Name, Arity, Args, Body, Comments, FullRange}) ->
    analyse_form({spec, Module, Name, Arity, Args, Body, Comments, FullRange});
analyse_form({callback, Name, Arity, Args, Body, Comments, FullRange}) ->
    analyse_form({spec, Name, Arity, Args, Body, Comments, FullRange});
analyse_form({spec, Module, Name, Arity, Args, Comments, Pos, FullRange}) ->
    Key = [{module,Module},{function, Name, Arity}],
    Defs = [#def{ctx=Key, name_range=none, info=#{spec=>FullRange, spec_comments=>Comments}}],
    Refs = [#ref{ctx=Key, range=Pos}],
    Ctx = new_ctx(),
    analyse_type_clauses(Args, Ctx, #model{defs=Defs, refs=Refs});
analyse_form({spec, Name, Arity, Args, Comments, Pos, FullRange}) ->
    Key = [{function, Name, Arity}],
    Ctx = new_ctx(),
    Defs = [#def{ctx=Key, name_range=none, info=#{spec=>FullRange, spec_comments=>Comments}}],
    Refs = [#ref{ctx=Key, range=Pos}],
    analyse_type_clauses(Args, Ctx, #model{defs=Defs, refs=Refs});
analyse_form({type, Name, Arity, Args, Def, Comments, Pos, FullRange}) ->
    Key = [{type, Name, Arity}],
    Ctx = new_ctx(Key),
    Defs = [#def{ctx=Key, name_range=Pos, info=#{body=>FullRange, comments=>Comments}}],
    Model0 = #model{defs=Defs},
    Model1 = merge([analyse_type(A, Ctx, Model0) || A<-Args]),
    Model2 = analyse_type(Def, Ctx, merge([Model0,Model1])),
    Model2;
analyse_form({module, Name, Comments, Pos}) ->
    Key = [{module, Name}],
    Defs = [#def{ctx=Key, name_range=Pos, info=#{comments=>Comments}}],
    #model{defs=Defs};
analyse_form({import, Module, Funcs, _Comments}) ->
    Key = {module, Module},
    Refs = [#ref{ctx=[Key,{function,F,A}],range=Pos} || {F,A,Pos}<-Funcs],
    #model{refs=Refs};
analyse_form({record, Name, Comments, Pos, Fields, FullRange}) ->
    Key = [{record, Name}],
    Ctx = new_ctx(Key),
    Defs = [#def{ctx=Key, name_range=Pos, info=#{body=>FullRange, comments=>Comments}}],
    Model0 = #model{defs=Defs},
    Model1 = analyse_fields(Fields, Ctx),
    merge([Model0, Model1]);
analyse_form({function, Name, Arity, Clauses, Comments, Pos, FullRange}) ->
    Key = [{function, Name, Arity}],
    Ctx = new_ctx(Key),
    Defs = [#def{ctx=Key, name_range=Pos, info=#{body=>FullRange, comments=>Comments}}],
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
    M0 = #model{defs=[#def{ctx=Key, name_range=Pos, info=#{}}]},
    M1 = analyse_type(Type, Ctx, M0),
    analyse_exprs(DefVal, Ctx, M1).

analyse_exprs_list(none, _Ctx, Model) ->
    Model;
analyse_exprs_list(List, Ctx, Model) ->
    Models = [analyse_exprs(A, Ctx) || A<-List],
    merge([Model|Models]).

analyse_exprs(Exprs, Ctx) ->
    analyse_exprs(Exprs, Ctx, #model{}).

has_type(Ctx) ->
    lists:any(fun(?k(type))->true;(_)->false end, get_ctx(Ctx)).

analyse_exprs([], _Ctx, Model) ->
    Model;
analyse_exprs(none, _Ctx, Model) ->
    Model;
analyse_exprs([{var,_,_,Name}=H|T], Ctx, Model=#model{defs=Defs, refs=Refs}) ->
    Key = get_ctx(push_ctx(Ctx, [{var, Name}])),
    NewDefs = [#def{ctx=Key, name_range=range(H), info=#{}}|Defs],
    NewRefs = [#ref{ctx=Key, range=range(H)}|Refs],
    analyse_exprs(T, Ctx, Model#model{defs=NewDefs, refs=NewRefs});
analyse_exprs([{macro,_,Name,none}=H|T], Ctx, Model=#model{refs=Refs}) ->
    Index = 0,
    Key = [{macro, macro_name(Name), -1, Index}],
    NewRefs = [#ref{ctx=Key, range=range(H)}|Refs],
    analyse_exprs(T, Ctx, Model#model{refs=NewRefs});
analyse_exprs([{macro,_,Name,Args}=H|T], Ctx, Model=#model{refs=Refs}) ->
    Index = 0,
    Key = [{macro, macro_name(Name), macro_arity(Args), Index}],
    NewRefs = [#ref{ctx=Key, range=range(H)}|Refs],
    merge([analyse_exprs(T, Ctx, Model#model{refs=NewRefs}) |
        [analyse_exprs(A, Ctx, Model) || A<-Args]]);
analyse_exprs([{call, ?v(Name)=F, Args} | T], Ctx, Model=#model{refs=Refs}) ->
    Arity = length(Args),
    case has_type(Ctx) of
        true ->
            NewCtx = push_ctx(Ctx, [{type, Name, Arity}]),
            Key = get_ctx(NewCtx),
            NewRefs = [#ref{ctx=Key, range=range(F)}|Refs],
            merge([analyse_exprs(T, Ctx, Model#model{refs=NewRefs}) |
                [analyse_exprs(A, Ctx, Model) || A<-Args]]);
        false ->
            Key = [{function, Name, Arity}],
            NewRefs = [#ref{ctx=Key, range=range(F)}|Refs],
            merge([analyse_exprs(T, Ctx, Model#model{refs=NewRefs}) |
                [analyse_exprs(A, Ctx, Model) || A<-Args]])
    end;
analyse_exprs([{call, ?v(Mod)=MM, ?v(Fun)=F, Args} | T], Ctx, Model=#model{refs=Refs}) ->
    Arity = length(Args),
    case has_type(Ctx) of
        true ->
            NewCtx = push_ctx(Ctx, [{type, Fun, Arity}]),
            Key = get_ctx(NewCtx),
            NewRefs = [#ref{ctx=Key, range=range(F)}|Refs],
            merge([analyse_exprs(T, Ctx, Model#model{refs=NewRefs}) |
                [analyse_exprs(A, Ctx, Model) || A<-Args]]);
        false ->
            NewRefs = case MM of
                        {macro,_,"?MODULE",'MODULE'} ->
                            Key1 = [{function, Fun, Arity}],
                            Key2 = [{macro, 'MODULE', -1, 0}],
                            [#ref{ctx=Key2, range=range(MM)},#ref{ctx=Key1, range=range(F)}|Refs];
                        _ ->
                            Key = [{module, Mod}, {function, Fun, Arity}],
                            [#ref{ctx=Key, range=range(F)}|Refs]
                    end,
            merge([analyse_exprs(T, Ctx, Model#model{refs=NewRefs}) |
                [analyse_exprs(A, Ctx, Model) || A<-Args]])
    end;
analyse_exprs([{funref, ?v(Mod), ?v(Fun)=F, ?v(A)} | T], Ctx, Model=#model{refs=Refs}) ->
    Key = [{module, Mod}, {function, Fun, A}],
    NewRefs = [#ref{ctx=Key, range=range(F)}|Refs],
    analyse_exprs(T, Ctx, Model#model{refs=NewRefs});
analyse_exprs([{defun, '', A, Ix, Clauses, _} | T], Ctx, Model) ->
    NewCtx = push_ctx(Ctx, [{function, Ix, A}]),
    merge([analyse_exprs(T, Ctx, Model) |
            [analyse_clause(C, NewCtx) || C<-Clauses]]);
analyse_exprs([{defun, FN, Args, _Ix, Clauses, Pos} | T], Ctx, Model=#model{defs=Defs}) ->
    NewCtx = push_ctx(Ctx, [{function, FN, Args}]),
    Key = get_ctx(NewCtx),
    NewDefs = [#def{ctx=Key, name_range=Pos, info=#{}}|Defs],
    merge([analyse_exprs(T, Ctx, Model#model{defs=NewDefs}) |
            [analyse_clause(C, NewCtx) || C<-Clauses]]);
analyse_exprs([{record, ?v(N)=R, Fs} | T], Ctx, Model=#model{refs=Refs}) ->
    Key = [{record, N}],
    NewRefs = [#ref{ctx=Key, range=range(R)}|Refs],
    merge([analyse_exprs(T, Ctx, Model#model{refs=NewRefs}),
        analyse_fields(Fs, Ctx)]);
analyse_exprs([{recfield, ?v(RN)=R, ?v(FN)=F} | T], Ctx, Model=#model{refs=Refs}) ->
    KeyR = [{record, RN}],
    KeyF = [{record, RN},{field, FN}],

    NewRefs = [#ref{ctx=KeyR, range=range(R)},#ref{ctx=KeyF, range=range(F)}|Refs],
    analyse_exprs(T, Ctx, Model#model{refs=NewRefs});
analyse_exprs([_|T], Ctx, Model) ->
    analyse_exprs(T, Ctx, Model).

analyse_type([], _Ctx, Model) ->
    Model;
analyse_type([{call,?v(Name)=Target,Args}|_], Ctx, Model) ->
    case lists:member(Name, predefined_types()) of
        true ->
            Model;
        _ ->
            Key = [{type, Name, length(Args)}],
            M = #model{
                refs=[
                    #ref{ctx=Key, range=range(Target)}
                ]
            },
            Model2 = [analyse_exprs(A, Ctx, Model)||A<-Args],
            merge([M,Model|Model2])
    end;
analyse_type([{call,?v(M),?v(Name)=Target,Args}|_], Ctx, Model) ->
    Key = [{module,M},{type, Name, length(Args)}],
    Model1 = #model{
        refs=[
            #ref{ctx=Key, range=range(Target)}
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

adjust_keys(M=#model{defs=D, refs=R}) ->
    Fun = fun(#def{ctx=[{module,_}]})->false; (_)->true end,
    L = lists:dropwhile(Fun, D),
    case L of
        [#def{ctx=[{module,_}=K]}|_] ->
            M#model{
                defs = [fix_key(E,K) || E<-D],
                refs = [fix_key(E,K) || E<-R]
            };
        _ ->
            M
    end.

fix_key(E=#def{ctx=KK}, K) ->
    E#def{ctx=fix_key(KK, K)};
fix_key(E=#ref{ctx=KK}, K) ->
    E#ref{ctx=fix_key(KK, K)};
fix_key(KK, K) ->
    case KK of
        [{module,_}|_] ->
            KK;
        _ ->
            [K|KK]
    end.

adjust_defs(M=#model{defs=Defs}) ->
    D = [adjust_map(X) || X<-Defs],
    M#model{defs=D}.

adjust_map(#def{info=M0}=E) ->
    M = case M0 of
        #{comments:=none} ->
            maps:remove(comments, M0);
        _ ->
            M0
    end,
    E#def{info=M}.

get_line_info(Text) ->
    %% we hope there are no mixed newlines
    Lines = binary:split(Text, [<<"\r\n">>, <<"\r">>, <<"\n">>], [global]),
    lists:mapfoldl(
        fun(X, {A, I}) -> L=size(X), {{A, L, I}, {A+L, I+1}} end,
        {0, 0},
        Lines
    ).

get_line_at_offset(Offset, Lines) ->
    L = lists:dropwhile(fun({O,_,_})-> O<Offset end, Lines),
    case L of
        [{_,_,I}|_] -> I;
        _ -> none
    end.

get_line_info(none, _) ->
    undefined;
get_line_info(Index, Lines) ->
    lists:nth(Index+1, Lines).
