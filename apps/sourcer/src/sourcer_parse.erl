%%% Parse top level structure of a source file, returning any
%%% definitions, references and documentation encountered.

-module(sourcer_parse).

-export([
    parse/1,

    range/1
]).

-define(DEBUG, 1).
-include("debug.hrl").

-include("sourcer_db.hrl").

-define(util, sourcer_parse_util).

-spec parse([sourcer_scan:token()]) -> {ok, [any()]}.
parse(Tokens) ->
    Forms = ?util:split_at_token(Tokens, dot),
    lists:flatten([parse_form(F, Dot) || {F, {_,Dot,_,_}}<-Forms]).

parse_form(Tokens, Dot) ->
    {NewTs, Comments} = preprocess_form(Tokens),
    parse_form(NewTs, Dot, Comments).

%% keep track of macro context
preprocess_form(Ts) ->
    {TopComments, Ts0} = ?util:extract_top_comments(Ts),
    Ts2 = sourcer_scan:filter_ws_comment_tokens(Ts0),
    {Ts2, TopComments}.

parse_form([{'-',P0,_,_}|Tail], Dot, Comments) ->
    parse_attribute(Tail, {P0, Dot}, Comments);
parse_form([{atom,P0,_,_}, ?k(?LPAR)|_]=Ts, Dot, Comments) ->
    parse_function(Ts, {P0, Dot}, Comments);
parse_form(_Ts, _Dot, _Comments) ->
    [].

parse_function(Ts, FullRange, Comments) ->
    Clauses = case hd(Ts) of
            ?k(atom) ->
                split_at_semicolon_name(Ts);
            _Other ->
                split_at_semicolon_paren(Ts)
        end,
    Clauses1 = [parse_clause(C, N)
                || {{C,_},N}<-lists:zip(Clauses,lists:seq(1,length(Clauses)))
            ],
    {_, _, Args1, _, _} = hd(Clauses1),
    Arity = length(Args1),
    case hd(Ts) of
        {atom, _, _, Name}=TName ->
            {function, Name, Arity, Clauses1, Comments, range(TName), FullRange};
        Other ->
            %% anonymous fun
            {function, '', Arity, Clauses1, Comments, range(Other), FullRange}
    end.

parse_clause(Ts, N) ->
    Toks = case hd(Ts) of
        ?k(atom) ->
            tl(Ts);
        _ ->
            Ts
    end,
    {Args, Rest} = ?util:take_block_list(Toks),
    {Guards0, _, Body} = ?util:take_until_token(Rest, '->'),
    Guards = case Guards0 of
                [?k('when')|T] ->
                    parse_expr(T);
                _ ->
                    []
            end,
    BodyExprs = [E || E<-parse_exprs(Body)],
    {clause, N, [parse_expr(A)||A<-Args], Guards, BodyExprs}.

parse_attribute([{atom,_, _, 'define'}|Ts], FullRange, Comments) ->
    [Name | Args0] = ?util:middle(Ts),
    {_,_,_,AName} = Name,
    {Args, Arity, Value} = case Args0 of
                            [?k(?LPAR)|_] ->
                                {Args1, Rest} = ?util:take_block_list(Args0),
                                {
                                    [parse_exprs(A)||A<-Args1],
                                    length(Args1),
                                    case Rest of [] -> []; _ -> parse_expr(tl(Rest)) end
                                };
                            _ ->
                                {none, -1, parse_expr(tl(Args0))}
                        end,
    {define, AName, Arity, Args, Value, Comments, range(Name), FullRange};
parse_attribute([{atom,Pos,_,'record'}|Ts], FullRange, Comments) ->
    [{atom,_,_,Name}=N, ?k(',') | Def] = ?util:middle(Ts),
    {record, Name, Comments, range(N), parse_record(Def), FullRange};
parse_attribute([{atom,Pos, _, 'type'}|Ts], FullRange, Comments) ->
    {type, {atom,_,_,Name}=N, Args, Def} = parse_type(Ts),
    {type, Name, length(Args), Args, Def, Comments, range(N), FullRange};
parse_attribute([{atom,Pos, _, 'opaque'}|Ts], FullRange, Comments) ->
    {type, {atom,_,_,Name}=N, Args, Def} = parse_type(Ts),
    {type, Name, length(Args), Args, Def, Comments, range(N), FullRange};
parse_attribute([{atom,Pos, _, 'spec'}|Ts], FullRange, Comments) ->
    case parse_spec(Ts) of
        {{atom,_,_,F}=N, A, Sigs} ->
            {spec, F, A, Sigs, Comments, range(N), FullRange};
        {{atom,_,_,M}, {atom,_,_,F}=N, A, Sigs} ->
            {spec, M, F, A, Sigs, Comments, range(N), FullRange}
    end;
parse_attribute([{atom,Pos, _, 'callback'}|Ts], FullRange, Comments) ->
    case parse_spec(Ts) of
        {{atom,_,_,F}=N, A, Sigs} ->
            {callback, F, A, Sigs, Comments, range(N), FullRange};
        {{atom,_,_,M}, {atom,_,_,F}=N, A, Sigs} ->
            {callback, M, F, A, Sigs, Comments, range(N), FullRange}
    end;
parse_attribute([{atom,_, _, 'export'}|Ts], FullRange, Comments) ->
    Fs0 = ?util:split_at_token(?util:middle(?util:middle(Ts)), ','),
    Fs = [{F,A,range(P1,P2,T2)} || {[{atom,P1,_,F},_,{integer,P2,T2,A}],_} <- Fs0],
    {export, Fs, Comments};
parse_attribute([{atom,_, _, 'export_type'}|Ts], FullRange, Comments) ->
    Fs0 = ?util:split_at_token(?util:middle(?util:middle(Ts)), ','),
    Fs = [{F,A,range(P1,P2,T2)} || {[{atom,P1,_,F},_,{integer,P2,T2,A}],_} <- Fs0],
    {export_type, Fs, Comments};
parse_attribute([{atom,Pos, _, 'import'}|Ts], FullRange, Comments) ->
    Ts1 = ?util:middle(Ts),
    {[{atom,_,_,M}], _, Fs0} = ?util:take_until_token(Ts1, ','),
    Fs1 = ?util:split_at_token(?util:middle(Fs0), ','),
    Fs = [{F,A,range(P1,P2,T2)} || {[{atom,P1,_,F},_,{integer,P2,T2,A}],_} <- Fs1],
    {import, M, Fs, Comments};
parse_attribute([{atom,_, _, 'module'}, ?k(?LPAR),{atom,_, _, Name}=N|_], FullRange, Comments) ->
    {module, Name, Comments, range(N)};
parse_attribute([{atom,Pos, _, 'compile'}|Ts], FullRange, Comments) ->
    {compile, Pos, ?util:middle(Ts), Comments};
parse_attribute([{atom,_, _, 'include'}, ?k(?LPAR),{string,_, _, Str}=S|_], FullRange, Comments) ->
    {include, Str, Comments, range(S)};
parse_attribute([{atom,_, _, 'include_lib'}, ?k(?LPAR),{string,_, _, Str}=S|_], FullRange, Comments) ->
    {include_lib, Str, Comments, range(S)};
parse_attribute([{atom,_, _, Name}|Ts], FullRange, Comments) ->
    {attribute, Name, ?util:middle(Ts), Comments}.

parse_record(Ts) ->
    Fields = ?util:split_at_token(?util:middle(Ts), ','),
    Fun = fun({[{atom,_, _, Name}=NT|TypeDef],_}) ->
                Def0 = case TypeDef of
                            [?k('=')|_] ->
                                tl(TypeDef);
                            _ ->
                                TypeDef
                        end,
                {Def, _, Type} = ?util:take_until_token(Def0, '::'),
                {field, range(NT), Name, parse_expr(Type), parse_expr(Def)}
          end,
        %% TODO split fields
    lists:map(Fun, Fields).

parse_type(Ts) ->
    Ts1 = case hd(Ts) of
              ?k(?LPAR) ->
                  ?util:middle(Ts);
              _ ->
                  Ts
          end,
    {[{atom,_,_,_}=Name|Args0], _, Def} = ?util:take_until_token(Ts1, '::'),
    {Args, _} = ?util:take_block_list(Args0),
    {type, Name, [parse_expr(A)||A<-Args], parse_expr(Def)}.


parse_spec(Ts) ->
    Ts1 = case hd(Ts) of
              ?k(?LPAR) ->
                  ?util:middle(Ts);
              _ ->
                  Ts
          end,
    {H, P, Rest} = ?util:take_until_token(Ts1, ?LPAR),
    Cls = ?util:split_at_token([P|Rest], ';'),
    Fun = fun({X,_}) ->
                  {Args0,_,Return} = ?util:take_until_token(X, '->'),
                  {Args, _} = ?util:take_block_list(Args0),
                  %% TODO process args and types here
                  {[parse_expr(A)||A<-Args], parse_expr(Return)}
          end,
    Sigs = [Fun(C) || C<-Cls],
    {As, _} = hd(Sigs),
    case ?util:take_until_token(H, ':') of
        {[Fx], _, []} ->
            {Fx, length(As), Sigs};
        {[Mx], _, [Fx]} ->
            {Mx, Fx, length(As), Sigs}
    end.

parse_exprs(none) ->
    none;
parse_exprs(Ts) ->
    Exprs = ?util:split_at_token(Ts, ','),
    lists:flatten([parse_expr(E) || {E, _}<-Exprs]).

parse_expr_list(L) ->
    [parse_expr(E) || E<-L].

parse_expr([]) ->
    [];
parse_expr(none) ->
    [];
parse_expr([{var,_,_,'_'}|T]) ->
    parse_expr(T);
parse_expr([?k(var)=V|T]) ->
    [V | parse_expr(T)];
parse_expr([?k('fun'), ?k(atom)=M, ?k(':'), ?k(atom)=F,
                ?k('/'), ?k(integer)=A | T]) ->
    [{funref, M, F, A} | parse_expr(T)];
parse_expr([?k('fun'), ?k(atom)=F, ?k('/'), ?k(integer)=A | T]) ->
    [{funref, F, A} | parse_expr(T)];
parse_expr([?k(atom)=M, ?k(':'), ?k(atom)=F, ?k(?LPAR)=B|T]) ->
    {Args, Rest} = ?util:take_block_list([B|T]),
    [{call, M, F, [parse_expr(A)||A<-Args]} | parse_expr(Rest)];
parse_expr([{macro,_,_,'?MODULE'}=M, ?k(':'), ?k(atom)=F, ?k(?LPAR)=B|T]) ->
    {Args, Rest} = ?util:take_block_list([B|T]),
    [{call, M, F, [parse_expr(A)||A<-Args]} | parse_expr(Rest)];
parse_expr([?k(atom)=F, ?k(?LPAR)=B|T]) ->
    {Args, Rest} = ?util:take_block_list([B|T]),
    [{call, F, [parse_expr(A)||A<-Args]} | parse_expr(Rest)];
parse_expr([?k('fun')=F|T]) ->
    {Args, Rest} = ?util:take_block_list([F|T]),
    {function, N, A, Clauses, _, Pos, _} = parse_function(hd(Args), none, []),
    Ix = 1, %% TODO global function index
    [{defun, N, A, Ix, Clauses, Pos} | parse_expr(Rest)];
parse_expr([{macro,P,N,_}, ?k(?LPAR)=B|T]) ->
    {Args, Rest} = ?util:take_block_list([B|T]),
    [{macro, P, N, [parse_expr(A)||A<-Args]} | parse_expr(Rest)];
parse_expr([{macro,P,N,_} | T]) ->
    [{macro, P, N, none} | parse_expr(T)];
parse_expr([?k('#'), ?k(atom)=R, ?k('.'), ?k(atom)=F | T]) ->
    [{recfield, R, F} | parse_expr(T)];
parse_expr([?k('#'), ?k(atom)=R, ?k(?LCURL)=B | T]) ->
    {Data, Rest} = ?util:take_block_list([B|T]),
    Fields = [parse_field(D)||D<-Data],
    [{record, R, Fields} | parse_expr(Rest)];
parse_expr([_|T]) ->
    parse_expr(T).

parse_field([?k(atom)=F, ?k('=')|T]) ->
    {F, parse_expr(T)}.

predef_macros(Module, File) ->
    Machine = list_to_atom(erlang:system_info(machine)),
    Anno = #{line=>1},
    [
     {'FILE',-1, [], [{string, Anno#{value=>File}}]},
     {'LINE',-1, [], [{integer, Anno#{value=>1}}]},
     {'MODULE', -1, [], [{atom, Anno#{value=>Module}}]},
     {'MODULE_STRING', -1, [], [{string, Anno#{value=>atom_to_list(Module)}}]},
     {'MACHINE',-1, [], [{atom, Anno#{value=>Machine}}]},
     {Machine,-1, [], [{atom, Anno#{value=>true}}]}
    ].

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

%% Split token list at "; Name ?LPAR", where Name is the same
%% as the first token which must be an atom.
take_until_semicolon_name([H|_]=L) ->
    {atom, _, _, Name} = H,
    Pred = fun  ([{atom,_,_,AName}, ?k(?LPAR)|_]) when AName==Name -> true;
                (_) -> false
            end,
    ?util:take_until_token(L , ';', Pred).

split_at_semicolon_name([H|_]=L) ->
    {atom, _, _, Name} = H,
    Pred = fun  ([{atom,_,_,AName}, ?k(?LPAR)|_]) when AName==Name -> true;
                (_) -> false
            end,
    ?util:split_at_token(L , ';', Pred).

%% Split token list at "; ?LPAR"
take_until_semicolon_paren(L) ->
    Pred = fun  ([?k(?LPAR)|_]) -> true;
                (_) -> false
            end,
    ?util:take_until_token(L , ';', Pred).

split_at_semicolon_paren(L) ->
    Pred = fun  ([?k(?LPAR)|_]) -> true;
                (_) -> false
            end,
    ?util:split_at_token(L , ';', Pred).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

take_until_semicolon_name_test_() ->
    [
     ?_assertEqual({scan("a,b"), none, []},
                   take_until_semicolon_name(scan("a,b"))),
     ?_assertEqual({scan("a;b"), none, []},
                   take_until_semicolon_name(scan("a;b"))),
     ?_assertEqual({scan("a;a,b"), none, []},
                   take_until_semicolon_name(scan("a;a,b"))),
     ?_assertEqual({scan("a"), {';',{0,2},";",undefined},
                    scan("  a(b")},
                   take_until_semicolon_name(scan("a;a(b"))),
     ?_assertEqual({scan("a;c(d,e)f"), none, []},
                   take_until_semicolon_name(scan("a;c(d,e)f")))
    ].

take_until_semicolon_paren_test_() ->
    [
     ?_assertEqual({scan("a,b"), none, []},
                   take_until_semicolon_paren(scan("a,b"))),
     ?_assertEqual({scan("a;b"), none, []},
                   take_until_semicolon_paren(scan("a;b"))),
     ?_assertEqual({scan("a"), {';',{0,2},";",undefined}, scan("  (b")},
                   take_until_semicolon_paren(scan("a;(b"))),
     ?_assertMatch({[], none, []},
                   take_until_semicolon_paren([]))
    ].

scan(D) ->
    {ok, Ts, _} = sourcer_scan:string(D),
    sourcer_scan:filter_ws_tokens(Ts).

-endif.
