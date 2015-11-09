%%% Copyright 2015 Vlad Dumitrescu
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

%% @doc Scanner that converts tokens returned by erl_scan_local into
%% ones defined in sourcer_token.
%%

-module(sourcer_scan).

-export([
         string/1,
         string/2
        ]).

-include("sourcer.hrl").

-spec string(string()) -> {'ok', sourcer:tokens(), sourcer:location()}
                              | {'error', sourcer:location()}.
string(D) ->
    string(D, {0, 1, 0}).

-spec string(string(), sourcer:location()) -> {'ok', sourcer:tokens(), sourcer:location()}
                                                  | {'error', sourcer:location()}.
string(String, {L, C, O}) ->
    case string_2(String, {L, C, O}) of
        {ok, _, _}=R ->
            R;
        {error, {_, _, {_, Quote, _}}, _} ->
            case string_2(String++[Quote], {L, C, O}) of
                {ok, _, _}=R1 ->
                    R1;
                _Err ->
                    _Err
            end;
        _Err2 ->
            _Err2
    end.

string_2(D, {L, C, O}) ->
    case erl_scan_local:string(D, {L,C}, [return, text]) of
        {ok, Toks, {L1, C1}} ->
            {Toks1, O1} = convert_tokens_2(Toks, O),
            {ok, fix_macro_tokens(Toks1), {L1, C1, O1}};
        _Err ->
            _Err
    end.

-spec convert_tokens_2([tuple()], sourcer:offset()) -> {[sourcer:token()], sourcer:offset()}.
convert_tokens_2(Toks, Ofs) when is_list(Toks) ->
    lists:mapfoldl(fun convert_tokens_2_/2, Ofs, Toks).

convert_tokens_2_({A,B}, Ofs) ->
    convert_tokens_3_({A,B,A}, Ofs);
convert_tokens_2_(T, Ofs) ->
    convert_tokens_3_(T, Ofs).

convert_tokens_3_({K,A,V}, Ofs) ->
    Text = erl_anno:text(A),
    Length = length(Text),
    NewText = case K of
                  comment -> unicode:characters_to_binary(Text);
                  white_space -> list_to_binary(Text);
                  _ -> Text
              end,
    NewAttrs = maps:from_list([{length,Length},{offset,Ofs},{value,V}|A]),
    {{K,NewAttrs#{text=>NewText}}, Ofs+Length}.

%% TODO: split "dot+space" in two tokens
%% convert_tokens_2([{dot, [{line,L}, {column,_O}, {text,Txt}]} | Rest], Ofs, Acc) ->
%%     %% erl_scan conflates the dot with following whitespace.
%%     T = #token{kind=dot, attrs=#attrs{line=L, offset=Ofs, length=1, text=[hd(Txt)]}},
%%     case Txt of
%%         "." ->
%%             convert_tokens(Rest, Ofs+1, [T | Acc]);
%%         _ ->
%%             T1 = #token{kind=white_space, attrs=#attrs{line=L, offset=Ofs+1, length=length(Txt)-1, text=list_to_binary(tl(Txt))}},
%%             convert_tokens(Rest, Ofs+length(Txt), [T1, T | Acc])
%%     end;

-spec fix_macro_tokens([any()]) -> [any()].
fix_macro_tokens(Toks) ->
    fix_macro_tokens(Toks, []).

%% TODO: handle whitespace between ? and name
fix_macro_tokens([], Acc) ->
    lists:reverse(Acc);
fix_macro_tokens([{'?',P1},{atom,P2}|T], Acc) ->
    fix_macro_tokens(T, [{macro, mash_pos(P1, P2)}|Acc]);
fix_macro_tokens([{'?',P1},{var,P2}|T], Acc) ->
    fix_macro_tokens(T, [{macro, mash_pos(P1, P2)}|Acc]);
fix_macro_tokens([{'?',P1},{'?',_},{atom,P2}|T], Acc) ->
    fix_macro_tokens(T, [{macro, mash_pos_2(P1, P2)}|Acc]);
fix_macro_tokens([{'?',P1},{'?',_},{var,P2}|T], Acc) ->
    fix_macro_tokens(T, [{macro, mash_pos_2(P1, P2)}|Acc]);
fix_macro_tokens([H|T], Acc) ->
    fix_macro_tokens(T, [H|Acc]).

%% TODO: unicode?

mash_pos(#{}=P1,#{text:="'"++_=T2}) ->
    P1#{text=>[$?|T2],length=>length(T2)+1, value=>list_to_atom(sourcer_util:middle(T2))};
mash_pos(#{}=P1,#{text:=T2}) ->
    P1#{text=>[$?|T2],length=>length(T2)+1, value=>list_to_atom(T2)}.

mash_pos_2(#{}=P1,#{text:="'"++_=T2}) ->
    P1#{text=>[$?,$?|T2],length=>length(T2)+2, value=>list_to_atom(sourcer_util:middle(T2))};
mash_pos_2(#{}=P1,#{text:=T2}) ->
    P1#{text=>[$?,$?|T2],length=>length(T2)+2, value=>list_to_atom(T2)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

convert_attributes_test_() ->
    Toks = [
            {'-',[{line,1},{column,1},{text,"-"}]},
            {atom,[{line,1},{column,2},{text,"module"}],module},
            {'(',[{line,2},{column,1},{text,"("}]},
            {atom,[{line,2},{column,2},{text,"asx"}],asx},
            {')',[{line,2},{column,5},{text,")"}]},
            {dot,[{line,2},{column,6},{text,"."}]}
           ],
    [
     ?_assertMatch({
                    [
                     {'-',#{line:=1,column:=1,text:="-",offset:=0}},
                     {atom,#{line:=1,column:=2,text:="module",offset:=1}},
                     {'(',#{line:=2,column:=1,text:="(",offset:=7}},
                     {atom,#{line:=2,column:=2,text:="asx",offset:=8}},
                     {')',#{line:=2,column:=5,text:=")",offset:=11}},
                     {dot,#{line:=2,column:=6,text:=".",offset:= 12}}
                    ],
                    13
                   }, convert_tokens_2(Toks, 0))
    ].

fix_macro_tokens_test_() ->
    {ok, X, _} = string("?a,?B,?'A',?'a',??a,??B,??'A',??'a'"),
    [
     ?_assertMatch([
                    {macro,#{text:="?a",value:=a}},
                    {',',_},
                    {macro,#{text:="?B",value:='B'}},
                    {',',_},
                    {macro,#{text:="?'A'",value:='A'}},
                    {',',_},
                    {macro,#{text:="?'a'",value:='a'}},
                    {',',_},
                    {macro,#{text:="??a",value:=a}},
                    {',',_},
                    {macro,#{text:="??B",value:='B'}},
                    {',',_},
                    {macro,#{text:="??'A'",value:='A'}},
                    {',',_},
                    {macro,#{text:="??'a'",value:='a'}}
                   ],
                   fix_macro_tokens(X))
    ].

-endif.
