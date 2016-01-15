-module(sourcer_content_provider).

-type provider() :: {Ref :: atom(), State :: any()}.
-type maybe_provider() :: provider() | 'undefined'.

-callback new(Data :: any()) -> Ref :: provider().
-callback get(Ref:: provider(), FileName :: string()) -> Content :: string().

-export([
         new/2,
         get/2
        ]).
-export_type([
              provider/0,
              maybe_provider/0
             ]).

new(Mod, Data) ->
    {Mod, Mod:new(Data)}.

get({Mod, Data}, FileName) ->
    Mod:get(Data, FileName).
