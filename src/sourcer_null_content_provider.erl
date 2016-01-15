-module(sourcer_null_content_provider).

-behaviour(sourcer_content_provider).

-export([
         new/1,
         get/2
        ]).

new(Src) ->
    Src.

get(Provider, _File) ->
    Provider.
