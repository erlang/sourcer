-module(sourcer_edoc).

-export([files/2]).

files(Files, Options) ->
    try
        %sourcer_log:logp(Files),
        %sourcer_log:logp(Options),
        [begin
            %sourcer_log:logp(F),
            edoc:files([F], Options) end || F<-Files]
    catch
        _:Err ->
            {error, Err}
    end.
