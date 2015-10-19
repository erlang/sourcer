
%% IMPORTANT: this record is API. Changing it means clients needs to be updated.
%% Clients should be implemented so that fields can be added at the end without disturbance.
 
-record(token, {kind=u, line=u, offset=u, length=u, value=u, text=u, last_line=u, column=u}).

-type token() :: #token{}.
