
%% IMPORTANT: this record is API. Changing it means clients needs to be updated.
%% Clients should be implemented so that fields can be added at the end without disturbance.
 
%% TODO use erl_anno? How to add new fields?
-record(attributes, {
					 line :: integer(), 
					 offset :: non_neg_integer(), 
					 length :: integer(), 
					 text :: binary(), 
					 last_line :: integer(), 
					 column :: integer()
					}). 
-type attributes() :: #attributes{}. 

-type symbol() :: atom() | float() | integer() | string().

-record(token, {kind:: atom(), attributes::attributes(), value::symbol()}).

-type token() :: #token{}.

