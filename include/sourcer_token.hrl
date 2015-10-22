
%% IMPORTANT: this record is API. Changing it means clients needs to be updated.
%% Clients should be implemented so that fields can be added at the end without disturbance.

%% TODO: use erl_anno? How to add new fields?

-record(attrs, {
				line      :: integer(), 
				last_line :: integer(), 
				column    :: non_neg_integer(),
				offset    :: non_neg_integer(), 
				text      :: binary() | string(), 
				length    :: integer() 
			   }). 
-type attrs() :: #attrs{}. 

-type symbol() :: atom() | float() | integer() | string().

-record(token, {
				kind  :: atom(), 
				attrs :: attrs(), 
				value :: symbol()
			   }).
-type token() :: #token{}.

