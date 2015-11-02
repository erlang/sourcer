-ifndef(TEST).
-define(TEST, true).
-endif.

%% -ifdef(TEST).
%% -compile(export_all).
%% -endif.

-type token() :: {atom(), #{}}.