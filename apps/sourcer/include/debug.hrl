%-define(Debug(T), sourcer_log:erlangLog(?MODULE, ?LINE, finest, T)).
%-define(DebugStack(T), sourcer_log:erlangLogStack(?MODULE, ?LINE, finest, T)).
%-define(Info(T), sourcer_log:erlangLog(?MODULE, ?LINE, info, T)).

-ifdef(DEBUG).
-define(D(F, T), io:format(F, T)).
-define(D(T), ?D("~p\n", [{??T, ?MODULE, ?LINE, T}])).
-else.
-define(D(F,T), ok).
-define(D(T), ok).
-endif.


