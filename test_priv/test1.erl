-module(mod). ?FILE ?LINE ?MODULE ?MODULE_STRING.
?MACHINE ?BEAM.
-define(A, a). ?A ?'A' ?A(). -undef(A).
-define(A, a). -define(A(), b). ?A ?A(). -undef(A).
-define(z, zx). ?z. -undef(z).
-define('Z', x). ?'Z' ?Z. -undef('Z').
-define(A(X), X). ?A(a1) ?A(a()+1). -undef(A).
-define(AS(X), ??X). ?AS(v). -undef(AS).
-define(A(X), X). -define(A(Y), Y). -undef(A).
-define(A(Q,W,E), Q{W). ?A(1,2,3). -undef(A).
-define(A(X), X). -define(B(X), ?A(v)). ?B(z). -undef(A). -undef(B).
-define(C(X), ?D(X)). -define(D(X), ?C(X)). ?C(0). -undef(C). -undef(D).
?Z.
?Z(3,4).
-define(X, x)0. -undef(X). x.
-define(A(X), X). ?A(f[). -undef(A).
?A(1,2,3,4,5).
-define(LINE, 1). x.
-define{X, 1}. x.
-define(W(A, A), A). x.
-define(F, ?G). -define(G, ?H). -define(H, ?I). ?F. -undef(F). -undef(G). -undef(H).
-define(F, ?G). -define(G, ?H). -define(H, ?I). -define(I, 3). ?F. -undef(F). -undef(G). -undef(H). -undef(I).
??A.
