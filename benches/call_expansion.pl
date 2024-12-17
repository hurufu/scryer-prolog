% Based on https://github.com/mthom/scryer-prolog/issues/1390

:- use_module(library(time)).
:- use_module(library(lists)).

test(2^N, Version) :-
    length(_,N),
    N > 17,
    BN is 2^N,
    length(L, BN),
    time(t(Version, L)).

t(call,L) :-
   call(unify_with_occurs_check(L),V),
   V \= [].
t(direct,L) :-
   unify_with_occurs_check(L,V),
   V \= [].
t(icall,L) :-
   G_1 = unify_with_occurs_check(L),
   call(G_1, V),
   V \= [].
