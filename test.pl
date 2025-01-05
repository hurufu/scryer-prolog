:- use_module(library(format)).
:- use_module(library(inline), [inline_goal/1]).
:- use_module(library(debug)).
:- use_module(library(loader), [goal_sanitized/2]).

:- discontiguous(inline:inline_goal/1).
:- multifile(inline:inline_goal/1).

run :-
    listing(test/1).


inline:inline_goal(iff_(?,0,0)).
inline:inline_goal(=(?,?,?)).


=(X, Y, T) :-
   (  X == Y -> T = true
   ;  X \= Y -> T = false
   ;  T = true, X = Y
   ;  T = false,
      dif(X, Y)
   ).

iff_(If_1, Then_0, Else_0) :-
   xcall(If_1, T),
   (  T == true -> Then_0
   ;  T == false -> Else_0
   ;  nonvar(T) -> throw(error(type_error(boolean,T),
                               type_error(call(If_1,T),2,boolean,T)))
   ;  throw(error(instantiation_error,instantiation_error(call(If_1,T),2)))
   ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user:goal_expansion(xcall(G_0), S) :-
    loader:complete_partial_goal(0, G_0, _, [], G),
    goal_sanitized(G, S).

user:goal_expansion(xcall(G_1,A), S) :-
    $(loader:complete_partial_goal(1, G_1, _, [A], G)),
    $goal_sanitized(G, S).

user:goal_expansion(xcall(G_2,A,B), S) :-
    loader:complete_partial_goal(2, G_2, _, [A,B], G),
    goal_sanitized(G, S).

user:goal_expansion(xcall(G_3,A,B,C), S) :-
    loader:complete_partial_goal(3, G_3, _, [A,B,C], G),
    goal_sanitized(G, S).



:- dynamic(test/0).
test :-
    iff_(1=2,a,b), nl.

b :- write(b).
a :- write(a).


:- meta_predicate(xcall(1, ?)).
xcall(A, B) :- call(A, B).
