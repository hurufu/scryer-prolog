:- use_module(library(format)).
:- use_module(library(inline), [inline_goal/1]).

:- discontiguous(inline:inline_goal/1).
:- multifile(inline:inline_goal/1).

run :-
    listing(test/1).


inline:inline_goal(foo(+,-)).

foo(a, X) :- X is 2.

:- dynamic(test/1).

test(X) :-
    foo(a, X).
