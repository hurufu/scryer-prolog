:- module(dcgs_tests, []).

:- use_module(library(dcgs)).
:- use_module(test_framework).

test("cuts inside DCG do not leak to outer scope #1", (
    findall(L, (phrase(foo, [_]); L=1), [1])
)).

test("cuts inside DCG do not leak to outer scope #2", (
    findall(L, (phrase(!, [_]); L=1), [1])
)).

foo --> !.
