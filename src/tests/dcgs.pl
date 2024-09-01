:- module(dcgs_tests, []).

:- use_module(library(dcgs)).
:- use_module(test_framework).

test("cuts inside DCG do not blead to outer scope #1", (
    findall(L, (phrase(foo, [_]); L=1), [1])
)).

test("cuts inside DCG do not blead to outer scope #2", (
    findall(L, (phrase(!, [_]); L=1), [1])
)).

test("cuts inside DCG do not blead to outer scope #3", (
    findall(L, (phrase(("a",!), [_]); L=1), [1])
)).

test("cuts inside DCG do not blead to outer scope #4", (
    findall(L, (phrase(("a";!), [_]); L=1), [1])
)).

test("cuts inside DCG do not blead to outer scope #5", (
    findall(L, (phrase(bar:','(!,!), [_]); L=1), [1])
)).

test("cuts inside DCG do not blead to outer scope #6", (
    findall(L, (phrase(bar:!, [_]); L=1), [1])
)).

test("cuts inside DCG do not blead to outer scope #7", (
    C=!, findall(L, (phrase(C, [_]); L=1), [1])
)).

test("cuts inside DCG do not blead to outer scope #8", (
    findall(L, (phrase({!}, [_]); L=1), [1])
)).

foo --> !.
