:- module(goal_expansion_tests, []).

:- use_module(library(clpz)).
:- use_module(library(format)).
:- use_module(library(lambda)).
:- use_module(library(iso_ext)).
:- use_module('../test_framework').
:- use_module(m).

clpz:monotonic.

test("#2361 1", (
    gs([length("a",L1),length("ab", L2)]),
    (L1,L2) == (1,2)
)).

test("#2361 2", (
    gs([(length("a",L1),length("ab", L2))]),
    (L1,L2) == (1,2)
)).

test("#2361 3", (
    findall(X, gs([(append("abc","cd",X);length("abcd",X))]), ["abccd", 4])
)).

test("#2361 4", (
    gs([(append("abc","cd",Ls)->length("abcd", L))]),
    (Ls,L) == ("abccd",4)
)).

test("#2236 1", (
    catch(_ #= 0, E, true),
    nonvar(E),
    E = error(instantiation_error,_)
)).

test("#2236 2", (
    catch(p(_), E, true),
    nonvar(E),
    E = error(instantiation_error,_)
)).

test("#2255 1", (G = (_+\(user:q)), G)).
test("#2255 2", (G = (_+\q), G)).
test("#2255 3", (call(_+\q))).
test("#2255 4", (X = (_+\q), same(X, Y), Y)).

test("#2255 5", (
    setup_call_cleanup(
        asserta((t2:-X=(_+\q),X)),
        t2,
        retractall(t2)
    )
)).

test("#2255 6", (
    setup_call_cleanup(
        asserta((t3:-call(_+\q))),
        q,
        retractall(t2)
    )
)).

test("#2202 1", (
    catch(call(1,_), E, true),
    nonvar(E),
    E = error(type_error(callable,1),_)
)).


p(X) :- X #= 0.
q.
same(A,A).
