:- module(goal_expansion_tests, []).

:- use_module(library(clpz)).
:- use_module(library(format)).
:- use_module(library(lambda)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module('../test_framework').
:- use_module(m).

% Support predicates
p(X) :- X #= 0.
q.
same(A,A).
clpz:monotonic.

% https://github.com/mthom/scryer-prolog/issues/2361
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

% https://github.com/mthom/scryer-prolog/issues/2255
test("#2255 1", (G = (_+\(goal_expansion_tests:q)), G)).
test("#2255 2", (G = (_+\q), G)).
test("#2255 3", (call(_+\q))).
test("#2255 4", (X = (_+\q), same(X, Y), Y)).
test("#2255 5", (
    catch(
        setup_call_cleanup(
            asserta((t2:-X=(_+\q),X)),
            t2,
            retractall(t2)
        ),
        E,
        (portray_clause(E),fail)
    )
)).
test("#2255 6", (
    setup_call_cleanup(
        asserta((t3:-call(_+\q))),
        q,
        retractall(t3)
    )
)).

% https://github.com/mthom/scryer-prolog/issues/2202
test("#2202 1", (
    catch(call(1,_), E, true),
    nonvar(E),
    E = error(type_error(callable,1),_)
)).

% https://github.com/mthom/scryer-prolog/issues/2062
test("#2062 1", (
    X = 2, maplist(#\=(X), [0,1])
)).
test("#2062 2", (
    catch(maplist(#\=(_), [0,1]), E, (portray_clause(E),fail))
)).
test("#2062 3", (
    catch(maplist(call(#\=,_), [0,1]), E, (portray_clause(E),fail))
)).
test("#2062 4", (
    catch(maplist(call(#\=(_)), [0,1]), E, (portray_clause(E),fail))
)).

% https://github.com/mthom/scryer-prolog/issues/1977
test("#1977 1", (
    call_with_inference_limit(true, 1, R),
    R == inference_limit_exceeded
)).
test("#1977 2", (
    call_with_inference_limit(true, 2, R),
    R == !
)).
test("#1977 3", (
    length(_, L),
    call_with_inference_limit(true, L, !)
)).

% https://github.com/mthom/scryer-prolog/issues/1987
test("#1987 1", (
    call_with_inference_limit(true,1,!)
)).

% https://github.com/mthom/scryer-prolog/issues/788
test("#788 1", (
   catch(clpz:neq_num(_,3), _, fail)
)).

test("#788 2", (
   catch(call(clpz:neq_num(_,3)), _, fail)
)).

test("#788 3", (
   catch(clpz:call(neq_num(_,3)), _, fail)
)).

test("#788 4", (
   maplist(clpz:call, [neq_num(_,2)])
)).

test("#788 5", (
   maplist(call, [clpz:neq_num(_,2)])
)).

test("#788 6", (
   maplist(maplist(maplist(maplist(clpz:call))), [[[[neq_num(_,2)]]]])
)).

% https://github.com/mthom/scryer-prolog/issues/1565
test("#1565 1", (
   catch(phrase_to_stream(("hello","there"), user_error), _, fail)
)).

% https://github.com/mthom/scryer-prolog/issues/1568
% TODO
