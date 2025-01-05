/* Universal predicate inlining
 *
 * TODO: Limit scope to either a module or a file in addition to global inlining
 *
 */

:- module(inline, [
    inline_goal/1
]).

:- use_module(library(lists), [maplist/3]).
:- use_module(library(debug)).

:- dynamic(inline_db/2).
:- multifile(inline_db/2).
:- discontiguous(inline_db/2).

:- dynamic(inline_goal/1).
:- multifile(inline_goal/1).
:- discontiguous(inline_goal/1).

mode(+, V) :- ground(V), !.
mode(-, V) :- var(V), !.
mode(?, _) :- !.
mode(0, V) :- $goal_sanitized(V, V).

inline_spec(Head, Spec) :-
    functor(Head, F, N),
    functor(Spec, F, N),
    inline_goal(Spec).

user:goal_expansion(Head, Body) :-
    inline_db(Head, Body),
    inline_spec(Head, Spec),
    Head =.. [F|Args],
    Spec =.. [F|Specs],
    maplist(mode, Specs, Args).

user:term_expansion((Head :- Body), _) :-
    inline_spec(Head, _),
    inline:assertz(inline_db(Head, Body)),
    false.
