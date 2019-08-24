:- module('Sets', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

/* 
File:		Sets.sup
Subject:	Implemention of the system module Sets
Author: 	Jiwei Wang
Date:		17 July 1992

================================================================================
*/


'$$module'('@(#)Sets.sup 1.5 last updated 93/12/09 12:28:00 by jiwei
').

/*==============================================================================
 * Following are functions support the constraint solving with sets.
 */

normalise(Vlist, List, set(NormalisedSet)) :-
   user:goedel_freeze(ground(Vlist), sort(List, NormalisedSet)).


plus(set(X), set(Y), set(Z)) :-
   user:goedel_freeze(ground([X,Y]), user:union(X, Y, Z)).


diff(set(X), set(Y), set(Z)) :-
   user:goedel_freeze(ground([X,Y]), user:difference(X, Y, Z)).

 
times(set(X), set(Y), set(Z)) :-
   user:goedel_freeze(ground([X,Y]), user:intersection(X, Y, Z)).

set_of(T, W, X, FreeVars) :-
   user:goedel_freeze(ground(FreeVars), 'Sets':set_of_aux(T, W, X)).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

set_of_aux(T, W, set(X)) :-
   ( setof(pair(T, Suspended), W^call_residue(W, Suspended), Y)
     -> ( 'Sets':clear_set(Y, X)
	  -> true
	  ;  format(user_error, 'Floundered in an intensional set.~n', []),
	     raise_exception(catch_in_query)
	)
     ;  X = []
   ),
   user:remove_ground_rep(X, XX),	% remove OProgram term
   prolog:term_variables(XX, Vars),
   ( Vars = []
     -> true
     ;  format(user_error, 'Floundered in an intensional set.~n', []),
        raise_exception(catch_in_query)
   ).

% this fails if there is one pair whose Suspended is not empty
clear_set([], []).
clear_set([pair(T, [])|A], [T|B]) :-
   clear_set(A, B).


%------------------------------------------------------------------------------

'Sets.In.P2'(A, X):-
   user:goedel_freeze(ground([X]), 'Sets':'Sets.In.P2.0'(A, X)).

'Sets.In.P2.0'(A, set(X)):-
   user:member(A, X).

'~Sets.In.P2'(A, X):-
   user:goedel_freeze(ground([X]), 'Sets':'Sets.In.P2.0'(A, X)).

'Sets.Size.P2'(X, Y):-
   user:goedel_freeze(ground([X]), 'Sets':'Sets.Size.P2.0'(X, Y)).

'Sets.Size.P2.0'(set(X), Y):-
   sort(X, Z),
   length(Z, Y).

'~Sets.Size.P2'(X, Y):-
   user:goedel_freeze(ground([X]), 'Sets':'Sets.Size.P2.0'(X, Y)).


'Sets.Subset.P2'(X, Y):-
   user:goedel_freeze(ground([Y]), 'Sets':'Sets.Subset.P2.0'(X, Y)).

'Sets.Subset.P2.0'(set(X), set(Y)):-
   'Sets.Subset.P2.1'(X, Y).

'Sets.Subset.P2.1'([], _).
'Sets.Subset.P2.1'([X|Xs], [X|Ys]) :-
   'Sets.Subset.P2.1'(Xs, Ys).
'Sets.Subset.P2.1'([X|Xs], [_|Ys]) :-
   'Sets.Subset.P2.1'([X|Xs], Ys).
   

'~Sets.Subset.P2'(X, Y):-
   user:goedel_freeze(ground([Y]), 'Sets':'Sets.Subset.P2.0'(X, Y)).


'Sets.StrictSubset.P2'(X, Y):-
   user:goedel_freeze(ground([Y]), 'Sets':'Sets.StrictSubset.P2.0'(X, Y)).

'Sets.StrictSubset.P2.0'(X, Y):-
   'Sets.Subset.P2'(X, Y),
   'Sets.Size.P2'(X, A),
   'Sets.Size.P2'(Y, B),
   A < B.

'~Sets.StrictSubset.P2'(X, Y):-
   user:goedel_freeze(ground([Y]), 'Sets':'Sets.StrictSubset.P2.0'(X, Y)).

