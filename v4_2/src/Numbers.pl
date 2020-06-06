:- module('Numbers', []).
:- multifile '$$module'/1.

:- op(500, yfx, and).
:- op(400, yfx, or).

/* 
File:		Numbers.sup
Subject:	Implemention of the system module Numbers
Author: 	Jiwei Wang
Date:		28 October 1992

================================================================================
*/


%% '$$module'('@(#)Numbers.sup 1.3 last updated 93/06/22 16:08:16 by jiwei').

'Numbers.IntegerString.P2'(X, Y) :-
   integer(X), !,
   name(X, Ints),
   'Strings':'Strings.StringInts.P2'(Y, Ints).

'Numbers.IntegerString.P2'(X, Y) :-
   nonvar(Y), !,
   'Strings':'Strings.StringInts.P2'(Y, Ints),
   Ints \== [],
   remove_sign(Ints, Ints2),
   sort(Ints2, Ints3),
   user:subset(Ints3, [0'0, 0'1, 0'2, 0'3, 0'4, 0'5, 0'6, 0'7, 0'8, 0'9]),
   name(X, Ints).

'Numbers.IntegerString.P2'(X, Y) :-
   user:goedel_freeze(ground([X]) or ground([Y]),
		'Numbers':'Numbers.IntegerString.P2'(X, Y) ).

remove_sign([0'-|Ints], Ints) :- !.
remove_sign(Ints, Ints).

'~Numbers.IntegerString.P2'(X, Y) :-
   'Numbers.IntegerString.P2'(X, Y).
