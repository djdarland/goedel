:- module('Integers', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

/* 
File:		Integers.sup
Subject:	Implemention of the whole system module Integers
Author: 	Jiwei Wang
Date:		17 September 1991

================================================================================
*/

'$$module'('@(#)Integers.sup 1.17 last updated 93/12/08 16:19:08 by jiwei
').

'Integers.=<.P2'(X, Y) :-
   ( integer(X), integer(Y)
     -> X =< Y
     ;  user:goedel_freeze(ground([X,Y]), X =< Y)
   ).

'~Integers.=<.P2'(X, Y) :-
   ( integer(X), integer(Y)
     -> X =< Y
     ;  user:goedel_freeze(ground([X,Y]), X =< Y)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

'Integers.>=.P2'(X, Y) :-
   ( integer(X), integer(Y)
     -> X >= Y
     ;  user:goedel_freeze(ground([X,Y]), X >= Y)
   ).

'~Integers.>=.P2'(X, Y) :-
   ( integer(X), integer(Y)
     -> X >= Y
     ;  user:goedel_freeze(ground([X,Y]), X >= Y)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

'Integers.>.P2'(X, Y) :-
   ( integer(X), integer(Y)
     -> X > Y
     ;  user:goedel_freeze(ground([X,Y]), X > Y)
   ).

'~Integers.>.P2'(X, Y) :-
   ( integer(X), integer(Y)
     -> X > Y
     ;  user:goedel_freeze(ground([X,Y]), X > Y)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

'Integers.<.P2'(X, Y) :-
   ( integer(X), integer(Y)
     -> X < Y
     ;  user:goedel_freeze(ground([X,Y]), X < Y)
   ).

'~Integers.<.P2'(X, Y) :-
   ( integer(X), integer(Y)
     -> X < Y
     ;  user:goedel_freeze(ground([X,Y]), X < Y)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

'Integers.Interval.P3'(L, N, H) :-
   nonvar(L), nonvar(H), !,
   ( var(N)
     -> L =< H,
        between(L, N, H)
     ;  integer(N),
        L =< N, N =< H
   ).

'Integers.Interval.P3'(L, N, H) :-
   user:goedel_freeze(ground([L,H]),
   		'Integers':'Integers.Interval.P3'(L, N, H) ).

'~Integers.Interval.P3'(L, N, H) :-
   'Integers':'Integers.Interval.P3'(L, N, H).


between(L, L, _).

between(L, N, H) :-
   L < H,
   L1 is L + 1,
   between(L1, N, H).
 
/*==============================================================================
 * Following are functions that support the constraint solving with integers.
 */

plus(X, Y, Z) :-
   integer(X), integer(Y), !, Z is X + Y.

plus(X, Y, Z) :-
   integer(X), integer(Z), !, Y is Z - X.

plus(X, Y, Z) :-
   integer(Y), integer(Z), !, X is Z - Y.

plus(X, Y, Z) :-
   user:goedel_freeze(ground([X,Z]) or ground([Y,Z]) or ground([X,Y]),
                        'Integers':plus(X, Y, Z)).

/*------------------------------------------------------------------------------
 */

negative(X, Y) :-
   integer(X), !, Y is -X.

negative(X, Y) :-
   integer(Y), !, X is -Y.

negative(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Integers':negative(X, Y) ).

/*------------------------------------------------------------------------------
 */

minus(X, Y, Z) :-
   integer(X), integer(Y), !, Z is X - Y.

minus(X, Y, Z) :-
   integer(X), integer(Z), !, Y is X - Z.

minus(X, Y, Z) :-
   integer(Y), integer(Z), !, X is Z + Y.

minus(X, Y, Z) :-
   user:goedel_freeze(ground([X,Z]) or ground([Y,Z]) or ground([X,Y]),
			'Integers':minus(X, Y, Z)).

/*------------------------------------------------------------------------------
 */
 
times(X, Y, Z) :-
   integer(X), integer(Y), !,
   Z is X * Y.

times(X, Y, Z) :-
   integer(X), integer(Z), !,
   ( X == 0, Z == 0
     -> true		% X is left uninstantiated
     ;  X =\= 0, 	% make sure X is not zero. If X is zero, it fails.
        Y is Z // X, Z =:= X * Y
   ).

times(X, Y, Z) :-
   integer(Y), integer(Z), !,
   ( Y == 0, Z == 0
     -> true		% X is left uninstantiated
     ;  Y =\= 0, 	% make sure Y is not zero. If X is zero, it fails.
        X is Z // Y, Z =:= X * Y
   ).

times(X, Y, Z) :-
   user:goedel_freeze(ground([X,Z]) or ground([Y,Z]) or ground([X,Y]),
			'Integers':times(X, Y, Z)).

/*------------------------------------------------------------------------------
 * divides does not allow 8 Div x = 2, yet, but x Div 3 = 2 is ok (which
 * returns multiple answers).
 */
 
divides(X, Y, Z) :-
   integer(X), integer(Y), !,
   ( Y = 0
     -> % Z = 0, but this value is never used
	format(user_error, "Arithmetic exception: division by 0.~n", []),
	raise_exception(catch_in_query)
     ;  Z is X // Y
   ).

divides(X, Y, Z) :-
   integer(Z), integer(Y), !,
   ( Y = 0
     -> % Z = 0, but this value is never used
        format(user_error, "Arithmetic exception: division by 0.~n", []),
	raise_exception(catch_in_query)
     ;  A is Y*Z,
	( A > 0
	  -> B is A + abs(Z) -1,
	     between(A, X, B)
	  ;  B is A - abs(Z) + 1,
	     between(B, X, A)
	)
   ).

divides(X, Y, Z) :-
   user:goedel_freeze(ground([Y,Z]) or ground([X,Y]),
		'Integers':divides(X, Y, Z)).

/*------------------------------------------------------------------------------
 */

power(X, Y, Z) :-
   integer(X), integer(Y), !,
   ( Y < 0
     -> % Z = 0, but this value is never used
	format(user_error, "Arithmetic exception: negative exponent.~n", []),
	raise_exception(catch_in_query)
     ;  ( Y = 0
	  -> Z = 1
	  ;  power2(Y, X, 1, Z)
	)
   ).

power(X, Y, Z) :-
   integer(X), integer(Z), !,
   X * Z >= 0,		% checking X and Z are of the same sign
   ( X = 0, Z = 0
     -> 'Integers.>.P2'(Y, 0)	% return constraint Y > 0
     ;  ( X = 0, Z = 1
	  -> Y = 0
	  ;  ( ( X = 0, Z =\= 0; X =\= 0, Z = 0 )
	       -> fail	% this is the incompatible case
               ;  ( X = 1
	            -> Z = 1	% if Z \= 1 this fails,
				% otherwise X is left uninstantiated
	            ;  Y is integer(round(log(abs(Z))/log(abs(X)))),
	  	       power2(Y, X, 1, Z)
		  )
	     )
	)
   ).

power(X, Y, Z) :-
   integer(Y), integer(Z), !,
   ( Y < 0
     -> format(user_error, "Arithmetic exception: negative exponent.~n", []),
	raise_exception(catch_in_query)
     ;  ( Y = 0
	  -> Z = 1		% X is left as a variable
	  ;  ( 0 =:= Y mod 2
	       -> ( Z =< 0
	            -> fail		% should fail
	            ;  ( X is integer(round(exp(log(Z)/Y)));
	                 X is - integer(round(exp(log(Z)/Y)))
		       ),
	  	       power2(Y, X, 1, Z)	% the test
	          )
	       ;  ( Z > 0
	            -> X is integer(round(exp(log(abs(Z))/Y)))
	            ;  ( Z < 0
		         -> X is - integer(round(exp(log(abs(Z))/Y)))
		         ;  fail	% when Z=0
		       )
	          ),
	  	  power2(Y, X, 1, Z)	% the test
	     )
	)
   ).

power(X, Y, Z) :-
   user:goedel_freeze(ground([X,Z]) or ground([Y,Z]) or ground([X,Y]),
		'Integers':power(X, Y, Z)).


power2(1, X, P, NewP) :-
   NewP is X*P, !.
power2(N, X, P, NewP) :-
   N > 1,
   P2 is X*P, 
   N2 is N - 1,
   power2(N2, X, P2, NewP).

/*------------------------------------------------------------------------------
 * mod is more restrictive than divides.
 */

mod(X, Y, Z) :-
   integer(X), integer(Y), !,
   ( Y = 0
     -> % Z = 0, but this value is never used
	format(user_error, "Arithmetic exception: mod by 0.~n", []),
	raise_exception(catch_in_query)
     ;  Z is X mod Y
   ).

mod(X, Y, Z) :-
   user:goedel_freeze(ground([X,Y]), 'Integers':mod(X, Y, Z)).

/*------------------------------------------------------------------------------
 * rem is more restrictive than divides.
 */

rem(X, Y, Z) :-
   integer(X), integer(Y), !,
   ( Y = 0
     -> % Z = 0, but this value is never used
	format(user_error, "Arithmetic exception: rem by 0.~n", []),
	raise_exception(catch_in_query)
     ;  A is X // Y,
        ( A > 0
	  -> A2 = A
	  ;  A2 is A - 1
	),
	Z is X - A2 * Y
   ).

rem(X, Y, Z) :-
   user:goedel_freeze(ground([X,Y]), 'Integers':rem(X, Y, Z)).

/*------------------------------------------------------------------------------
 */

absolute(X, Y) :-
   integer(X), !,
   ( X >= 0
     -> Y is X
     ;  Y is -X
   ).

absolute(X, Y) :-
   integer(Y), !, Y >= 0, 
   ( X is Y
   ;
     X is -Y
   ).

absolute(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Integers':absolute(X, Y) ).

/*------------------------------------------------------------------------------
 * maximum and minimum assum that X and Y are integers.
 */

sign(X, Y) :-
   integer(X), !,
   ( X = 0
     -> Y = 0
     ;  ( X > 0
          -> Y = 1
	  ;  Y = -1
	)
   ).

sign(X, Y) :-
   user:goedel_freeze(ground([X]), 'Integers':sign(X, Y) ).


/*------------------------------------------------------------------------------
 * maximum and minimum assum that X and Y are integers.
 */
 
maximum(X, Y, Z) :-
   integer(X), integer(Y), !,
   ( X < Y
     -> Z = Y
     ;  Z = X
   ).

maximum(X, Y, Z) :-
   user:goedel_freeze(ground([X,Y]), 'Integers':maximum(X, Y, Z) ).

/*------------------------------------------------------------------------------
 */

minimum(X, Y, Z) :-
   integer(X), integer(Y), !,
   ( X < Y
     -> Z = X
     ;  Z = Y
   ).

minimum(X, Y, Z) :-
   user:goedel_freeze(ground([X,Y]), 'Integers':minimum(X, Y, Z) ).
 
