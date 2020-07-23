:- module('Floats', []).
:- multifile '$$module'/1.

:- op(500, yfx, and).
:- op(400, yfx, or).

/* 
File:		Floats.pl
Subject:	Implemention of the whole system module Floats
Author: 	Dennis Darland
Date:		17 July 2020

================================================================================
*/

%% '$$module'('@(#)Integers.sup 1.17 last updated 93/12/08 16:19:08 by jiwei').
'FloatInts'(Float, IntList) :-
   nonvar(Float), !,
   name(Float, [0'"|IntList]).

'FloatInts'(Float, IntList) :-
   ground(IntList), !,
   name(Float, [0'"|IntList]).

'FloatInts'(Float, IntList) :-
   user:goedel_freeze(ground([IntList]) or ground([Float]),
		'FloatInts'(Float, IntList)).

'~FloatInts'(Float, IntList) :-
   'FloatInts'(Float, IntList).

'Floats.=<.P2'(X, Y) :-
   ( float(X), float(Y)
     -> X =< Y
     ;  user:goedel_freeze(ground([X,Y]), X =< Y)
   ).

'~Floats.=<.P2'(X, Y) :-
   ( float(X), float(Y)
     -> X =< Y
     ;  user:goedel_freeze(ground([X,Y]), X =< Y)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

'Floats.>=.P2'(X, Y) :-
   ( float(X), float(Y)
     -> X >= Y
     ;  user:goedel_freeze(ground([X,Y]), X >= Y)
   ).

'~Floats.>=.P2'(X, Y) :-
   ( float(X), float(Y)
     -> X >= Y
     ;  user:goedel_freeze(ground([X,Y]), X >= Y)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

'Floats.>.P2'(X, Y) :-
   ( float(X), float(Y)
     -> X > Y
     ;  user:goedel_freeze(ground([X,Y]), X > Y)
   ).

'~Floats.>.P2'(X, Y) :-
   ( float(X), float(Y)
     -> X > Y
     ;  user:goedel_freeze(ground([X,Y]), X > Y)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

'Floats.<.P2'(X, Y) :-
   ( float(X), float(Y)
     -> X < Y
     ;  user:goedel_freeze(ground([X,Y]), X < Y)
   ).

'~Floats.<.P2'(X, Y) :-
   ( float(X), float(Y)
     -> X < Y
     ;  user:goedel_freeze(ground([X,Y]), X < Y)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%%% 'Floats.Interval.P3'(L, N, H) :-
%%%    nonvar(L), nonvar(H), !,
%%%    ( var(N)
%%%      -> L =< H,
%%%         between(L, N, H)
%%%      ;  float(N),
%%%         L =< N, N =< H
%%%    ).

%%% 'Floats.Interval.P3'(L, N, H) :-
%%%    user:goedel_freeze(ground([L,H]),
%%%    		'Floats.Floats.Interval.P3'(L, N, H) ).

%%% '~Floats.Interval.P3'(L, N, H) :-
%%%    'Floats.Floats.Interval.P3'(L, N, H).


%%% between(L, L, _).

%%% between(L, N, H) :-
%%%    L < H,
%%%    L1 is L + 1,
%%%    between(L1, N, H).
 
/*==============================================================================
 * Following are functions that support the constraint solving with floats.
 */

'plus'(X, Y, Z) :-
   float(X), float(Y), !, Z is X + Y.

'plus'(X, Y, Z) :-
   float(X), float(Z), !, Y is Z - X.

'plus'(X, Y, Z) :-
   float(Y), float(Z), !, X is Z - Y.

'plus'(X, Y, Z) :-
   user:goedel_freeze(ground([X,Z]) or ground([Y,Z]) or ground([X,Y]),
                        'Floats':plus(X, Y, Z)).

/*------------------------------------------------------------------------------
 */

'negative'(X, Y) :-
   float(X), !, Y is -X.

'negative'(X, Y) :-
   float(Y), !, X is -Y.

'negative'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':negative(X, Y) ).

/*------------------------------------------------------------------------------
 */

'minus'(X, Y, Z) :-
   float(X), float(Y), !, Z is X - Y.

'minus'(X, Y, Z) :-
   float(X), float(Z), !, Y is X - Z.

'minus'(X, Y, Z) :-
   float(Y), float(Z), !, X is Z + Y.

'minus'(X, Y, Z) :-
   user:goedel_freeze(ground([X,Z]) or ground([Y,Z]) or ground([X,Y]),
			'Floats':minus(X, Y, Z)).

/*------------------------------------------------------------------------------
 */
 
'times'(X, Y, Z) :-
   float(X), float(Y), !,
   Z is X * Y.
%   write(Z).

'times'(X, Y, Z) :-
   float(X), float(Z), !,
   ( X == 0, Z == 0
     -> true		% X is left uninstantiated
     ;  X =\= 0, 	% make sure X is not zero. If X is zero, it fails.
        Y is Z / X, Z =:= X * Y
   ).

'times'(X, Y, Z) :-
   float(Y), float(Z), !,
   ( Y == 0, Z == 0
     -> true		% X is left uninstantiated
     ;  Y =\= 0, 	% make sure Y is not zero. If X is zero, it fails.
        X is Z / Y, Z =:= X * Y
   ).

'times'(X, Y, Z) :-
   user:goedel_freeze(ground([X,Z]) or ground([Y,Z]) or ground([X,Y]),
			'Floats':times(X, Y, Z)).

/*------------------------------------------------------------------------------
 * divides does not allow 8 Div x = 2, yet, but x Div 3 = 2 is ok (which
 * returns multiple answers).
 */
 
'divides'(X, Y, Z) :-
   float(X), float(Y), !,
   ( Y = 0
     -> % Z = 0, but this value is never used
	format(user_error, "Arithmetic exception: division by 0.~n", []),
	raise_exception(catch_in_query)
     ;  Z is X / Y
   ).

'divides'(X, Y, Z) :-
   float(Z), float(Y), !,
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

'divides'(X, Y, Z) :-
   user:goedel_freeze(ground([Y,Z]) or ground([X,Y]),
		'Floats':divides(X, Y, Z)).

/*------------------------------------------------------------------------------
 */

'power'(X, Y, Z) :-
   float(X), float(Y), !,
   ( Y < 0
     -> % Z = 0, but this value is never used
	format(user_error, "Arithmetic exception: negative exponent.~n", []),
	raise_exception(catch_in_query)
     ;  ( Y = 0
	  -> Z = 1
	  ;  power2(Y, X, 1, Z)
	)
   ).

'power'(X, Y, Z) :-
   float(X), float(Z), !,
   X * Z >= 0,		% checking X and Z are of the same sign
   ( X = 0, Z = 0
     -> 'Floats.>.P2'(Y, 0)	% return constraint Y > 0
     ;  ( X = 0, Z = 1
	  -> Y = 0
	  ;  ( ( X = 0, Z =\= 0; X =\= 0, Z = 0 )
	       -> fail	% this is the incompatible case
               ;  ( X = 1
	            -> Z = 1	% if Z \= 1 this fails,
				% otherwise X is left uninstantiated
	            ;  Y is float(round(log(abs(Z))/log(abs(X)))),
	  	       power2(Y, X, 1, Z)
		  )
	     )
	)
   ).

'power'(X, Y, Z) :-
   float(Y), float(Z), !,
   ( Y < 0
     -> format(user_error, "Arithmetic exception: negative exponent.~n", []),
	raise_exception(catch_in_query)
     ;  ( Y = 0
	  -> Z = 1		% X is left as a variable
	  ;  ( 0 =:= Y mod 2
	       -> ( Z =< 0
	            -> fail		% should fail
	            ;  ( X is float(round(exp(log(Z)/Y)));
	                 X is - float(round(exp(log(Z)/Y)))
		       ),
	  	       power2(Y, X, 1, Z)	% the test
	          )
	       ;  ( Z > 0
	            -> X is float(round(exp(log(abs(Z))/Y)))
	            ;  ( Z < 0
		         -> X is - float(round(exp(log(abs(Z))/Y)))
		         ;  fail	% when Z=0
		       )
	          ),
	  	  power2(Y, X, 1, Z)	% the test
	     )
	)
   ).

'power'(X, Y, Z) :-
   user:goedel_freeze(ground([X,Z]) or ground([Y,Z]) or ground([X,Y]),
		'Floats':power(X, Y, Z)).


'power2'(1, X, P, NewP) :-
   NewP is X*P, !.
'power2'(N, X, P, NewP) :-
   N > 1,
   P2 is X*P, 
   N2 is N - 1,
   power2(N2, X, P2, NewP).

/*------------------------------------------------------------------------------
 * mod is more restrictive than divides.
 */

'mod'(X, Y, Z) :-
   float(X), float(Y), !,
   ( Y = 0
     -> % Z = 0, but this value is never used
	format(user_error, "Arithmetic exception: mod by 0.~n", []),
	raise_exception(catch_in_query)
     ;  Z is X mod Y
   ).

'mod'(X, Y, Z) :-
   user:goedel_freeze(ground([X,Y]), 'Floats':mod(X, Y, Z)).

/*------------------------------------------------------------------------------
 * rem is more restrictive than divides.
 */

'rem'(X, Y, Z) :-
   float(X), float(Y), !,
   ( Y = 0
     -> % Z = 0, but this value is never used
	format(user_error, "Arithmetic exception: rem by 0.~n", []),
	raise_exception(catch_in_query)
     ;  A is X / Y,
        ( A > 0
	  -> A2 = A
	  ;  A2 is A - 1
	),
	Z is X - A2 * Y
   ).

'rem'(X, Y, Z) :-
   user:goedel_freeze(ground([X,Y]), 'Floats':rem(X, Y, Z)).

/*------------------------------------------------------------------------------
 */

 'absolute'(X, Y) :-
   float(X), !,
   ( X >= 0
     -> Y is X
     ;  Y is -X
   ).

'absolute'(X, Y) :-
   float(Y), !, Y >= 0, 
   ( X is Y
   ;
     X is -Y
   ).

'absolute'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':absolute(X, Y) ).

/*------------------------------------------------------------------------------
 * maximum and minimum assum that X and Y are floats.
 */

'sign'(X, Y) :-
   float(X), !,
   ( X = 0
     -> Y = 0
     ;  ( X > 0
          -> Y = 1
	  ;  Y = -1
	)
   ).

'sign'(X, Y) :-
   user:goedel_freeze(ground([X]), 'Floats':sign(X, Y) ).


/*------------------------------------------------------------------------------
 * maximum and minimum assum that X and Y are floats.
 */
 
'maximum'(X, Y, Z) :-
   float(X), float(Y), !,
   ( X < Y
     -> Z = Y
     ;  Z = X
   ).

'maximum'(X, Y, Z) :-
   user:goedel_freeze(ground([X,Y]), 'Floats':maximum(X, Y, Z) ).

/*------------------------------------------------------------------------------
 */

'minimum'(X, Y, Z) :-
   float(X), float(Y), !,
   ( X < Y
     -> Z = X
     ;  Z = Y
   ).

'minimum'(X, Y, Z) :-
user:goedel_freeze(ground([X,Y]), 'Floats':minimum(X, Y, Z) ).
 
'sinrad'(X, Y) :-
float(X), !,
   Y is sin(X).

'sinrad'(X, Y) :-
  float(Y), !,
   X is asin(Y).

'sinrad'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':sinrad(X, Y) ).

'tanrad'(X, Y) :-
float(X), !,
   Y is tan(X).

'tanrad'(X, Y) :-
  float(Y), !,
   X is atan(Y).

'tan rad'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':tanrad(X, Y) ).


'sqrtg'(X, Y) :-
float(X), !,
   Y is sqrt(X).

'sqrtg'(X, Y) :-
  float(Y), !,
   X is Y * Y.

'sqrtg'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':sqrtg(X, Y) ).

'successorg'(X, Y) :-
float(X), !,
   Y is X + 1.0.

'successorg'(X, Y) :-
  float(Y), !,
   X is Y - 1.0.

'successorg'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':successorg(X, Y) ).


'truncateg'(X, Y) :-
float(X), !,
   Y is truncate(X).

'truncateg'(X, Y) :-
   user:goedel_freeze(ground([X]), 'Floats':truncateg(X, Y) ).


'roundg'(X, Y) :-
float(X), !,
   Y is round(X).

'round'(X, Y) :-
   user:goedel_freeze(ground([X]), 'Floats':roundg(X, Y) ).

'integerpartg'(X, Y) :-
float(X), !,
   Y is integerpart(X).

'integerpart'(X, Y) :-
   user:goedel_freeze(ground([X]), 'Floats':integerpartg(X, Y) ).

'arcsinrad'(X, Y) :-
float(X), !,
   Y is asin(X).

'arcsinrad'(X, Y) :-
  float(Y), !,
   X is sin(Y).

'arcsinrad'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':arcsinrad(X, Y) ).

'arctanrad'(X, Y) :-
float(X), !,
   Y is atan(X).

'arctanrad'(X, Y) :-
  float(Y), !,
   X is tan(Y).

'arctanrad'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':arctanrad(X, Y) ).

'expe'(X, Y) :-
float(X), !,
   Y is exp(X).

'expe'(X, Y) :-
  float(Y), !,
  X is log(Y).

'expe'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':expe(X, Y) ).


'floorg'(X, Y) :-
float(X), !,
   Y is floorg(X).

'floorg'(X, Y) :-
   user:goedel_freeze(ground([X]), 'Floats':floorg(X, Y) ).

%%%%%%%%%%%%%%%%%%
'ceilingg'(X, Y) :-
float(X), !,
   Y is ceiling(X).

'ceilingg'(X, Y) :-
   user:goedel_freeze(ground([X]), 'Floats':ceilingg(X, Y) ).

%%%%%%%%%%%%%%%%%%
'cosrad'(X, Y) :-
float(X), !,
   Y is cos(X).

'cosrad'(X, Y) :-
  float(Y), !,
  X is acos(Y).

'cosrad'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':cosrad(X, Y) ).

%%%%%%%%%%%%%%%%%%
'cotrad'(X, Y) :-
float(X), !,
   Y is cot(X).

'cotrad'(X, Y) :-
  float(Y), !,
  X is acot(Y).

'cotrad'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':cotrad(X, Y) ).

%%%%%%%%%%%%%%%%%%
'sinhg'(X, Y) :-
float(X), !,
   Y is sinh(X).

'sinhg'(X, Y) :-
  float(Y), !,
  X is asinh(Y).

'sinhg'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':sinhg(X, Y) ).

%%%%%%%%%%%%%%%%%%
'coshg'(X, Y) :-
float(X), !,
   Y is cosh(X).

'coshg'(X, Y) :-
  float(Y), !,
  X is acosh(Y).

'coshg'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':coshg:(X, Y) ).

%%%%%%%%%%%%%%%%%%
'tanhg'(X, Y) :-
float(X), !,
   Y is tanh(X).

'tanhg'(X, Y) :-
  float(Y), !,
  X is atanh(Y).

'tanhg'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':tanhg(X, Y) ).

%%%%%%%%%%%%%%%%%%
'acosrad'(X, Y) :-
float(X), !,
   Y is acos(X).

'acosrad'(X, Y) :-
  float(Y), !,
  X is cos(Y).

'acosrad'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':acosrad(X, Y) ).

%%%%%%%%%%%%%%%%%%
'acotrad'(X, Y) :-
float(X), !,
   Y is acot(X).

'acotrad'(X, Y) :-
  float(Y), !,
  X is cot(Y).

'acotrad'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':acotrad(X, Y) ).

%%%%%%%%%%%%%%%%%%
'asinhg'(X, Y) :-
float(X), !,
   Y is asinh(X).

'asinhg'(X, Y) :-
  float(Y), !,
  X is sinh(Y).

'asinhg'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':asinhg(X, Y) ).

%%%%%%%%%%%%%%%%%%
'acoshg'(X, Y) :-
float(X), !,
   Y is acosh(X).

'acoshg'(X, Y) :-
  float(Y), !,
  X is cosh(Y).

'acoshg'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':acoshg(X, Y) ).

%%%%%%%%%%%%%%%%%%
'atanhg'(X, Y) :-
float(X), !,
   Y is atanh(X).

'atanhg'(X, Y) :-
  float(Y), !,
  X is tanh(Y).

'atanhg'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':atanhg(X, Y) ).

%%%%%%%%%%%%%%%%%%
'acothg'(X, Y) :-
float(X), !,
   Y is acoth(X).

'acothg'(X, Y) :-
  float(Y), !,
  X is coth(Y).

'acothg'(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Floats':acothg(X, Y) ).

