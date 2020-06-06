:- module('Rationals', []).
:- multifile '$$module'/1.

:- op(500, yfx, and).
:- op(400, yfx, or).

/* 
File:		Rationals.sup
Subject:	Implementation of the system module Rationals.
Author: 	Jiwei Wang
Date:		29 April 1992

================================================================================
*/


%% '$$module'('@(#)Rationals.sup 1.8 last updated 93/12/08 16:19:16 by jiwei').

%------------------------------------------------------------------------------
% not completely certain about the optimized implementation.
%------------------------------------------------------------------------------


'Rationals.=<.P2'(X, Y) :-
   nonvar(X), nonvar(Y), !,
   eval(X, XX), eval(Y, YY), XX =< YY.

'Rationals.=<.P2'(X, Y) :-
   user:goedel_freeze(ground([X,Y]),
   	('Rationals':eval(X, XX), 'Rationals':eval(Y, YY), XX =< YY)).

'~Rationals.=<.P2'(X, Y) :-
   nonvar(X), nonvar(Y), !,
   eval(X, XX), eval(Y, YY), XX =< YY.

'~Rationals.=<.P2'(X, Y) :-
   user:goedel_freeze(ground([X,Y]),
	('Rationals':eval(X, XX), 'Rationals':eval(Y, YY), XX =< YY)).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

'Rationals.>=.P2'(X, Y) :-
   nonvar(X), nonvar(Y), !,
   eval(X, XX), eval(Y, YY), XX >= YY.

'Rationals.>=.P2'(X, Y) :-
   user:goedel_freeze(ground([X,Y]),
   	('Rationals':eval(X, XX), 'Rationals':eval(Y, YY), XX >= YY)).

'~Rationals.>=.P2'(X, Y) :-
   nonvar(X), nonvar(Y), !,
   eval(X, XX), eval(Y, YY), XX >= YY.

'~Rationals.>=.P2'(X, Y) :-
   user:goedel_freeze(ground([X,Y]),
   	('Rationals':eval(X, XX), 'Rationals':eval(Y, YY), XX >= YY)).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

'Rationals.>.P2'(X, Y) :-
   nonvar(X), nonvar(Y), !,
   eval(X, XX), eval(Y, YY), XX > YY.

'Rationals.>.P2'(X, Y) :-
   user:goedel_freeze(ground([X,Y]),
   	('Rationals':eval(X, XX), 'Rationals':eval(Y, YY), XX > YY)).

'~Rationals.>.P2'(X, Y) :-
   nonvar(X), nonvar(Y), !,
   eval(X, XX), eval(Y, YY), XX > YY.

'~Rationals.>.P2'(X, Y) :-
   user:goedel_freeze(ground([X,Y]),
   	('Rationals':eval(X, XX), 'Rationals':eval(Y, YY), XX > YY)).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

'Rationals.<.P2'(X, Y) :-
   nonvar(X), nonvar(Y), !,
   eval(X, XX), eval(Y, YY), XX < YY.

'Rationals.<.P2'(X, Y) :-
   user:goedel_freeze(ground([X,Y]),
   	('Rationals':eval(X, XX), 'Rationals':eval(Y, YY), XX < YY)).

'~Rationals.<.P2'(X, Y) :-
   nonvar(X), nonvar(Y), !,
   eval(X, XX), eval(Y, YY), XX < YY.

'~Rationals.<.P2'(X, Y) :-
   user:goedel_freeze(ground([X,Y]),
   	('Rationals':eval(X, XX), 'Rationals':eval(Y, YY), XX < YY)).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

'Rationals.StandardRational.P3'(X, Y, Z) :-
   user:goedel_freeze(ground([X]), X = Y//Z).

/*==============================================================================
 * Following are functions that support the constraint solving with integers.
 */

% for //
% Note: x//y= 3//5 has infinite solutions.

rational(X//Y, Z) :-
   integer(X), integer(Y), !,
   simplify(X//Y, Z).

rational(X//Y, Z) :-
   nonvar(Z), integer(X), !,
   assign(X//Y, Z).

rational(X//Y, Z) :-
   nonvar(Z), integer(Y), !,
   assign(X//Y, Z).

rational(X//Y, Z) :-
   user:goedel_freeze(ground([X,Z]) or ground([Y,Z]) or ground([X,Y]),
			'Rationals':rational(X//Y, Z) ).

%------------------------------------------------------------------------------

plus(A//B, C//D, X) :-
   integer(A), integer(B),
   integer(C), integer(D), !,
   simplify( (A*D + B*C) // (B*D), Z),
   assign(X, Z).

plus(A//B, X, C//D) :-
   integer(A), integer(B),
   integer(C), integer(D), !,
   simplify( (-A*D + B*C) // (B*D), Z),
   assign(X, Z).

plus(X, A//B, C//D) :-
   integer(A), integer(B),
   integer(C), integer(D), !,
   simplify( (-A*D + B*C) // (B*D), Z),
   assign(X, Z).

plus(X, Y, Z) :-
   user:goedel_freeze(ground([X,Z]) or ground([Y,Z]) or ground([X,Y]),
			'Rationals':plus(X, Y, Z)).

%------------------------------------------------------------------------------

negative(A//B, X) :-
   nonvar(A), !,
   simplify((-A)//B, Z),
   assign(X, Z).

negative(-(A//B), X) :-
   nonvar(A), !,
   simplify(A//B, Z),
   assign(X, Z).

negative(X, A//B) :-
   nonvar(A), !,
   simplify((-A)//B, Z),
   assign(X, Z).

negative(X, -(A//B)) :-
   nonvar(A), !,
   simplify(A//B, Z),
   assign(X, Z).

negative(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Rationals':negative(X, Y)).


%------------------------------------------------------------------------------

minus(A//B, C//D, X) :-
   integer(A), integer(B),
   integer(C), integer(D), !,
   simplify( (A*D - B*C) // (B*D), Z),
   assign(X, Z).

minus(A//B, X, C//D) :-
   integer(A), integer(B),
   integer(C), integer(D), !,
   simplify( (A*D - B*C) // (B*D), Z),
   assign(X, Z).

minus(X, A//B, C//D) :-
   integer(A), integer(B),
   integer(C), integer(D), !,
   simplify( (A*D + B*C) // (B*D), Z),
   assign(X, Z).

minus(X, Y, Z) :-
   user:goedel_freeze(ground([X,Z]) or ground([Y,Z]) or ground([X,Y]),
			'Rationals':minus(X, Y, Z)).

%------------------------------------------------------------------------------
 
times(A//B, C//D, X) :-
   integer(A), integer(B),
   integer(C), integer(D), !,
   simplify( (A*C) // (B*D), Z),
   assign(X, Z).

times(A//B, X, C//D) :-
   integer(A), integer(B),
   integer(C), integer(D), !,
   ( A =:= 0, C =:= 0
     -> true		% X is left uninstantiated
     ;  A =\= 0,	% make sure A//B is not zero. If A//B is zero, it fails.
        simplify( (B*C) // (A*D), Z),
        assign(X, Z)
   ).

times(X, A//B, C//D) :-
   integer(A), integer(B),
   integer(C), integer(D), !,
   ( A =:= 0, C =:= 0
     -> true		% X is left uninstantiated
     ;  A =\= 0,	% make sure A//B is not zero. If A//B is zero, it fails.
        simplify( (B*C) // (A*D), Z),
        assign(X, Z)
   ).

times(X, Y, Z) :-
   user:goedel_freeze(ground([X,Z]) or ground([Y,Z]) or ground([X,Y]),
			'Rationals':times(X, Y, Z)).

%------------------------------------------------------------------------------
 
divides(A//B, C//D, X) :-
   integer(A), integer(B),
   integer(C), integer(D), !,
   ( C = 0
     -> % X = 0, but this value is never used
        format(user_error, "Arithmetic exception: division by 0.~n", []),
	raise_exception(catch_in_query)
     ;  simplify( (A*D) // (B*C), Z),
        assign(X, Z)
   ).

divides(X, A//B, C//D) :-
   integer(A), integer(B),
   integer(C), integer(D), !,
   ( A = 0
     -> % X = 0, but this value is never used
        format(user_error, "Arithmetic exception: division by 0.~n", []),
	raise_exception(catch_in_query)
     ;  simplify( (A*C) // (B*D), Z),
        assign(X, Z)
   ).

divides(A//B, X, C//D) :-
   integer(A), integer(B),
   integer(C), integer(D), !,
   simplify( (A*C) // (B*D), Z),
   assign(X, Z).

divides(X, Y, Z) :-
   user:goedel_freeze(ground([Y,Z]) or ground([X,Z]) or ground([X,Y]),
		'Rationals':divides(X, Y, Z)).

%------------------------------------------------------------------------------

power(A//B, Y, Z) :-
   integer(A), integer(B),
   integer(Y), !,
   ( Y = 0
     -> Z = 1//1
     ;  Y2 is abs(Y),
        power2(Y2, A, 1, A2),
        power2(Y2, B, 1, B2),
        ( Y > 0
          -> simplify(A2 // B2, C)
          ;  simplify(B2 // A2, C)
        ),
   	assign(Z, C)
   ).

power(A//B, X, C//D) :-
   integer(A), integer(B),
   integer(C), integer(D), !,
   A*B*C*D >= 0,		% checking X and Z are of the same sign
   ( A = 0, C = 0
     -> 'Rationals.>.P2'(X, 0)	% add constraint X > 0 
     ;  ( A = 0, C = 1, D = 1
	  -> X = 0//1
	  ;  ( (A = 0, C =\= 0; A =\= 0, C = 0)
	       -> fail
	       ;  ( A = 1
		    -> C = 1	% if C \= 1 this fails, X is left uninstantiated
		    ;  X is integer(round(log(abs(C/D))/log(abs(A/B)))),
     		       X2 is abs(X),
        	       power2(X2, A, 1, A2),
        	       power2(X2, B, 1, B2),
        	       ( X > 0
          		 -> simplify(A2 // B2, Z)
          		 ;  simplify(B2 // A2, Z)
        	       ),
		       simplify(C//D, Z)	% testing integers
		  )
	     )
	)
   ).

power(X, Y, C//D) :-
   integer(Y), integer(C), integer(D), !,
   ( Y = 0
     -> C = 1              % X is left as a variable
     ;  A = integer(round(exp(log(abs(C))/Y))),
	B = integer(round(exp(log(abs(D))/Y))),
        Y2 is abs(Y),
        power2(Y2, A, 1, A2),
        power2(Y2, B, 1, B2),
        ( Y > 0
          -> simplify(A2 // B2, S)
          ;  simplify(B2 // A2, S)
        ),
        simplify(abs(C)//abs(D), S),	% testing integers
	( 0 =:= Y mod 2
	  -> ( C < 0
	       -> fail			% should fail
	       ;  ( Y > 0
	            -> ( simplify(A//B, Z), assign(X, Z);
		         simplify((-A)//B, Z), assign(X, Z)
		       )
	            ;  ( simplify(B//A, Z), assign(X, Z);
		         simplify((-B)//A, Z), assign(X, Z)
		       )
		  )
	     )
	  ;  ( Y > 0
	       -> simplify(A//B, Z), assign(X, Z)
	       ;  simplify(B//A, Z), assign(X, Z)
	     )
	)
   ).

power(X, Y, Z) :-
   user:goedel_freeze(ground([X,Z]) or ground([Y,Z]) or ground([X,Y]),
		'Rationals':power(X, Y, Z)).


power2(1, X, P, NewP) :-
   NewP is X*P, !.
power2(N, X, P, NewP) :-
   N > 1,
   P2 is X*P, 
   N2 is N - 1,
   power2(N2, X, P2, NewP).

%------------------------------------------------------------------------------

absolute(A // B, Y) :-
   integer(A), !,
   ( A >= 0
     -> Y = A // B
     ;  simplify((-A) // B, Y)
   ).

absolute(X, A//B) :-
   integer(A), !,
   ( X = A//B
   ;
     simplify((-A) // B, X)
   ).

absolute(X, Y) :-
   user:goedel_freeze(ground([Y]) or ground([X]), 'Rationals':absolute(X, Y) ).

%------------------------------------------------------------------------------
% The first case prevents a zero rational slips through.

sign(X, 0) :-
   nonvar(X), X = 0, !.

sign(A // _, Y) :-
   integer(A), !,
   ( A = 0
     -> Y = 0
     ;  ( A > 0
          -> Y = 1
	  ;  Y = -1
	)
   ).

sign(X, Y) :-
   user:goedel_freeze(ground([X]), 'Rationals':sign(X, Y) ).

/*------------------------------------------------------------------------------
 * maximum and minimum assum that X and Y are integers.
 */
 
maximum(X, Y, Z) :-
   user:goedel_freeze(ground([X,Y]), 'Rationals':maximum_aux(X, Y, Z) ).

maximum_aux(X, Y, Z) :-
   eval(X, A),
   eval(Y, B),
   ( B > A
     -> Z = Y
     ;  Z = X
   ).

/*------------------------------------------------------------------------------
 */

minimum(X, Y, Z) :-
   user:goedel_freeze(ground([X,Y]), 'Rationals':minimum_aux(X, Y, Z) ).
 
minimum_aux(X, Y, Z) :-
   eval(X, A),
   eval(Y, B),
   ( B < A
     -> Z = Y
     ;  Z = X
   ).

%------------------------------------------------------------------------------

% // is defined as a function in SICStus
simplify(Expr1 // Expr2, Result) :- !,
   X is Expr1,
   Y is Expr2,
   ( Y = 0
     -> Result = 0,
	format(user_error, "Arithmetic exception: illegal rational number.~n",
		[]),
	raise_exception(catch_in_query)
     ;  ( Xabs is abs(X),
	  Yabs is abs(Y),
	  gcd(Xabs, Yabs, GCD),
	  M is Xabs // GCD,
	  N is Yabs // GCD,
	  ( X = 0
	    -> Result = 0 // 1
	    ; ( ( X > 0, Y > 0; X < 0, Y < 0 ) 
	        -> Result = M // N
	        ;  NegM is -M,
		   Result = NegM // N
	      )
	  )
	)
   ).

simplify(Expr, Result) :- 
   Result is Expr.


% Euclid's algorithm
gcd(X, Y, Z) :-
   A is X mod Y,
   ( A = 0
     -> Z = Y
     ;  gcd(Y, A, Z)
   ).

%------------------------------------------------------------------------------

eval(X // Y, Z) :- !,
   Z is X / Y.

eval(X, X).

%------------------------------------------------------------------------------
% assign(X//Y, A//B) assigns the value of A//B to X//Y in which X or Y or both
% can be variables.

assign(X//Y, A//B) :-
   var(X), var(Y), !,
   X = A,
   Y = B.

assign(X//Y, A//B) :-
   var(X), !,
   (Y*A) mod B =:= 0,
   X is (Y*A) // B.

assign(X//Y, A//B) :-
   var(Y), !,
   (X*B) mod A =:= 0,
   Y is (X*B) // A.

