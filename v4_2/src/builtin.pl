:- multifile '$$module'/1.

% Copyright (C) Goedel Group, University of Bristol, June 1992.
% Title and ownership of all Goedel software originating from the Goedel
% Group at the University of Bristol remains with the Goedel Group.
%
% The Goedel language was designed by P.M. Hill and J.W. Lloyd, and the 
% language was implemented by A. Bowers and J. Wang. The design and 
% implementation of the language was partly supported by the ESPRIT Basic 
% Research Action 3012 (Compulog), a SERC Grant GR/F/26256, and the 
% University of Bristol.
%
% This software is available ONLY for research and educational purposes.
% In particular, it may not be used for any commercial purpose whatsoever.
% The software is free and is provided "as is", without warranty of any kind.
% Users of the software are free to modify and experiment with it for 
% research and educational purposes. However, users may not distribute 
% the software (modified or not) to third parties without the express 
% permission of the Goedel Group. The normal method of obtaining this 
% software is by ftp from ftp.cs.kuleuven.ac.be.
%
% Any enquiries about this copyright notice or the use of this software 
% should be directed to J.W. Lloyd at jwl@compsci.bristol.ac.uk.


/* 
File:		builtin.pl
Subject:	predicates used in Program.pl
Author: 	Jiwei Wang
Date:		16 Feb. 1992

================================================================================
*/

%% '$$module'('@(#)builtin.pl 1.15 last updated 93/12/21 12:21:41 by jiwei').


:- op(500, yfx, and).
:- op(400, yfx, or).

/*------------------------------------------------------------------------------
 * Integers
 *------------------------------------------------------------------------------
 */


'Integers.=<.P2'(X, Y) :-
   X =< Y.

'Integers.>=.P2'(X, Y) :-
   X >= Y.


'Integers.>.P2'(X, Y) :-
   X > Y.

'Integers.<.P2'(X, Y) :-
   X < Y.

/*------------------------------------------------------------------------------
 */

plus(X, Y, Z) :-
   integer(X), integer(Y), !, Z is X + Y.

plus(X, Y, Z) :-
   integer(X), integer(Z), !, Y is Z - X.

plus(X, Y, Z) :-
   integer(Y), integer(Z), !, X is Z - Y.

plus(X, Y, Z) :-
   goedel_freeze(ground([X,Y]) or ground([Y,Z]) or ground([Z,X]), 
		plus(X, Y, Z) ).

/*------------------------------------------------------------------------------
 */

minus(X, Y, Z) :-
   plus(Z, Y, X).
 
%------------------------------------------------------------------------------

negative(X, Y) :-
   integer(X), !, Y is -X.

negative(X, Y) :-
   integer(Y), !, X is -Y.

negative(X, Y) :-
   goedel_freeze(ground([X]) or ground([Y]), negative(X, Y) ).

/*------------------------------------------------------------------------------
 */
 
times(X, Y, Z) :-
   integer(X), integer(Y), !, Z is X * Y.

times(X, Y, Z) :-
   integer(X), integer(Z), !, Y is Z // X, Z =:= X * Y.

times(X, Y, Z) :-
   integer(Y), integer(Z), !, X is Z // Y, Z =:= X * Y.

times(X, Y, Z) :-
   goedel_freeze(ground([X,Y]) or ground([Y,Z]) or ground([Z,X]), 
		times(X, Y, Z) ).

/*------------------------------------------------------------------------------
 */
 
divides(X, Y, Z, W) :-
   integer(X), integer(Y), !,
   ( Y > 0 -> 
     X1 = X
   ; X >= 0 ->
     X1 = X - Y
   ; X1 = X + Y
   ),
   Z is X1 // Y,
   W is X - Y * Z.

divides(X, Y, Z, W) :-
   integer(X), integer(Z), integer(W), !,
   Y is (X - W) // Z,
   Y * Z =:= X - W, 
   ( Y > 0 ->
     0 =< W, W < Y
   ; Y < 0, Y < W, W =< 0
   ).  

divides(X, Y, Z, W) :-
   integer(Y), integer(Z), integer(W), !,
   ( Y > 0 ->
     0 =< W, W < Y
   ; Y < 0, Y < W, W =< 0
   ),  
   X is Y * Z + W.

divides(X, Y, Z, W) :-
   goedel_freeze(ground([X,Y]) or ground([X,Z,W]) or ground([Z,Y,W]), 
		divides(X, Y, Z, W) ).

/*------------------------------------------------------------------------------
 'Integers.Is.P2'(L1, L) is not supported !
 */


'Integers.Interval.P3'(L, H, N) :-
   goedel_freeze(ground([L,H]), 'Integers.Interval.0'(L, H, N) ).

'Integers.Interval.0'(L, H, N) :-
   'Integers.Is.P2'(L1, L), 'Integers.Is.P2'(H1, H),
   ( var(N) ->
     L1 =< H1,
     between(L1, H1, N)
   ; 
     integer(N),
     L1 =< N, N =< H1
   ).


between(L, _, L).

between(L, H, N) :-
   L < H,
   L1 is L + 1,
   between(L1, H, N).
 
/*------------------------------------------------------------------------------
 */

'Integers.Absolute.P2'(X, Y) :-
   integer(X), !,
   ( X >= 0 ->
     Y is X
   ; Y is -X
   ).

'Integers.Absolute.P2'(X, Y) :-
   integer(Y), !, Y >= 0,
   ( X is Y
   ;
     X is -Y
   ).

'Integers.Absolute.P2'(X, Y) :-
   goedel_freeze(ground([X]) or ground([Y]), 'Integers.Absolute.P2'(X, Y) ).

/*------------------------------------------------------------------------------
 * Strings
 *------------------------------------------------------------------------------
 */

'Strings.StringInts.P2'(String, IntList) :-
   name(String, [0'"|IntList]).


concat(X, Y, Z) :-
   nonvar(X), nonvar(Y), !,
   'Strings.StringInts.P2'(X, U),
   'Strings.StringInts.P2'(Y, V),
   'Lists':'Lists.Append.P3'(U, V, W),
   'Strings.StringInts.P2'(Z, W).

concat(X, Y, Z) :-
   nonvar(Z), !,
   'Strings.StringInts.P2'(Z, W),
   'Lists':'Lists.Append.P3'(U, V, W),
   'Strings.StringInts.P2'(X, U),
   'Strings.StringInts.P2'(Y, V).

concat(X, Y, Z) :-
   goedel_freeze(ground([X,Y]) or ground([Z]), concat(X,Y,Z)).

/*------------------------------------------------------------------------------
 */

'Strings.<.P2'(X, Y) :-
   X @< Y.

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

'Strings.>.P2'(X, Y) :-
   X @> Y.


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

'Strings.=<.P2'(X, Y) :-
   X @=< Y.


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

'Strings.>=.P2'(X, Y) :-
   X @>= Y.


/*------------------------------------------------------------------------------
 * Lists
 *------------------------------------------------------------------------------
 */

'Lists.Member.P2'(A, [A|_]).
'Lists.Member.P2'(A, [_|B]) :-
        'Lists.Member.P2'(A, B).
'Lists.Delete.P3'(A, [A|B], B).
'Lists.Delete.P3'(A, [B|C], [B|D]) :-
        'Lists.Delete.P3'(A, C, D).
'Lists.Append.P3'([], A, A).
'Lists.Append.P3'([A|B], C, [A|D]) :-
        'Lists.Append.P3'(B, C, D).
'Lists.DeleteFirst.P3'(A, [A|B], B) :- !.
'Lists.DeleteFirst.P3'(A, [B|C], [B|D]) :-
        not_equal([A,B], A, B), !,
        'Lists.DeleteFirst.P3'(A, C, D).
'Lists.Length.P2'(A, B) :-
        length(A, B).
'Lists.Reverse.P2'(A, B) :-
        'Lists.Reverse3.P3'(A, [], B).
'Lists.Permutation.P2'([], []).
'Lists.Permutation.P2'([A|B], [C|D]) :-
        'Lists.Delete.P3'(C, [A|B], E),
        'Lists.Permutation.P2'(E, D).
'Lists.MemberCheck.P2'(A, [A|_]) :- !.
'Lists.MemberCheck.P2'(A, [_|C]) :-
        'Lists.MemberCheck.P2'(A, C).
'Lists.Merge.P3'([], A, A).
'Lists.Merge.P3'(A, [], A).
'Lists.Merge.P3'([A|B], [C|D], [A|E]) :-
        'Integers.<.P2'(A, C),
        'Lists.Merge.P3'(B, [C|D], E).
'Lists.Merge.P3'([A|B], [C|D], [C|E]) :-
        'Integers.>=.P2'(A, C),
        'Lists.Merge.P3'([A|B], D, E).
'Lists.Sorted.P1'([]).
'Lists.Sorted.P1'([_]).
'Lists.Sorted.P1'([A,B|C]) :-
        'Integers.=<.P2'(A, B),
        'Lists.Sorted.P1'([B|C]).
'Lists.Reverse3.P3'([A|B], C, D) :-
        'Lists.Reverse3.P3'(B, [A|C], D).
'Lists.Reverse3.P3'([], A, A).

/*------------------------------------------------------------------------------
 * Lists
 *------------------------------------------------------------------------------
 */


'IO.NewLine.P1'(A) :-
        'IO.Put.P2'(A, 10).

'IO.WriteCharList.P2'(_, []).
'IO.WriteCharList.P2'(A, [B|C]) :-
        'IO.Put.P2'(A, B),
        'IO.WriteCharList.P2'(A, C).

'IO.WriteString.P2'(A, B) :-
        'Strings.StringInts.P2'(B,C),
        'IO.WriteCharList.P2'(A, C).


?- dynamic '$end_of_stream'/1.


'IO.Get.P2'('IO.StdIn.C0', Char) :-
   !,
   prompt(Old, ''),
   get_byte(user_input, Char),
   prompt(_, Old).

'IO.Get.P2'(ResultOfFind, Char) :-
   '$end_of_stream'(ResultOfFind), !,
   Char = -1.

'IO.Get.P2'(ResultOfFind, Char) :-
   'IO':translate_stream(ResultOfFind, Stream),
   get_byte(Stream, Char),
   check_for_end_of_stream(ResultOfFind, Char).

%------------------------------------------------------------------------------

'IO.ReadChar.P2'('IO.StdIn.C0', String) :-
   !,
   prompt(Old, ''),
   get_byte(user_input, Char),
   'Strings.StringInts.P2'(String, [Char]),
   prompt(_, Old).

'IO.ReadChar.P2'(ResultOfFind, String) :-
   '$end_of_stream'(ResultOfFind), !,
   'Strings.StringInts.P2'(String, []).   % get empty string

'IO.ReadChar.P2'(ResultOfFind, String) :-
   'IO':translate_stream(ResultOfFind, Stream),
   get_byte(Stream, Char),
   'Strings.StringInts.P2'(String, [Char]),
   check_for_end_of_stream(ResultOfFind, Char).

%------------------------------------------------------------------------------

'IO.Put.P2'(X, Y) :-
   user:goedel_freeze(ground([X,Y]), 'IO':'IO.Put.P2.1'(X,Y)).

'IO.Put.P2.1'(ResultOfFind, Char) :-
   Char > 0, Char < 128,
   'IO':translate_stream(ResultOfFind, Stream),
   put_byte(Stream, Char).

%------------------------------------------------------------------------------

check_for_end_of_stream(_, C) :-
   C \== -1, !.

check_for_end_of_stream(ID, -1) :-
   !, assert('$end_of_stream'(ID)).

%------------------------------------------------------------------------------

'IO.FindInput.P2'(FileString, ResultOfFind) :-
   user:gstring2string(FileString, FileName),
   ( open(FileName, read, Stream, [type(binary)])
     -> 'IO':translate_stream('IO.InputStreamDescriptor.F1'(List), Stream),
	ResultOfFind = 'IO.In.F1'('IO.InputStreamDescriptor.F1'(List))
     ;  ResultOfFind = 'IO.NotFound.C0'
   ).

%------------------------------------------------------------------------------
 
'IO.FindOutput.P2'(FileString, ResultOfFind) :-
   user:gstring2string(FileString, FileName),
   ( open(FileName, write, Stream, [type(binary)])
     -> 'IO':translate_stream('IO.OutputStreamDescriptor.F1'(List), Stream),
	ResultOfFind = 'IO.Out.F1'('IO.OutputStreamDescriptor.F1'(List))
     ;  ResultOfFind = 'IO.NotFound.C0'
   ).

%------------------------------------------------------------------------------

'IO.FindUpdate.P2'(FileString, ResultOfFind) :-
   user:gstring2string(FileString, FileName),
   ( open(FileName, append, Stream, [type(binary)])
     -> 'IO':translate_stream('IO.OutputStreamDescriptor.F1'(List), Stream),
	ResultOfFind = 'IO.Out.F1'('IO.OutputStreamDescriptor.F1'(List))
     ;  ResultOfFind = 'IO.NotFound.C0'
   ).

%------------------------------------------------------------------------------

'IO.EndInput.P1'(GoedelStream) :-
   retractall('$end_of_stream'(GoedelStream)),
   'IO':translate_stream(GoedelStream, Stream),
   close(Stream).

%------------------------------------------------------------------------------

'IO.EndOutput.P1'(GoedelStream) :-
   'IO':translate_stream(GoedelStream, Stream),
   close(Stream).

%------------------------------------------------------------------------------

'IO.Flush.P1'(GoedelStream) :-
   'IO':translate_stream(GoedelStream, Stream),
   flush_output(Stream).

