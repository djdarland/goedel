:- module('M1', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

'M1.Append.P3'('M1.Nil.C0', A, A).
'~M1.Append.P3'('M1.Nil.C0', A, A).
'M1.Append.P3'('M1.Cons.F2'(A,B), C, 'M1.Cons.F2'(A,D)) :-
        'M1.Append.P3'(B, C, D).
'~M1.Append.P3'('M1.Cons.F2'(A,B), C, 'M1.Cons.F2'(A,D)) :-
        '~M1.Append.P3'(B, C, D).
'M1.Append3.P4'(A, B, C, D) :-
        'M1.Append.P3'(A, B, E),
        'M1.Append.P3'(E, C, D).
'~M1.Append3.P4'(A, B, C, D) :-
        '~M1.Append.P3'(A, B, E),
        '~M1.Append.P3'(E, C, D).
