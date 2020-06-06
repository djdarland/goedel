:- module('FlocksIO', []).
:- multifile '$$module'/1.


:- op(500, yfx, and).
:- op(400, yfx, or).

'FlocksIO.GetFlock.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'FlocksIO':'FlocksIO.GetFlock.P2.0'(A,B)).
'~FlocksIO.GetFlock.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'FlocksIO':'~FlocksIO.GetFlock.P2.0'(A,B)).
'FlocksIO.GetFlock.P2.0'(A, B) :-
        'FlocksIO.ReadFlock.P2'(A, B).
'~FlocksIO.GetFlock.P2.0'(A, B) :-
        '~FlocksIO.ReadFlock.P2'(A, B).
'FlocksIO.PutFlock.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'FlocksIO':'FlocksIO.PutFlock.P2.0'(A,B)).
'~FlocksIO.PutFlock.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'FlocksIO':'~FlocksIO.PutFlock.P2.0'(A,B)).
'FlocksIO.PutFlock.P2.0'(A, B) :-
        'FlocksIO.WriteFlock.P2'(A, B).
'~FlocksIO.PutFlock.P2.0'(A, B) :-
        '~FlocksIO.WriteFlock.P2'(A, B).
/*
Subject:	Supporting routines for FlocksIO module
Author:		Jiwei Wang
Date:		2 Dec. 1992

-----------------------------------------------------------------------------*/


%% '$$module'('@(#)FlocksIO.sup 1.3 last updated 93/09/02 16:47:07 by jiwei').

%------------------------------------------------------------------------------

'FlocksIO.WriteFlock.P2'(Stream, Flock) :-
   'IO':translate_stream(Stream, PrologStream),
   writeq(PrologStream, Flock),
   write(PrologStream, '.'),
   nl(PrologStream).

'~FlocksIO.WriteFlock.P2'(Stream, Flock) :-
   'FlocksIO.WriteFlock.P2'(Stream, Flock).


'FlocksIO.ReadFlock.P2'(Stream, Flock) :-
   'IO':translate_stream(Stream, PrologStream),
   read(PrologStream, Flock).

'~FlocksIO.ReadFlock.P2'(Stream, Flock) :-
   'FlocksIO.ReadFlock.P2'(Stream, Flock).

%------------------------------------------------------------------------------
 
'FlocksIO.FlockCompile.P2'(GString, 'Flocks.Flock.F1'(FlockList)) :-
   nonvar(GString), !,
   user:gstring2string(GString, String),
   ( open(String, read, Stream, [type(binary)])
     -> user:read_file(Stream, Chars),
        'Units':token_identifiers(Chars, Tokens, []),
        user:flock_compile_cmd_aux(Tokens, FlockList), !,
        close(Stream)
     ;  format(user_error, '~nError: cannot open file "~a".~n', [String])
   ).

'FlocksIO.FlockCompile.P2'(GString, Flock) :-
   user:goedel_freeze(ground([GString]),
        'FlocksIO':'FlocksIO.FlockCompile.P2'(GString, Flock) ).


'~FlocksIO.FlockCompile.P2'(GString, Flock) :-
   'FlocksIO.FlockCompile.P2'(GString, Flock).

%------------------------------------------------------------------------------

'FlocksIO.FlockDecompile.P2'(Flock, GString) :-
   nonvar(Flock), !,
   nonvar(GString), !,
   user:gstring2string(GString, String),
   Flock = 'Flocks.Flock.F1'(List),
   ( open(String, write, Stream, [type(binary)])
     -> user:flock_decompile_cmd_aux(List, Stream), !,
        close(Stream)
     ;  format(user_error, '~nError: cannot open file "~a".~n', [String])
   ).


'FlocksIO.FlockDecompile.P2'(Flock, GString) :-
   user:goedel_freeze(ground([GString, Flock]),
        'FlocksIO':'FlocksIO.FlockDecompile.P2'(Flock, GString) ).


'~FlocksIO.FlockDecompile.P2'(Flock, GString) :-
   'FlocksIO.FlockDecompile.P2'(Flock, GString).

