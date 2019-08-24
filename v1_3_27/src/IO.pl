:- module('IO', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

'IO.NewLine.P1'(A) :-
        user:goedel_freeze(ground([A]), 'IO':'IO.NewLine.P1.0'(A)).
'~IO.NewLine.P1'(A) :-
        user:goedel_freeze(ground([A]), 'IO':'~IO.NewLine.P1.0'(A)).
'IO.NewLine.P1.0'(A) :-
        'IO.Put.P2'(A, 10).
'~IO.NewLine.P1.0'(A) :-
        '~IO.Put.P2'(A, 10).
'IO.WriteCharList.P2'([], _).
'~IO.WriteCharList.P2'([], _).
'IO.WriteCharList.P2'([A|B], C) :-
        'IO.Put.P2'(C, A),
        'IO.WriteCharList.P2'(B, C).
'~IO.WriteCharList.P2'([A|B], C) :-
        '~IO.Put.P2'(C, A),
        '~IO.WriteCharList.P2'(B, C).
'IO.WriteString.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'IO':'IO.WriteString.P2.0'(A,B)).
'~IO.WriteString.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'IO':'~IO.WriteString.P2.0'(A,B)).
'IO.WriteString.P2.0'(A, B) :-
        'Strings':'Strings.StringInts.P2'(B, C),
        'IO.WriteCharList.P2'(C, A).
'~IO.WriteString.P2.0'(A, B) :-
        'Strings':'~Strings.StringInts.P2'(B, C),
        '~IO.WriteCharList.P2'(C, A).
/* 
File:      IO.sup
Subject:   the system module IO.
Author:	   Jiwei Wang
Date:      17 September 1991

================================================================================
*/

'$$module'('@(#)IO.sup 1.12 last updated 93/07/31 14:53:33 by jiwei
').


?- dynamic '$end_of_stream'/1.


'IO.Get.P2'(X, Y) :-
   nonvar(X), !,
   'IO.Get.P2.1'(X,Y).

'IO.Get.P2'(X, Y) :-
   user:goedel_freeze(ground([X]), 'IO':'IO.Get.P2.1'(X,Y)).


'IO.Get.P2.1'('IO.StdIn.c0', Char) :-
   !,
   prompt(Old, ''),
   get0(user_input, Char),
   prompt(_, Old).

'IO.Get.P2.1'(ResultOfFind, Char) :-
   '$end_of_stream'(ResultOfFind), !,
   Char = -1.

'IO.Get.P2.1'(ResultOfFind, Char) :-
   translate_stream(ResultOfFind, Stream),
   get0(Stream, Char),
   check_for_end_of_stream(ResultOfFind, Char).

'~IO.Get.P2'(ResultOfFind, Char) :-
   'IO.Get.P2'(ResultOfFind, Char).

%------------------------------------------------------------------------------

'IO.ReadChar.P2'(X, Y) :-
   nonvar(X), !,
   'IO.ReadChar.P2.1'(X,Y).

'IO.ReadChar.P2'(X, Y) :-
   user:goedel_freeze(ground([X]), 'IO':'IO.ReadChar.P2.1'(X,Y)).


'IO.ReadChar.P2.1'('IO.StdIn.C0', String) :-
   !,
   prompt(Old, ''),
   get0(user_input, Char),
   name(String, [0'", Char]),
   prompt(_, Old).

'IO.ReadChar.P2.1'(ResultOfFind, String) :-
   '$end_of_stream'(ResultOfFind), !,
   name(String, [0'"]).   % return empty string

'IO.ReadChar.P2.1'(ResultOfFind, String) :-
   translate_stream(ResultOfFind, Stream),
   get0(Stream, Char),
   ( Char == -1
     -> assert('$end_of_stream'(ResultOfFind)),
        name(String, [0'"])   % return empty string
     ;  name(String, [0'", Char])
   ).

'~IO.ReadChar.P2'(ResultOfFind, String) :-
   'IO.ReadChar.P2'(ResultOfFind, String).

%------------------------------------------------------------------------------

'IO.Put.P2'(X, Y) :-
   nonvar(X), integer(Y), !,
   'IO.Put.P2.1'(X,Y).

'IO.Put.P2'(X, Y) :-
   user:goedel_freeze(ground([X,Y]), 'IO':'IO.Put.P2.1'(X,Y)).


'IO.Put.P2.1'(ResultOfFind, Char) :-
   Char > 0, Char < 128,
   translate_stream(ResultOfFind, Stream),
   put(Stream, Char).

'~IO.Put.P2'(ResultOfFind, Char) :-
   'IO.Put.P2'(ResultOfFind, Char).

%------------------------------------------------------------------------------

check_for_end_of_stream(ResultOfFind, C) :-
   ( C == -1
     -> assert('$end_of_stream'(ResultOfFind))
     ;  true
   ).

%------------------------------------------------------------------------------

'IO.FindInput.P2'(X, Y) :-
   user:goedel_freeze(ground([X]), 'IO':'IO.FindInput.P2.1'(X,Y)).

'IO.FindInput.P2.1'(FileString, ResultOfFind) :-
   user:gstring2string(FileString, FileName),
   ( open(FileName, read, Stream)
     -> translate_stream('IO.InputStreamDescriptor.F1'(List), Stream),
	ResultOfFind = 'IO.In.F1'('IO.InputStreamDescriptor.F1'(List))
     ;  ResultOfFind = 'IO.NotFound.C0'
   ).

'~IO.FindInput.P2'(FileString, ResultOfFind) :-
   'IO.FindInput.P2'(FileString, ResultOfFind).

%------------------------------------------------------------------------------
 
'IO.FindOutput.P2'(X, Y) :-
   user:goedel_freeze(ground([X]), 'IO':'IO.FindOutput.P2.1'(X,Y)).

'IO.FindOutput.P2.1'(FileString, ResultOfFind) :-
   user:gstring2string(FileString, FileName),
   ( open(FileName, write, Stream)
     -> translate_stream('IO.OutputStreamDescriptor.F1'(List), Stream),
	ResultOfFind = 'IO.Out.F1'('IO.OutputStreamDescriptor.F1'(List))
     ;  ResultOfFind = 'IO.NotFound.C0'
   ).

'~IO.FindOutput.P2'(FileString, ResultOfFind) :-
   'IO.FindOutput.P2'(FileString, ResultOfFind).

%------------------------------------------------------------------------------

'IO.FindUpdate.P2'(X, Y) :-
   user:goedel_freeze(ground([X]), 'IO':'IO.FindUpdate.P2.1'(X,Y)).

'IO.FindUpdate.P2.1'(FileString, ResultOfFind) :-
   user:gstring2string(FileString, FileName),
   ( open(FileName, append, Stream)
     -> translate_stream('IO.OutputStreamDescriptor.F1'(List), Stream),
	ResultOfFind = 'IO.Out.F1'('IO.OutputStreamDescriptor.F1'(List))
     ;  ResultOfFind = 'IO.NotFound.C0'
   ).

'~IO.FindUpdate.P2'(FileString, ResultOfFind) :-
   'IO.FindUpdate.P2'(FileString, ResultOfFind).

%------------------------------------------------------------------------------

'IO.EndInput.P1'(X) :-
   user:goedel_freeze(ground([X]), 'IO':'IO.EndInput.P1.1'(X)).

'IO.EndInput.P1.1'(GoedelStream) :-
   retractall('$end_of_stream'(GoedelStream)),
   translate_stream(GoedelStream, Stream),
   close(Stream).

'~IO.EndInput.P1'(GoedelStream) :-
   'IO.EndInput.P1'(GoedelStream).

%------------------------------------------------------------------------------

'IO.EndOutput.P1'(X) :-
   user:goedel_freeze(ground([X]), 'IO':'IO.EndOutput.P1.1'(X)).

'IO.EndOutput.P1'(GoedelStream) :-
   translate_stream(GoedelStream, Stream),
   close(Stream).

'~IO.EndOutput.P1'(GoedelStream) :-
   'IO.EndOutput.P1'(GoedelStream).

%------------------------------------------------------------------------------

'IO.Flush.P1'(X) :-
   nonvar(X), !,
   'IO.Flush.P1.1'(X).

'IO.Flush.P1'(X) :-
   user:goedel_freeze(ground([X]), 'IO':'IO.Flush.P1.1'(X)).

'IO.Flush.P1.1'(GoedelStream) :-
   translate_stream(GoedelStream, Stream),
   flush_output(Stream).

'~IO.Flush.P1'(GoedelStream) :-
   'IO.Flush.P1'(GoedelStream).

%------------------------------------------------------------------------------
 
translate_stream('IO.InputStreamDescriptor.F1'(List), Stream) :-
   Stream =.. ['$stream'|List].
translate_stream('IO.OutputStreamDescriptor.F1'(List), Stream) :-
   Stream =.. ['$stream'|List].
translate_stream('IO.StdIn.C0', user_input).
translate_stream('IO.StdOut.C0', user_output).
translate_stream('IO.StdErr.C0', user_error).
