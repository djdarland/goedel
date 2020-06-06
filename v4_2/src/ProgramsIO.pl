:- module('ProgramsIO', []).
:- multifile '$$module'/1.

:- op(500, yfx, and).
:- op(400, yfx, or).

'ProgramsIO.ProgramCompile.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'ProgramsIO':'ProgramsIO.ProgramCompile.P2.0'(A,B)).
'~ProgramsIO.ProgramCompile.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'ProgramsIO':'~ProgramsIO.ProgramCompile.P2.0'(A,B)).
'ProgramsIO.ProgramCompile.P2.0'(A, B) :-
        'ProgramsIO.ProgramCompileAux.P2'(A, B).
'~ProgramsIO.ProgramCompile.P2.0'(A, B) :-
        '~ProgramsIO.ProgramCompileAux.P2'(A, B).
'ProgramsIO.GetProgram.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'ProgramsIO':'ProgramsIO.GetProgram.P2.0'(A,B)).
'~ProgramsIO.GetProgram.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'ProgramsIO':'~ProgramsIO.GetProgram.P2.0'(A,B)).
'ProgramsIO.GetProgram.P2.0'(A, B) :-
        'ProgramsIO.GetProgramAux.P2'(A, B).
'~ProgramsIO.GetProgram.P2.0'(A, B) :-
        '~ProgramsIO.GetProgramAux.P2'(A, B).
'ProgramsIO.PutProgram.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'ProgramsIO':'ProgramsIO.PutProgram.P2.0'(A,B)).
'~ProgramsIO.PutProgram.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'ProgramsIO':'~ProgramsIO.PutProgram.P2.0'(A,B)).
'ProgramsIO.PutProgram.P2.0'(A, B) :-
        'ProgramsIO.PutProgramAux.P2'(A, B).
'~ProgramsIO.PutProgram.P2.0'(A, B) :-
        '~ProgramsIO.PutProgramAux.P2'(A, B).
'ProgramsIO.ProgramDecompile.P1'(A) :-
        user:goedel_freeze(ground([A]), 'ProgramsIO':'ProgramsIO.ProgramDecompile.P1.0'(A)).
'~ProgramsIO.ProgramDecompile.P1'(A) :-
        user:goedel_freeze(ground([A]), 'ProgramsIO':'~ProgramsIO.ProgramDecompile.P1.0'(A)).
'ProgramsIO.ProgramDecompile.P1.0'(A) :-
        'ProgramsIO.ProgramDecompileAux.P1'(A).
'~ProgramsIO.ProgramDecompile.P1.0'(A) :-
        '~ProgramsIO.ProgramDecompileAux.P1'(A).
/* 
File:		ProgramsIO.sup
Subject:	Implemention of the system module ProgramsIO
Author: 	Jiwei Wang
Date:		15 June 1992

================================================================================
*/

%% '$$module'('@(#)ProgramsIO.sup 1.26 last updated 93/11/10 17:37:10 by bowers').

%------------------------------------------------------------------------------

% 'ProgramsIO.GetProgramAux.P2'(Stream, 'ProgDefs.CachedProgram.F1'(Main)) :-
%    'IO':translate_stream(Stream, PrologStream),
%    user:readin_program(PrologStream, Program),
%    Program = 'ProgDefs.Program.F4'(Main, _, _, _),
%    retractall(user:cached_program(Main, _)),
%    assert(user:cached_program(Main, Program)).

'ProgramsIO.GetProgramAux.P2'(Stream, Program) :-
    'IO':translate_stream(Stream, PrologStream),
    user:readin_program(PrologStream, Program).

'~ProgramsIO.GetProgramAux.P2'(Stream, Program) :-
   'ProgramsIO.GetProgramAux.P2'(Stream, Program).

%------------------------------------------------------------------------------

'ProgramsIO.PutProgramAux.P2'(Stream, Program) :-
   'IO':translate_stream(Stream, PrologStream),
   user:file_version(V),
   format(PrologStream, 'version(''~a'', prm).~n', [V]),
   user:create_prm_file_aux(Program, PrologStream), !.

'~ProgramsIO.PutProgramAux.P2'(Stream, Program) :-
   'ProgramsIO.PutProgramAux.P2'(Stream, Program).

%------------------------------------------------------------------------------

'ProgramsIO.ProgramCompileAux.P2'(GModuleName, Program) :-
   user:gstring2string(GModuleName, ModuleName),
   ( user:system_module_name(GModuleName)
     -> user:load_system_module(ModuleName, Program)
     ;  retractall(user:there_is_error),
        assert(user:no_message),
        user:parse_program(ModuleName, make, Program), 
        retractall(user:no_message), !,
	\+ user:there_is_error
   ).

%------------------------------------------------------------------------------

'ProgramsIO.ProgramDecompileAux.P1'(Program) :-
   user:'ParserPrograms.Decompile.P2'(Program, 'ParserPrograms.Quiet.C0').

