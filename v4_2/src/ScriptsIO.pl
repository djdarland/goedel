:- module('ScriptsIO', []).
:- multifile '$$module'/1.

:- op(500, yfx, and).
:- op(400, yfx, or).

/* 
File:		ScriptsIO.sup
Subject:	Implemention of the system module ScriptsIO
Author: 	Jiwei Wang
Date:		7 April 1992

================================================================================
*/

%% '$$module'('@(#)ScriptsIO.sup 1.4 last updated 93/05/22 23:56:01 by jiwei').

/*------------------------------------------------------------------------------
 */

'ScriptsIO.GetScript.P2'(Stream, Script) :-
	user:goedel_freeze(ground([Stream]),
		'ScriptsIO':'ScriptsIO.GetScript.P2.0'(Stream, Script)).

'~ScriptsIO.GetScript.P2'(Stream, Script) :-
	'ScriptsIO.GetScript.P2'(Stream, Script).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

'ScriptsIO.GetScript.P2.0'(Stream, Script) :-
   'IO':translate_stream(Stream, PrologStream),
   readin_script(PrologStream, Script), !.

readin_script(Stream, Script) :-
   ( user:file_version(V),
     read(Stream, version(V, scr)) 		% test the version number
     -> Script = 'Scripts.Script.F5'(MainModuleName, ModuleDefAVL, Language,
			GoalLanguage, Code),
	read(Stream, MainModuleName),
	read(Stream, ModuleDefAVL),
	read(Stream, Language),
	read(Stream, GoalLanguage),
	read(Stream, Code)
     ;  format(user_error, 'Error: ".scr" file is out of date.~n', []),
	fail
   ).

/*------------------------------------------------------------------------------
 */

'ScriptsIO.PutScript.P2'(Stream, Script) :-
   user:goedel_freeze(ground([Stream,Script]),
		'ScriptsIO':'ScriptsIO.PutScript.P2.0'(Stream, Script)).

'~ScriptsIO.PutScript.P2'(Stream, Script) :-
   'ScriptsIO.PutScript.P2'(Stream, Script).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

'ScriptsIO.PutScript.P2.0'(Stream, Script) :-
   'IO':translate_stream(Stream, PrologStream),
   Script = 'Scripts.Script.F5'(MainModuleName, ModuleDefAVL, Language,
				GoalLanguage, Code),
   user:file_version(V),
   format(PrologStream, 'version(''~a'', scr).~n', [V]), 
   write_canonical(PrologStream, MainModuleName),
   format(PrologStream, '.~n', []),
   write_canonical(PrologStream, ModuleDefAVL),
   format(PrologStream, '.~n', []),
   write_canonical(PrologStream, Language),
   format(PrologStream, '.~n', []),
   write_canonical(PrologStream, GoalLanguage),
   format(PrologStream, '.~n', []),
   write_canonical(PrologStream, Code),
   format(PrologStream, '.~n', []).
