:-	use_module(library(system)).
:-	use_module(library(file_systems)).

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


				% change the following two directory addresses if necessary

				% default system source code dir /usr/local/lib/Goedel/src
system_directory('').


				% Please don't change anything below this line.
				% ---------------------------------------------

goedel_version('1.3.27').
file_version('1.3').

'$$init'('@(#)init.pl 1.59 last updated 94/01/24 13:05:26 by jiwei').

%% :- multifile '$$module'/1.


				% This file suplies routines for the runtime system.  Never included in
				% the saved states.
%% runtime_system_file(init).
%% runtime_system_file(aux).
				% runtime_system_file(sys_modules).	% this'll have to be removed when
				% SICStus works properly.

				%------------------------------------------------------------------------------

goedel:-
	write(dennis_was_here_1),
	save_goedel,
	prolog_flag(unknown, _, fail), % cause undefined predicates to fail.
	write(dennis_was_here_1),
				%	( prolog_flag(compiling, _, fastcode);
				%	( prolog_flag(compiling, _, compiling);
				%	  true				% in case user's SICStus doesn't
				%					% support fastcode
				%	), !,
	goedel_version(Version),
	write(dennis_was_here_2),
	format(user_error, "Goedel ~a~nType ;h. for help.~n", [Version]),
	write(dennis_was_here_3),
				%	nofileerrors,
	write(dennis_was_here_4),
	top_loop('', null, null),
	write(dennis_was_here_5).
				%	fileerrors.

gauge_goedel:-
%	use_module(library(gauge)),
				%	prolog_flag(compiling, _, profiledcode),
	findall(F, ( system_file(F),
		       system_directory(D),
		       join_string(D, F, DF),
		       compile(DF)
		   ), _).

/* cannot work because of the bug in SICStus
save_goedel:-
	( prolog_flag(compiling, _, fastcode);
	    true                          % in case user's SICStus doesn't %
				% support fastcode %
        ), !,
	findall(F, ( system_file(F),
		       system_directory(D),
		       join_string(D, F, DF),
		       compile(DF)
		   ), _),
	save(goedel, Status),
	process(Status).
*/

save_goedel:-
	write("Hello Dennis"),
				% top-level
	consult(toplev),
	consult(utilities),
	consult(tracer),
	
				% library
	consult(lib),
	consult(gfreeze),
	
				% parser
	consult(tokenizer),
	consult(tokenizer2),
	consult(term),
	consult(formula),
	consult(statement),
	consult(parser),
	consult(checking),
	
				% ground representation
	consult(system),
	consult(avltrees),
	consult(builtin),
	
				% compiler
	consult(compiler),
	consult(delay),
	consult(constraint),
	consult(transform),
	
				% language files of system modules
				% compile(sys_modules), % removed DJD - problems
				% NOTE =========================has been commented out=============
	
				% system modules code
	consult('Integers'),
	consult('Rationals'),
	consult('Floats'),
	consult('Numbers'),
	consult('Lists'),
	consult('Sets'),
	consult('Strings'),
	
	consult('Tables'),
	consult('Units'),
	consult('Flocks'),
	consult('FlocksIO'),
	consult('IO'),
	consult('NumbersIO'),
	
	consult('Syntax'),
	consult('ExtraSyntax'),
	consult('SharedSyntax'),
	consult('Substs'),
	
	consult('Programs'),
	consult('SharedPrograms'),
	consult('ProgramCache'),
	
	consult('Scripts'),
	consult('ScriptsIO'),
	
	consult('AVLTrees'),
	
	consult('ProgramsIO'),
	consult('Theories'),
	consult('TheoriesIO').
				%	compile(sys_modules),
				%	( prolog_flag(compiling, _, fastcode);
				%	  true				% in case user's SICStus doesn't
				%					% support fastcode
				%	), !.
/*
findall(F, ( system_file(F),
	       system_directory(D),
	       join_string(D, F, DF),
	       compile(DF)
	   ), _),
save(goedel, Status),
process(Status).


				% creating .ql files for the runtime system %
make_runtime:-
	fcompile(sys_modules),		% this is compiled in compact code %
				%	prolog_flag(compiling, _, fastcode), %
	findall(F, ( system_file(F),
		       system_directory(D),
		       join_string(D, F, DF),
		       fcompile(DF)
		   ), _),
				% especially for the runtime system %
	findall(F, ( runtime_system_file(F),
		       system_directory(D),
		       join_string(D, F, DF),
		       fcompile(DF)
		   ), _)
*/


process(0) :-
	goedel_version(Version),
	format('Goedel version ~a created~n', [Version]).

process(1) :-
	goedel,
	unix(exit(1)).

/*------------------------------------------------------------------------------
* utility routines for init.pl only.
*/

join_string(A, B, C):-
	name(A, A1),
	name(B, B1),
	init_append(A1, B1, C1),
	name(C, C1).

init_append([],Y,Y).
init_append([U|X],Y,[U|Z]):-
	init_append(X,Y,Z).

goedel.