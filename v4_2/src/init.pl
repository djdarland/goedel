:- use_module(library(system)).
:- use_module(library(file_systems)).
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


% change the following two directory addresses if necessary

% default system source code dir /usr/local/lib/Goedel/src
system_directory('').


				% Please don't change anything below this line.
				% ---------------------------------------------

goedel_version('4.2').
file_version('4.2').

%%'$$init'('@(#)init.pl 1.64 last updated 94/04/24 15:56:56 by jiwei').

get_ready(It) :-
	consult(It).


goedel:-
				% top-level
	get_ready(toplev),
	get_ready(utilities),
	get_ready(tracer),

				% library
	get_ready(lib),
	get_ready(gfreeze),

				% parser
	get_ready(tokenizer),
	get_ready(tokenizer2),
	get_ready(term),
	get_ready(formula),
	get_ready(statement),
	get_ready(parser),
	get_ready(checking),

				% ground representation
	get_ready(system),
	get_ready(avltrees),
	get_ready(builtin),

				% compiler
	get_ready(compiler),
	get_ready(delay),
	get_ready(constraint),
	get_ready(transform),

				% language files of system modules
				% get_ready(sys_modules),
				% NOTE =========================has been commented out=============

				% system modules code
	get_ready('Integers'),
	get_ready('Rationals'),
	get_ready('Floats'),
	get_ready('Numbers'),
	get_ready('Lists'),
	get_ready('Sets'),
	get_ready('Strings'),

	get_ready('Tables'),
	get_ready('Units'),
	get_ready('Flocks'),
	get_ready('FlocksIO'),
	get_ready('IO'),
	get_ready('NumbersIO'),

	get_ready('Syntax'),
	get_ready('ExtraSyntax'),
	get_ready('SharedSyntax'),
	get_ready('Substs'),

	get_ready('Programs'),
	get_ready('SharedPrograms'),
	get_ready('ProgramCache'),

	get_ready('Scripts'),
	get_ready('ScriptsIO'),

	get_ready('AVLTrees'),

	get_ready('ProgramsIO'),
	get_ready('Theories'),
	get_ready('TheoriesIO'),
	get_ready('aux'),

				% This file suplies routines for the runtime system.  Never included in
				% the saved states.
	%% runtime_system_file(init).
	%% runtime_system_file(aux).
				% runtime_system_file(sys_modules).	% this'll have to be removed when
				% SICStus works properly.

				%------------------------------------------------------------------------------
				%	prolog_flag(unknown, _, fail),	% cause undefined predicates to fail.
				%	( prolog_flag(compiling, _, fastcode);
				%	  true				% in case user's SICStus doesn't
				%					% support fastcode
				%	), !,
	goedel_version(Version),
	format(user_output, "Goedel ~a~nType ;h. for help.~n", [Version]),
				%	nofileerrors,
	top_loop('', null, null).
				%	fileerrors.

gauge_goedel:-
	use_module(library(gauge)),
	prolog_flag(compiling, _, profiledcode),
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
	compile(sys_modules),
	( prolog_flag(compiling, _, fastcode);
	    true		% in case user's SICStus doesn't
				% support fastcode
	), !,
	findall(F, ( system_file(F),
		       system_directory(D),
		       join_string(D, F, DF),
		       compile(DF)
		   ), _),
	save(goedel, Status),
	process(Status).


				% creating .ql files for the runtime system
make_runtime:-
	fcompile(sys_modules),	% this is compiled in compact code
	prolog_flag(compiling, _, fastcode),
	findall(F, ( system_file(F),
		       system_directory(D),
		       join_string(D, F, DF),
		       fcompile(DF)
		   ), _),
				% especially for the runtime system 
	findall(F, ( runtime_system_file(F),
		       system_directory(D),
		       join_string(D, F, DF),
		       fcompile(DF)
		   ), _).

				% Creating .ql files for the runtime system on Linux
				% All system modules are compiled in compact code
make_pc_runtime:-
	prolog_flag(compiling, _, compactcode),
	fcompile(sys_modules),
	findall(F, ( system_file(F),
		       system_directory(D),
		       join_string(D, F, DF),
		       fcompile(DF)
		   ), _),
	findall(F, ( runtime_system_file(F),
		       system_directory(D),
		       join_string(D, F, DF),
		       fcompile(DF)
		   ), _).

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

