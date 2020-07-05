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
File:		aux.pl
Subject:	Supporting routines for the Goedel runtime system.
Author: 	Jiwei Wang
Date:		30 June 1992

================================================================================
*/

%%'$$module'('@(#)aux.pl 1.3 last updated 93/02/10 17:24:36 by jiwei').


% a signal for the runtime system 
is_runtime_system :- fail.


my_load(File):-
	consult(File).
	/*
	trace,
   sappend(File, '.pl', File3),
   sappend('GL/', File3, File2),
   open(File2, read, Stream, [type(text)]),
   read(Stream, module(ModuleName)),
   abolish_module(ModuleName),
   ( read(Stream, Clause)
     -> my_load_aux(Clause, ModuleName, Stream)
     ;  true		% in case that the program is empty
   ),
   close(Stream).
*/
my_load(Path, File2):-
	sappend(Path, File2, File),
	consult(File).
	/*
	trace,
   sappend(File, '.pl', File3),
   sappend('GL/', File3, File2),
   open(File2, read, Stream, [type(text)]),
   read(Stream, module(ModuleName)),
   abolish_module(ModuleName),
   ( read(Stream, Clause)
     -> my_load_aux(Clause, ModuleName, Stream)
     ;  true		% in case that the program is empty
   ),
   close(Stream).
*/
my_load_aux(end_of_file, _, _):-
   !.

my_load_aux(Clause, ModuleName, Stream) :-
   assertz(ModuleName:Clause),
   read(Stream, Clause2),
   my_load_aux(Clause2, ModuleName, Stream).

%----------------------------------------------------------------------------

abolish_module(M) :-
   current_predicate(_, M:Pred),
   functor(Pred, F, A),
   abolish(M:F, A),
   fail.
abolish_module(_).

