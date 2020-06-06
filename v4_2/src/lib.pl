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


/*------------------------------------------------------------------------------
 * The system module for the compiled Goedel programs 
 *		Author: Jiwei Wang
 *		Date:	7 August 1991
 *		Modified on 5 Jan. 1992 for the new parser
 */

%% '$$module'('@(#)lib.pl 1.30 last updated 93/12/08 17:56:31 by jiwei').

/*------------------------------------------------------------------------------
 * Operator declarations for delay declaratoins
 */

:- op(500, yfx, and).
:- op(400, yfx, or).

/*------------------------------------------------------------------------------
 * Semantics for not_equal(FreeVarlist, Term1, Term2),
 * 	if Term1 and Term2 do not unify, it succeeds;
 *	if Term1 and Term2 unify without binding any variables, it fails;
 *	if Term1 and Term2 unify and bind some non_underscore variables,
 *	   it delays until some non_underscore variable become nonvar.
 *
 * not_equal/4 uses dif when there is no underscore in terms.
 * not_equal/3 has the problem shown above.
 */

not_equal([], _, Term1, Term2) :-
   dif(Term1, Term2).

not_equal([_|_], FreeVars, Term1, Term2) :-
   prolog:term_variables(FreeVars, FreeVars2),
   not_equal(FreeVars2, Term1, Term2).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * all the cuts in not_equal have been removed.
 */

not_equal(FreeVarList, Term1, Term2):-
   ( prolog:term_variables(FreeVarList, NewVarList),
			% prevent instantiated variables
     Term1 = Term2,
     not_instantiated(NewVarList)
     -> fail
     ;  ( \+ ( Term1 = Term2)
	  -> true
	  ;  goedel_freeze(nonvar_or(FreeVarList), 
		  not_equal(FreeVarList, Term1, Term2))
	)
   ).

not_instantiated(FreeVarList):-
   var_list(FreeVarList),	% check if there is no variable instantiated
   sort(FreeVarList, VarList),
   length(VarList, Length),
   length(FreeVarList, Length).

%------------------------------------------------------------------------------

one_solution(Goals):-
   call_residue(Goals, Residue),
   ( Residue = []
     -> !
     ;  release_suspended(Residue)
   ).


% one_solution(Goals):-
%    call_residue(Goals, Residue),
%    ( Residue = []
%      -> true
%      ;  user:flounder_commit(Residue)
%    ), !.


%------------------------------------------------------------------------------

goedel_not(Goals):-
   ( call_residue(Goals, Suspended)
     -> Suspended \== [],
	format(user_error, 'Floundered. Unsolved goals are:~n', []),
	process_unsolved_goals(Suspended),
	raise_exception(catch_in_query)
     ;  true
   ).

%------------------------------------------------------------------------------

flounder_commit(Residue) :-
   format(user_error, 'Floundered in the scope of a commit. Suspended goals are:~n', []),
   process_unsolved_goals(Residue),
   raise_exception(catch_in_query).

%------------------------------------------------------------------------------

release_suspended([]).
release_suspended([V-UnsolvedGoal|Rest]):-
   freeze(V, UnsolvedGoal),
   release_suspended(Rest).

