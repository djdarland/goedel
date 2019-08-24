
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


/*----------------------------------------------------------------------------
 * The suspension mechanism for the transformation of Goedel to SICStus.
 * This may be added as library to the target code of Goedel programs
 * (in SICStus).
 *				Author:  Jiwei Wang
 *				Date:    15 Aug. 1991
 *----------------------------------------------------------------------------
 */

'$$module'('@(#)gfreeze.pl 1.13 last updated 93/06/15 16:18:20 by bowers
').


:- op(500, yfx, and).
:- op(400, yfx, or).


/*----------------------------------------------------------------------------
 * This is a version using carefully transformed program.
 * It adds another layer on the top of the previous goedel-freeze.
 */

goedel_freeze(Delay, Goal) :-
	evaluate_delay(Delay, Signal),
	freeze(Signal, Goal).

/*----------------------------------------------------------------------------
 * evaluate_delay
 */

evaluate_delay(nonvar(A), A).

evaluate_delay(ground(A), X):-
	remove_ground_rep(A, B),
	freeze_ground(B, X).

evaluate_delay(nonvar_and([Var|Vars]), X):- 
	freeze_nonvar_and(Vars, Var, X).

evaluate_delay(nonvar_or(Vars), X):- 
	prolog:term_variables(Vars, Vlist),	% ensure disj-geler works
	prolog:'$disjunctive_geler'(Vlist, call(X=1)).

evaluate_delay(A and B, X):-
	evaluate_delay_aux(A and B, X). 

evaluate_delay(A or B, X):-
	evaluate_delay_aux(A or B, X). 

/*----------------------------------------------------------------------------
 * remove_ground_representation takes away ground representation.
 * At the moment, only Program and Script terms are removed. 
 * Attention: this may not be the effective way !!!!
 */

remove_ground_rep([], []).
remove_ground_rep([Arg|Argus], B) :- 
	( nonvar(Arg), removable(Arg)
	  -> remove_ground_rep(Argus, B)
	  ;  B = [Arg|B2],
	     remove_ground_rep(Argus, B2)
	).

removable('ProgDefs.Program.F4'(_, _, _, _)).
removable('Scripts.Script.F5'(_, _, _, _, _)).

/* This is a better (safer) copy, but slower.
remove_ground_rep([], []) :- !.
remove_ground_rep([Arg|Argus], B) :-  !,
	( nonvar(Arg), Arg = 'ProgDefs.Program.F4'(_, _, _, _)
	  -> remove_ground_rep(Argus, B)
	  ;  B = [Arg|B2],
	     remove_ground_rep(Argus, B2)
	).

remove_ground_rep(A, A).
*/

/*----------------------------------------------------------------------------
 * freeze_ground
 */

freeze_ground(A, X):-
	prolog:term_variables(A, Vlist),
	( Vlist = []
	  -> X = 1
	  ;  Vlist = [Var|Vars], freeze_on_vlist(Vars, Var, Vlist, X)
	).


:- block freeze_on_vlist(?, -,  ?, ?). 

freeze_on_vlist([], _, Vlist, X):-
	freeze_ground(Vlist, X). 
freeze_on_vlist([Var|Vars], _, Vlist, X):-
	freeze_on_vlist(Vars, Var, Vlist, X).


/*------------------------------------------------------------------------------
 * freeze_nonvar_and
 */

:- block freeze_nonvar_and(?, -,  ?). 

freeze_nonvar_and([], _, 1).
freeze_nonvar_and([Var|Vars], _, X):-
	freeze_nonvar_and(Vars, Var, X).

/*------------------------------------------------------------------------------
 * evaluate_delay_aux
 */

evaluate_delay_aux(Delay, X):-
	goedel_freeze_aux(Delay, [], Var_list, NewDelay),
	( var(NewDelay)
	  -> X = 1
	  ;  prolog:'$disjunctive_geler'(Var_list, 
				call(evaluate_delay_aux(NewDelay, X)) )
	).

% Version F.2
	
/*------------------------------------------------------------------------------
 * The suspension mechanism for the transformation of Goedel to SICStus.
 * This may be added as library to the target code of Goedel programs
 * (in SICStus).
 *				Author:  Jiwei Wang
 *				Date:    5 Aug. 1991
 *------------------------------------------------------------------------------
 */

'$$module'('@(#)freeze.pl 1.21 jiwei 91/08/11 15:10:45
').

:- op(500, yfx, and).
:- op(400, yfx, or).


/*-----------------------------------------------------------------------------
 * A fast, correct and beautiful goedel_freeze.  It works on program without
 * particular transformation.
 * This is an improved version of Version F on ground test.
 * It looks for earliest failure and minimal freeze variables.
 */


goedel_freeze_aux(nonvar(A), Vlist0, Vlist, Delay) :-
	( var(A)
	  -> Vlist = [A|Vlist0], Delay = nonvar(A)
	  ;  Vlist = Vlist0
	).

goedel_freeze_aux(ground(A), Vlist0, Vlist, Delay):-
	prolog:term_variables(A, V),
	( V = []
	  -> Vlist = Vlist0
	  ;  Delay = ground(V), V = [H|_], Vlist = [H|Vlist0] 
	).

goedel_freeze_aux(A or B, Vlist0, Vlist, Delay) :-
	goedel_freeze_aux(A, Vlist0, Va, DelayA),
	( var(DelayA)
	  -> Vlist = Vlist0
	  ;  goedel_freeze_aux(B, Va, Vb, DelayB),
	     ( var(DelayB) 
	        -> Vlist = Vlist0
	        ;  Vlist = Vb, Delay = DelayA or DelayB
	     )
	).

goedel_freeze_aux(A and B, Vlist0, Vlist, Delay) :-
	goedel_freeze_aux(A, Vlist0, Va, DelayA),
	( var(DelayA) 
	  -> goedel_freeze_aux(B, Vlist0, Vlist, Delay)
	  ;  Vlist = Va, Delay = DelayA and B
	).
