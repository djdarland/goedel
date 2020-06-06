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


/* File:	checking.pl
Subject:	Singleton variable checking, missing quantified variable
		checking, floundering checking.
Author: 	Jiwei Wang
Date:		18 November 1993

================================================================================
*/

'$$module'('@(#)checking.pl 1.3 last updated 93/11/29 14:25:02 by jiwei
').


%------------------------------------------------------------------------------
% singleton_variable_checking is done before transform
% This checking excludes the case of variables appeared in disjuncts (or
% (Than and Else parts).  E.g. P(x) <- Q(x,y) \/ R(y).

singleton_variable_checking(Formula, GPredicate, Arity, Ln1, Ln2) :-
   collect_variables(Formula, [], Vars),
   singleton_variables(Vars, SingletonVars),
   strip_variables(SingletonVars, SingletonVars2),
   ( SingletonVars2 = []
     -> true
     ;  gstring2string(GPredicate, Predicate),
	print_out_error('Warning: singleton variable(s) ~w in predicate "~w/~d"',
		[SingletonVars2, Predicate, Arity], local, Ln1, Ln2, [], null)
   ).

%------------------------------------------------------------------------------
% collect_variables gathers free varibles.

collect_variables('MetaDefs.Var.F1'(V), Vars, ['MetaDefs.Var.F1'(V)|Vars]).
collect_variables('MetaDefs.Var.F2'(V,N), Vars, ['MetaDefs.Var.F2'(V,N)|Vars]).

collect_variables([], Vars, Vars).
collect_variables([H|T], Vars, Vars2) :-
	collect_variables(H, Vars, Vars3),
	collect_variables(T, Vars3, Vars2).

collect_variables('MetaDefs.CTerm.F1'(_), Vars, Vars).
collect_variables('MetaDefs.Num.F1'(_), Vars, Vars).	% for Rationals
collect_variables('MetaDefs.Int.F1'(_), Vars, Vars).
collect_variables('MetaDefs.Str.F1'(_), Vars, Vars).
collect_variables('MetaDefs.Flo.F1'(_), Vars, Vars).
collect_variables('MetaDefs.Term.F2'(_, Terms), Vars, NewVars) :-
	collect_variables(Terms, Vars, NewVars).

collect_variables('MetaDefs.SuchThat.F2'(Term1, Term2), Vars, NewVars) :-
	collect_variables(Term1, [], Vars1),
	collect_variables(Term2, [], Vars2),
	delete_sublist(Vars2, Vars1, Vars, NewVars).

collect_variables('MetaDefs.Empty.C0', Vars, Vars).
collect_variables('MetaDefs.PAtom.F1'(_), Vars, Vars).
collect_variables('MetaDefs.Atom.F2'(_, Terms), Vars, Vars2) :-
	collect_variables(Terms, Vars, Vars2).

collect_variables('MetaDefs.Commit.F2'(_, Formula), Vars, Vars2):-
	collect_variables(Formula, Vars, Vars2).

collect_variables('MetaDefs.~''.F1'(Formula), Vars, Vars2) :-
	collect_variables(Formula, Vars, Vars2).

collect_variables('MetaDefs.&''.F2'(Formula1, Formula2), Vars, Vars2) :-
	collect_variables(Formula1, Vars, Vars3),
	collect_variables(Formula2, Vars3, Vars2).

collect_variables('MetaDefs.\/''.F2'(Formula1, Formula2), Vars, Vars2) :-
	collect_variables(Formula1, Vars, Vars3),
	collect_variables(Formula2, Vars3, Vars2).

collect_variables('MetaDefs.->''.F2'(Formula1, Formula2), Vars, Vars2) :-
	collect_variables(Formula1, Vars, Vars3),
	collect_variables(Formula2, Vars3, Vars2).

collect_variables('MetaDefs.<-''.F2'(Formula1, Formula2), Vars, Vars2) :-
	collect_variables(Formula1, Vars, Vars3),
	collect_variables(Formula2, Vars3, Vars2).

collect_variables('MetaDefs.<->''.F2'(Formula1, Formula2), Vars, Vars2) :-
	collect_variables(Formula1, Vars, Vars3),
	collect_variables(Formula2, Vars3, Vars2).

collect_variables('MetaDefs.IT.F2'(Formula1, Formula2), Vars, Vars2) :-
	collect_variables(Formula1, Vars, Vars3),
	collect_variables(Formula2, Vars3, Vars2).

collect_variables('MetaDefs.ITE.F3'(Formula1, Formula2, Formula3),
			Vars, Vars2) :-
	collect_variables(Formula1, Vars, Vars3),
	collect_variables(Formula2, Vars3, Vars4),
	collect_variables(Formula3, Vars4, Vars2).

collect_variables('MetaDefs.All.F2'(VarList, Formula), Vars, Vars2) :-
	collect_variables(Formula, [], Vars3),
	delete_sublist(Vars3, VarList, Vars, Vars2).

collect_variables('MetaDefs.Some.F2'(VarList, Formula), Vars, Vars2) :-
	collect_variables(Formula, [], Vars3),
	delete_sublist(Vars3, VarList, Vars, Vars2).

collect_variables('MetaDefs.IST.F3'(VarList, Formula1, Formula2),
		Vars, Vars2) :-
	collect_variables(Formula1, [], Vars3),
	collect_variables(Formula2, Vars3, Vars4),
	delete_sublist(Vars4, VarList, Vars, Vars2).

collect_variables('MetaDefs.ISTE.F4'(VarList, Formula1, Formula2, Formula3),
		Vars, Vars2) :-
	collect_variables(Formula1, [], Vars3),
	collect_variables(Formula2, Vars3, Vars4),
	delete_sublist(Vars4, VarList, Vars, Vars5),
	collect_variables(Formula3, Vars5, Vars2).

% The following are for delay conditions
collect_variables('ProgDefs.TrueCond.C0', Vars, Vars).

collect_variables('ProgDefs.Ground.F1'(CondVar), Vars, Vars2) :-
	collect_variables(CondVar, Vars, Vars2).

collect_variables('ProgDefs.Nonvar.F1'(CondVar), Vars, Vars2) :-
	collect_variables(CondVar, Vars, Vars2).

collect_variables('ProgDefs.And.F2'(Cond1, Cond2), Vars, Vars2) :-
	collect_variables(Cond1, Vars, Vars3),
	collect_variables(Cond2, Vars3, Vars2).

collect_variables('ProgDefs.Or.F2'(Cond1, Cond2), Vars, Vars2) :-
	collect_variables(Cond1, Vars, Vars3),
	collect_variables(Cond2, Vars3, Vars2).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The following are for formulas after Lloyd-Topor transformation.
% For collecting productive vars, the following are rather different from
% collecting free vars.

% negative transformed formulas do not contribute to variable instantiation. 
collect_variables('MetaDefs.Neg.F2'(_, _), Vars, Vars).

% intensional sets do not contribute to variable instantiation. 
collect_variables('MetaDefs.SuchThat.F3'(_, _, _), Vars, Vars).

% losing quantifiers in transformation is not a serious problem, as
% bound variables are uniquely renamed.
% It's still problematic that Then and Else are taken as conjucts.
collect_variables('MetaDefs.If.F4'(_Vars, _Cond, Then, Else), Vars, Vars2) :-
   collect_variables(Then, Vars, Vars3),
   collect_variables(Else, Vars3, Vars2).

collect_variables('MetaDefs.IfSome.F4'(_Vars, _Cond, Then, Else), Vars, Vars2):-
   collect_variables(Then, Vars, Vars3),
   collect_variables(Else, Vars3, Vars2).

%------------------------------------------------------------------------------
% underscore variables are removed.

strip_variables([], []).
strip_variables(['MetaDefs.Var.F1'(_)|VarList], ['v'|VarList2]) :- !,
   strip_variables(VarList, VarList2).
strip_variables(['MetaDefs.Var.F2'(GVar, _)|VarList], VarList2) :-
   ( name(GVar, [0'",0'_|_])
     -> VarList2 = VarList3
     ;  VarList2 = [Var|VarList3],
        gstring2string(GVar, Var)
   ),
   strip_variables(VarList, VarList3).


delete_sublist([], _, Vars, Vars).
delete_sublist([Var|OldVars], VarList, Vars, Vars2) :-
   ( member(Var, VarList)
     -> Vars2 = Vars3,
        delete_sublist(OldVars, VarList, Vars, Vars3)
     ;  Vars2 = [Var|Vars3],
        delete_sublist(OldVars, VarList, Vars, Vars3)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

singleton_variables([], []).
singleton_variables([Var|Vars], SingletonVars) :-
	( special_delete_all(Vars, Var, Vars2)
	  -> singleton_variables(Vars2, SingletonVars)
	  ;  singleton_variables(Vars, SingletonVars2),
	     SingletonVars = [Var|SingletonVars2]
	).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * special_delete_all(+Vars, +Var, -NewVars, -NewVars2)
 * It fails if Var does not occur in Vars. 
 */

special_delete_all([Var|Vars], Var, NewVars) :-
	!,
	special_delete_all_aux(Vars, Var, NewVars).
special_delete_all([Var2|Vars], Var, [Var2|NewVars]) :-
	special_delete_all(Vars, Var, NewVars).


special_delete_all_aux([], _, []).
special_delete_all_aux([Var|Vars], Var, NewVars) :-
	!,
	special_delete_all_aux(Vars, Var, NewVars).
special_delete_all_aux([Var2|Vars], Var, [Var2|NewVars]) :-
	special_delete_all_aux(Vars, Var, NewVars).

%------------------------------------------------------------------------------

% quantified_variable_checking([], Ln1, Ln2) :- !. % unnecessary

quantified_variable_checking([Formula|Formulae], Ln1, Ln2) :-
   !,
   quantified_variable_checking(Formula, Ln1, Ln2),
   quantified_variable_checking(Formulae, Ln1, Ln2).

quantified_variable_checking('MetaDefs.Term.F2'(_, Terms), Ln1, Ln2) :-
   !,
   quantified_variable_checking(Terms, Ln1, Ln2).

quantified_variable_checking('MetaDefs.SuchThat.F2'(_, Formula), Ln1, Ln2) :-
   !,
   quantified_variable_checking(Formula, Ln1, Ln2).

quantified_variable_checking('MetaDefs.SuchThat.F3'(_, _, Formula), Ln1, Ln2) :-
   !,
   quantified_variable_checking(Formula, Ln1, Ln2).

quantified_variable_checking('MetaDefs.Atom.F2'(_, Terms), Ln1, Ln2) :-
   !,
   quantified_variable_checking(Terms, Ln1, Ln2).

quantified_variable_checking('MetaDefs.Commit.F2'(_, Formula), Ln1, Ln2):-
   !,
   quantified_variable_checking(Formula, Ln1, Ln2).

quantified_variable_checking('MetaDefs.~''.F1'(Formula), Ln1, Ln2) :-
   !,
   quantified_variable_checking(Formula, Ln1, Ln2).

quantified_variable_checking('MetaDefs.&''.F2'(Formula1, Formula2), Ln1, Ln2) :-
   !,
   quantified_variable_checking(Formula1, Ln1, Ln2),
   quantified_variable_checking(Formula2, Ln1, Ln2).

quantified_variable_checking('MetaDefs.\/''.F2'(Formula1, Formula2), Ln1, Ln2):-
   !,
   quantified_variable_checking(Formula1, Ln1, Ln2),
   quantified_variable_checking(Formula2, Ln1, Ln2).

quantified_variable_checking('MetaDefs.->''.F2'(Formula1, Formula2), Ln1, Ln2):-
   !,
   quantified_variable_checking(Formula1, Ln1, Ln2),
   quantified_variable_checking(Formula2, Ln1, Ln2).

quantified_variable_checking('MetaDefs.<-''.F2'(Formula1, Formula2), Ln1, Ln2):-
   !,
   quantified_variable_checking(Formula1, Ln1, Ln2),
   quantified_variable_checking(Formula2, Ln1, Ln2).

quantified_variable_checking('MetaDefs.<->''.F2'(Formula1, Formula2), Ln1, Ln2) :-
   !,
   quantified_variable_checking(Formula1, Ln1, Ln2),
   quantified_variable_checking(Formula2, Ln1, Ln2).

quantified_variable_checking('MetaDefs.IT.F2'(Formula1, Formula2), Ln1, Ln2) :-
   !,
   quantified_variable_checking(Formula1, Ln1, Ln2),
   quantified_variable_checking(Formula2, Ln1, Ln2).

quantified_variable_checking('MetaDefs.ITE.F3'(Formula1, Formula2, Formula3),
         Ln1, Ln2) :-
   !,
   quantified_variable_checking(Formula1, Ln1, Ln2),
   quantified_variable_checking(Formula2, Ln1, Ln2),
   quantified_variable_checking(Formula3, Ln1, Ln2).

quantified_variable_checking('MetaDefs.All.F2'(VarList, Formula), Ln1, Ln2) :-
   !,
   strip_variables(VarList, VarList2),
   process_quantified_variable(VarList2, VarList3, Ln1, Ln2),
   collect_variables(Formula, [], Vars),
   strip_variables(Vars, Vars2),
   set_minus(VarList3, Vars2, NotOccurredVars),
   ( NotOccurredVars = []
     -> true
     ;  print_out_error('Warning: quantified variable(s) ~w do not appear in their scope',
		     [NotOccurredVars], local, Ln1, Ln2, [], null)
   ),
   quantified_variable_checking(Formula, Ln1, Ln2).

quantified_variable_checking('MetaDefs.Some.F2'(VarList, Formula), Ln1, Ln2) :-
   !,
   strip_variables(VarList, VarList2),
   process_quantified_variable(VarList2, VarList3, Ln1, Ln2),
   collect_variables(Formula, [], Vars),
   strip_variables(Vars, Vars2),                                               
   set_minus(VarList3, Vars2, NotOccurredVars),
   ( NotOccurredVars = []
     -> true
     ;  print_out_error('Warning: quantified variable(s) ~w do not appear in their scope',
		     [NotOccurredVars], local, Ln1, Ln2, [], null)
   ),
   quantified_variable_checking(Formula, Ln1, Ln2).

quantified_variable_checking('MetaDefs.IST.F3'(VarList, Formula1, Formula2),
      Ln1, Ln2) :-
   !,
   strip_variables(VarList, VarList2),
   process_quantified_variable(VarList2, VarList3, Ln1, Ln2),
   collect_variables([Formula1, Formula2], [], Vars),
   strip_variables(Vars, Vars2),
   set_minus(VarList3, Vars2, NotOccurredVars),
   ( NotOccurredVars = []
     -> true
     ;  print_out_error('Warning: quantified variable(s) ~w do not appear in their scope',
		     [NotOccurredVars], local, Ln1, Ln2, [], null)
   ),
   quantified_variable_checking(Formula1, Ln1, Ln2),
   quantified_variable_checking(Formula2, Ln1, Ln2).

quantified_variable_checking(
		'MetaDefs.ISTE.F4'(VarList, Formula1, Formula2, Formula3),
      		Ln1, Ln2) :-
   !,
   strip_variables(VarList, VarList2),
   process_quantified_variable(VarList2, VarList3, Ln1, Ln2),
   collect_variables([Formula1, Formula2], [], Vars),
   strip_variables(Vars, Vars2),
   set_minus(VarList3, Vars2, NotOccurredVars),
   ( NotOccurredVars = []
     -> true
     ;  print_out_error('Warning: quantified variable(s) ~w do not appear in their scope',
		     [NotOccurredVars], local, Ln1, Ln2, [], null)
   ),
   quantified_variable_checking(Formula1, Ln1, Ln2),
   quantified_variable_checking(Formula2, Ln1, Ln2),
   quantified_variable_checking(Formula3, Ln1, Ln2).

quantified_variable_checking(_, _, _).

%------------------------------------------------------------------------------

process_quantified_variable(Vars, NewVars, Ln1, Ln2) :-
   process_quantified_variable_aux(Vars, NewVars, DupVars, []),
   ( DupVars = []
     -> true
     ;  print_out_error('Warning: quantified variable(s) ~w are specified more than once',
				   [DupVars], local, Ln1, Ln2, [], null)
   ).


process_quantified_variable_aux([], [], DupVars, DupVars).
process_quantified_variable_aux([Var|Vars], NewVars, DupVars, DupVars2) :-
   ( name(Var, [0'_|_])		% this is not in Gstring
     -> process_quantified_variable_aux(Vars, NewVars, DupVars, DupVars2)
     ;  ( member_check(Var, Vars)
	  -> DupVars = [Var|DupVars3],
	     process_quantified_variable_aux(Vars, NewVars, DupVars3, DupVars2)
	  ;  NewVars = [Var|NewVars2],
	     process_quantified_variable_aux(Vars, NewVars2, DupVars, DupVars2)
	)
   ).

%------------------------------------------------------------------------------
% floundering_checking(Statement, Ln1, Ln2)
% floundering checking needs the statement to be transformed.

floundering_checking('MetaDefs.<-''.F2'(HeadAtom, Body), Ln1, Ln2) :-
   ( Body = 'MetaDefs.Empty.C0'
     -> true
     ;  collect_variables(HeadAtom, [], HeadVars),
        toplev_conjunctions(Body, [], GoalList),
        f_checking(GoalList, HeadVars, [], Culprits),
	( Culprits = []
	  -> true
	  ;  sort(Culprits, Culprits2),
	     print_out_error('Warning: will flounder on variable(s) ~w',
				[Culprits2], local, Ln1, Ln2, [], null)
	)
   ).


floundering_checking_goal(Goal, Error) :-
   toplev_conjunctions(Goal, [], FGoalList),
   f_checking(FGoalList, [], [], Culprits),
   ( Culprits = []
     -> Error = no
     ;  sort(Culprits, Culprits2),
        Error = yes,
        print_out_error('Error: goal will flounder on variable(s) ~w',
                                [Culprits2], null, 0, 0, [], null)
   ).


floundering_checking_aux(Body, HeadVars, Culprits, Culprits2) :-
   conjunction_to_list(Body, [], GoalList),
   f_checking(GoalList, HeadVars, Culprits, Culprits2).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f_checking([], _, Culprits, Culprits).
f_checking([pair(Fomula, Vs)|Goals], PVars, Culprits, Culprits2) :-
   gather_productive_vars(Goals, PVars, PVars3),
   f_checking_aux(Fomula, PVars3, Culprits, Culprits3),
   append(Vs, PVars, PVars2),
   f_checking(Goals, PVars2, Culprits3, Culprits2).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f_checking_aux('MetaDefs.IfSome.F4'(FreeVars, Cond, Then, Else), PVars,
		Culprits, Culprits2):-
   set_minus(FreeVars, PVars, CulpritVars),
   strip_variables(CulpritVars, CulpritVars2),
   append(CulpritVars2, Culprits, Culprits3),
   collect_variables(Cond, PVars, PVars2),
   floundering_checking_aux(Then, PVars2, Culprits3, Culprits4),
   floundering_checking_aux(Else, PVars, Culprits4, Culprits2).

f_checking_aux('MetaDefs.If.F4'(FreeVars, _, Then, Else), PVars,
		Culprits, Culprits2):-
   set_minus(FreeVars, PVars, CulpritVars),
   strip_variables(CulpritVars, CulpritVars2),
   append(CulpritVars2, Culprits, Culprits3),
   floundering_checking_aux(Then, PVars, Culprits3, Culprits4),      
   floundering_checking_aux(Else, PVars, Culprits4, Culprits2).

f_checking_aux('MetaDefs.Neg.F2'(FreeVars, Formula), PVars,
		Culprits, Culprits2):-
   set_minus(FreeVars, PVars, CulpritVars),
   strip_variables(CulpritVars, CulpritVars2),
   append(CulpritVars2, Culprits, Culprits3),
   floundering_checking_aux(Formula, PVars, Culprits3, Culprits2).      

f_checking_aux('MetaDefs.SuchThat.F3'(FreeVars, _Term, Formula), PVars,
		Culprits, Culprits2):-
   set_minus(FreeVars, PVars, CulpritVars),
   strip_variables(CulpritVars, CulpritVars2),
   append(CulpritVars2, Culprits, Culprits3),
   floundering_checking_aux(Formula, PVars, Culprits3, Culprits2).      
   	% f_checking_aux(Term, PVars, Culprits3, Culprits4),
	% Free variables in Term are taken as quantified variables
	% floundering checking is not necessary

% to cope with lists of terms.
f_checking_aux([], _, Culprits, Culprits).
f_checking_aux([H|T], PVars, Culprits, Culprits2):-
   f_checking_aux(H, PVars, Culprits, Culprits3),
   f_checking_aux(T, PVars, Culprits3, Culprits2).

f_checking_aux('MetaDefs.Var.F1'(_), _, Culprits, Culprits).
f_checking_aux('MetaDefs.Var.F2'(_, _), _, Culprits, Culprits).
f_checking_aux('MetaDefs.CTerm.F1'(_), _, Culprits, Culprits).
f_checking_aux('MetaDefs.Num.F1'(_), _, Culprits, Culprits).
f_checking_aux('MetaDefs.Int.F1'(_), _, Culprits, Culprits).
f_checking_aux('MetaDefs.Str.F1'(_), _, Culprits, Culprits).
f_checking_aux('MetaDefs.Flo.F1'(_), _, Culprits, Culprits).

f_checking_aux('MetaDefs.Term.F2'(_, Terms), PVars, Culprits, Culprits2):-
   f_checking_aux(Terms, PVars, Culprits, Culprits2).

f_checking_aux('MetaDefs.Empty.C0', _, Culprits, Culprits).
f_checking_aux('MetaDefs.PAtom.F1'(_), _, Culprits, Culprits).

f_checking_aux('MetaDefs.Atom.F2'(_, Terms), PVars, Culprits, Culprits2):-
   f_checking_aux(Terms, PVars, Culprits, Culprits2).

f_checking_aux('MetaDefs.Commit.F2'(_, Formula), PVars, Culprits, Culprits2):-
   floundering_checking_aux(Formula, PVars, Culprits, Culprits2).

f_checking_aux('MetaDefs.&''.F2'(Formula1, Formula2), PVars,
		Culprits, Culprits2):-
   floundering_checking_aux('MetaDefs.&''.F2'(Formula1, Formula2), PVars,
		Culprits, Culprits2).

f_checking_aux('MetaDefs.\/''.F2'(Formula1, Formula2), PVars,
		Culprits, Culprits2):-
   floundering_checking_aux(Formula1, PVars, Culprits, Culprits3),
   floundering_checking_aux(Formula2, PVars, Culprits3, Culprits2).

%------------------------------------------------------------------------------

gather_productive_vars([], PVars, PVars).
gather_productive_vars([pair(_, Vs)|List], PVars, PVars2) :-
   append(Vs, PVars, PVars3),
   gather_productive_vars(List, PVars3, PVars2).

%------------------------------------------------------------------------------
% productive free var: a free variable is productive if it occurs in
%	1. a positive goal (after transformation)
%	2. appears both in the both branch of disjuction, Then and Else parts
% 	3. in IF SOME
% 	4. variables in the head
% However, it should be aware that this is not saying that the productive
% free vars are definitely produce instantiations for the variables.
% Thus the floundering checking may miss some floundering cases.
%
% One way of improving this is to consider the delay declarations for
% each predicate.

conjunction_to_list('MetaDefs.&''.F2'(Formula1, Formula2), GoalList,
		GoalList2) :-
   !,
   conjunction_to_list(Formula1, GoalList, GoalList3),
   conjunction_to_list(Formula2, GoalList3, GoalList2).

conjunction_to_list(Formula, GoalList, [pair(Formula, PVars)|GoalList]):-
   collect_variables(Formula, [], PVars). % collect productive vars

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% this to make sure that formulas are transformed only once

toplev_conjunctions('MetaDefs.&''.F2'(Formula1, Formula2), GoalList,
                GoalList2) :-
   !,
   toplev_conjunctions(Formula1, GoalList, GoalList3),
   toplev_conjunctions(Formula2, GoalList3, GoalList2).

toplev_conjunctions(Formula, GoalList, [pair(Transformed, PVars)|GoalList]):-
   transform_body(Formula, Transformed, _),
   collect_variables(Transformed, [], PVars). % collect productive vars

