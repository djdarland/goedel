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


%------------------------------------------------------------------------------
% Transform - applies Lloyd-Topor transformations to statements
%             prior to compilation
%------------------------------------------------------------------------------
 
% This version adapted for the Goedel ground representation 28.01.92

'$$module'('@(#)transform.pl 1.18 last updated 93/12/14 12:00:18 by bowers
').

/*------------------------------------------------------------------------------
 * transform_body(Formula?, Transformed^, FreeVariables^)
 *------------------------------------------------------------------------------
 */

transform_body(Formula, Transformed, FreeVars) :-
        'SharedSyntax.SFormulaMaxVarIndex.P2'([Formula], Index),
	transform_body(Formula, [], Index, _, Transformed, FreeVars), !.

/*------------------------------------------------------------------------------
 */

transform_body('MetaDefs.Empty.C0', _, VarNo, VarNo, 'MetaDefs.Empty.C0', []).

transform_body('MetaDefs.PAtom.F1'(Name), _, VarNo, VarNo
      , 'MetaDefs.PAtom.F1'(Name), []).

transform_body('MetaDefs.Atom.F2'(Predicate, ArgList), 
	       VarDict, VarNo, NewVarNo, 
	       'MetaDefs.Atom.F2'(Predicate, NewArgList), FreeVars
	      ) :-
   transform_terms(ArgList, VarDict, VarNo, NewVarNo, NewArgList, FreeVars).

transform_body('MetaDefs.~''.F1'('MetaDefs.Atom.F2'(Predicate, ArgList))
      , VarDict, VarNo, NewVarNo
      , 'MetaDefs.Neg.F2'(FreeVars, 'MetaDefs.Atom.F2'(Predicate, NewArgList))
      , FreeVars
      ) :-
   transform_terms(ArgList, VarDict, VarNo, NewVarNo, NewArgList, FreeVars).

transform_body('MetaDefs.~''.F1'('MetaDefs.PAtom.F1'(Proposition))
      , _VarDict, VarNo, VarNo
      , 'MetaDefs.Neg.F2'([], 'MetaDefs.PAtom.F1'(Proposition)), []
      ).

transform_body('MetaDefs.~''.F1'('MetaDefs.~''.F1'(Formula)), 
               VarDict, VarNo, NewVarNo, 
	       NewFormula, FreeVars
              ) :-
	transform_body(Formula, VarDict, VarNo, 
                       NewVarNo, NewFormula, FreeVars).

transform_body('MetaDefs.Some.F2'(BoundVars, Formula), /* Some */
	       VarDict, VarNo, NewVarNo, 
	       NewFormula, FreeVars
	      ) :-
	rename_variables(BoundVars, VarDict, VarNo, NewBound, VarDict1, VarNo1),
	transform_body(Formula, VarDict1, VarNo1, 
                       NewVarNo, NewFormula, FreeInside),
	difference(FreeInside, NewBound, FreeVars).

transform_body('MetaDefs.~''.F1'('MetaDefs.Some.F2'(BoundVars, Formula)), 
	       VarDict, VarNo, NewVarNo, 
	       'MetaDefs.Neg.F2'(FreeVars, NewFormula), FreeVars
              ) :-
	rename_variables(BoundVars, VarDict, VarNo, NewBound, VarDict1, VarNo1),
        transform_body(Formula, VarDict1, VarNo1, 
                       NewVarNo, NewFormula, FreeInside),
        difference(FreeInside, NewBound, FreeVars).
 
transform_body('MetaDefs.&''.F2'(Formula1, Formula2), 
	       VarDict, VarNo, NewVarNo, 
	       'MetaDefs.&''.F2'(NewFormula1, NewFormula2), FreeVars
              ) :-
	transform_body(Formula1, VarDict, VarNo, 
                       VarNo1, NewFormula1, FreeVars1),
	transform_body(Formula2, VarDict, VarNo1,
                       NewVarNo, NewFormula2, FreeVars2),
	union(FreeVars1, FreeVars2, FreeVars).

transform_body('MetaDefs.~''.F1'('MetaDefs.&''.F2'(Formula1, Formula2)), 
               VarDict, VarNo, NewVarNo,
               'MetaDefs.\/''.F2'(NewFormula1, NewFormula2), FreeVars
              ) :-
	transform_body('MetaDefs.~''.F1'(Formula1), VarDict, VarNo, 
                       VarNo1, NewFormula1, FreeVars1),
	transform_body('MetaDefs.~''.F1'(Formula2), VarDict, VarNo1, 
                       NewVarNo, NewFormula2, FreeVars2),
	union(FreeVars1, FreeVars2, FreeVars).

transform_body('MetaDefs.\/''.F2'(Formula1, Formula2), 
	       VarDict, VarNo, NewVarNo,
	       'MetaDefs.\/''.F2'(NewFormula1, NewFormula2), FreeVars
              ) :-
	transform_body(Formula1, VarDict, VarNo, 
                       VarNo1, NewFormula1, FreeVars1),
	transform_body(Formula2, VarDict, VarNo1,
                       NewVarNo, NewFormula2, FreeVars2),
	union(FreeVars1, FreeVars2, FreeVars).

transform_body('MetaDefs.~''.F1'('MetaDefs.\/''.F2'(Formula1, Formula2)), 
	       VarDict, VarNo, NewVarNo,
               'MetaDefs.&''.F2'(NewFormula1, NewFormula2), FreeVars
              ) :-
	transform_body('MetaDefs.~''.F1'(Formula1), VarDict, VarNo,
                       VarNo1, NewFormula1, FreeVars1),
	transform_body('MetaDefs.~''.F1'(Formula2), VarDict, VarNo1,
                       NewVarNo, NewFormula2, FreeVars2),
        union(FreeVars1, FreeVars2, FreeVars).

transform_body('MetaDefs.->''.F2'(Formula1, Formula2), 
	       VarDict, VarNo, NewVarNo, 
	       'MetaDefs.\/''.F2'(NewFormula1, NewFormula2), FreeVars
              ) :-
	transform_body('MetaDefs.~''.F1'(Formula1), VarDict, VarNo,
                       VarNo1, NewFormula1, FreeVars1),
	transform_body(Formula2, VarDict, VarNo1,
	               NewVarNo, NewFormula2, FreeVars2),
	union(FreeVars1, FreeVars2, FreeVars).

transform_body('MetaDefs.<-''.F2'(Formula1, Formula2), 
	       VarDict, VarNo, NewVarNo, 
	       'MetaDefs.\/''.F2'(NewFormula1, NewFormula2), FreeVars
              ) :-
	transform_body('MetaDefs.~''.F1'(Formula2), VarDict, VarNo,
                       VarNo1, NewFormula2, FreeVars1),
	transform_body(Formula1, VarDict, VarNo1,
	               NewVarNo, NewFormula1, FreeVars2),
	union(FreeVars1, FreeVars2, FreeVars).

transform_body('MetaDefs.~''.F1'('MetaDefs.->''.F2'(Formula1, Formula2)), 
	       VarDict, VarNo, NewVarNo, 
	       'MetaDefs.&''.F2'(NewFormula1, NewFormula2), FreeVars
              ) :-
	transform_body(Formula1, VarDict, VarNo, 
	               VarNo1, NewFormula1, FreeVars1),
	transform_body('MetaDefs.~''.F1'(Formula2), VarDict, VarNo1, 
	               NewVarNo, NewFormula2, FreeVars2),
	union(FreeVars1, FreeVars2, FreeVars).

transform_body('MetaDefs.~''.F1'('MetaDefs.<-''.F2'(Formula1, Formula2)), 
	       VarDict, VarNo, NewVarNo, 
	       'MetaDefs.&''.F2'(NewFormula1, NewFormula2), FreeVars
              ) :-
	transform_body(Formula2, VarDict, VarNo, 
	               VarNo1, NewFormula2, FreeVars1),
	transform_body('MetaDefs.~''.F1'(Formula1), VarDict, VarNo1, 
	               NewVarNo, NewFormula1, FreeVars2),
	union(FreeVars1, FreeVars2, FreeVars).

transform_body('MetaDefs.<->''.F2'(Formula1, Formula2),
               VarDict, VarNo, NewVarNo, 
               'MetaDefs.&''.F2'(NewFormula1, NewFormula2), FreeVars
              ) :-
        transform_body('MetaDefs.->''.F2'(Formula1, Formula2), VarDict, VarNo,
                       VarNo1, NewFormula1, FreeVars1),
        transform_body('MetaDefs.<-''.F2'(Formula1, Formula2), VarDict, VarNo1,
                       NewVarNo, NewFormula2, FreeVars2),
        union(FreeVars1, FreeVars2, FreeVars).

transform_body('MetaDefs.~''.F1'('MetaDefs.<->''.F2'(Formula1, Formula2)),
               VarDict, VarNo, NewVarNo, 
               'MetaDefs.\/''.F2'(NewFormula1, NewFormula2), FreeVars
              ) :-
        transform_body('MetaDefs.~''.F1'('MetaDefs.->''.F2'(Formula1, Formula2)), 
                       VarDict, VarNo, VarNo1, NewFormula1, FreeVars1),
        transform_body('MetaDefs.~''.F1'('MetaDefs.<-''.F2'(Formula1, Formula2)), 
                       VarDict, VarNo1, NewVarNo, NewFormula2, FreeVars2),
        union(FreeVars1, FreeVars2, FreeVars).

transform_body('MetaDefs.All.F2'(BoundVars, Formula), /* All */
	       VarDict, VarNo, NewVarNo, 
	       NewFormula, FreeVars
              ) :-
   transform_body('MetaDefs.~''.F1'('MetaDefs.Some.F2'(BoundVars
      , 'MetaDefs.~''.F1'(Formula))), VarDict, VarNo, NewVarNo, NewFormula
      , FreeVars).

transform_body('MetaDefs.~''.F1'('MetaDefs.All.F2'(BoundVars, Formula)), 
	       VarDict, VarNo, NewVarNo, 
	       NewFormula, FreeVars
              ) :-
   transform_body('MetaDefs.Some.F2'(BoundVars, 'MetaDefs.~''.F1'(Formula))
      , VarDict, VarNo, NewVarNo, NewFormula, FreeVars).

transform_body('MetaDefs.Commit.F2'(Label, Formula), 
	       VarDict, VarNo, NewVarNo, 
	       'MetaDefs.Commit.F2'(Label, NewFormula), FreeVars
              ) :-
   transform_body(Formula, VarDict, VarNo, NewVarNo, NewFormula, FreeVars).

transform_body('MetaDefs.ISTE.F4'(BoundVars, Cond, Then, Else),
	       VarDict, VarNo, NewVarNo,
               NewFormula, FreeVars 
              ) :-
	rename_variables(BoundVars, VarDict, VarNo, NewBound, VarDict1, VarNo1),
	transform_body(Cond, VarDict1, VarNo1, VarNo2, NewCond, FreeInCond),
	transform_body(Then, VarDict1, VarNo2, VarNo3, NewThen, FreeInThen),
        transform_body(Else, VarDict, VarNo3, NewVarNo, NewElse, FreeInElse),
        difference(FreeInCond, NewBound, SuspendVars),
        ( intersection(NewBound, FreeInThen, []) ->
          NewFormula = 'MetaDefs.If.F4'(SuspendVars, NewCond, NewThen, NewElse)
        ;
          NewFormula = 'MetaDefs.IfSome.F4'(SuspendVars, NewCond,
                                            NewThen, NewElse)
        ),
        difference(FreeInThen, NewBound, FreeVars1),
        union(SuspendVars, FreeVars1, FreeVars2),
        union(FreeVars2, FreeInElse, FreeVars).

transform_body('MetaDefs.IST.F3'(BoundVars, Cond, Then),
	       VarDict, VarNo, NewVarNo,
               NewFormula, FreeVars 
              ) :-
	rename_variables(BoundVars, VarDict, VarNo, NewBound, VarDict1, VarNo1),
	transform_body(Cond, VarDict1, VarNo1, VarNo2, NewCond, FreeInCond),
	transform_body(Then, VarDict1, VarNo2, NewVarNo, NewThen, FreeInThen),
        NewElse = 'MetaDefs.PAtom.F1'('MetaDefs.Name.F4'('"', '"True'
           , 'MetaDefs.Proposition.C0', 0)),
        difference(FreeInCond, NewBound, SuspendVars),
        ( intersection(NewBound, FreeInThen, []) ->
          NewFormula = 'MetaDefs.If.F4'(SuspendVars, NewCond, NewThen, NewElse)
        ;
          NewFormula = 'MetaDefs.IfSome.F4'(SuspendVars, NewCond,
                                            NewThen, NewElse)
        ),
        difference(FreeInThen, NewBound, FreeVars1),
        union(SuspendVars, FreeVars1, FreeVars).

transform_body('MetaDefs.ITE.F3'(Cond, Then, Else),
	       VarDict, VarNo, NewVarNo,
               'MetaDefs.If.F4'(FreeInCond, NewCond, NewThen, NewElse), 
	       FreeVars 
              ) :-
	transform_body(Cond, VarDict, VarNo, VarNo1, NewCond, FreeInCond),
	transform_body(Then, VarDict, VarNo1, VarNo2, NewThen, FreeInThen),
        transform_body(Else, VarDict, VarNo2, NewVarNo, NewElse, FreeInElse),
        union(FreeInCond, FreeInThen, FreeVars1),
        union(FreeVars1, FreeInElse, FreeVars).

transform_body('MetaDefs.IT.F2'(Cond, Then),
	       VarDict, VarNo, NewVarNo,
               'MetaDefs.If.F4'(FreeInCond, NewCond, NewThen, NewElse), 
	       FreeVars 
              ) :-
	transform_body(Cond, VarDict, VarNo, VarNo1, NewCond, FreeInCond),
	transform_body(Then, VarDict, VarNo1, NewVarNo, NewThen, FreeInThen),
        NewElse = 'MetaDefs.PAtom.F1'('MetaDefs.Name.F4'('"', '"True'
           , 'MetaDefs.Proposition.C0', 0)),
        union(FreeInCond, FreeInThen, FreeVars).

transform_body('MetaDefs.~''.F1'('MetaDefs.ISTE.F4'(BoundVars, Cond, Then, Else)),
	       VarDict, VarNo, NewVarNo,
               'MetaDefs.Neg.F2'(FreeVars, NewFormula), FreeVars 
              ) :-
	rename_variables(BoundVars, VarDict, VarNo, NewBound, VarDict1, VarNo1),
	transform_body(Cond, VarDict1, VarNo1, VarNo2, NewCond, FreeInCond),
	transform_body(Then, VarDict1, VarNo2, VarNo3, NewThen, FreeInThen),
        transform_body(Else, VarDict, VarNo3, NewVarNo, NewElse, FreeInElse),
        difference(FreeInCond, NewBound, SuspendVars),
        ( intersection(NewBound, FreeInThen, []) ->
          NewFormula = 'MetaDefs.If.F4'(SuspendVars, NewCond, NewThen, NewElse)
        ;
          NewFormula = 'MetaDefs.IfSome.F4'(SuspendVars, NewCond,
                                            NewThen, NewElse)
        ),
        difference(FreeInThen, NewBound, FreeVars1),
        union(SuspendVars, FreeVars1, FreeVars2),
        union(FreeVars2, FreeInElse, FreeVars).

transform_body('MetaDefs.~''.F1'('MetaDefs.IST.F3'(BoundVars, Cond, Then)),
	       VarDict, VarNo, NewVarNo,
               'MetaDefs.Neg.F2'(FreeVars, NewFormula), FreeVars 
              ) :-
	rename_variables(BoundVars, VarDict, VarNo, NewBound, VarDict1, VarNo1),
	transform_body(Cond, VarDict1, VarNo1, VarNo2, NewCond, FreeInCond),
	transform_body(Then, VarDict1, VarNo2, NewVarNo, NewThen, FreeInThen),
        NewElse = 'MetaDefs.PAtom.F1'('MetaDefs.Name.F4'('"', '"True'
           , 'MetaDefs.Proposition.C0', 0)),
        difference(FreeInCond, NewBound, SuspendVars),
        ( intersection(NewBound, FreeInThen, []) ->
          NewFormula = 'MetaDefs.If.F4'(SuspendVars, NewCond, NewThen, NewElse)
        ;
          NewFormula = 'MetaDefs.IfSome.F4'(SuspendVars, NewCond,
                                            NewThen, NewElse)
        ),
        difference(FreeInThen, NewBound, FreeVars1),
        union(SuspendVars, FreeVars1, FreeVars).

transform_body('MetaDefs.~''.F1'('MetaDefs.ITE.F3'(Cond, Then, Else)),
	       VarDict, VarNo, NewVarNo,
               'MetaDefs.If.F4'(FreeInCond, NewCond, NewThen, NewElse), 
	       FreeVars 
              ) :-
	transform_body(Cond, VarDict, VarNo, VarNo1, NewCond, FreeInCond),
	transform_body('MetaDefs.~''.F1'(Then), VarDict, VarNo1, VarNo2
           , NewThen, FreeInThen),
        transform_body('MetaDefs.~''.F1'(Else), VarDict, VarNo2, NewVarNo
           , NewElse, FreeInElse),
        union(FreeInCond, FreeInThen, FreeVars1),
        union(FreeVars1, FreeInElse, FreeVars).

transform_body('MetaDefs.~''.F1'('MetaDefs.IT.F2'(Cond, Then)),
	       VarDict, VarNo, NewVarNo,
               'MetaDefs.If.F4'(FreeInCond, NewCond, NewThen, NewElse), 
	       FreeVars 
              ) :-
	transform_body(Cond, VarDict, VarNo, VarNo1, NewCond, FreeInCond),
	transform_body('MetaDefs.~''.F1'(Then), VarDict, VarNo1, NewVarNo
           , NewThen, FreeInThen),
        NewElse = 'MetaDefs.PAtom.F1'('MetaDefs.Name.F4'('"', '"False'
           , 'MetaDefs.Proposition.C0', 0)),
        union(FreeInCond, FreeInThen, FreeVars).

/*------------------------------------------------------------------------------
 */

transform_terms(TermList, VarDict, VarNo, NewVarNo, NewTermList, VarSet) :-
   transform_term_list(TermList, VarDict, VarNo, NewVarNo, NewTermList
      , HiggledyVars),
   straighten(HiggledyVars, VarList, []),
   sort(VarList, VarSet).

/*------------------------------------------------------------------------------
 */
 
transform_term_list([], _, VarNo, VarNo, [], []).

transform_term_list([Term|Terms], VarDict, VarNo, NewVarNo, 
                    [NewTerm|NewTerms], [TermVars|OtherVars]
                   ) :-
   transform_one_term(Term, VarDict, VarNo, VarNo1, NewTerm, TermVars),
   transform_term_list(Terms, VarDict, VarNo1, NewVarNo, NewTerms
      , OtherVars).

/*------------------------------------------------------------------------------
 */

transform_one_term('MetaDefs.Var.F2'(Vs, Vn), VarDict, VarNo, VarNo, NewVar
      , NewVar
      ) :-
   variable_name('MetaDefs.Var.F2'(Vs, Vn), VarDict, NewVar).

transform_one_term('MetaDefs.Var.F1'(Vn), VarDict, VarNo, VarNo, NewVar
      , NewVar
      ) :-
   variable_name('MetaDefs.Var.F1'(Vn), VarDict, NewVar).

transform_one_term('MetaDefs.Term.F2'(Function, ArgList), VarDict
      , VarNo, NewVarNo, 'MetaDefs.Term.F2'(Function, NewArgList), Vars
      ) :-
   transform_term_list(ArgList, VarDict, VarNo, NewVarNo, NewArgList, Vars).

transform_one_term('MetaDefs.CTerm.F1'(Constant), _VarDict, VarNo, VarNo
      , 'MetaDefs.CTerm.F1'(Constant), []).

transform_one_term('MetaDefs.Int.F1'(Constant), _VarDict, VarNo, VarNo
      , 'MetaDefs.Int.F1'(Constant), []).

transform_one_term('MetaDefs.Flo.F1'(Constant), _VarDict, VarNo, VarNo
      , 'MetaDefs.Flo.F1'(Constant), []).

transform_one_term('MetaDefs.Str.F1'(Constant), _VarDict, VarNo, VarNo
      , 'MetaDefs.Str.F1'(Constant), []).

transform_one_term('MetaDefs.Prm.F1'(Constant), _VarDict, VarNo, VarNo
      , 'MetaDefs.Prm.F1'(Constant), []).

transform_one_term('MetaDefs.SuchThat.F2'(Term, Formula), VarDict, VarNo
      , NewVarNo, 'MetaDefs.SuchThat.F3'(FreeVars, NewTerm, NewFormula), FreeVars 
      ) :-
   'SharedSyntax.STermFreeVars.P2'(Term, BoundVars),
   rename_variables(BoundVars, VarDict, VarNo, NewBound, NewVarDict, VarNo1),
   transform_one_term(Term, NewVarDict, VarNo1, VarNo2, NewTerm, _),
   transform_body(Formula, NewVarDict, VarNo2, NewVarNo, NewFormula
      , FreeInFormula),
   difference(FreeInFormula, NewBound, FreeVars).

/*------------------------------------------------------------------------------
 */

variable_name(Var, [], Var).

variable_name(Var, [Var/New|_], New).

variable_name(Var, [Key/_|Entries], New) :-
	Var \== Key,
	variable_name(Var, Entries, New).

/*------------------------------------------------------------------------------
 */

rename_variables(Vars, VarDict, VarNo, NewVarSet, NewVarDict, NewVarNo) :-
   rename_variables_1(Vars, VarDict, VarNo, NewVarList, NewVarDict, NewVarNo),
   sort(NewVarList, NewVarSet).

rename_variables_1([], VarDict, N, [], VarDict, N).

rename_variables_1([Var|Vars], VarDict, N, [NewVar|NewVars], NewVarDict
      , NewN
      ) :-
        user:'SharedSyntax.SVariableName.P3'(Var, Vs, _),
        ( name(Vs, [0'",0'_|_]) ->
	  NewVar = Var, % parser guarantees all underscores are unique for now
          VarDict1 = VarDict,
          N1 is N
        ;
          user:'SharedSyntax.SVariableName.P3'(NewVar, Vs, N),
	  update_dictionary(VarDict, Var/NewVar, VarDict1),
	  N1 is N + 1
        ),
	rename_variables_1(Vars, VarDict1, N1, NewVars, NewVarDict, NewN).

/*------------------------------------------------------------------------------
 */

update_dictionary([], Entry, [Entry]).

update_dictionary([Var/_|Entries], Var/New, [Var/New|Entries]).

update_dictionary([Key/Name|Entries], Var/New, [Key/Name|NewEntries]) :-
	Var \== Key,
	update_dictionary(Entries, Var/New, NewEntries).

/*------------------------------------------------------------------------------
 */
 
straighten([], Xs, Xs).

straighten([X|Xs], Ys, Zs) :-
	straighten(X, Ys, Us),
	straighten(Xs, Us, Zs).

straighten('MetaDefs.Var.F2'(Vs, Vn), ['MetaDefs.Var.F2'(Vs, Vn)|Ys], Ys).

straighten('MetaDefs.Var.F1'(Vn), ['MetaDefs.Var.F1'(Vn)|Ys], Ys).

