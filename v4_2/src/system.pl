:- op(500, yfx, and).
:- op(400, yfx, or).

'SharedSyntax.STermMaxVarIndex.P2'(A, B) :-
        plus(C, 1, B),
        negative(1, D),
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, D, C).
'SharedSyntax.MaxVarIndexInTerms.P3'([], A, A).
'SharedSyntax.MaxVarIndexInTerms.P3'([A|B], C, D) :-
        'SharedSyntax.MaxVarIndexInTerm.P3'(A, C, E),
        'SharedSyntax.MaxVarIndexInTerms.P3'(B, E, D).
'SharedSyntax.MaxVarIndex.P3'([], A, A).
'SharedSyntax.MaxVarIndex.P3'([A|B], C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex.P3'(B, E, D).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Par.F1'(A), B, C, 'MetaDefs.Par.F2'(D,E)) :-
        'SharedSyntax.SNotNewBinding.P4'(B, C, 'MetaDefs.Par.F1'(A), 'MetaDefs.Par.F2'(D,E)).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Par.F1'(A), B, C, 'MetaDefs.Par.F1'(D)) :-
        'SharedSyntax.SNotNewBinding.P4'(B, C, 'MetaDefs.Par.F1'(A), 'MetaDefs.Par.F1'(D)).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Par.F2'(A,B), C, D, 'MetaDefs.Par.F1'(E)) :-
        'SharedSyntax.SNotNewBinding.P4'(C, D, 'MetaDefs.Par.F2'(A,B), 'MetaDefs.Par.F1'(E)).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Par.F2'(A,B), C, D, 'MetaDefs.Par.F2'(E,F)) :-
        'SharedSyntax.SNotNewBinding.P4'(C, D, 'MetaDefs.Par.F2'(A,B), 'MetaDefs.Par.F2'(E,F)).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.BType.F1'(A), B, B, 'MetaDefs.BType.F1'(A)).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Type.F2'(A,B), C, D, 'MetaDefs.Type.F2'(A,E)) :-
        'SharedSyntax.SCheckVariantTypes.P4'(B, C, D, E).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.XBType.F1'(A), B, B, 'MetaDefs.XBType.F1'(A)).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.XType.F2'(A,B), C, D, 'MetaDefs.XType.F2'(A,E)) :-
        'SharedSyntax.SCheckVariantTypes.P4'(B, C, D, E).
'SharedSyntax.ArgFreeVars.P2'([], []).
'SharedSyntax.ArgFreeVars.P2'([A|B], C) :-
        'SharedSyntax.STermFreeVars.P2'(A, D),
        'SharedSyntax.ArgFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.GetVariable.P4'(A, B, C, D) :-
        (   user:not_equal([], [A,B], A, B) ->
            'SharedSyntax.TermNotOccur.P2'(A, B),
            'Substs.BindVariable.P4'(B, A, C, D)
        ;   D=C
        ).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.Empty.C0', A, A).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.PAtom.F1'(_), A, A).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.Atom.F2'(_,A), B, C) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, B, C).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.&''.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.\\/''.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.->''.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.<-''.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.<->''.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.~''.F1'(A), B, C) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, B, C).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.Some.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.All.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.IT.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.IST.F3'(A,B,C), D, E) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, D, F),
        'SharedSyntax.MaxVarIndex1.P3'(B, F, G),
        'SharedSyntax.MaxVarIndex1.P3'(C, G, E).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.ITE.F3'(A,B,C), D, E) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, D, F),
        'SharedSyntax.MaxVarIndex1.P3'(B, F, G),
        'SharedSyntax.MaxVarIndex1.P3'(C, G, E).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.ISTE.F4'(A,B,C,D), E, F) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, E, G),
        'SharedSyntax.MaxVarIndex1.P3'(B, G, H),
        'SharedSyntax.MaxVarIndex1.P3'(C, H, I),
        'SharedSyntax.MaxVarIndex1.P3'(D, I, F).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.Commit.F2'(_,A), B, C) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, B, C).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Var.F1'(A), B, C) :-
        (   'Integers.>.P2'(A, B) ->
            C=A
        ;   C=B
        ).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Var.F2'(_,A), B, C) :-
        (   'Integers.>.P2'(A, B) ->
            C=A
        ;   C=B
        ).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.CTerm.F1'(_), A, A).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.XCTerm.F2'(_,_), A, A).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Term.F2'(_,A), B, C) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, B, C).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.XTerm.F3'(_,A,_), B, C) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, B, C).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Int.F1'(_), A, A).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Str.F1'(_), A, A).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Prm.F1'(_), A, A).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.SuchThat.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndexInTerm.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.SFormulaMaxVarIndex.P2'(A, B) :-
        plus(C, 1, B),
        negative(1, D),
        'SharedSyntax.MaxVarIndex.P3'(A, D, C).
'SharedSyntax.SEmptyTermSubst.P1'('Substs.TermSubst.F2'(A,[])) :-
        'Substs.EmptyHeap.P1'(A).
'SharedSyntax.SCheckVariantTypes.P4'([], A, A, []).
'SharedSyntax.SCheckVariantTypes.P4'([A|B], C, D, [E|F]) :-
        'SharedSyntax.CheckVariantTypes1.P4'(A, C, G, E),
        'SharedSyntax.SCheckVariantTypes.P4'(B, G, D, F).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.Empty.C0', []).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.PAtom.F1'(_), []).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.Atom.F2'(_,A), B) :-
        'SharedSyntax.ArgFreeVars.P2'(A, B).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.XPAtom.F1'(_), []).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.XAtom.F2'(_,A), B) :-
        'SharedSyntax.ArgFreeVars.P2'(A, B).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.~''.F1'(A), B) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, B).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.&''.F2'(A,B), C) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.\\/''.F2'(A,B), C) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.->''.F2'(A,B), C) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.<-''.F2'(A,B), C) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.<->''.F2'(A,B), C) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.All.F2'(A,B), C) :-
        'SharedSyntax.Order.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Difference.P3'(E, D, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.Some.F2'(A,B), C) :-
        'SharedSyntax.Order.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Difference.P3'(E, D, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.ISTE.F4'(A,B,C,D), E) :-
        'SharedSyntax.Order.P2'(A, F),
        'SharedSyntax.SFormulaFreeVars.P2'(B, G),
        'SharedSyntax.SFormulaFreeVars.P2'(C, H),
        'SharedSyntax.SFormulaFreeVars.P2'(D, I),
        'SharedSyntax.Union.P3'(G, H, J),
        'SharedSyntax.Difference.P3'(J, F, K),
        'SharedSyntax.Union.P3'(K, I, E).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.ITE.F3'(A,B,C), D) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, E),
        'SharedSyntax.SFormulaFreeVars.P2'(B, F),
        'SharedSyntax.SFormulaFreeVars.P2'(C, G),
        'SharedSyntax.Union.P3'(E, F, H),
        'SharedSyntax.Union.P3'(H, G, D).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.IST.F3'(A,B,C), D) :-
        'SharedSyntax.Order.P2'(A, E),
        'SharedSyntax.SFormulaFreeVars.P2'(B, F),
        'SharedSyntax.SFormulaFreeVars.P2'(C, G),
        'SharedSyntax.Union.P3'(F, G, H),
        'SharedSyntax.Difference.P3'(H, E, D).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.IT.F2'(A,B), C) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.Commit.F2'(_,A), B) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, B).
'SharedSyntax.SNotNewBinding.P4'([], ['MetaDefs.!.F2'(A,B)], A, B).
'SharedSyntax.SNotNewBinding.P4'(['MetaDefs.!.F2'(B,C)|A], D, E, F) :-
        (   B=E ->
            C=F,
            D=G
        ;   user:not_equal([], [C,F], C, F),
            D=['MetaDefs.!.F2'(B,C)|G]
        ),
        'SharedSyntax.SNotNewBinding.P4'(A, G, E, F).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Var.F2'(A,B), ['MetaDefs.Var.F2'(A,B)]).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Var.F1'(A), ['MetaDefs.Var.F1'(A)]).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Term.F2'(_,A), B) :-
        'SharedSyntax.ArgFreeVars.P2'(A, B).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.XTerm.F3'(_,A,_), B) :-
        'SharedSyntax.ArgFreeVars.P2'(A, B).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.CTerm.F1'(_), []).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.XCTerm.F2'(_,_), []).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Int.F1'(_), []).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Num.F1'(_), []).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Str.F1'(_), []).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Prm.F1'(_), []).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.SuchThat.F2'(A,B), C) :-
        'SharedSyntax.STermFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Difference.P3'(E, D, C).
'SharedSyntax.UnifyTermArgs.P5'([], [], _, A, A).
'SharedSyntax.UnifyTermArgs.P5'([A|B], [C|D], E, F, G) :-
        'SharedSyntax.UnifyTerms1.P5'(A, C, E, F, H),
        'SharedSyntax.UnifyTermArgs.P5'(B, D, E, H, G).
'SharedSyntax.SVariableName.P3'('MetaDefs.Var.F2'(A,B), A, B) :-
        call_residue(user:not_equal([],[A],A,'"v'), C),
        (   C=[] ->
            true
        ;   user:flounder_commit(C)
        ), !.
'SharedSyntax.SVariableName.P3'('MetaDefs.Var.F1'(A), '"v', A) :- !.
'SharedSyntax.SUnifyTerms.P4'(A, B, C, D) :-
        'Substs.SubstApplyToTerm.P3'(A, C, E),
        'SharedSyntax.UnifyTerms0.P4'(E, B, C, D).
'SharedSyntax.SUnifyAtoms.P4'('MetaDefs.PAtom.F1'(A), 'MetaDefs.PAtom.F1'(A), B, B).
'SharedSyntax.SUnifyAtoms.P4'('MetaDefs.XPAtom.F1'(A), 'MetaDefs.XPAtom.F1'(A), B, B).
'SharedSyntax.SUnifyAtoms.P4'('MetaDefs.Atom.F2'(A,B), 'MetaDefs.Atom.F2'(A,C), D, E) :-
        'SharedSyntax.UnifyingTermSubst.P4'(B, C, D, E).
'SharedSyntax.SUnifyAtoms.P4'('MetaDefs.XAtom.F2'(A,B), 'MetaDefs.XAtom.F2'(A,C), D, E) :-
        'SharedSyntax.UnifyingTermSubst.P4'(B, C, D, E).
'SharedSyntax.TermNotOccur1.P2'([], _).
'SharedSyntax.TermNotOccur1.P2'([A|B], C) :-
        'SharedSyntax.TermNotOccur.P2'(A, C),
        'SharedSyntax.TermNotOccur1.P2'(B, C).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.Var.F2'(A,B), C) :-
        user:not_equal([], [C,A,B], C, 'MetaDefs.Var.F2'(A,B)).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.Var.F1'(A), B) :-
        user:not_equal([], [B,A], B, 'MetaDefs.Var.F1'(A)).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.CTerm.F1'(_), _).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.Term.F2'(_,A), B) :-
        'SharedSyntax.TermNotOccur1.P2'(A, B).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.XCTerm.F2'(_,_), _).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.Int.F1'(_), _).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.Str.F1'(_), _).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.Prm.F1'(_), _).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.XTerm.F3'(_,A,_), B) :-
        'SharedSyntax.TermNotOccur1.P2'(A, B).
'SharedSyntax.TermOccurCheck.P3'('Substs.Read.C0', A, B) :-
        'SharedSyntax.TermNotOccur1.P2'(B, A).
'SharedSyntax.TermOccurCheck.P3'('Substs.Write.C0', _, _).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.Var.F2'(A,B), C, D, E, F) :-
        'Substs.UnifyKnownVariable.P5'(D, C, 'MetaDefs.Var.F2'(A,B), E, F).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.Var.F1'(A), B, C, D, E) :-
        'Substs.UnifyKnownVariable.P5'(C, B, 'MetaDefs.Var.F1'(A), D, E).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.CTerm.F1'(A), B, C, D, E) :-
        'Substs.UnifyConstant.P5'(C, B, 'MetaDefs.CTerm.F1'(A), D, E).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.Term.F2'(A,B), C, D, E, F) :-
        'Substs.UnifyFunction.P6'(D, C, 'MetaDefs.Term.F2'(A,G), H, E, I),
        'SharedSyntax.TermOccurCheck.P3'(H, C, B),
        'SharedSyntax.UnifyTermArgs.P5'(B, G, H, I, F).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.XTerm.F3'(A,B,C), D, E, F, G) :-
        'Substs.UnifyFunction.P6'(E, D, 'MetaDefs.XTerm.F3'(A,H,C), I, F, J),
        'SharedSyntax.TermOccurCheck.P3'(I, D, B),
        'SharedSyntax.UnifyTermArgs.P5'(B, H, I, J, G).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.XCTerm.F2'(A,B), C, D, E, F) :-
        'Substs.UnifyConstant.P5'(D, C, 'MetaDefs.XCTerm.F2'(A,B), E, F).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.Int.F1'(A), B, C, D, E) :-
        'Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Int.F1'(A), D, E).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.Str.F1'(A), B, C, D, E) :-
        'Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Str.F1'(A), D, E).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.Prm.F1'(A), B, C, D, E) :-
        'Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Prm.F1'(A), D, E).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.Var.F2'(A,B), C, D, E) :-
        'SharedSyntax.GetVariable.P4'(C, 'MetaDefs.Var.F2'(A,B), D, E).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.Var.F1'(A), B, C, D) :-
        'SharedSyntax.GetVariable.P4'(B, 'MetaDefs.Var.F1'(A), C, D).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.CTerm.F1'(A), B, C, D) :-
        'Substs.GetConstant.P4'(B, 'MetaDefs.CTerm.F1'(A), C, D).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.Term.F2'(A,B), C, D, E) :-
        'Substs.GetFunction.P5'(C, 'MetaDefs.Term.F2'(A,F), G, D, H),
        'SharedSyntax.TermOccurCheck.P3'(G, C, B),
        'SharedSyntax.UnifyTermArgs.P5'(B, F, G, H, E).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.XTerm.F3'(A,B,C), D, E, F) :-
        'Substs.GetFunction.P5'(D, 'MetaDefs.XTerm.F3'(A,G,C), H, E, I),
        'SharedSyntax.TermOccurCheck.P3'(H, D, B),
        'SharedSyntax.UnifyTermArgs.P5'(B, G, H, I, F).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.XCTerm.F2'(A,B), C, D, E) :-
        'Substs.GetConstant.P4'(C, 'MetaDefs.XCTerm.F2'(A,B), D, E).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.Int.F1'(A), B, C, D) :-
        'Substs.GetConstant.P4'(B, 'MetaDefs.Int.F1'(A), C, D).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.Str.F1'(A), B, C, D) :-
        'Substs.GetConstant.P4'(B, 'MetaDefs.Str.F1'(A), C, D).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.Prm.F1'(A), B, C, D) :-
        'Substs.GetConstant.P4'(B, 'MetaDefs.Prm.F1'(A), C, D).
'SharedSyntax.UnifyingTermSubst.P4'([], [], A, A).
'SharedSyntax.UnifyingTermSubst.P4'([A|B], [C|D], E, F) :-
        'SharedSyntax.SUnifyTerms.P4'(A, C, E, G),
        'SharedSyntax.UnifyingTermSubst.P4'(B, D, G, F).
%------------------------------------------------------------------------------
% Supplementary Syntax routines shared by the parser
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Pseudo-set maniplation predicates
%------------------------------------------------------------------------------

'SharedSyntax.Order.P2'(X, Y) :-
   sort(X, Y).

'~SharedSyntax.Order.P2'(X, Y) :-
   sort(X, Y).
 
'SharedSyntax.Union.P3'(X, Y, Z) :-
   user:union(X, Y, Z).

'~SharedSyntax.Union.P3'(X, Y, Z) :-
   user:union(X, Y, Z).

'SharedSyntax.Difference.P3'(X, Y, Z) :-
   user:difference(X, Y, Z).

'~SharedSyntax.Difference.P3'(X, Y, Z) :-
   user:difference(X, Y, Z).

%------------------------------------------------------------------------------
% Integer to Character List conversion
%------------------------------------------------------------------------------
 
'SharedSyntax.IntegerToCharDL.P3'(Int, Chars, CharsT) :-
   ( Int = 0 ->
     Chars = [0'0|CharsT]
   ; Int < 0 ->
     Chars = [0'-|Chars1],
     Int1 is -Int,
     int_to_char_dl(Int1, Chars1, CharsT) 
   ; int_to_char_dl(Int, Chars, CharsT)
   ).

'~SharedSyntax.IntegerToCharDL.P3'(Int, Chars, CharsT) :-
   'SharedSyntax.IntegerToCharDL.P3'(Int, Chars, CharsT).

int_to_char_dl(Int, Chars, CharsT) :-
   ( Int = 0 ->
     Chars = CharsT
   ; C is 0'0 + Int mod 10,
     Int1 is Int // 10,
     int_to_char_dl(Int1, Chars, [C|CharsT])
   ).

:- op(500, yfx, and).
:- op(400, yfx, or).

'ParserPrograms.ParserCheckStatement.P4'(A, B, C, D) :-
        'ParserPrograms.CheckStatementAux.P6'(A, B, 0, [], E, F),
        (   E=[G] ->
            C=G,
            D=[]
        ;   E=[] ->
            C='MetaDefs.Empty.C0',
            D=F
        ;   'ParserPrograms.AmbiguityError.P2'(E, H),
            D=[H],
            C='MetaDefs.Empty.C0'
        ).
'ParserPrograms.ExportSymbolTable.P3'('ProgDefs.Program.F4'(_,A,'ProgDefs.Language.F1'(B),_), C, 'ParserPrograms.SymbTab.F4'(C,D,E,F)) :-
        (   'AVLTrees.AVLSearch.P3'(B, C, G) ->
            D=G,
            G='ProgDefs.Module.F3'(_,_,H)
        ;   'SharedPrograms.EmptyCategoryTable.P1'(I),
            D='ProgDefs.Module.F3'('ProgDefs.Hidden.C0',I,H),
            H=[]
        ),
        'AVLTrees.AVLSearch.P3'(A, C, 'ProgDefs.ModDef.F4'(_,J,_,K)),
        'ParserPrograms.InitialImportedSymbols.P2'(B, L),
        'ParserPrograms.AddLiftedSymbols.P3'(H, L, M),
        'ParserPrograms.ImportedSymbols.P5'(J, A, B, M, E),
        'ParserPrograms.ImportedSymbols.P5'(K, A, B, [], F).
'ParserPrograms.DecompileImports.P2'([], _).
'ParserPrograms.DecompileImports.P2'([A|B], C) :-
        'IO.WriteString.P2'(C, '"IMPORT '),
        'IO.WriteString.P2'(C, A),
        'ParserPrograms.WriteRestOfStringList.P2'(B, C),
        'IO.WriteString.P2'(C, '".'),
        'ParserPrograms.NewLines.P2'(C, 2).
'ParserPrograms.CheckStatementAux.P6'([], _, _, A, A, []).
'ParserPrograms.CheckStatementAux.P6'([A|B], C, D, E, F, G) :-
        'SharedPrograms.StatementInLanguage.P5'(A, C, H, I, J),
        (   J=[],
            'Integers.>=.P2'(I, D) ->
            K=I,
            (   I=D ->
                L=[H|E]
            ;   L=[H]
            )
        ;   L=E,
            K=D
        ),
        'Lists.Append.P3'(J, M, G),
        'ParserPrograms.CheckStatementAux.P6'(B, C, K, L, F, M).
'ParserPrograms.CandidateNames.P6'([], _, _, _, A, A).
'ParserPrograms.CandidateNames.P6'(['ProgDefs.Symbol.F2'(B,C)|A], D, E, F, G, H) :-
        (   'SharedPrograms.Accessible.P2'(F, B) ->
            'ParserPrograms.MakeCandidate.P4'(C, D, E, I),
            J=[I|G]
        ;   J=G
        ),
        'ParserPrograms.CandidateNames.P6'(A, D, E, F, J, H).
'ParserPrograms.AmbiguityError.P2'(_, 'SharedPrograms.Ambiguity.C0').
'ParserPrograms.AddLiftedSymbols.P3'([], A, A).
'ParserPrograms.AddLiftedSymbols.P3'(['ProgDefs.Lift.F2'(B,C)|A], D, E) :-
        (   'Lists.Member.P2'('ParserPrograms.ImportItem.F3'(B,_,_), D) ->
            F=D
        ;   'Lists.DeleteFirst.P3'('ParserPrograms.LiftedItem.F2'(B,G), D, H) ->
            'AVLTrees.AVLJoin.P3'(C, G, I),
            F=['ParserPrograms.LiftedItem.F2'(B,I)|H]
        ;   F=['ParserPrograms.LiftedItem.F2'(B,C)|D]
        ),
        'ParserPrograms.AddLiftedSymbols.P3'(A, F, E).
'ParserPrograms.BinExportDelaysOnly.P3'('AVLTrees.Empty.C0', A, A).
'ParserPrograms.BinExportDelaysOnly.P3'('AVLTrees.Tree.F4'(A,B,C,D), E, F) :-
        'ParserPrograms.ListExportDelaysOnly.P2'(C, G),
        'AVLTrees.AVLInsert.P4'(E, B, G, H),
        'ParserPrograms.BinExportDelaysOnly.P3'(A, H, I),
        'ParserPrograms.BinExportDelaysOnly.P3'(D, I, F).
'ParserPrograms.CandidateDeclaration.P3'('ParserPrograms.FCandidate.F2'(A,_), B, C) :-
        'SharedPrograms.SymbolInLanguage.P3'(A, B, C).
'ParserPrograms.CandidateDeclaration.P3'('ParserPrograms.PCandidate.F2'(A,_), B, C) :-
        'SharedPrograms.SymbolInLanguage.P3'(A, B, C).
'ParserPrograms.CandidateTypes.P6'([], _, _, _, A, A).
'ParserPrograms.CandidateTypes.P6'([A|B], C, D, E, F, G) :-
        (   'SharedPrograms.MatchingDescriptor.P5'(A, E, H, I, _) ->
            J=['MetaDefs.Name.F4'(C,D,H,I)|F]
        ;   J=F
        ),
        'ParserPrograms.CandidateTypes.P6'(B, C, D, E, J, G).
'ParserPrograms.CheckFormulaAux.P6'([], _, _, A, A, []).
'ParserPrograms.CheckFormulaAux.P6'([A|B], C, D, E, F, G) :-
        'SharedPrograms.MeltedFormulaTyping.P8'(A, C, [], _, H, I, [], J),
        (   J=[] ->
            'SharedPrograms.FixRationals.P2'(I, K)
        ;   K=0
        ),
        (   J=[],
            'Integers.>=.P2'(K, D) ->
            L=K,
            (   K=D ->
                M=[H|E]
            ;   M=[H]
            )
        ;   L=D,
            M=E
        ),
        'Lists.Append.P3'(J, N, G),
        'ParserPrograms.CheckFormulaAux.P6'(B, C, L, M, F, N).
'ParserPrograms.DecompileDomainType.P4'([A|B], C, D, E) :-
        'SharedPrograms.SProgramTypeToString.P4'(C, D, A, F),
        'IO.WriteString.P2'(E, F),
        'ParserPrograms.DecompileRestTypes.P4'(B, C, D, E).
'ParserPrograms.DecompileConstants.P4'([], _, _, _).
'ParserPrograms.DecompileConstants.P4'([A|B], C, D, E) :-
        'IO.WriteString.P2'(E, '"CONSTANT'),
        'ParserPrograms.NewLines.P2'(E, 1),
        'ParserPrograms.DecompileIdentifiers.P4'([A|B], C, D, E).
'ParserPrograms.Decompile.P2'(A, B) :-
        A='ProgDefs.Program.F4'(_,C,_,_),
        'AVLTrees.AVLToBinary.P2'(C, D),
        'ParserPrograms.DecompileModuleTree.P3'(D, A, B).
'ParserPrograms.CheckTermAux.P6'([], _, _, A, A, []).
'ParserPrograms.CheckTermAux.P6'([A|B], C, D, E, F, G) :-
        'SharedPrograms.MeltedTermType.P9'(A, _, C, [], _, H, I, [], J),
        (   J=[] ->
            'SharedPrograms.FixRationals.P2'(I, K)
        ;   K=0
        ),
        (   J=[],
            'Integers.>=.P2'(K, D) ->
            L=K,
            (   K=D ->
                M=[H|E]
            ;   M=[H]
            )
        ;   L=D,
            M=E
        ),
        'Lists.Append.P3'(J, N, G),
        'ParserPrograms.CheckTermAux.P6'(B, C, L, M, F, N).
'ParserPrograms.DecompileBases.P2'([], _).
'ParserPrograms.DecompileBases.P2'(['ParserPrograms.ID.F2'(B,_)|A], C) :-
        'IO.WriteString.P2'(C, '"BASE '),
        'IO.WriteString.P2'(C, B),
        'ParserPrograms.WriteRestOfIdentifiers.P2'(A, C),
        'IO.WriteString.P2'(C, '".'),
        'ParserPrograms.NewLines.P2'(C, 2).
'ParserPrograms.DecompileDelay.P4'('ProgDefs.Delay.F2'(A,B), C, D, E) :-
        'SharedPrograms.SProgramFormulaToString.P4'(C, D, A, F),
        'IO.WriteString.P2'(E, F),
        'IO.WriteString.P2'(E, '" UNTIL '),
        'SharedPrograms.SConditionToString.P2'(B, G),
        'IO.WriteString.P2'(E, G).
'ParserPrograms.DecompileConstructors.P2'([], _).
'ParserPrograms.DecompileConstructors.P2'(['ParserPrograms.ID.F2'(B,C)|A], D) :-
        'IO.WriteString.P2'(D, '"CONSTRUCTOR '),
        'ParserPrograms.WriteConstructor.P3'(B, C, D),
        'ParserPrograms.WriteRestOfConstructors.P2'(A, D),
        'IO.WriteString.P2'(D, '".'),
        'ParserPrograms.NewLines.P2'(D, 2).
'ParserPrograms.DecompileDelays.P4'([], _, _, _).
'ParserPrograms.DecompileDelays.P4'([A|B], C, D, E) :-
        'IO.WriteString.P2'(E, '"DELAY '),
        'ParserPrograms.DecompileDelay.P4'(A, C, D, E),
        'ParserPrograms.DecompileRestDelays.P4'(B, C, D, E),
        'IO.WriteString.P2'(E, '".'),
        'ParserPrograms.NewLines.P2'(E, 2).
'ParserPrograms.DecompileFunctions.P4'([], _, _, _).
'ParserPrograms.DecompileFunctions.P4'([A|B], C, D, E) :-
        'IO.WriteString.P2'(E, '"FUNCTION'),
        'ParserPrograms.NewLines.P2'(E, 1),
        'ParserPrograms.DecompileIdentifiers.P4'([A|B], C, D, E).
'ParserPrograms.DecompileExportCode.P4'('AVLTrees.Empty.C0', _, _, _).
'ParserPrograms.DecompileExportCode.P4'('AVLTrees.Tree.F4'(A,_,B,C), D, E, F) :-
        'ParserPrograms.DecompileExportPredicates.P4'(B, D, E, F),
        'ParserPrograms.DecompileExportCode.P4'(A, D, E, F),
        'ParserPrograms.DecompileExportCode.P4'(C, D, E, F).
'ParserPrograms.DecompileExportPredicates.P4'([], _, _, _).
'ParserPrograms.DecompileExportPredicates.P4'(['ProgDefs.PredDef.F4'(_,_,B,_)|A], C, D, E) :-
        'ParserPrograms.DecompileDelays.P4'(B, C, D, E),
        'ParserPrograms.DecompileExportPredicates.P4'(A, C, D, E).
'ParserPrograms.DecompileIdentifiers.P4'([A|B], C, D, E) :-
        'ParserPrograms.DecompileGroup.P6'(A, B, C, D, E, F),
        'ParserPrograms.DecompileRestIdentifiers.P4'(F, C, D, E),
        'IO.WriteString.P2'(E, '".'),
        'ParserPrograms.NewLines.P2'(E, 2).
'ParserPrograms.DecompileGroup.P6'('ParserPrograms.ID.F2'(A,B), C, D, E, F, G) :-
        'ParserPrograms.Indent.P1'(F),
        'IO.WriteString.P2'(F, A),
        'ParserPrograms.DecompileRestGroup.P4'(C, B, F, G),
        'IO.WriteString.P2'(F, '" : '),
        'ParserPrograms.DecompileIndicator.P2'(B, F),
        'ParserPrograms.DecompileType.P4'(B, D, E, F).
'ParserPrograms.DecompilePredicates.P4'([], _, _, _).
'ParserPrograms.DecompilePredicates.P4'([A|B], C, D, E) :-
        'IO.WriteString.P2'(E, '"PREDICATE'),
        'ParserPrograms.NewLines.P2'(E, 1),
        'ParserPrograms.DecompileIdentifiers.P4'([A|B], C, D, E).
'ParserPrograms.DecompileLifts.P2'([], _).
'ParserPrograms.DecompileLifts.P2'([A|B], C) :-
        'IO.WriteString.P2'(C, '"LIFT '),
        'IO.WriteString.P2'(C, A),
        'ParserPrograms.WriteRestOfStringList.P2'(B, C),
        'IO.WriteString.P2'(C, '".'),
        'ParserPrograms.NewLines.P2'(C, 2).
'ParserPrograms.DecompileLanguage.P5'(A, B, C, D, E) :-
        'ParserPrograms.GatherDeclarations.P14'(A, B, _, _, _, _, F, [], G, [], H, [], I, []),
        'ParserPrograms.DecompileConstants.P4'(F, C, D, E),
        'ParserPrograms.DecompileFunctions.P4'(G, C, D, E),
        'ParserPrograms.DecompilePropositions.P2'(H, E),
        'ParserPrograms.DecompilePredicates.P4'(I, C, D, E).
'ParserPrograms.DecompileIndicator.P2'(A, B) :-
        (   A='ProgDefs.FunctionDecl.F4'(_,C,_,_),
            user:not_equal([], [C], C, 'Syntax.NoFunctInd.C0') ->
            'ParserPrograms.FunctionIndicator.P3'(C, D, E),
            'IO.WriteString.P2'(B, D),
            'IO.WriteString.P2'(B, '"('),
            'SharedSyntax.IntegerToCharDL.P3'(E, F, []),
            'Strings.StringInts.P2'(G, F),
            'IO.WriteString.P2'(B, G),
            'IO.WriteString.P2'(B, '") : ')
        ;   A='ProgDefs.PredicateDecl.F3'(_,H,_),
            user:not_equal([], [H], H, 'Syntax.NoPredInd.C0') ->
            'ParserPrograms.PredicateIndicator.P2'(H, D),
            'IO.WriteString.P2'(B, D),
            'IO.WriteString.P2'(B, '" : ')
        ;   true
        ).
'ParserPrograms.DecompileModule.P7'(_, A, _, _, _, _, _) :-
        call_residue('SharedPrograms.SystemModule.P1'(A), B),
        (   B=[] ->
            true
        ;   user:flounder_commit(B)
        ), !.
'ParserPrograms.DecompileModule.P7'('ProgDefs.ClosedKind.C0', _, _, _, _, _, _) :- !.
'ParserPrograms.DecompileModule.P7'('ProgDefs.NormalKind.C0', A, B, C, D, E, F) :-
        call_residue(goedel_not('SharedPrograms.SystemModule.P1'(A)), G),
        (   G=[] ->
            true
        ;   user:flounder_commit(G)
        ), !,
        (   F='ParserPrograms.Noisy.C0' ->
            'IO.WriteString.P2'('IO.StdErr.C0', '"Decompiling module "'),
            'IO.WriteString.P2'('IO.StdErr.C0', A),
            'IO.WriteString.P2'('IO.StdErr.C0', '""'),
            'IO.NewLine.P1'('IO.StdErr.C0')
        ;   true
        ),
        E='ProgDefs.Program.F4'(_,_,'ProgDefs.Language.F1'(H),I),
        'AVLTrees.AVLSearch.P3'(H, A, 'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(J,K),L)),
        'AVLTrees.AVLToBinary.P2'(J, M),
        'AVLTrees.AVLToBinary.P2'(K, N),
        'AVLTrees.AVLSearch.P3'(I, A, 'ProgDefs.Code.F2'(_,O)),
        'AVLTrees.AVLToBinary.P2'(O, P),
        (   'ParserPrograms.FindModulePart.P3'(A, '".exp', Q) ->
            'ParserPrograms.ModuleDeclaration.P3'('"EXPORT', A, Q),
            'ParserPrograms.DecompileImports.P2'(B, Q),
            'ParserPrograms.DecompileTypeLanguage.P4'(M, 'ProgDefs.Exported.C0', L, Q),
            'ParserPrograms.DecompileLanguage.P5'(N, 'ProgDefs.Exported.C0', E, A, Q),
            'ParserPrograms.DecompileExportCode.P4'(P, E, A, Q),
            'IO.EndOutput.P1'(Q)
        ;   true
        ),
        (   'ParserPrograms.FindModulePart.P3'(A, '".loc', R) ->
            'ParserPrograms.ModuleDeclaration.P3'('"LOCAL', A, R),
            'ParserPrograms.DecompileImports.P2'(C, R),
            'ParserPrograms.DecompileLifts.P2'(D, R),
            'ParserPrograms.DecompileTypeLanguage.P4'(M, 'ProgDefs.Hidden.C0', [], R),
            'ParserPrograms.DecompileLanguage.P5'(N, 'ProgDefs.Hidden.C0', E, A, R),
            'ParserPrograms.DecompileLocalCode.P4'(P, E, A, R),
            'IO.EndOutput.P1'(R)
        ;   true
        , trace  %% DJD
	).
'ParserPrograms.DecompileModule.P7'('ProgDefs.ModuleKind.C0', A, _, B, C, D, E) :-
        call_residue(goedel_not('SharedPrograms.SystemModule.P1'(A)), F),
        (   F=[] ->
            true
        ;   user:flounder_commit(F)
        ), !,
        (   E='ParserPrograms.Noisy.C0' ->
            'IO.WriteString.P2'('IO.StdErr.C0', '"Decompiling module "'),
            'IO.WriteString.P2'('IO.StdErr.C0', A),
            'IO.WriteString.P2'('IO.StdErr.C0', '""'),
            'IO.NewLine.P1'('IO.StdErr.C0')
        ;   true
        ),
        (   'ParserPrograms.FindModulePart.P3'(A, '".loc', G) ->
            D='ProgDefs.Program.F4'(_,_,'ProgDefs.Language.F1'(H),I),
            'AVLTrees.AVLSearch.P3'(H, A, 'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(J,K),_)),
            'AVLTrees.AVLToBinary.P2'(J, L),
            'AVLTrees.AVLToBinary.P2'(K, M),
            'AVLTrees.AVLSearch.P3'(I, A, 'ProgDefs.Code.F2'(_,N)),
            'AVLTrees.AVLToBinary.P2'(N, O),
            'ParserPrograms.ModuleDeclaration.P3'('"MODULE', A, G),
            'ParserPrograms.DecompileImports.P2'(B, G),
            'ParserPrograms.DecompileLifts.P2'(C, G),
            'ParserPrograms.DecompileTypeLanguage.P4'(L, 'ProgDefs.Hidden.C0', [], G),
            'ParserPrograms.DecompileLanguage.P5'(M, 'ProgDefs.Hidden.C0', D, A, G),
            'ParserPrograms.DecompileLocalCode.P4'(O, D, A, G),
            'IO.EndOutput.P1'(G)
        ;   true
        ).
'ParserPrograms.DecompileLocalCode.P4'('AVLTrees.Empty.C0', _, _, _).
'ParserPrograms.DecompileLocalCode.P4'('AVLTrees.Tree.F4'(A,_,B,C), D, E, F) :-
        'ParserPrograms.DecompileLocalPredicates.P4'(B, D, E, F),
        'ParserPrograms.DecompileLocalCode.P4'(A, D, E, F),
        'ParserPrograms.DecompileLocalCode.P4'(C, D, E, F).
'ParserPrograms.DecompileLocalPredicates.P4'([], _, _, _).
'ParserPrograms.DecompileLocalPredicates.P4'(['ProgDefs.PredDef.F4'(_,B,_,C)|A], D, E, F) :-
        'ParserPrograms.DecompileDelays.P4'(C, D, E, F),
        'ParserPrograms.DecompileStatements.P4'(B, D, E, F),
        'ParserPrograms.NewLines.P2'(F, 1),
        'ParserPrograms.DecompileLocalPredicates.P4'(A, D, E, F).
'ParserPrograms.DecompileModuleTree.P3'('AVLTrees.Empty.C0', _, _).
'ParserPrograms.DecompileModuleTree.P3'('AVLTrees.Tree.F4'(A,B,'ProgDefs.ModDef.F4'(C,D,E,F),G), H, I) :-
        'ParserPrograms.DecompileModule.P7'(C, B, D, E, F, H, I),
        'ParserPrograms.DecompileModuleTree.P3'(A, H, I),
        'ParserPrograms.DecompileModuleTree.P3'(G, H, I).
'ParserPrograms.DecompileType.P4'('ProgDefs.ConstantDecl.F1'(A), B, C, D) :-
        'SharedPrograms.SProgramTypeToString.P4'(B, C, A, E),
        'IO.WriteString.P2'(D, E).
'ParserPrograms.DecompileType.P4'('ProgDefs.FunctionDecl.F4'(_,_,A,B), C, D, E) :-
        'ParserPrograms.DecompileDomainType.P4'(A, C, D, E),
        'IO.WriteString.P2'(E, '" -> '),
        'SharedPrograms.SProgramTypeToString.P4'(C, D, B, F),
        'IO.WriteString.P2'(E, F).
'ParserPrograms.DecompileType.P4'('ProgDefs.PredicateDecl.F3'(_,_,A), B, C, D) :-
        'ParserPrograms.DecompileDomainType.P4'(A, B, C, D).
'ParserPrograms.DecompileRestIdentifiers.P4'([], _, _, _).
'ParserPrograms.DecompileRestIdentifiers.P4'([A|B], C, D, E) :-
        'IO.WriteString.P2'(E, '";'),
        'ParserPrograms.NewLines.P2'(E, 1),
        'ParserPrograms.DecompileGroup.P6'(A, B, C, D, E, F),
        'ParserPrograms.DecompileRestIdentifiers.P4'(F, C, D, E).
'ParserPrograms.DecompileRestDelays.P4'([], _, _, _).
'ParserPrograms.DecompileRestDelays.P4'([A|B], C, D, E) :-
        'IO.WriteString.P2'(E, '";'),
        'ParserPrograms.NewLines.P2'(E, 1),
        'ParserPrograms.DecompileDelay.P4'(A, C, D, E),
        'ParserPrograms.DecompileRestDelays.P4'(B, C, D, E).
'ParserPrograms.DecompilePropositions.P2'([], _).
'ParserPrograms.DecompilePropositions.P2'(['ParserPrograms.ID.F2'(B,_)|A], C) :-
        'IO.WriteString.P2'(C, '"PROPOSITION '),
        'IO.WriteString.P2'(C, B),
        'ParserPrograms.WriteRestOfIdentifiers.P2'(A, C),
        'IO.WriteString.P2'(C, '".'),
        'ParserPrograms.NewLines.P2'(C, 2).
'ParserPrograms.DecompileRestGroup.P4'([], _, _, []).
'ParserPrograms.DecompileRestGroup.P4'(['ParserPrograms.ID.F2'(B,C)|A], D, E, F) :-
        (   'ParserPrograms.VariantDeclarations.P2'(C, D) ->
            'IO.WriteString.P2'(E, '", '),
            'IO.WriteString.P2'(E, B),
            F=G
        ;   F=['ParserPrograms.ID.F2'(B,C)|G]
        ),
        'ParserPrograms.DecompileRestGroup.P4'(A, D, E, G).
'ParserPrograms.DecompileRestTypes.P4'([], _, _, _).
'ParserPrograms.DecompileRestTypes.P4'([A|B], C, D, E) :-
        'IO.WriteString.P2'(E, '" * '),
        'SharedPrograms.SProgramTypeToString.P4'(C, D, A, F),
        'IO.WriteString.P2'(E, F),
        'ParserPrograms.DecompileRestTypes.P4'(B, C, D, E).
'ParserPrograms.DecompileStatements.P4'([], _, _, _).
'ParserPrograms.DecompileStatements.P4'(['MetaDefs.<-''.F2'(B,C)|A], D, E, F) :-
        'SharedPrograms.FormulaToIntList.P4'(D, E, B, G),
        'ParserPrograms.WriteCodes.P2'(G, F),
        (   user:not_equal([], [C], C, 'MetaDefs.Empty.C0') ->
            'SharedPrograms.FormulaToIntList.P4'(D, E, C, H),
            'IO.WriteString.P2'(F, '" <-'),
            'ParserPrograms.NewLines.P2'(F, 1),
            'IO.WriteString.P2'(F, '"   '),
            'ParserPrograms.WriteCodes.P2'(H, F)
        ;   true
        ),
        'IO.WriteString.P2'(F, '".'),
        'ParserPrograms.NewLines.P2'(F, 1),
        'ParserPrograms.DecompileStatements.P4'(A, D, E, F).
'ParserPrograms.DisplayPossibleTypes.P2'(A, B) :-
        'SharedPrograms.SMainModuleInProgram.P2'(A, C),
        'ParserPrograms.GoalSymbolTable.P3'(A, C, D),
        'ParserPrograms.ParserSymbols.P3'(B, D, E),
        (   E=[] ->
            'IO.WriteString.P2'('IO.StdErr.C0', '"No such symbol.'),
            'IO.NewLine.P1'('IO.StdErr.C0')
        ;   'SharedPrograms.ModuleLanguage.P3'(A, C, F),
            'ParserPrograms.DivideCandidates.P11'(E, F, B, G, [], H, [], I, [], J, []),
            'ParserPrograms.DecompileConstants.P4'(G, A, C, 'IO.StdErr.C0'),
            'ParserPrograms.DecompileFunctions.P4'(H, A, C, 'IO.StdErr.C0'),
            'ParserPrograms.DecompilePropositions.P2'(I, 'IO.StdErr.C0'),
            'ParserPrograms.DecompilePredicates.P4'(J, A, C, 'IO.StdErr.C0')
        ).
'ParserPrograms.DictionaryCandidates.P6'(A, B, C, D, E, F) :-
        (   'AVLTrees.AVLSearch.P3'(A, C, G) ->
            'ParserPrograms.CandidateNames.P6'(G, B, C, D, E, F)
        ;   F=E
        ).
'ParserPrograms.DecompileTypeLanguage.P4'(A, B, C, D) :-
        'ParserPrograms.GatherDeclarations.P14'(A, B, E, F, G, H, _, _, _, _, _, _, _, _),
        'ParserPrograms.GatherLiftedDeclarations.P5'(C, F, [], H, []),
        'ParserPrograms.DecompileBases.P2'(E, D),
        'ParserPrograms.DecompileConstructors.P2'(G, D).
'ParserPrograms.DictionaryTypeCandidates.P6'(A, B, C, D, E, F) :-
        (   'AVLTrees.AVLSearch.P3'(A, C, G) ->
            'ParserPrograms.CandidateTypes.P6'(G, B, C, D, E, F)
        ;   F=E
        ).
'ParserPrograms.ExportDelaysOnly.P2'(A, B) :-
        'AVLTrees.AVLToBinary.P2'(A, C),
        'AVLTrees.AVLIsEmpty.P1'(D),
        'ParserPrograms.BinExportDelaysOnly.P3'(C, D, B).
'ParserPrograms.DivideCandidates.P11'([], _, _, A, A, B, B, C, C, D, D).
'ParserPrograms.DivideCandidates.P11'([A|B], C, D, E, F, G, H, I, J, K, L) :-
        'ParserPrograms.CandidateDeclaration.P3'(A, C, M),
        'ParserPrograms.GatherDeclaration.P14'(M, D, _, _, _, _, E, N, G, O, I, P, K, Q),
        'ParserPrograms.DivideCandidates.P11'(B, C, D, N, F, O, H, P, J, Q, L).
'ParserPrograms.LocalSymbolTable.P3'('ProgDefs.Program.F4'(_,A,'ProgDefs.Language.F1'(B),_), C, 'ParserPrograms.SymbTab.F4'(C,D,E,[])) :-
        (   'AVLTrees.AVLSearch.P3'(B, C, F) ->
            D=F
        ;   'SharedPrograms.EmptyCategoryTable.P1'(G),
            D='ProgDefs.Module.F3'('ProgDefs.Hidden.C0',G,[])
        ),
        'AVLTrees.AVLSearch.P3'(A, C, 'ProgDefs.ModDef.F4'(_,H,I,J)),
        'Lists.Append.P3'(H, I, K),
        'Lists.Append.P3'(J, K, L),
        'ParserPrograms.InitialImportedSymbols.P2'(B, M),
        'ParserPrograms.ImportedSymbols.P5'(L, A, B, M, E).
'ParserPrograms.GoalSymbolTable.P3'(A, B, C) :-
        (   'SharedPrograms.SOpenModule.P2'(A, B) ->
            'ParserPrograms.LocalSymbolTable.P3'(A, B, C)
        ;   'ParserPrograms.ExportSymbolTable.P3'(A, B, C)
        ).
'ParserPrograms.GatherDeclarations.P14'('AVLTrees.Empty.C0', _, A, A, B, B, C, C, D, D, E, E, F, F).
'ParserPrograms.GatherDeclarations.P14'('AVLTrees.Tree.F4'(A,B,C,D), E, F, G, H, I, J, K, L, M, N, O, P, Q) :-
        'ParserPrograms.GatherDeclarationsAux.P15'(C, B, E, F, R, H, S, J, T, L, U, N, V, P, W),
        'ParserPrograms.GatherDeclarations.P14'(A, E, R, X, S, Y, T, Z, U, A1, V, B1, W, C1),
        'ParserPrograms.GatherDeclarations.P14'(D, E, X, G, Y, I, Z, K, A1, M, B1, O, C1, Q).
'ParserPrograms.FunctionIndicator.P3'('Syntax.XFX.F1'(A), '"xFx', A).
'ParserPrograms.FunctionIndicator.P3'('Syntax.XFY.F1'(A), '"xFy', A).
'ParserPrograms.FunctionIndicator.P3'('Syntax.YFX.F1'(A), '"yFx', A).
'ParserPrograms.FunctionIndicator.P3'('Syntax.XF.F1'(A), '"xF', A).
'ParserPrograms.FunctionIndicator.P3'('Syntax.FX.F1'(A), '"Fx', A).
'ParserPrograms.FunctionIndicator.P3'('Syntax.YF.F1'(A), '"yF', A).
'ParserPrograms.FunctionIndicator.P3'('Syntax.FY.F1'(A), '"Fy', A).
'ParserPrograms.FindModulePart.P3'(A, B, C) :-
        (   'ParserPrograms.ModuleExists.P2'(A, B) ->
            'ParserPrograms.RenameModuleAsOld.P2'(A, B)
        ;   true
        ),
        (   concat(A, B, D),
            'IO.FindOutput.P2'(D, 'IO.Out.F1'(E)) ->
            C=E
        ;   'IO.WriteString.P2'('IO.StdErr.C0', '"Error: couldn''t open file '),
            concat(A, B, F),
            'IO.WriteString.P2'('IO.StdErr.C0', F),
            'IO.NewLine.P1'('IO.StdErr.C0'),
            fail
        ).
'ParserPrograms.GatherDeclaration.P14'('ProgDefs.BaseDecl.C0', A, ['ParserPrograms.ID.F2'(A,'ProgDefs.BaseDecl.C0')|B], B, C, C, D, D, E, E, F, F, G, G).
'ParserPrograms.GatherDeclaration.P14'('ProgDefs.ConstructorDecl.F1'(A), B, C, C, ['ParserPrograms.ID.F2'(B,'ProgDefs.ConstructorDecl.F1'(A))|D], D, E, E, F, F, G, G, H, H).
'ParserPrograms.GatherDeclaration.P14'('ProgDefs.ConstantDecl.F1'(A), B, C, C, D, D, ['ParserPrograms.ID.F2'(B,'ProgDefs.ConstantDecl.F1'(A))|E], E, F, F, G, G, H, H).
'ParserPrograms.GatherDeclaration.P14'('ProgDefs.FunctionDecl.F4'(A,B,C,D), E, F, F, G, G, H, H, ['ParserPrograms.ID.F2'(E,'ProgDefs.FunctionDecl.F4'(A,B,C,D))|I], I, J, J, K, K).
'ParserPrograms.GatherDeclaration.P14'('ProgDefs.PropositionDecl.C0', A, B, B, C, C, D, D, E, E, ['ParserPrograms.ID.F2'(A,'ProgDefs.PropositionDecl.C0')|F], F, G, G).
'ParserPrograms.GatherDeclaration.P14'('ProgDefs.PredicateDecl.F3'(A,B,C), D, E, E, F, F, G, G, H, H, I, I, ['ParserPrograms.ID.F2'(D,'ProgDefs.PredicateDecl.F3'(A,B,C))|J], J).
'ParserPrograms.GatherDeclarationsAux.P15'([], _, _, A, A, B, B, C, C, D, D, E, E, F, F).
'ParserPrograms.GatherDeclarationsAux.P15'([A|B], C, D, E, F, G, H, I, J, K, L, M, N, O, P) :-
        (   A='ProgDefs.Symbol.F2'(D,Q) ->
            'ParserPrograms.GatherDeclaration.P14'(Q, C, E, R, G, S, I, T, K, U, M, V, O, W)
        ;   E=R,
            G=S,
            I=T,
            K=U,
            M=V,
            O=W
        ),
        'ParserPrograms.GatherDeclarationsAux.P15'(B, C, D, R, F, S, H, T, J, U, L, V, N, W, P).
'ParserPrograms.GatherLiftedDeclarations.P5'([], A, A, B, B).
'ParserPrograms.GatherLiftedDeclarations.P5'(['ProgDefs.Lift.F2'(_,B)|A], C, D, E, F) :-
        'AVLTrees.AVLToBinary.P2'(B, G),
        'ParserPrograms.GatherDeclarations.P14'(G, 'ProgDefs.Exported.C0', C, H, E, I, _, _, _, _, _, _, _, _),
        'ParserPrograms.GatherLiftedDeclarations.P5'(A, H, D, I, F).
'ParserPrograms.ItemCandidates.P4'('ParserPrograms.ImportItem.F3'(A,_,B), C, D, E) :-
        'ParserPrograms.DictionaryCandidates.P6'(B, A, C, 'ProgDefs.Exported.C0', D, E).
'ParserPrograms.ItemCandidates.P4'('ParserPrograms.LiftedItem.F2'(_,_), _, A, A).
'ParserPrograms.Indent.P1'(A) :-
        'IO.WriteString.P2'(A, '"   ').
'ParserPrograms.ImportedSymbols.P5'([], _, _, A, A).
'ParserPrograms.ImportedSymbols.P5'([A|B], C, D, E, F) :-
        (   'Lists.Member.P2'('ParserPrograms.ImportItem.F3'(A,_,_), E) ->
            G=E,
            H=B
        ;   (   'Lists.DeleteFirst.P3'('ParserPrograms.LiftedItem.F2'(A,_), E, I) ->
                J=I
            ;   J=E
            ),
            (   'AVLTrees.AVLSearch.P3'(D, A, 'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(K,L),M)) ->
                'ParserPrograms.AddLiftedSymbols.P3'(M, J, N),
                G=['ParserPrograms.ImportItem.F3'(A,K,L)|N]
            ;   'AVLTrees.AVLIsEmpty.P1'(O),
                G=['ParserPrograms.ImportItem.F3'(A,O,O)|J]
            ),
            'AVLTrees.AVLSearch.P3'(C, A, 'ProgDefs.ModDef.F4'(_,P,_,_)),
            'Lists.Append.P3'(P, B, H)
        ),
        'ParserPrograms.ImportedSymbols.P5'(H, C, D, G, F).
'ParserPrograms.InitialImportedSymbols.P2'(A, ['ParserPrograms.ImportItem.F3'('"',B,C)]) :-
        'AVLTrees.AVLSearch.P3'(A, '"', 'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(B,C),_)).
'ParserPrograms.ItemTypeCandidates.P4'('ParserPrograms.LiftedItem.F2'(A,B), C, D, E) :-
        'ParserPrograms.DictionaryTypeCandidates.P6'(B, A, C, 'ProgDefs.Exported.C0', D, E).
'ParserPrograms.ItemTypeCandidates.P4'('ParserPrograms.ImportItem.F3'(A,B,_), C, D, E) :-
        'ParserPrograms.DictionaryTypeCandidates.P6'(B, A, C, 'ProgDefs.Exported.C0', D, E).
'ParserPrograms.ListExportDelaysOnly.P2'([], []).
'ParserPrograms.ListExportDelaysOnly.P2'(['ProgDefs.PredDef.F4'(B,_,C,_)|A], ['ProgDefs.PredDef.F4'(B,[],C,[])|D]) :-
        'ParserPrograms.ListExportDelaysOnly.P2'(A, D).
'ParserPrograms.ModuleTypeCandidates.P5'('ProgDefs.Module.F3'(A,'ProgDefs.Categories.F2'(B,_),_), C, D, E, F) :-
        'ParserPrograms.DictionaryTypeCandidates.P6'(B, C, D, A, E, F).
'ParserPrograms.ModuleCandidates.P5'('ProgDefs.Module.F3'(A,'ProgDefs.Categories.F2'(_,B),_), C, D, E, F) :-
        'ParserPrograms.DictionaryCandidates.P6'(B, C, D, A, E, F).
'ParserPrograms.MakeCandidate.P4'('ProgDefs.ConstantDecl.F1'(_), A, B, 'ParserPrograms.FCandidate.F2'('MetaDefs.Name.F4'(A,B,'MetaDefs.Constant.C0',0),'Syntax.NoFunctInd.C0')).
'ParserPrograms.MakeCandidate.P4'('ProgDefs.FunctionDecl.F4'(A,B,_,_), C, D, 'ParserPrograms.FCandidate.F2'('MetaDefs.Name.F4'(C,D,'MetaDefs.Function.C0',A),B)).
'ParserPrograms.MakeCandidate.P4'('ProgDefs.PropositionDecl.C0', A, B, 'ParserPrograms.PCandidate.F2'('MetaDefs.Name.F4'(A,B,'MetaDefs.Proposition.C0',0),'Syntax.NoPredInd.C0')).
'ParserPrograms.MakeCandidate.P4'('ProgDefs.PredicateDecl.F3'(A,B,_), C, D, 'ParserPrograms.PCandidate.F2'('MetaDefs.Name.F4'(C,D,'MetaDefs.Predicate.C0',A),B)).
'ParserPrograms.ModuleDeclaration.P3'(A, B, C) :-
        'IO.WriteString.P2'(C, A),
        'IO.WriteString.P2'(C, '" '),
        'IO.WriteString.P2'(C, B),
        'IO.WriteString.P2'(C, '".'),
        'ParserPrograms.NewLines.P2'(C, 2).
'ParserPrograms.ParserCheckFormula.P4'(A, B, C, D) :-
        'ParserPrograms.CheckFormulaAux.P6'(A, B, 0, [], E, F),
        (   E=[G] ->
            C=G,
            D=[]
        ;   E=[] ->
            D=F,
            C='MetaDefs.Empty.C0'
        ;   'ParserPrograms.AmbiguityError.P2'(E, H),
            D=[H],
            C='MetaDefs.Empty.C0'
        ).
'ParserPrograms.NewLines.P2'(A, B) :-
        (   'Integers.>.P2'(B, 0) ->
            'IO.NewLine.P1'(A),
            minus(B, 1, C),
            'ParserPrograms.NewLines.P2'(A, C)
        ;   true
        ).
'ParserPrograms.QuickInsertModuleDescriptor.P4'('ProgDefs.Language.F1'(A), B, C, 'ProgDefs.Language.F1'(D)) :-
        'AVLTrees.AVLInsert.P4'(A, B, C, D).
'ParserPrograms.ParserTypeSymbols.P3'(A, 'ParserPrograms.SymbTab.F4'(B,C,D,_), E) :-
        'ParserPrograms.ModuleTypeCandidates.P5'(C, B, A, [], F),
        'ParserPrograms.TableTypeCandidates.P4'(D, A, F, E).
'ParserPrograms.ParserSymbols.P3'(A, 'ParserPrograms.SymbTab.F4'(B,C,D,_), E) :-
        'ParserPrograms.ModuleCandidates.P5'(C, B, A, [], F),
        'ParserPrograms.TableCandidates.P4'(D, A, F, E).
'ParserPrograms.ParserInsert.P5'('ParserPrograms.SymbTab.F4'(A,'ProgDefs.Module.F3'(B,'ProgDefs.Categories.F2'(C,D),E),F,G), 'MetaDefs.Name.F4'(A,H,_,_), I, J, 'ParserPrograms.SymbTab.F4'(A,'ProgDefs.Module.F3'(B,'ProgDefs.Categories.F2'(C,K),E),F,G)) :-
        'AVLTrees.AVLAmend.P6'(D, H, ['ProgDefs.Symbol.F2'(I,J)|L], [], K, L).
'ParserPrograms.ParserTypeInsert.P5'('ParserPrograms.SymbTab.F4'(A,'ProgDefs.Module.F3'(B,'ProgDefs.Categories.F2'(C,D),E),F,G), 'MetaDefs.Name.F4'(A,H,I,J), K, L, 'ParserPrograms.SymbTab.F4'(A,'ProgDefs.Module.F3'(B,'ProgDefs.Categories.F2'(M,D),N),O,G)) :-
        (   K='ProgDefs.Exported.C0',
            'ParserPrograms.TypeFoundInModules.P5'(G, H, I, J, P),
            user:not_equal([], [P], P, []) ->
            M=C,
            'ParserPrograms.UpdateLiftedSymbols.P5'(P, H, L, E, N),
            'ParserPrograms.UpdateImportedSymbols.P4'(P, N, F, O)
        ;   O=F,
            N=E,
            'AVLTrees.AVLAmend.P6'(C, H, ['ProgDefs.Symbol.F2'(K,L)|Q], [], M, Q)
        ).
'ParserPrograms.QuickInsertImport.P5'('ProgDefs.Program.F4'(A,B,'ProgDefs.Language.F1'(C),D), E, F, G, 'ProgDefs.Program.F4'(A,H,'ProgDefs.Language.F1'(I),J)) :-
        'AVLTrees.AVLUpdate.P5'(B, E, 'ProgDefs.ModDef.F4'(K,L,M,N), O, 'ProgDefs.ModDef.F4'(K,P,Q,N)),
        'SharedPrograms.AddImportToPart.P6'(F, G, P, Q, L, M),
        (   'AVLTrees.AVLSearch.P3'(O, G, _) ->
            H=O,
            I=C,
            J=D
        ;   'SharedPrograms.AddModule.P7'(G, O, C, D, H, I, J)
        ).
'ParserPrograms.QuickInsertDelay.P7'('ProgDefs.Program.F4'(A,B,C,D), E, F, G, H, I, 'ProgDefs.Program.F4'(A,B,C,J)) :-
        'AVLTrees.AVLUpdate.P5'(D, E, 'ProgDefs.Code.F2'(K,L), J, 'ProgDefs.Code.F2'(K,M)),
        'AVLTrees.AVLAmend.P6'(M, F, N, [], L, O),
        (   'Lists.DeleteFirst.P3'('ProgDefs.PredDef.F4'(G,P,Q,R), O, S) ->
            'SharedPrograms.UpdateDelays.P6'(H, I, Q, R, T, U),
            N=['ProgDefs.PredDef.F4'(G,P,T,U)|S]
        ;   'SharedPrograms.UpdateDelays.P6'(H, I, [], [], T, U),
            N=['ProgDefs.PredDef.F4'(G,[],T,U)|O]
        ).
'ParserPrograms.PredicateIndicator.P2'('Syntax.ZPZ.C0', '"zPz').
'ParserPrograms.PredicateIndicator.P2'('Syntax.ZP.C0', '"zP').
'ParserPrograms.PredicateIndicator.P2'('Syntax.PZ.C0', '"Pz').
'ParserPrograms.QuickInsertModuleCode.P4'('ProgDefs.Program.F4'(A,B,C,D), E, F, 'ProgDefs.Program.F4'(A,B,C,G)) :-
        'SharedPrograms.SystemModule.P1'(E),
        'AVLTrees.AVLInsert.P4'(D, E, 'ProgDefs.Code.F2'(0,F), G).
'ParserPrograms.QuickInsertLift.P4'('ProgDefs.Program.F4'(A,B,'ProgDefs.Language.F1'(C),D), E, F, 'ProgDefs.Program.F4'(A,G,'ProgDefs.Language.F1'(H),I)) :-
        'AVLTrees.AVLUpdate.P5'(B, E, 'ProgDefs.ModDef.F4'(J,K,L,[F|M]), N, 'ProgDefs.ModDef.F4'(J,K,L,M)),
        (   'AVLTrees.AVLSearch.P3'(N, F, _) ->
            G=N,
            H=C,
            I=D
        ;   'SharedPrograms.AddModule.P7'(F, N, C, D, G, H, I)
        ).
'ParserPrograms.QuickInsertModuleDef.P4'('ProgDefs.Program.F4'(A,B,C,D), E, F, 'ProgDefs.Program.F4'(A,G,C,D)) :-
        'AVLTrees.AVLAmend.P6'(B, E, F, _, G, _).
'ParserPrograms.TableTypeCandidates.P4'([], _, A, A).
'ParserPrograms.TableTypeCandidates.P4'([A|B], C, D, E) :-
        'ParserPrograms.ItemTypeCandidates.P4'(A, C, D, F),
        'ParserPrograms.TableTypeCandidates.P4'(B, C, F, E).
'ParserPrograms.QuickNewProgram.P3'(A, B, C) :-
        'SharedPrograms.ProgramSkeleton.P4'(A, B, 0, C).
'ParserPrograms.QuickMakeModule.P3'(A, 'ProgDefs.Program.F4'(B,C,'ProgDefs.Language.F1'(D),E), 'ProgDefs.Program.F4'(B,F,'ProgDefs.Language.F1'(G),H)) :-
        (   'AVLTrees.AVLSearch.P3'(C, A, _) ->
            F=C,
            G=D,
            H=E
        ;   'SharedPrograms.AddModule.P7'(A, C, D, E, F, G, H)
        ).
'ParserPrograms.QuickInsertStatement.P6'('ProgDefs.Program.F4'(A,B,C,D), E, F, G, H, 'ProgDefs.Program.F4'(A,B,C,I)) :-
        'AVLTrees.AVLUpdate.P5'(D, E, 'ProgDefs.Code.F2'(J,K), I, 'ProgDefs.Code.F2'(J,L)),
        'AVLTrees.AVLAmend.P6'(L, F, M, [], K, N),
        (   'Lists.DeleteFirst.P3'('ProgDefs.PredDef.F4'(G,O,P,Q), N, R) ->
            M=['ProgDefs.PredDef.F4'(G,[H|O],P,Q)|R]
        ;   M=['ProgDefs.PredDef.F4'(G,[H],[],[])|N]
        ).
'ParserPrograms.ReplaceProgramLanguage.P3'('ProgDefs.Program.F4'(A,B,_,C), D, 'ProgDefs.Program.F4'(A,B,D,C)).
'ParserPrograms.RenameModuleAsOld.P2'(A, B) :-
        (   goedel_not((((concat(A,B,E),concat(E,'".old',D)),concat(A,B,C)),'ParserPrograms.RenameFile.P2'(C,D))) ->
            'IO.WriteString.P2'('IO.StdErr.C0', '"Error: couldn''t rename '),
            concat(A, B, F),
            'IO.WriteString.P2'('IO.StdErr.C0', F),
            'IO.WriteString.P2'('IO.StdErr.C0', '" as '),
            concat(A, B, H),
            concat(H, '".old', G),
            'IO.WriteString.P2'('IO.StdErr.C0', G),
            'IO.NewLine.P1'('IO.StdErr.C0'),
            fail
        ;   true
        ).
'ParserPrograms.TableCandidates.P4'([], _, A, A).
'ParserPrograms.TableCandidates.P4'([A|B], C, D, E) :-
        'ParserPrograms.ItemCandidates.P4'(A, C, D, F),
        'ParserPrograms.TableCandidates.P4'(B, C, F, E).
'ParserPrograms.UpdateProgramLanguage.P4'('ProgDefs.Program.F4'(A,B,'ProgDefs.Language.F1'(C),D), E, 'ParserPrograms.SymbTab.F4'(E,F,_,_), 'ProgDefs.Program.F4'(A,B,'ProgDefs.Language.F1'(G),D)) :-
        'AVLTrees.AVLAmend.P6'(C, E, F, _, G, _).
'ParserPrograms.TypeFoundInModules.P5'([], _, _, _, []).
'ParserPrograms.TypeFoundInModules.P5'([A|B], C, D, E, F) :-
        (   'ParserPrograms.TypeFoundInItem.P5'(A, C, D, E, G) ->
            F=[G|H]
        ;   F=H
        ),
        'ParserPrograms.TypeFoundInModules.P5'(B, C, D, E, H).
'ParserPrograms.TypeFoundInItem.P5'('ParserPrograms.ImportItem.F3'(A,B,_), C, D, E, A) :-
        'ParserPrograms.TypeFoundInDictionary.P4'(B, C, D, E).
'ParserPrograms.TypeFoundInItem.P5'('ParserPrograms.LiftedItem.F2'(A,B), C, D, E, A) :-
        'ParserPrograms.TypeFoundInDictionary.P4'(B, C, D, E).
'ParserPrograms.TypeFoundInDictionary.P4'(A, B, C, D) :-
        'AVLTrees.AVLSearch.P3'(A, B, E),
        'SharedPrograms.FindDescriptor.P5'(E, 'ProgDefs.Exported.C0', C, D, _).
'ParserPrograms.UpdateLiftedSymbols.P5'([], _, _, A, A).
'ParserPrograms.UpdateLiftedSymbols.P5'([A|B], C, D, E, F) :-
        (   'Lists.DeleteFirst.P3'('ProgDefs.Lift.F2'(A,G), E, H) ->
            I=G,
            J=H
        ;   J=E,
            'AVLTrees.AVLIsEmpty.P1'(I)
        ),
        'AVLTrees.AVLAmend.P6'(I, C, ['ProgDefs.Symbol.F2'('ProgDefs.Exported.C0',D)|K], [], L, K),
        'ParserPrograms.UpdateLiftedSymbols.P5'(B, C, D, ['ProgDefs.Lift.F2'(A,L)|J], F).
'ParserPrograms.UpdateImportedSymbols.P4'([], _, A, A).
'ParserPrograms.UpdateImportedSymbols.P4'([A|B], C, D, E) :-
        'Lists.MemberCheck.P2'('ProgDefs.Lift.F2'(A,F), C),
        (   'Lists.DeleteFirst.P3'('ParserPrograms.LiftedItem.F2'(A,_), D, G) ->
            H=['ParserPrograms.LiftedItem.F2'(A,F)|G]
        ;   'Lists.MemberCheck.P2'('ParserPrograms.ImportItem.F3'(A,_,_), D) ->
            H=D
        ;   H=['ParserPrograms.LiftedItem.F2'(A,F)|D]
        ),
        'ParserPrograms.UpdateImportedSymbols.P4'(B, C, H, E).
'ParserPrograms.WriteRestOfConstructors.P2'([], _).
'ParserPrograms.WriteRestOfConstructors.P2'(['ParserPrograms.ID.F2'(B,C)|A], D) :-
        'IO.WriteString.P2'(D, '", '),
        'ParserPrograms.WriteConstructor.P3'(B, C, D),
        'ParserPrograms.WriteRestOfConstructors.P2'(A, D).
'ParserPrograms.WriteCodes.P2'([], _).
'ParserPrograms.WriteCodes.P2'([A|B], C) :-
        'IO.Put.P2'(C, A),
        'ParserPrograms.WriteCodes.P2'(B, C).
'ParserPrograms.VariantDeclarations.P2'('ProgDefs.ConstantDecl.F1'(A), 'ProgDefs.ConstantDecl.F1'(B)) :-
        'ParserPrograms.VariantTypeLists.P2'([A], [B]).
'ParserPrograms.VariantDeclarations.P2'('ProgDefs.FunctionDecl.F4'(_,_,A,B), 'ProgDefs.FunctionDecl.F4'(_,_,C,D)) :-
        'ParserPrograms.VariantTypeLists.P2'([B|A], [D|C]).
'ParserPrograms.VariantDeclarations.P2'('ProgDefs.PredicateDecl.F3'(_,_,A), 'ProgDefs.PredicateDecl.F3'(_,_,B)) :-
        'ParserPrograms.VariantTypeLists.P2'(A, B).
'ParserPrograms.WriteConstructor.P3'(A, 'ProgDefs.ConstructorDecl.F1'(B), C) :-
        'IO.WriteString.P2'(C, A),
        'IO.WriteString.P2'(C, '"/'),
        'SharedSyntax.IntegerToCharDL.P3'(B, D, []),
        'Strings.StringInts.P2'(E, D),
        'IO.WriteString.P2'(C, E).
'ParserPrograms.WriteRestOfIdentifiers.P2'([], _).
'ParserPrograms.WriteRestOfIdentifiers.P2'(['ParserPrograms.ID.F2'(B,_)|A], C) :-
        'IO.WriteString.P2'(C, '", '),
        'IO.WriteString.P2'(C, B),
        'ParserPrograms.WriteRestOfIdentifiers.P2'(A, C).
'ParserPrograms.WriteRestOfStringList.P2'([], _).
'ParserPrograms.WriteRestOfStringList.P2'([A|B], C) :-
        'IO.WriteString.P2'(C, '", '),
        'IO.WriteString.P2'(C, A),
        'ParserPrograms.WriteRestOfStringList.P2'(B, C).
%------------------------------------------------------------------------------
% Basic file manipulation, for decompiler
%------------------------------------------------------------------------------
 
'ParserPrograms.ModuleExists.P2'(GModule, GSuffix) :-
   gstring2string(GModule, Module),
   gstring2string(GSuffix, Suffix),
   file_exist(Module, Suffix).

'~ParserPrograms.ModuleExists.P2'(GModule, GSuffix) :- 
   'ParserPrograms.ModuleExists.P2'(GModule, GSuffix).

'ParserPrograms.RenameFile.P2'(GFrom, GTo) :-
   gstring2string(GFrom, From),
   gstring2string(GTo, To),
   sappend(To, ' 2>/dev/null', Cmnd1),
   sappend(' ', Cmnd1, Cmnd2),
   sappend(From, Cmnd2, Cmnd3),
   sappend('mv ', Cmnd3, Command),
   unix(system(Command, 0)).

'~ParserPrograms.RenameFile.P2'(GFrom, GTo) :-
   'ParserPrograms.RenameFile.P2'(GFrom, GTo).

%------------------------------------------------------------------------------
% Check for variant types, for decompiler
%------------------------------------------------------------------------------

'ParserPrograms.VariantTypeLists.P2'(ListA, ListB) :-
   'ParserSyntax.PCheckVariantTypes.P4'(ListA, [], _, ListB).

'~ParserPrograms.VariantTypeLists.P2'(ListA, ListB) :-
   'ParserPrograms.VariantTypeLists.P2'(ListA, ListB).
:- op(500, yfx, and).
:- op(400, yfx, or).

'SharedPrograms.ModulePartInProgram.P3'('ProgDefs.Program.F4'(_,A,_,_), B, C) :-
        'SharedPrograms.AVLFind.P3'(A, B, 'ProgDefs.ModDef.F4'(D,_,_,_)),
        'SharedPrograms.KindHasPart.P2'(D, C).
'SharedPrograms.InitialiseLanguage.P2'(A, B) :-
        'SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'('"','"True','MetaDefs.Proposition.C0',0), A, 'ProgDefs.Exported.C0', 'ProgDefs.PropositionDecl.C0', C),
        'SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'('"','"False','MetaDefs.Proposition.C0',0), C, 'ProgDefs.Exported.C0', 'ProgDefs.PropositionDecl.C0', D),
        'SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'('"','"=','MetaDefs.Predicate.C0',2), D, 'ProgDefs.Exported.C0', 'ProgDefs.PredicateDecl.F3'(2,'Syntax.ZPZ.C0',['MetaDefs.Par.F1'(0),'MetaDefs.Par.F1'(0)]), E),
        'SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'('"','"~=','MetaDefs.Predicate.C0',2), E, 'ProgDefs.Exported.C0', 'ProgDefs.PredicateDecl.F3'(2,'Syntax.ZPZ.C0',['MetaDefs.Par.F1'(0),'MetaDefs.Par.F1'(0)]), B).
'SharedPrograms.ChainExprToIntDL.P6'('MetaDefs.CTerm.F1'(_), _, _, _, A, A).
'SharedPrograms.ChainExprToIntDL.P6'('MetaDefs.Var.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('"|', F, H),
        'SharedPrograms.TermToIntDL.P9'('MetaDefs.Var.F2'(A,B), C, D, E, _, _, _, H, G).
'SharedPrograms.ChainExprToIntDL.P6'('MetaDefs.Var.F1'(A), B, C, D, E, F) :-
        'SharedPrograms.CharDL.P3'('"|', E, G),
        'SharedPrograms.TermToIntDL.P9'('MetaDefs.Var.F1'(A), B, C, D, _, _, _, G, F).
'SharedPrograms.ChainExprToIntDL.P6'('MetaDefs.Term.F2'(_,[A,B]), C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('",', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'SharedPrograms.ChainExprToIntDL.P6'(B, C, D, E, I, G).
'SharedPrograms.AddOtherModulesAux.P7'([], A, B, C, A, B, C).
'SharedPrograms.AddOtherModulesAux.P7'([A|B], C, D, E, F, G, H) :-
        'SharedPrograms.AddModule.P7'(A, C, D, E, I, J, K),
        'SharedPrograms.AddOtherModulesAux.P7'(B, I, J, K, F, G, H).
'SharedPrograms.AddModule.P7'(A, B, C, D, E, F, G) :-
        (   'SharedPrograms.SystemModule.P1'(A) ->
            'SharedPrograms.SystemModuleRep.P4'(A, H, I, J),
            'AVLTrees.AVLInsert.P4'(B, A, H, K),
            'AVLTrees.AVLInsert.P4'(C, A, I, L),
            'AVLTrees.AVLInsert.P4'(D, A, 'ProgDefs.Code.F2'(0,J), M),
            'SharedPrograms.AddOtherModules.P7'(H, K, L, M, E, F, G)
        ;   'AVLTrees.AVLInsert.P4'(B, A, 'ProgDefs.ModDef.F4'('ProgDefs.NormalKind.C0',[],[],[]), E),
            F=C,
            'AVLTrees.AVLIsEmpty.P1'(N),
            'SharedPrograms.NextModuleVersion.P2'(A, O),
            'AVLTrees.AVLInsert.P4'(D, A, 'ProgDefs.Code.F2'(O,N), G)
        ).
'SharedPrograms.AddImportToPart.P6'('ProgDefs.Export.C0', A, B, C, [A|B], C).
'SharedPrograms.AddImportToPart.P6'('ProgDefs.Closed.C0', A, B, C, [A|B], C).
'SharedPrograms.AddImportToPart.P6'('ProgDefs.Local.C0', A, B, C, B, [A|C]).
'SharedPrograms.AddImportToPart.P6'('ProgDefs.Module.C0', A, B, C, B, [A|C]).
'SharedPrograms.Accessible.P2'('ProgDefs.Exported.C0', 'ProgDefs.Exported.C0').
'SharedPrograms.Accessible.P2'('ProgDefs.Hidden.C0', _).
'SharedPrograms.AddLiftedLanguage.P4'([], _, A, A).
'SharedPrograms.AddLiftedLanguage.P4'(['ProgDefs.Lift.F2'(B,C)|A], D, E, F) :-
        (   'Lists.Member.P2'(B, D) ->
            G=E
        ;   'AVLTrees.AVLIsEmpty.P1'(H),
            (   'AVLTrees.AVLUpdate.P5'(E, B, 'ProgDefs.Module.F3'('ProgDefs.Exported.C0','ProgDefs.Categories.F2'(I,H),[]), J, 'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(K,_),_)) ->
                'AVLTrees.AVLJoin.P3'(C, K, I),
                G=J
            ;   'AVLTrees.AVLInsert.P4'(E, B, 'ProgDefs.Module.F3'('ProgDefs.Exported.C0','ProgDefs.Categories.F2'(C,H),[]), G)
            )
        ),
        'SharedPrograms.AddLiftedLanguage.P4'(A, D, G, F).
'SharedPrograms.AddOtherModules.P7'('ProgDefs.ModDef.F4'(_,A,B,C), D, E, F, G, H, I) :-
        'Lists.Append.P3'(C, A, J),
        'Lists.Append.P3'(B, J, K),
        'SharedPrograms.AddOtherModulesAux.P7'(K, D, E, F, G, H, I).
'SharedPrograms.BinaryConnective.P3'('MetaDefs.&''.F2'(A,B), A, B).
'SharedPrograms.BinaryConnective.P3'('MetaDefs.\\/''.F2'(A,B), A, B).
'SharedPrograms.BinaryConnective.P3'('MetaDefs.->''.F2'(A,B), A, B).
'SharedPrograms.BinaryConnective.P3'('MetaDefs.<-''.F2'(A,B), A, B).
'SharedPrograms.BinaryConnective.P3'('MetaDefs.<->''.F2'(A,B), A, B).
'SharedPrograms.BaseInLanguage.P2'(A, B) :-
        'SharedPrograms.TypeSymbolInLanguage.P3'(A, B, 'ProgDefs.BaseDecl.C0').
'SharedPrograms.AtomToIntDL.P8'('Syntax.NoPredInd.C0', 'MetaDefs.Name.F4'(A,B,C,D), E, F, G, H, I, J) :-
        (   A='"Integers',
            B='"Interval',
            C='MetaDefs.Predicate.C0',
            D=3 ->
            E=[K,L,M],
            'SharedPrograms.ClassifyToken.P2'('"=<', N),
            'Strings.StringInts.P2'('"=<', O),
            'SharedPrograms.TermToIntDL.P9'(K, F, G, H, _, _, P, I, Q),
            'SharedPrograms.SpaceIfNeeded.P4'(P, N, Q, R),
            'Lists.Append.P3'(O, S, R),
            'SharedPrograms.TermToIntDL.P9'(L, F, G, H, _, T, U, V, W),
            'SharedPrograms.SpaceIfNeeded.P4'(N, T, S, V),
            'SharedPrograms.SpaceIfNeeded.P4'(U, N, W, X),
            'Lists.Append.P3'(O, Y, X),
            'SharedPrograms.TermToIntDL.P9'(M, F, G, H, _, Z, _, A1, J),
            'SharedPrograms.SpaceIfNeeded.P4'(N, Z, Y, A1)
        ;   'Strings.StringInts.P2'(B, B1),
            'Lists.Append.P3'(B1, Q, I),
            'SharedPrograms.CharDL.P3'('"(', Q, R),
            'SharedPrograms.TermListToIntDL.P6'(E, F, G, H, R, S),
            'SharedPrograms.CharDL.P3'('")', S, J)
        ).
'SharedPrograms.AtomToIntDL.P8'('Syntax.ZPZ.C0', 'MetaDefs.Name.F4'(_,A,_,_), [B,C], D, E, F, G, H) :-
        'SharedPrograms.TermToIntDL.P9'(B, D, E, F, _, _, I, G, J),
        'SharedPrograms.ClassifyToken.P2'(A, K),
        'SharedPrograms.SpaceIfNeeded.P4'(I, K, J, L),
        'Strings.StringInts.P2'(A, M),
        'Lists.Append.P3'(M, N, L),
        'SharedPrograms.TermToIntDL.P9'(C, D, E, F, _, O, _, P, H),
        'SharedPrograms.SpaceIfNeeded.P4'(O, K, N, P).
'SharedPrograms.AtomToIntDL.P8'('Syntax.ZP.C0', 'MetaDefs.Name.F4'(_,A,_,_), [B], C, D, E, F, G) :-
        'SharedPrograms.TermToIntDL.P9'(B, C, D, E, _, _, H, F, I),
        'SharedPrograms.ClassifyToken.P2'(A, J),
        'SharedPrograms.SpaceIfNeeded.P4'(H, J, I, K),
        'Strings.StringInts.P2'(A, L),
        'Lists.Append.P3'(L, G, K).
'SharedPrograms.AtomToIntDL.P8'('Syntax.PZ.C0', 'MetaDefs.Name.F4'(_,A,_,_), [B], C, D, E, F, G) :-
        'SharedPrograms.ClassifyToken.P2'(A, H),
        'Strings.StringInts.P2'(A, I),
        'Lists.Append.P3'(I, J, F),
        'SharedPrograms.TermToIntDL.P9'(B, C, D, E, _, K, _, L, G),
        'SharedPrograms.SpaceIfNeeded.P4'(K, H, J, L).
'SharedPrograms.AndSeqToIntDL.P3'('ProgDefs.And.F2'(A,B), C, D) :-
        'SharedPrograms.SimpleCondToIntDL.P3'(A, C, E),
        'Strings.StringInts.P2'('" & ', F),
        'Lists.Append.P3'(F, G, E),
        (   B='ProgDefs.And.F2'(_,_) ->
            'SharedPrograms.AndSeqToIntDL.P3'(B, G, D)
        ;   'SharedPrograms.SimpleCondToIntDL.P3'(B, G, D)
        ).
'SharedPrograms.BinConnectiveChars.P3'('MetaDefs.&''.F2'(_,_), A, B) :-
        'Strings.StringInts.P2'('" & ', C),
        'Lists.Append.P3'(C, B, A).
'SharedPrograms.BinConnectiveChars.P3'('MetaDefs.\\/''.F2'(_,_), A, B) :-
        'Strings.StringInts.P2'('" \\/ ', C),
        'Lists.Append.P3'(C, B, A).
'SharedPrograms.BinConnectiveChars.P3'('MetaDefs.->''.F2'(_,_), A, B) :-
        'Strings.StringInts.P2'('" -> ', C),
        'Lists.Append.P3'(C, B, A).
'SharedPrograms.BinConnectiveChars.P3'('MetaDefs.<-''.F2'(_,_), A, B) :-
        'Strings.StringInts.P2'('" <- ', C),
        'Lists.Append.P3'(C, B, A).
'SharedPrograms.BinConnectiveChars.P3'('MetaDefs.<->''.F2'(_,_), A, B) :-
        'Strings.StringInts.P2'('" <-> ', C),
        'Lists.Append.P3'(C, B, A).
'SharedPrograms.BinaryInfixToIntDL.P12'(A, 'MetaDefs.Name.F4'(B,C,_,_), D, E, F, G, H, I, J, K, L, M) :-
        (   B='"Rationals',
            C='"//',
            D='MetaDefs.Int.F1'(N),
            E='MetaDefs.Int.F1'(O) ->
            'SharedPrograms.RationalToIntDL.P9'(N, O, F, G, I, J, K, L, M)
        ;   I=A,
            'SharedPrograms.TermToIntDL.P9'(D, F, G, H, P, Q, R, S, T),
            'SharedPrograms.TermToIntDL.P9'(E, F, G, H, U, V, W, X, Y),
            'SharedPrograms.ClassifyToken.P2'(C, Z),
            (   'SharedPrograms.RPrec.P2'(P, A1),
                'SharedPrograms.LPrec.P2'(A, B1),
                'SharedPrograms.BindsTighter.P2'(A1, B1) ->
                J=Q,
                L=S,
                'SharedPrograms.SpaceIfNeeded.P4'(R, Z, T, C1)
            ;   J='SharedPrograms.Bounded.C0',
                'SharedPrograms.CharDL.P3'('"(', L, S),
                'SharedPrograms.CharDL.P3'('")', T, C1)
            ),
            'Strings.StringInts.P2'(C, D1),
            'Lists.Append.P3'(D1, E1, C1),
            (   'SharedPrograms.LPrec.P2'(U, F1),
                'SharedPrograms.RPrec.P2'(A, G1),
                'SharedPrograms.BindsTighter.P2'(F1, G1) ->
                K=W,
                'SharedPrograms.SpaceIfNeeded.P4'(Z, V, E1, X),
                M=Y
            ;   K='SharedPrograms.Bounded.C0',
                'SharedPrograms.CharDL.P3'('"(', E1, X),
                'SharedPrograms.CharDL.P3'('")', Y, M)
            )
        ).
'SharedPrograms.BinaryFormulaToIntDL.P6'(A, B, C, D, E, F) :-
        'SharedPrograms.BinaryConnective.P3'(A, G, H),
        'SharedPrograms.FormulaToIntDL.P6'(G, B, C, D, I, J),
        'SharedPrograms.FormulaToIntDL.P6'(H, B, C, D, K, L),
        (   'SharedPrograms.CRPrec.P2'(G, M),
            'SharedPrograms.CLPrec.P2'(A, N),
            'SharedPrograms.BindsTighter.P2'(M, N) ->
            E=I,
            O=J
        ;   'SharedPrograms.CharDL.P3'('"(', E, I),
            'SharedPrograms.CharDL.P3'('")', J, O)
        ),
        'SharedPrograms.BinConnectiveChars.P3'(A, O, P),
        (   'SharedPrograms.CLPrec.P2'(H, Q),
            'SharedPrograms.CRPrec.P2'(A, R),
            'SharedPrograms.BindsTighter.P2'(Q, R) ->
            P=K,
            F=L
        ;   'SharedPrograms.CharDL.P3'('"(', P, K),
            'SharedPrograms.CharDL.P3'('")', L, F)
        ).
'SharedPrograms.CLPrec.P2'('MetaDefs.Empty.C0', 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.PAtom.F1'(_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.Atom.F2'(_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.~''.F1'(_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.&''.F2'(_,_), 'SharedPrograms.Prec.F2'(100,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.\\/''.F2'(_,_), 'SharedPrograms.Prec.F2'(90,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.->''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.<-''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.<->''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.Some.F2'(_,_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.All.F2'(_,_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.Commit.F2'(_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.ISTE.F4'(_,_,_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.IST.F3'(_,_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.ITE.F3'(_,_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.IT.F2'(_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.BindsTighter.P2'('SharedPrograms.Infinity.C0', _).
'SharedPrograms.BindsTighter.P2'('SharedPrograms.Prec.F2'(A,B), 'SharedPrograms.Prec.F2'(C,D)) :-
        call_residue(A=C, E),
        (   E=[] ->
            true
        ;   user:flounder_commit(E)
        ), !,
        B='SharedPrograms.X.C0',
        D='SharedPrograms.Y.C0'.
'SharedPrograms.BindsTighter.P2'('SharedPrograms.Prec.F2'(A,_), 'SharedPrograms.Prec.F2'(B,_)) :-
        call_residue('Integers.>.P2'(A,B), C),
        (   C=[] ->
            true
        ;   user:flounder_commit(C)
        ), !.
'SharedPrograms.CRPrec.P2'('MetaDefs.Empty.C0', 'SharedPrograms.Infinity.C0').
'SharedPrograms.CRPrec.P2'('MetaDefs.PAtom.F1'(_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CRPrec.P2'('MetaDefs.Atom.F2'(_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CRPrec.P2'('MetaDefs.~''.F1'(_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.&''.F2'(_,_), 'SharedPrograms.Prec.F2'(100,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.\\/''.F2'(_,_), 'SharedPrograms.Prec.F2'(90,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.->''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.<-''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.<->''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.X.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.Some.F2'(_,_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.All.F2'(_,_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.Commit.F2'(_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CRPrec.P2'('MetaDefs.ISTE.F4'(_,_,_,_), 'SharedPrograms.Prec.F2'(70,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.IST.F3'(_,_,_), 'SharedPrograms.Prec.F2'(70,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.ITE.F3'(_,_,_), 'SharedPrograms.Prec.F2'(70,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.IT.F2'(_,_), 'SharedPrograms.Prec.F2'(70,'SharedPrograms.Y.C0')).
'SharedPrograms.EmptyCategoryTable.P1'('ProgDefs.Categories.F2'(A,A)) :-
        'AVLTrees.AVLIsEmpty.P1'(A).
'SharedPrograms.ConstantInLanguage.P3'(A, B, C) :-
        'SharedPrograms.SymbolInLanguage.P3'(A, B, 'ProgDefs.ConstantDecl.F1'(C)).
'SharedPrograms.CheckHeadArgs1.P4'([], _, _, []).
'SharedPrograms.CheckHeadArgs1.P4'([A|B], [C|D], [E|F], G) :-
        'SharedPrograms.VariantTypes.P4'(A, C, E, H),
        'Lists.Append.P3'(H, I, G),
        'SharedPrograms.CheckHeadArgs1.P4'(B, D, F, I).
'SharedPrograms.CheckHeadArgs.P4'('MetaDefs.PAtom.F1'(_), _, _, []).
'SharedPrograms.CheckHeadArgs.P4'('MetaDefs.Atom.F2'(A,B), C, D, E) :-
        'SharedPrograms.PredicateInLanguage.P4'(A, D, _, F),
        'SharedPrograms.MeltTypeList.P4'(F, [], _, G),
        'SharedPrograms.CheckHeadArgs1.P4'(G, C, B, E).
'SharedPrograms.ConditionToIntDL.P3'(A, B, C) :-
        (   A='ProgDefs.And.F2'(_,_) ->
            'SharedPrograms.AndSeqToIntDL.P3'(A, B, C)
        ;   A='ProgDefs.Or.F2'(_,_) ->
            'SharedPrograms.OrSeqToIntDL.P3'(A, B, C)
        ;   'SharedPrograms.SimpleCondToIntDL.P3'(A, B, C)
        ).
'SharedPrograms.CookRationalTerm.P2'(A, B) :-
        (   'Integers.<.P2'(A, 0) ->
            negative(A, D),
            C=D,
            B='MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Rationals','"-','MetaDefs.Function.C0',1),['MetaDefs.Int.F1'(C)])
        ;   B='MetaDefs.Int.F1'(A)
        ).
'SharedPrograms.ConstructorInLanguage.P3'(A, B, C) :-
        'SharedPrograms.TypeSymbolInLanguage.P3'(A, B, 'ProgDefs.ConstructorDecl.F1'(C)).
'SharedPrograms.ConstantToIntDL.P4'('MetaDefs.Name.F4'(A,B,C,_), D, E, F) :-
        (   A='"Lists',
            B='"Nil',
            C='MetaDefs.Constant.C0' ->
            D='SharedPrograms.Bounded.C0',
            'SharedPrograms.CharDL.P3'('"[', E, G),
            'SharedPrograms.CharDL.P3'('"]', G, F)
        ;   A='"Sets',
            B='"Null',
            C='MetaDefs.Constant.C0' ->
            D='SharedPrograms.Bounded.C0',
            'SharedPrograms.CharDL.P3'('"{', E, G),
            'SharedPrograms.CharDL.P3'('"}', G, F)
        ;   'SharedPrograms.ClassifyToken.P2'(B, D),
            'Strings.StringInts.P2'(B, H),
            'Lists.Append.P3'(H, F, E)
        ).
'SharedPrograms.ContainsConditional.P1'('MetaDefs.ISTE.F4'(_,_,_,_)).
'SharedPrograms.ContainsConditional.P1'('MetaDefs.IST.F3'(_,_,_)).
'SharedPrograms.ContainsConditional.P1'('MetaDefs.ITE.F3'(_,_,_)).
'SharedPrograms.ContainsConditional.P1'('MetaDefs.IT.F2'(_,_)).
'SharedPrograms.ContainsConditional.P1'('MetaDefs.&''.F2'(A,B)) :-
        user:one_solution(('SharedPrograms.ContainsConditional.P1'(A);'SharedPrograms.ContainsConditional.P1'(B))).
'SharedPrograms.DivideBindings.P4'([], A, A, []).
'SharedPrograms.DivideBindings.P4'([A|B], C, D, E) :-
        'SharedPrograms.IsVariable.P1'(A),
        'SharedPrograms.DivideBindingsAux.P5'(C, A, F, E, G),
        'SharedPrograms.DivideBindings.P4'(B, F, D, G).
'SharedPrograms.DeleteUnderscores.P2'([], []).
'SharedPrograms.DeleteUnderscores.P2'([A|B], C) :-
        (   concat('"_', _, D),
            A='MetaDefs.Var.F2'(D,_) ->
            C=E
        ;   C=[A|E]
        ),
        'SharedPrograms.DeleteUnderscores.P2'(B, E).
'SharedPrograms.DivideBindingsAux.P5'([], _, [], A, A).
'SharedPrograms.DivideBindingsAux.P5'(['MetaDefs.@.F2'(B,C)|A], D, E, F, G) :-
        (   D=B ->
            F=['MetaDefs.@.F2'(B,C)|G],
            E=A
        ;   E=['MetaDefs.@.F2'(B,C)|H],
            'SharedPrograms.DivideBindingsAux.P5'(A, D, H, F, G)
        ).
'SharedPrograms.FormulaInLanguage.P4'(A, B, 'MetaDefs.VarTyping.F1'(C), 'MetaDefs.VarTyping.F1'(D)) :-
        'SharedPrograms.MeltVariableTyping.P4'(C, B, [], E),
        'SharedPrograms.MeltedFormulaTyping.P8'(A, B, E, D, _, [], [], []),
        'SharedPrograms.FixVariableTyping.P2'(D, 0).
'SharedPrograms.ExportLanguage.P3'('ProgDefs.Program.F4'(_,A,'ProgDefs.Language.F1'(B),_), C, 'ProgDefs.Language.F1'(D)) :-
        'SharedPrograms.EmptyLanguage.P1'('ProgDefs.Language.F1'(E)),
        'SharedPrograms.ImportedLanguage.P6'([C], A, B, [], E, D).
'SharedPrograms.ExpandString.P3'(A, B, C) :-
        'SharedPrograms.CharDL.P3'('""', B, D),
        'SharedPrograms.ExpandStringAux.P3'(A, D, C).
'SharedPrograms.EmptyLanguage.P1'(A) :-
        'AVLTrees.AVLIsEmpty.P1'(B),
        'SharedPrograms.InitialiseLanguage.P2'('ProgDefs.Language.F1'(B), A).
'SharedPrograms.ExpandStringAux.P3'([], A, B) :-
        'SharedPrograms.CharDL.P3'('""', A, B).
'SharedPrograms.ExpandStringAux.P3'([A|B], C, D) :-
        (   A=34 ->
            'SharedPrograms.CharDL.P3'('"\\', C, E),
            'SharedPrograms.CharDL.P3'('""', E, F)
        ;   A=92 ->
            'SharedPrograms.CharDL.P3'('"\\', C, E),
            'SharedPrograms.CharDL.P3'('"\\', E, F)
        ;   C=[A|F]
        ),
        'SharedPrograms.ExpandStringAux.P3'(B, F, D).
'SharedPrograms.FixVariableTyping.P2'([], _).
'SharedPrograms.FixVariableTyping.P2'(['MetaDefs.@.F2'(_,B)|A], C) :-
        'SharedPrograms.FixType.P3'(B, C, D),
        'SharedPrograms.FixVariableTyping.P2'(A, D).
'SharedPrograms.FunctionInLanguage.P5'(A, B, C, D, E) :-
        'SharedPrograms.SymbolInLanguage.P3'(A, B, 'ProgDefs.FunctionDecl.F4'(_,C,D,E)).
'SharedPrograms.FormulaToIntList.P4'(A, B, C, D) :-
        'SharedPrograms.ProgramLanguage.P2'(A, E),
        'SharedPrograms.ModuleLanguage.P3'(A, B, F),
        'SharedPrograms.FormulaInLanguage.P4'(C, E, 'MetaDefs.VarTyping.F1'([]), 'MetaDefs.VarTyping.F1'(G)),
        'SharedPrograms.FormulaToIntDL.P6'(C, F, E, G, D, []).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Empty.C0', _, _, _, A, A).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.PAtom.F1'(A), B, _, _, C, D) :-
        'SharedPrograms.PropositionInLanguage.P2'(A, B),
        A='MetaDefs.Name.F4'(_,E,_,_),
        'Strings.StringInts.P2'(E, F),
        'Lists.Append.P3'(F, D, C).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Atom.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.PredicateInLanguage.P4'(A, C, H, _),
        'SharedPrograms.AtomToIntDL.P8'(H, A, B, C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.~''.F1'(A), B, C, D, E, F) :-
        'SharedPrograms.CharDL.P3'('"~', E, G),
        'SharedPrograms.FormulaToIntDL.P6'(A, B, C, D, H, I),
        (   'SharedPrograms.CLPrec.P2'(A, J),
            'SharedPrograms.CRPrec.P2'('MetaDefs.~''.F1'(_), K),
            'SharedPrograms.BindsTighter.P2'(J, K) ->
            G=H,
            F=I
        ;   'SharedPrograms.CharDL.P3'('"(', G, H),
            'SharedPrograms.CharDL.P3'('")', I, F)
        ).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.&''.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.&''.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.\\/''.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.\\/''.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.->''.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.->''.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.<-''.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.<-''.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.<->''.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.<->''.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.All.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.QuantifiedFormulaToIntDL.P6'('MetaDefs.All.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Some.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.QuantifiedFormulaToIntDL.P6'('MetaDefs.Some.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.ISTE.F4'(A,B,C,D), E, F, G, H, I) :-
        'Strings.StringInts.P2'('"IF ', J),
        'Lists.Append.P3'(J, K, H),
        'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Some.F2'(A,B), E, F, G, K, L),
        'Strings.StringInts.P2'('" THEN ', M),
        'Lists.Append.P3'(M, N, L),
        'SharedPrograms.ThenPartToIntDL.P6'(C, E, F, G, N, O),
        'Strings.StringInts.P2'('" ELSE ', P),
        'Lists.Append.P3'(P, Q, O),
        'SharedPrograms.FormulaToIntDL.P6'(D, E, F, G, Q, I).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.ITE.F3'(A,B,C), D, E, F, G, H) :-
        'Strings.StringInts.P2'('"IF ', I),
        'Lists.Append.P3'(I, J, G),
        'SharedPrograms.FormulaToIntDL.P6'(A, D, E, F, K, L),
        (   A='MetaDefs.Some.F2'(_,_) ->
            'SharedPrograms.CharDL.P3'('"(', J, K),
            'SharedPrograms.CharDL.P3'('")', L, M)
        ;   J=K,
            M=L
        ),
        'Strings.StringInts.P2'('" THEN ', N),
        'Lists.Append.P3'(N, O, M),
        'SharedPrograms.ThenPartToIntDL.P6'(B, D, E, F, O, P),
        'Strings.StringInts.P2'('" ELSE ', Q),
        'Lists.Append.P3'(Q, R, P),
        'SharedPrograms.FormulaToIntDL.P6'(C, D, E, F, R, H).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.IST.F3'(A,B,C), D, E, F, G, H) :-
        'Strings.StringInts.P2'('"IF ', I),
        'Lists.Append.P3'(I, J, G),
        'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Some.F2'(A,B), D, E, F, J, K),
        'Strings.StringInts.P2'('" THEN ', L),
        'Lists.Append.P3'(L, M, K),
        'SharedPrograms.ThenPartToIntDL.P6'(C, D, E, F, M, H).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.IT.F2'(A,B), C, D, E, F, G) :-
        'Strings.StringInts.P2'('"IF ', H),
        'Lists.Append.P3'(H, I, F),
        'SharedPrograms.FormulaToIntDL.P6'(A, C, D, E, J, K),
        (   A='MetaDefs.Some.F2'(_,_) ->
            'SharedPrograms.CharDL.P3'('"(', I, J),
            'SharedPrograms.CharDL.P3'('")', K, L)
        ;   I=J,
            L=K
        ),
        'Strings.StringInts.P2'('" THEN ', M),
        'Lists.Append.P3'(M, N, L),
        'SharedPrograms.ThenPartToIntDL.P6'(B, C, D, E, N, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Commit.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('"{', F, H),
        'SharedPrograms.FormulaToIntDL.P6'(B, C, D, E, H, I),
        'SharedPrograms.CharDL.P3'('"}', I, J),
        'SharedPrograms.CharDL.P3'('"_', J, K),
        'SharedSyntax.IntegerToCharDL.P3'(A, K, G).
'SharedPrograms.ImportedLanguage.P6'([], _, _, _, A, A).
'SharedPrograms.ImportedLanguage.P6'([A|B], C, D, E, F, G) :-
        (   'Lists.Member.P2'(A, E) ->
            H=F,
            I=E,
            J=B
        ;   I=[A|E],
            (   'AVLTrees.AVLSearch.P3'(D, A, 'ProgDefs.Module.F3'(_,K,L)) ->
                'AVLTrees.AVLAmend.P6'(F, A, 'ProgDefs.Module.F3'('ProgDefs.Exported.C0',K,[]), _, M, _),
                'SharedPrograms.AddLiftedLanguage.P4'(L, I, M, H)
            ;   'SharedPrograms.EmptyCategoryTable.P1'(N),
                'AVLTrees.AVLAmend.P6'(F, A, 'ProgDefs.Module.F3'('ProgDefs.Exported.C0',N,[]), _, H, _)
            ),
            'AVLTrees.AVLSearch.P3'(C, A, 'ProgDefs.ModDef.F4'(_,O,_,_)),
            'Lists.Append.P3'(O, B, J)
        ),
        'SharedPrograms.ImportedLanguage.P6'(J, C, D, I, H, G).
'SharedPrograms.HeadAtom.P9'('MetaDefs.PAtom.F1'(A), B, C, C, 'MetaDefs.PAtom.F1'(A), D, D, [], []) :-
        'SharedPrograms.PropositionInLanguage.P2'(A, B).
'SharedPrograms.HeadAtom.P9'('MetaDefs.Atom.F2'(A,B), C, D, E, 'MetaDefs.Atom.F2'(A,F), G, H, I, J) :-
        'SharedPrograms.PredicateInLanguage.P4'(A, C, _, K),
        'SharedPrograms.MeltTypeList.P4'(K, [], _, I),
        'SharedPrograms.MeltedArgumentTyping.P9'(I, B, C, D, E, F, G, H, J).
'SharedPrograms.MatchingDescriptor.P5'('ProgDefs.Symbol.F2'(A,B), C, D, E, B) :-
        'SharedPrograms.Accessible.P2'(C, A),
        'SharedPrograms.MatchingName.P3'(B, D, E).
'SharedPrograms.KindHasPart.P2'('ProgDefs.ModuleKind.C0', 'ProgDefs.Module.C0').
'SharedPrograms.KindHasPart.P2'('ProgDefs.NormalKind.C0', A) :-
        'SharedPrograms.FindMember.P2'(A, ['ProgDefs.Export.C0','ProgDefs.Local.C0']).
'SharedPrograms.KindHasPart.P2'('ProgDefs.ClosedKind.C0', 'ProgDefs.Closed.C0').
'SharedPrograms.InsertTypeSymbol.P5'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G, 'ProgDefs.Language.F1'(H)) :-
        'AVLTrees.AVLIsEmpty.P1'(I),
        'AVLTrees.AVLIsEmpty.P1'(J),
        'AVLTrees.AVLAmend.P6'(E, A, 'ProgDefs.Module.F3'(K,'ProgDefs.Categories.F2'(L,M),N), 'ProgDefs.Module.F3'('ProgDefs.Hidden.C0','ProgDefs.Categories.F2'(I,J),[]), H, 'ProgDefs.Module.F3'(K,'ProgDefs.Categories.F2'(O,M),N)),
        'AVLTrees.AVLAmend.P6'(O, B, P, [], L, Q),
        'SharedPrograms.MatchingName.P3'(G, C, D),
        'SharedPrograms.AddToDescriptorList.P6'(Q, K, C, D, 'ProgDefs.Symbol.F2'(F,G), P).
'SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G, 'ProgDefs.Language.F1'(H)) :-
        'AVLTrees.AVLIsEmpty.P1'(I),
        'AVLTrees.AVLIsEmpty.P1'(J),
        'AVLTrees.AVLAmend.P6'(E, A, 'ProgDefs.Module.F3'(K,'ProgDefs.Categories.F2'(L,M),N), 'ProgDefs.Module.F3'('ProgDefs.Hidden.C0','ProgDefs.Categories.F2'(I,J),[]), H, 'ProgDefs.Module.F3'(K,'ProgDefs.Categories.F2'(L,O),N)),
        'AVLTrees.AVLAmend.P6'(O, B, P, [], M, Q),
        'SharedPrograms.MatchingName.P3'(G, C, D),
        'SharedPrograms.AddToDescriptorList.P6'(Q, K, C, D, 'ProgDefs.Symbol.F2'(F,G), P).
'SharedPrograms.IsVariable.P1'('MetaDefs.Var.F2'(_,_)).
'SharedPrograms.IsVariable.P1'('MetaDefs.Var.F1'(_)).
'SharedPrograms.LookupParDict.P4'(A, B, C, D) :-
        'SharedPrograms.LookupParDictAux.P5'(A, B, A, C, D).
'SharedPrograms.ListExprToIntDL.P6'([A,B], C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('"[', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'SharedPrograms.ChainExprToIntDL.P6'(B, C, D, E, I, J),
        'SharedPrograms.CharDL.P3'('"]', J, G).
'SharedPrograms.LPrec.P2'('Syntax.NoFunctInd.C0', 'SharedPrograms.Infinity.C0').
'SharedPrograms.LPrec.P2'('Syntax.XFX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.LPrec.P2'('Syntax.XFY.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.LPrec.P2'('Syntax.YFX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.Y.C0')).
'SharedPrograms.LPrec.P2'('Syntax.FX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.LPrec.P2'('Syntax.FY.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.LPrec.P2'('Syntax.XF.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.LPrec.P2'('Syntax.YF.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.Y.C0')).
'SharedPrograms.ListTypesInLanguage.P2'([], _).
'SharedPrograms.ListTypesInLanguage.P2'([A|B], C) :-
        'SharedPrograms.TypeInLanguage.P2'(A, C),
        'SharedPrograms.ListTypesInLanguage.P2'(B, C).
'SharedPrograms.LookupVarTyping.P4'(A, B, C, D) :-
        'SharedPrograms.LookupVarTypingAux.P5'(A, B, A, C, D).
'SharedPrograms.LookupParDictAux.P5'([], A, B, ['MetaDefs.!.F2'(A,C)|B], C).
'SharedPrograms.LookupParDictAux.P5'(['MetaDefs.!.F2'(B,C)|A], D, E, F, G) :-
        (   D=B ->
            G=C,
            F=E
        ;   'SharedPrograms.LookupParDictAux.P5'(A, D, E, F, G)
        ).
'SharedPrograms.LookupVarTypingAux.P5'([], A, B, ['MetaDefs.@.F2'(A,C)|B], C).
'SharedPrograms.LookupVarTypingAux.P5'(['MetaDefs.@.F2'(B,C)|A], D, E, F, G) :-
        (   D=B ->
            G=C,
            F=E
        ;   'SharedPrograms.LookupVarTypingAux.P5'(A, D, E, F, G)
        ).
'SharedPrograms.MeltedFormulaTyping.P8'(A, B, C, D, E, F, G, H) :-
        (   A='MetaDefs.<-''.F2'(I,J) ->
            'SharedPrograms.MeltedBodyTyping.P8'(I, B, C, K, L, F, M, N),
            (   N=[] ->
                'SharedPrograms.MeltedBodyTyping.P8'(J, B, K, D, O, M, G, H),
                E='MetaDefs.<-''.F2'(L,O)
            ;   H=N
            )
        ;   'SharedPrograms.MeltedBodyTyping.P8'(A, B, C, D, E, F, G, H)
        ).
'SharedPrograms.MeltVariableTyping.P4'([], _, _, []).
'SharedPrograms.MeltVariableTyping.P4'(['MetaDefs.@.F2'(B,C)|A], D, E, ['MetaDefs.@.F2'(B,G)|F]) :-
        'SharedPrograms.IsVariable.P1'(B),
        'SharedPrograms.TypeInLanguage.P2'(C, D),
        'SharedPrograms.MeltType.P4'(C, E, H, G),
        'SharedPrograms.MeltVariableTyping.P4'(A, D, H, F).
'SharedPrograms.MeltType.P4'('MetaDefs.Par.F2'(A,B), C, D, E) :-
        'SharedPrograms.LookupParDict.P4'(C, 'MetaDefs.Par.F2'(A,B), D, E).
'SharedPrograms.MeltType.P4'('MetaDefs.Par.F1'(A), B, C, D) :-
        'SharedPrograms.LookupParDict.P4'(B, 'MetaDefs.Par.F1'(A), C, D).
'SharedPrograms.MeltType.P4'('MetaDefs.BType.F1'(A), B, B, 'MetaDefs.BType.F1'(A)).
'SharedPrograms.MeltType.P4'('MetaDefs.XBType.F1'(A), B, B, 'MetaDefs.XBType.F1'(A)).
'SharedPrograms.MeltType.P4'('MetaDefs.Type.F2'(A,B), C, D, 'MetaDefs.Type.F2'(A,E)) :-
        'SharedPrograms.MeltTypeList.P4'(B, C, D, E).
'SharedPrograms.MeltType.P4'('MetaDefs.XType.F2'(A,B), C, D, 'MetaDefs.XType.F2'(A,E)) :-
        'SharedPrograms.MeltTypeList.P4'(B, C, D, E).
'SharedPrograms.MatchingName.P3'('ProgDefs.BaseDecl.C0', 'MetaDefs.Base.C0', 0).
'SharedPrograms.MatchingName.P3'('ProgDefs.ConstructorDecl.F1'(A), 'MetaDefs.Constructor.C0', A).
'SharedPrograms.MatchingName.P3'('ProgDefs.ConstantDecl.F1'(_), 'MetaDefs.Constant.C0', 0).
'SharedPrograms.MatchingName.P3'('ProgDefs.FunctionDecl.F4'(A,_,_,_), 'MetaDefs.Function.C0', A).
'SharedPrograms.MatchingName.P3'('ProgDefs.PropositionDecl.C0', 'MetaDefs.Proposition.C0', 0).
'SharedPrograms.MatchingName.P3'('ProgDefs.PredicateDecl.F3'(A,_,_), 'MetaDefs.Predicate.C0', A).
'SharedPrograms.MeltTypeList.P4'([], A, A, []).
'SharedPrograms.MeltTypeList.P4'([A|B], C, D, [E|F]) :-
        'SharedPrograms.MeltType.P4'(A, C, G, E),
        'SharedPrograms.MeltTypeList.P4'(B, G, D, F).
'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.PAtom.F1'(A), B, C, C, 'MetaDefs.PAtom.F1'(A), D, D, []) :-
        'SharedPrograms.PropositionInLanguage.P2'(A, B).
'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.XPAtom.F1'(A), B, C, C, 'MetaDefs.XPAtom.F1'(A), D, D, []) :-
        'SharedPrograms.PropositionInLanguage.P2'(A, B).
'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.Atom.F2'(A,B), C, D, E, 'MetaDefs.Atom.F2'(A,F), G, H, I) :-
        'SharedPrograms.PredicateInLanguage.P4'(A, C, _, J),
        'SharedPrograms.MeltTypeList.P4'(J, [], _, K),
        'SharedPrograms.MeltedArgumentTyping.P9'(K, B, C, D, E, F, G, H, I).
'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.XAtom.F2'(A,B), C, D, E, 'MetaDefs.XAtom.F2'(A,F), G, H, I) :-
        'SharedPrograms.PredicateInLanguage.P4'(A, C, _, J),
        'SharedPrograms.MeltTypeList.P4'(J, [], _, K),
        'SharedPrograms.MeltedArgumentTyping.P9'(K, B, C, D, E, F, G, H, I).
'SharedPrograms.MeltedArgumentTyping.P9'([], [], _, A, A, [], B, B, []).
'SharedPrograms.MeltedArgumentTyping.P9'([A|B], [C|D], E, F, G, [H|I], J, K, L) :-
        'SharedPrograms.MeltedTermType.P9'(C, A, E, F, M, H, J, N, O),
        (   O=[] ->
            'SharedPrograms.MeltedArgumentTyping.P9'(B, D, E, M, G, I, N, K, L)
        ;   L=O
        ).
'SharedPrograms.MeltedBodyTyping.P8'(A, B, C, D, E, F, G, H) :-
        (   A='MetaDefs.Empty.C0' ->
            D=C,
            F=G,
            H=[],
            E='MetaDefs.Empty.C0'
        ;   A='MetaDefs.&''.F2'(I,J) ->
            'SharedPrograms.MeltedBodyTyping.P8'(I, B, C, K, L, F, M, N),
            (   N=[] ->
                'SharedPrograms.MeltedBodyTyping.P8'(J, B, K, D, O, M, G, H),
                E='MetaDefs.&''.F2'(L,O)
            ;   H=N
            )
        ;   A='MetaDefs.Commit.F2'(P,Q) ->
            'SharedPrograms.MeltedBodyTyping.P8'(Q, B, C, D, L, F, G, H),
            E='MetaDefs.Commit.F2'(P,L)
        ;   'SharedPrograms.MeltedStandardTyping.P8'(A, B, C, D, E, F, G, H)
        ).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Var.F2'(A,B), C, _, D, E, 'MetaDefs.Var.F2'(A,B), F, F, G) :-
        'SharedPrograms.LookupVarTyping.P4'(D, 'MetaDefs.Var.F2'(A,B), E, H),
        'SharedPrograms.UnifyMeltedTypes.P4'(C, H, 'MetaDefs.Var.F2'(A,B), G).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Var.F1'(A), B, _, C, D, 'MetaDefs.Var.F1'(A), E, E, F) :-
        'SharedPrograms.LookupVarTyping.P4'(C, 'MetaDefs.Var.F1'(A), D, G),
        'SharedPrograms.UnifyMeltedTypes.P4'(B, G, 'MetaDefs.Var.F1'(A), F).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.CTerm.F1'(A), B, C, D, D, 'MetaDefs.CTerm.F1'(A), E, E, F) :-
        'SharedPrograms.ConstantInLanguage.P3'(A, C, G),
        'SharedPrograms.MeltType.P4'(G, [], _, H),
        'SharedPrograms.UnifyMeltedTypes.P4'(B, H, 'MetaDefs.CTerm.F1'(A), F).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.XCTerm.F2'(A,_), B, C, D, D, 'MetaDefs.XCTerm.F2'(A,E), F, F, G) :-
        'SharedPrograms.ConstantInLanguage.P3'(A, C, H),
        'SharedPrograms.MeltType.P4'(H, [], _, E),
        'SharedPrograms.UnifyMeltedTypes.P4'(B, E, 'MetaDefs.CTerm.F1'(A), G).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Term.F2'(A,B), C, D, E, F, 'MetaDefs.Term.F2'(A,G), H, I, J) :-
        'SharedPrograms.FunctionInLanguage.P5'(A, D, _, K, L),
        'SharedPrograms.MeltType.P4'(L, [], M, N),
        'SharedPrograms.MeltTypeList.P4'(K, M, _, O),
        'SharedPrograms.UnifyMeltedTypes.P4'(C, N, 'MetaDefs.Term.F2'(A,B), P),
        (   P=[] ->
            'SharedPrograms.MeltedArgumentTyping.P9'(O, B, D, E, F, G, H, I, J)
        ;   J=P
        ).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.XTerm.F3'(A,B,_), C, D, E, F, 'MetaDefs.XTerm.F3'(A,G,H), I, J, K) :-
        'SharedPrograms.FunctionInLanguage.P5'(A, D, _, L, M),
        'SharedPrograms.MeltType.P4'(M, [], N, H),
        'SharedPrograms.MeltTypeList.P4'(L, N, _, O),
        'SharedPrograms.UnifyMeltedTypes.P4'(C, H, 'MetaDefs.Term.F2'(A,B), P),
        (   P=[] ->
            'SharedPrograms.MeltedArgumentTyping.P9'(O, B, D, E, F, G, I, J, K)
        ;   K=P
        ).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Int.F1'(A), B, C, D, D, 'MetaDefs.Int.F1'(A), E, E, F) :-
        G='MetaDefs.Name.F4'('"Integers','"Integer','MetaDefs.Base.C0',0),
        'SharedPrograms.BaseInLanguage.P2'(G, C),
        'SharedPrograms.UnifyMeltedTypes.P4'(B, 'MetaDefs.BType.F1'(G), 'MetaDefs.Int.F1'(A), F).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Str.F1'(A), B, C, D, D, 'MetaDefs.Str.F1'(A), E, E, F) :-
        G='MetaDefs.Name.F4'('"Strings','"String','MetaDefs.Base.C0',0),
        'SharedPrograms.BaseInLanguage.P2'(G, C),
        'SharedPrograms.UnifyMeltedTypes.P4'(B, 'MetaDefs.BType.F1'(G), 'MetaDefs.Str.F1'(A), F).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Prm.F1'(A), B, C, D, D, 'MetaDefs.Prm.F1'(A), E, E, F) :-
        G='MetaDefs.Name.F4'('"ProgDefs','"Program','MetaDefs.Base.C0',0),
        'SharedPrograms.BaseInLanguage.P2'(G, C),
        'SharedPrograms.UnifyMeltedTypes.P4'(B, 'MetaDefs.BType.F1'(G), 'MetaDefs.Prm.F1'(A), F).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.SuchThat.F2'(A,B), C, D, E, F, 'MetaDefs.SuchThat.F2'(G,H), I, J, K) :-
        L='MetaDefs.Name.F4'('"Sets','"Set','MetaDefs.Constructor.C0',1),
        'SharedPrograms.ConstructorInLanguage.P3'(L, D, _),
        'SharedPrograms.UnifyMeltedTypes.P4'(C, 'MetaDefs.Type.F2'(L,[M]), 'MetaDefs.SuchThat.F2'(A,B), N),
        (   N=[] ->
            'SharedSyntax.STermFreeVars.P2'(A, O),
            'SharedPrograms.DivideBindings.P4'(O, E, P, Q),
            'SharedPrograms.MeltedStandardTyping.P8'(B, D, P, R, H, I, S, T),
            (   T=[] ->
                'SharedPrograms.MeltedTermType.P9'(A, M, D, R, U, G, S, J, K),
                (   K=[] ->
                    'SharedPrograms.DivideBindings.P4'(O, U, V, _),
                    'Lists.Append.P3'(Q, V, F)
                ;   true
                )
            ;   K=T
            )
        ;   K=N
        ).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Num.F1'(A), B, C, D, D, E, F, G, H) :-
        (   'SharedPrograms.BaseInLanguage.P2'('MetaDefs.Name.F4'('"Rationals','"Rational','MetaDefs.Base.C0',0), C) ->
            F=['SharedPrograms.NumType.F3'(A,'MetaDefs.BType.F1'(I),E)|G],
            'SharedPrograms.UnifyMeltedTypes.P4'(B, 'MetaDefs.BType.F1'(I), 'MetaDefs.Num.F1'(A), H)
        ;   'SharedPrograms.MeltedTermType.P9'('MetaDefs.Int.F1'(A), B, C, D, _, E, F, G, H)
        ).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.ISTE.F4'(A,B,C,D), E, F, G, 'MetaDefs.ISTE.F4'(A,H,I,J), K, L, M) :-
        'SharedPrograms.DivideBindings.P4'(A, F, N, O),
        'SharedPrograms.MeltedStandardTyping.P8'(B, E, N, P, H, K, Q, R),
        (   R=[] ->
            'SharedPrograms.MeltedStandardTyping.P8'(C, E, P, S, I, Q, T, U),
            (   U=[] ->
                'SharedPrograms.DivideBindings.P4'(A, S, V, _),
                'Lists.Append.P3'(O, V, W),
                'SharedPrograms.MeltedStandardTyping.P8'(D, E, W, G, J, T, L, M)
            ;   M=U
            )
        ;   M=R
        ).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.IST.F3'(A,B,C), D, E, F, 'MetaDefs.IST.F3'(A,G,H), I, J, K) :-
        'SharedPrograms.DivideBindings.P4'(A, E, L, M),
        'SharedPrograms.MeltedStandardTyping.P8'(B, D, L, N, G, I, O, P),
        (   P=[] ->
            'SharedPrograms.MeltedStandardTyping.P8'(C, D, N, Q, H, O, J, K),
            (   K=[] ->
                'SharedPrograms.DivideBindings.P4'(A, Q, R, _),
                'Lists.Append.P3'(M, R, F)
            ;   true
            )
        ;   K=P
        ).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.ITE.F3'(A,B,C), D, E, F, 'MetaDefs.ITE.F3'(G,H,I), J, K, L) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, D, E, M, G, J, N, O),
        (   O=[] ->
            'SharedPrograms.MeltedStandardTyping.P8'(B, D, M, P, H, N, Q, R),
            (   R=[] ->
                'SharedPrograms.MeltedStandardTyping.P8'(C, D, P, F, I, Q, K, L)
            ;   L=R
            )
        ;   L=O
        ).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.IT.F2'(A,B), C, D, E, 'MetaDefs.IT.F2'(F,G), H, I, J) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        (   M=[] ->
            'SharedPrograms.MeltedStandardTyping.P8'(B, C, K, E, G, L, I, J)
        ;   J=M
        ).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.&''.F2'(A,B), C, D, E, 'MetaDefs.&''.F2'(F,G), H, I, J) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        (   M=[] ->
            'SharedPrograms.MeltedStandardTyping.P8'(B, C, K, E, G, L, I, J)
        ;   J=M
        ).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.\\/''.F2'(A,B), C, D, E, 'MetaDefs.\\/''.F2'(F,G), H, I, J) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        (   M=[] ->
            'SharedPrograms.MeltedStandardTyping.P8'(B, C, K, E, G, L, I, J)
        ;   J=M
        ).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.->''.F2'(A,B), C, D, E, 'MetaDefs.->''.F2'(F,G), H, I, J) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        (   M=[] ->
            'SharedPrograms.MeltedStandardTyping.P8'(B, C, K, E, G, L, I, J)
        ;   J=M
        ).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.<-''.F2'(A,B), C, D, E, 'MetaDefs.<-''.F2'(F,G), H, I, J) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        (   M=[] ->
            'SharedPrograms.MeltedStandardTyping.P8'(B, C, K, E, G, L, I, J)
        ;   J=M
        ).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.<->''.F2'(A,B), C, D, E, 'MetaDefs.<->''.F2'(F,G), H, I, J) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        (   M=[] ->
            'SharedPrograms.MeltedStandardTyping.P8'(B, C, K, E, G, L, I, J)
        ;   J=M
        ).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.~''.F1'(A), B, C, D, 'MetaDefs.~''.F1'(E), F, G, H) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, B, C, D, E, F, G, H).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.All.F2'(A,B), C, D, E, 'MetaDefs.All.F2'(A,F), G, H, I) :-
        'SharedPrograms.DivideBindings.P4'(A, D, J, K),
        'SharedPrograms.MeltedStandardTyping.P8'(B, C, J, L, F, G, H, I),
        'SharedPrograms.DivideBindings.P4'(A, L, M, _),
        'Lists.Append.P3'(K, M, E).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.Some.F2'(A,B), C, D, E, 'MetaDefs.Some.F2'(A,F), G, H, I) :-
        'SharedPrograms.DivideBindings.P4'(A, D, J, K),
        'SharedPrograms.MeltedStandardTyping.P8'(B, C, J, L, F, G, H, I),
        'SharedPrograms.DivideBindings.P4'(A, L, M, _),
        'Lists.Append.P3'(K, M, E).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.PAtom.F1'(A), B, C, D, E, F, G, H) :-
        'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.PAtom.F1'(A), B, C, D, E, F, G, H).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.Atom.F2'(A,B), C, D, E, F, G, H, I) :-
        'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.Atom.F2'(A,B), C, D, E, F, G, H, I).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.XPAtom.F1'(A), B, C, D, E, F, G, H) :-
        'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.XPAtom.F1'(A), B, C, D, E, F, G, H).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.XAtom.F2'(A,B), C, D, E, F, G, H, I) :-
        'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.XAtom.F2'(A,B), C, D, E, F, G, H, I).
'SharedPrograms.ModuleLanguage.P3'('ProgDefs.Program.F4'(_,A,'ProgDefs.Language.F1'(B),_), C, 'ProgDefs.Language.F1'(D)) :-
        'SharedPrograms.EmptyLanguage.P1'('ProgDefs.Language.F1'(E)),
        'AVLTrees.AVLSearch.P3'(A, C, 'ProgDefs.ModDef.F4'(F,G,H,I)),
        (   F='ProgDefs.ClosedKind.C0' ->
            'SharedPrograms.ImportedLanguage.P6'([C], A, B, [], E, D)
        ;   (   'AVLTrees.AVLSearch.P3'(B, C, J) ->
                'AVLTrees.AVLInsert.P4'(E, C, J, K)
            ;   K=E
            ),
            'Lists.Append.P3'(G, H, L),
            'Lists.Append.P3'(I, L, M),
            'SharedPrograms.ImportedLanguage.P6'(M, A, B, [C], K, D)
        ).
'SharedPrograms.SymbolInLanguage.P3'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F) :-
        'SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(G,'ProgDefs.Categories.F2'(_,H),_)),
        'SharedPrograms.AVLFind.P3'(H, B, I),
        'SharedPrograms.FindDescriptor.P5'(I, G, C, D, F).
'SharedPrograms.SControlInProgram.P5'('ProgDefs.Program.F4'(_,A,'ProgDefs.Language.F1'(B),C), D, E, F, G) :-
        E='MetaDefs.Name.F4'(D,H,_,I),
        'SharedPrograms.AVLFind.P3'(B, D, 'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(_,J),_)),
        'SharedPrograms.AVLFind.P3'(J, H, K),
        (   D='"' ->
            L='ProgDefs.ClosedKind.C0',
            M='ProgDefs.Exported.C0'
        ;   'AVLTrees.AVLSearch.P3'(A, D, 'ProgDefs.ModDef.F4'(L,_,_,_)),
            (   L='ProgDefs.ClosedKind.C0' ->
                M='ProgDefs.Exported.C0'
            ;   M='ProgDefs.Hidden.C0'
            )
        ),
        'SharedPrograms.FindDescriptor.P5'(K, M, _, I, 'ProgDefs.PredicateDecl.F3'(_,_,_)),
        (   'AVLTrees.AVLSearch.P3'(C, D, 'ProgDefs.Code.F2'(_,N)),
            'AVLTrees.AVLSearch.P3'(N, H, O),
            'SharedPrograms.FindPredDef.P3'(I, O, 'ProgDefs.PredDef.F4'(I,_,P,Q)) ->
            (   L='ProgDefs.ClosedKind.C0' ->
                R=P
            ;   'Lists.Append.P3'(P, Q, R)
            ),
            'SharedPrograms.ReformatDelays.P3'(R, F, G)
        ;   F=[],
            G=[]
        ).
'SharedPrograms.ProgramSkeleton.P4'(A, B, C, 'ProgDefs.Program.F4'(A,D,E,F)) :-
        'AVLTrees.AVLIsEmpty.P1'(G),
        'AVLTrees.AVLIsEmpty.P1'(H),
        'AVLTrees.AVLIsEmpty.P1'(I),
        'AVLTrees.AVLInsert.P4'(G, A, 'ProgDefs.ModDef.F4'(B,[],[],[]), D),
        'AVLTrees.AVLInsert.P4'(H, A, 'ProgDefs.Code.F2'(C,I), F),
        'SharedPrograms.EmptyLanguage.P1'(E).
'SharedPrograms.OpenModuleAux.P2'(A, B) :-
        'SharedPrograms.AVLFind.P3'(A, B, 'ProgDefs.ModDef.F4'(C,_,_,_)),
        'SharedPrograms.OpenKind.P1'(C).
'SharedPrograms.OpenKind.P1'('ProgDefs.NormalKind.C0').
'SharedPrograms.OpenKind.P1'('ProgDefs.ModuleKind.C0').
'SharedPrograms.MustBracketThen.P1'('MetaDefs.ITE.F3'(_,_,_)).
'SharedPrograms.MustBracketThen.P1'('MetaDefs.ISTE.F4'(_,_,_,_)).
'SharedPrograms.MustBracketThen.P1'('MetaDefs.&''.F2'(A,B)) :-
        'SharedPrograms.ContainsConditional.P1'('MetaDefs.&''.F2'(A,B)).
'SharedPrograms.PredicateInLanguage.P4'(A, B, C, D) :-
        'SharedPrograms.SymbolInLanguage.P3'(A, B, 'ProgDefs.PredicateDecl.F3'(_,C,D)).
'SharedPrograms.OrSeqToIntDL.P3'('ProgDefs.Or.F2'(A,B), C, D) :-
        'SharedPrograms.SimpleCondToIntDL.P3'(A, C, E),
        'Strings.StringInts.P2'('" \\/ ', F),
        'Lists.Append.P3'(F, G, E),
        (   B='ProgDefs.Or.F2'(_,_) ->
            'SharedPrograms.OrSeqToIntDL.P3'(B, G, D)
        ;   'SharedPrograms.SimpleCondToIntDL.P3'(B, G, D)
        ).
'SharedPrograms.ProgramLanguage.P2'('ProgDefs.Program.F4'(_,_,A,_), A).
'SharedPrograms.RationalTerm.P2'(A, 'MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Rationals','"//','MetaDefs.Function.C0',2),['MetaDefs.Int.F1'(A),'MetaDefs.Int.F1'(1)])).
'SharedPrograms.QuantifiedFormulaToIntDL.P6'(A, B, C, D, E, F) :-
        'SharedPrograms.QuantifiedFormula.P3'(A, G, H),
        'SharedPrograms.DeleteUnderscores.P2'(G, I),
        (   I=[] ->
            'SharedPrograms.FormulaToIntDL.P6'(H, B, C, D, E, F)
        ;   'SharedPrograms.QuantifierChars.P3'(A, E, J),
            'SharedPrograms.VarListToIntDL.P6'(G, B, C, D, J, K),
            'SharedPrograms.CharDL.P3'('" ', K, L),
            'SharedPrograms.FormulaToIntDL.P6'(H, B, C, D, M, N),
            (   'SharedPrograms.CLPrec.P2'(H, O),
                'SharedPrograms.CRPrec.P2'(A, P),
                'SharedPrograms.BindsTighter.P2'(O, P) ->
                L=M,
                F=N
            ;   'SharedPrograms.CharDL.P3'('"(', L, M),
                'SharedPrograms.CharDL.P3'('")', N, F)
            )
        ).
'SharedPrograms.PropositionInLanguage.P2'(A, B) :-
        'SharedPrograms.SymbolInLanguage.P3'(A, B, 'ProgDefs.PropositionDecl.C0').
'SharedPrograms.QuantifiedFormula.P3'('MetaDefs.Some.F2'(A,B), A, B).
'SharedPrograms.QuantifiedFormula.P3'('MetaDefs.All.F2'(A,B), A, B).
'SharedPrograms.RPrec.P2'('Syntax.NoFunctInd.C0', 'SharedPrograms.Infinity.C0').
'SharedPrograms.RPrec.P2'('Syntax.XFX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.RPrec.P2'('Syntax.XFY.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.Y.C0')).
'SharedPrograms.RPrec.P2'('Syntax.YFX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.RPrec.P2'('Syntax.FX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.RPrec.P2'('Syntax.FY.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.Y.C0')).
'SharedPrograms.RPrec.P2'('Syntax.XF.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.RPrec.P2'('Syntax.YF.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.QuantifierChars.P3'('MetaDefs.Some.F2'(_,_), A, B) :-
        'Strings.StringInts.P2'('"SOME ', C),
        'Lists.Append.P3'(C, B, A).
'SharedPrograms.QuantifierChars.P3'('MetaDefs.All.F2'(_,_), A, B) :-
        'Strings.StringInts.P2'('"ALL ', C),
        'Lists.Append.P3'(C, B, A).
'SharedPrograms.ReformatDelays.P3'([], [], []).
'SharedPrograms.ReformatDelays.P3'(['ProgDefs.Delay.F2'(B,C)|A], [B|D], [C|E]) :-
        'SharedPrograms.ReformatDelays.P3'(A, D, E).
'SharedPrograms.RationalToIntDL.P9'(A, B, C, D, E, F, G, H, I) :-
        'SharedPrograms.CookRationalTerm.P2'(A, J),
        (   B=1 ->
            'SharedPrograms.TermToIntDL.P9'(J, C, D, [], E, F, G, H, I)
        ;   'SharedPrograms.CookRationalTerm.P2'(B, K),
            'SharedPrograms.TermToIntDL.P9'('MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Rationals','"/','MetaDefs.Function.C0',2),[J,K]), C, D, [], E, F, G, H, I)
        ).
'SharedPrograms.RestVarListToIntDL.P6'([], _, _, _, A, A).
'SharedPrograms.RestVarListToIntDL.P6'([A|B], C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('",', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'SharedPrograms.RestVarListToIntDL.P6'(B, C, D, E, I, G).
'SharedPrograms.SConditionToString.P2'(A, B) :-
        'SharedPrograms.ConditionToIntDL.P3'(A, C, []),
        'Strings.StringInts.P2'(B, C).
'SharedPrograms.SProgramTypeToString.P4'(A, B, C, D) :-
        'SharedPrograms.ModuleLanguage.P3'(A, B, E),
        'SharedPrograms.TypeToIntDL.P4'(C, E, F, []),
        'Strings.StringInts.P2'(D, F).
'SharedPrograms.SOpenModule.P2'('ProgDefs.Program.F4'(_,A,_,_), B) :-
        'SharedPrograms.OpenModuleAux.P2'(A, B).
'SharedPrograms.SMainModuleInProgram.P2'('ProgDefs.Program.F4'(A,_,_,_), A).
'SharedPrograms.SProgramTermToString.P4'(A, B, C, D) :-
        'SharedPrograms.TermToIntList.P4'(A, B, C, E),
        'Strings.StringInts.P2'(D, E).
'SharedPrograms.SProgramFormulaToString.P4'(A, B, C, D) :-
        'SharedPrograms.FormulaToIntList.P4'(A, B, C, E),
        'Strings.StringInts.P2'(D, E).
'SharedPrograms.SpaceIfNeeded.P4'(A, B, C, D) :-
        (   A=B,
            user:not_equal([], [A], A, 'SharedPrograms.Bounded.C0') ->
            'SharedPrograms.CharDL.P3'('" ', C, D)
        ;   C=D
        ).
'SharedPrograms.SetExprToIntDL.P6'([A,B], C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('"{', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'SharedPrograms.ChainExprToIntDL.P6'(B, C, D, E, I, J),
        'SharedPrograms.CharDL.P3'('"}', J, G).
'SharedPrograms.SimpleCondToIntDL.P3'('ProgDefs.Nonvar.F1'(A), B, C) :-
        'Strings.StringInts.P2'('"NONVAR(', D),
        'Lists.Append.P3'(D, E, B),
        'SharedPrograms.TermToIntDL.P9'(A, _, _, _, _, _, _, E, F),
        'SharedPrograms.CharDL.P3'('")', F, C).
'SharedPrograms.SimpleCondToIntDL.P3'('ProgDefs.Ground.F1'(A), B, C) :-
        'Strings.StringInts.P2'('"GROUND(', D),
        'Lists.Append.P3'(D, E, B),
        'SharedPrograms.TermToIntDL.P9'(A, _, _, _, _, _, _, E, F),
        'SharedPrograms.CharDL.P3'('")', F, C).
'SharedPrograms.SimpleCondToIntDL.P3'('ProgDefs.And.F2'(A,B), C, D) :-
        'SharedPrograms.CharDL.P3'('"(', C, E),
        'SharedPrograms.AndSeqToIntDL.P3'('ProgDefs.And.F2'(A,B), E, F),
        'SharedPrograms.CharDL.P3'('")', F, D).
'SharedPrograms.SimpleCondToIntDL.P3'('ProgDefs.Or.F2'(A,B), C, D) :-
        'SharedPrograms.CharDL.P3'('"(', C, E),
        'SharedPrograms.OrSeqToIntDL.P3'('ProgDefs.Or.F2'(A,B), E, F),
        'SharedPrograms.CharDL.P3'('")', F, D).
'SharedPrograms.StatementInLanguage.P5'('MetaDefs.<-''.F2'(A,B), C, 'MetaDefs.<-''.F2'(D,E), F, G) :-
        'SharedPrograms.MeltedBodyTyping.P8'(B, C, [], H, E, I, J, K),
        (   K=[] ->
            'SharedPrograms.HeadAtom.P9'(A, C, H, _, D, J, [], L, M),
            (   M=[] ->
                'SharedPrograms.FixRationals.P2'(I, F),
                'SharedPrograms.CheckHeadArgs.P4'(D, L, C, G)
            ;   G=M
            )
        ;   G=K
        ).
'SharedPrograms.TypeInLanguage.P2'('MetaDefs.Par.F2'(_,_), _).
'SharedPrograms.TypeInLanguage.P2'('MetaDefs.Par.F1'(_), _).
'SharedPrograms.TypeInLanguage.P2'('MetaDefs.BType.F1'(A), B) :-
        'SharedPrograms.BaseInLanguage.P2'(A, B).
'SharedPrograms.TypeInLanguage.P2'('MetaDefs.XBType.F1'(A), B) :-
        'SharedPrograms.BaseInLanguage.P2'(A, B).
'SharedPrograms.TypeInLanguage.P2'('MetaDefs.Type.F2'(A,B), C) :-
        'SharedPrograms.ConstructorInLanguage.P3'(A, C, D),
        'Lists.Length.P2'(B, D),
        'SharedPrograms.ListTypesInLanguage.P2'(B, C).
'SharedPrograms.TypeInLanguage.P2'('MetaDefs.XType.F2'(A,B), C) :-
        'SharedPrograms.ConstructorInLanguage.P3'(A, C, D),
        'Lists.Length.P2'(B, D),
        'SharedPrograms.ListTypesInLanguage.P2'(B, C).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.Term.F2'(A,B), C, D, E, F, G, H, I, J) :-
        (   'SharedPrograms.FunctionInLanguage.P5'(A, C, K, _, _) ->
            'SharedPrograms.TermToIntDLAux.P11'(K, A, B, C, D, E, L, G, H, I, J),
            F=L
        ;   F='Syntax.NoFunctInd.C0',
            G='SharedPrograms.Bounded.C0',
            H='SharedPrograms.Bounded.C0',
            'SharedPrograms.TermInLanguage.P5'('MetaDefs.Term.F2'(A,B), D, 'MetaDefs.VarTyping.F1'(E), _, M),
            'SharedPrograms.CharDL.P3'('"<', I, N),
            'SharedPrograms.TypeToIntDL.P4'(M, C, N, O),
            'SharedPrograms.CharDL.P3'('">', O, J)
        ).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.XTerm.F3'(_,_,A), B, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', C, D) :-
        'SharedPrograms.CharDL.P3'('"<', C, E),
        'SharedPrograms.TypeToIntDL.P4'(A, B, E, F),
        'SharedPrograms.CharDL.P3'('">', F, D).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.CTerm.F1'(A), B, C, D, 'Syntax.NoFunctInd.C0', E, E, F, G) :-
        (   'SharedPrograms.ConstantInLanguage.P3'(A, B, _) ->
            'SharedPrograms.ConstantToIntDL.P4'(A, E, F, G)
        ;   E='SharedPrograms.Bounded.C0',
            'SharedPrograms.TermInLanguage.P5'('MetaDefs.CTerm.F1'(A), C, 'MetaDefs.VarTyping.F1'(D), _, H),
            'SharedPrograms.CharDL.P3'('"<', F, I),
            'SharedPrograms.TypeToIntDL.P4'(H, B, I, J),
            'SharedPrograms.CharDL.P3'('">', J, G)
        ).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.XCTerm.F2'(_,A), B, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', C, D) :-
        'SharedPrograms.CharDL.P3'('"<', C, E),
        'SharedPrograms.TypeToIntDL.P4'(A, B, E, F),
        'SharedPrograms.CharDL.P3'('">', F, D).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.Var.F2'(A,B), _, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.AlphaNum.C0', 'SharedPrograms.AlphaNum.C0', C, D) :-
        'Strings.StringInts.P2'(A, E),
        'Lists.Append.P3'(E, F, C),
        (   (   B=0
            ;   concat('"_', _, G),
                A=G
            ) ->
            D=F
        ;   'SharedPrograms.CharDL.P3'('"_', F, H),
            'SharedSyntax.IntegerToCharDL.P3'(B, H, D)
        ).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.Var.F1'(A), _, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.AlphaNum.C0', 'SharedPrograms.AlphaNum.C0', B, C) :-
        'Strings.StringInts.P2'('"v', D),
        'Lists.Append.P3'(D, E, B),
        (   A=0 ->
            C=E
        ;   'SharedPrograms.CharDL.P3'('"_', E, F),
            'SharedSyntax.IntegerToCharDL.P3'(A, F, C)
        ).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.Str.F1'(A), B, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', C, D) :-
        'SharedPrograms.BaseInLanguage.P2'('MetaDefs.Name.F4'('"Strings','"String','MetaDefs.Base.C0',0), B),
        'Strings.StringInts.P2'(A, E),
        'SharedPrograms.ExpandString.P3'(E, C, D).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.Int.F1'(A), B, _, _, C, D, 'SharedPrograms.AlphaNum.C0', E, F) :-
        'SharedPrograms.BaseInLanguage.P2'('MetaDefs.Name.F4'('"Integers','"Integer','MetaDefs.Base.C0',0), B),
        'SharedSyntax.IntegerToCharDL.P3'(A, E, F),
        (   'Integers.<.P2'(A, 0) ->
            'SharedPrograms.FunctionInLanguage.P5'('MetaDefs.Name.F4'('"Integers','"-','MetaDefs.Function.C0',1), B, C, _, _),
            D='SharedPrograms.Graphic.C0'
        ;   C='Syntax.NoFunctInd.C0',
            D='SharedPrograms.AlphaNum.C0'
        ).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.Prm.F1'(A), B, C, D, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', E, F) :-
        'SharedPrograms.TermInLanguage.P5'('MetaDefs.Prm.F1'(A), C, 'MetaDefs.VarTyping.F1'(D), _, G),
        'SharedPrograms.CharDL.P3'('"<', E, H),
        'SharedPrograms.TypeToIntDL.P4'(G, B, H, I),
        'SharedPrograms.CharDL.P3'('">', I, F).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.SuchThat.F2'(A,B), C, D, E, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', F, G) :-
        'SharedPrograms.CharDL.P3'('"{', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'Strings.StringInts.P2'('" : ', J),
        'Lists.Append.P3'(J, K, I),
        'SharedPrograms.FormulaToIntDL.P6'(B, C, D, E, K, L),
        'SharedPrograms.CharDL.P3'('"}', L, G).
'SharedPrograms.TermListToIntDL.P6'([A|B], C, D, E, F, G) :-
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, F, H),
        'SharedPrograms.TermListToIntDLAux.P6'(B, C, D, E, H, G).
'SharedPrograms.TermInLanguage.P5'(A, B, 'MetaDefs.VarTyping.F1'(C), 'MetaDefs.VarTyping.F1'(D), E) :-
        'SharedPrograms.MeltVariableTyping.P4'(C, B, [], F),
        'SharedPrograms.MeltedTermType.P9'(A, E, B, F, D, _, [], [], []),
        'SharedPrograms.FixType.P3'(E, 0, G),
        'SharedPrograms.FixVariableTyping.P2'(D, G).
'SharedPrograms.TermListToIntDLAux.P6'([], _, _, _, A, A).
'SharedPrograms.TermListToIntDLAux.P6'([A|B], C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('",', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'SharedPrograms.TermListToIntDLAux.P6'(B, C, D, E, I, G).
'SharedPrograms.TermToIntList.P4'(A, B, C, D) :-
        'SharedPrograms.ProgramLanguage.P2'(A, E),
        'SharedPrograms.ModuleLanguage.P3'(A, B, F),
        'SharedPrograms.TermInLanguage.P5'(C, E, 'MetaDefs.VarTyping.F1'([]), 'MetaDefs.VarTyping.F1'(G), _),
        'SharedPrograms.TermToIntDL.P9'(C, F, E, G, _, _, _, D, []).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.NoFunctInd.C0', 'MetaDefs.Name.F4'(A,B,C,D), E, F, G, H, 'Syntax.NoFunctInd.C0', I, 'SharedPrograms.Bounded.C0', J, K) :-
        (   A='"Lists',
            B='"Cons',
            C='MetaDefs.Function.C0',
            D=2 ->
            I='SharedPrograms.Bounded.C0',
            'SharedPrograms.ListExprToIntDL.P6'(E, F, G, H, J, K)
        ;   A='"Sets',
            B='"Inc',
            C='MetaDefs.Function.C0',
            D=2 ->
            I='SharedPrograms.Bounded.C0',
            'SharedPrograms.SetExprToIntDL.P6'(E, F, G, H, J, K)
        ;   'SharedPrograms.ClassifyToken.P2'(B, I),
            'Strings.StringInts.P2'(B, L),
            'Lists.Append.P3'(L, M, J),
            'SharedPrograms.CharDL.P3'('"(', M, N),
            'SharedPrograms.TermListToIntDL.P6'(E, F, G, H, N, O),
            'SharedPrograms.CharDL.P3'('")', O, K)
        ).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.XFX.F1'(A), B, [C,D], E, F, G, H, I, J, K, L) :-
        'SharedPrograms.BinaryInfixToIntDL.P12'('Syntax.XFX.F1'(A), B, C, D, E, F, G, H, I, J, K, L).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.XFY.F1'(A), B, [C,D], E, F, G, H, I, J, K, L) :-
        'SharedPrograms.BinaryInfixToIntDL.P12'('Syntax.XFY.F1'(A), B, C, D, E, F, G, H, I, J, K, L).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.YFX.F1'(A), B, [C,D], E, F, G, H, I, J, K, L) :-
        'SharedPrograms.BinaryInfixToIntDL.P12'('Syntax.YFX.F1'(A), B, C, D, E, F, G, H, I, J, K, L).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.FX.F1'(A), B, [C], D, E, F, 'Syntax.FX.F1'(A), G, H, I, J) :-
        'SharedPrograms.UnaryPrefixToIntDL.P10'('Syntax.FX.F1'(A), B, C, D, E, F, G, H, I, J).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.FY.F1'(A), B, [C], D, E, F, 'Syntax.FY.F1'(A), G, H, I, J) :-
        'SharedPrograms.UnaryPrefixToIntDL.P10'('Syntax.FY.F1'(A), B, C, D, E, F, G, H, I, J).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.XF.F1'(A), B, [C], D, E, F, 'Syntax.XF.F1'(A), G, H, I, J) :-
        'SharedPrograms.UnaryPostfixToIntDL.P10'('Syntax.XF.F1'(A), B, C, D, E, F, G, H, I, J).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.YF.F1'(A), B, [C], D, E, F, 'Syntax.YF.F1'(A), G, H, I, J) :-
        'SharedPrograms.UnaryPostfixToIntDL.P10'('Syntax.YF.F1'(A), B, C, D, E, F, G, H, I, J).
'SharedPrograms.ThenPartToIntDL.P6'(A, B, C, D, E, F) :-
        'SharedPrograms.FormulaToIntDL.P6'(A, B, C, D, G, H),
        (   'SharedPrograms.MustBracketThen.P1'(A) ->
            'SharedPrograms.CharDL.P3'('"(', E, G),
            'SharedPrograms.CharDL.P3'('")', H, F)
        ;   E=G,
            F=H
        ).
'SharedPrograms.TypeSymbolInLanguage.P3'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F) :-
        'SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(G,'ProgDefs.Categories.F2'(H,_),_)),
        'SharedPrograms.AVLFind.P3'(H, B, I),
        'SharedPrograms.FindDescriptor.P5'(I, G, C, D, F).
'SharedPrograms.TypeListToIntDL.P4'([A|B], C, D, E) :-
        'SharedPrograms.TypeToIntDL.P4'(A, C, D, F),
        'SharedPrograms.TypeListToIntDLAux.P4'(B, C, F, E).
'SharedPrograms.TypeListToIntDLAux.P4'([], _, A, A).
'SharedPrograms.TypeListToIntDLAux.P4'([A|B], C, D, E) :-
        'SharedPrograms.CharDL.P3'('",', D, F),
        'SharedPrograms.TypeToIntDL.P4'(A, C, F, G),
        'SharedPrograms.TypeListToIntDLAux.P4'(B, C, G, E).
'SharedPrograms.UnaryPrefixToIntDL.P10'(A, 'MetaDefs.Name.F4'(_,B,_,_), C, D, E, F, G, H, I, J) :-
        'SharedPrograms.TermToIntDL.P9'(C, D, E, F, K, L, M, N, O),
        'SharedPrograms.ClassifyToken.P2'(B, G),
        'Strings.StringInts.P2'(B, P),
        'Lists.Append.P3'(P, Q, I),
        (   'SharedPrograms.LPrec.P2'(K, R),
            'SharedPrograms.RPrec.P2'(A, S),
            'SharedPrograms.BindsTighter.P2'(R, S) ->
            H=M,
            'SharedPrograms.SpaceIfNeeded.P4'(G, L, Q, N),
            J=O
        ;   H='SharedPrograms.Bounded.C0',
            'SharedPrograms.CharDL.P3'('"(', Q, N),
            'SharedPrograms.CharDL.P3'('")', O, J)
        ).
'SharedPrograms.TypeToIntDL.P4'('MetaDefs.Type.F2'(A,B), C, D, E) :-
        'SharedPrograms.ConstructorInLanguage.P3'(A, C, F),
        'Lists.Length.P2'(B, F),
        A='MetaDefs.Name.F4'(_,G,_,_),
        'Strings.StringInts.P2'(G, H),
        'Lists.Append.P3'(H, I, D),
        'SharedPrograms.CharDL.P3'('"(', I, J),
        'SharedPrograms.TypeListToIntDL.P4'(B, C, J, K),
        'SharedPrograms.CharDL.P3'('")', K, E).
'SharedPrograms.TypeToIntDL.P4'('MetaDefs.BType.F1'(A), B, C, D) :-
        'SharedPrograms.BaseInLanguage.P2'(A, B),
        A='MetaDefs.Name.F4'(_,E,_,_),
        'Strings.StringInts.P2'(E, F),
        'Lists.Append.P3'(F, D, C).
'SharedPrograms.TypeToIntDL.P4'('MetaDefs.Par.F2'(A,B), _, C, D) :-
        'Strings.StringInts.P2'(A, E),
        'Lists.Append.P3'(E, F, C),
        (   B=0 ->
            D=F
        ;   'SharedPrograms.CharDL.P3'('"_', F, G),
            'SharedSyntax.IntegerToCharDL.P3'(B, G, D)
        ).
'SharedPrograms.TypeToIntDL.P4'('MetaDefs.Par.F1'(A), _, B, C) :-
        'SharedPrograms.CharDL.P3'('"p', B, D),
        (   A=0 ->
            C=D
        ;   'SharedPrograms.CharDL.P3'('"_', D, E),
            'SharedSyntax.IntegerToCharDL.P3'(A, E, C)
        ).
'SharedPrograms.UnaryPostfixToIntDL.P10'(A, 'MetaDefs.Name.F4'(_,B,_,_), C, D, E, F, G, H, I, J) :-
        'SharedPrograms.TermToIntDL.P9'(C, D, E, F, K, L, M, N, O),
        'SharedPrograms.ClassifyToken.P2'(B, H),
        (   'SharedPrograms.RPrec.P2'(K, P),
            'SharedPrograms.LPrec.P2'(A, Q),
            'SharedPrograms.BindsTighter.P2'(P, Q) ->
            G=L,
            I=N,
            'SharedPrograms.SpaceIfNeeded.P4'(M, H, O, R)
        ;   G='SharedPrograms.Bounded.C0',
            'SharedPrograms.CharDL.P3'('"(', I, N),
            'SharedPrograms.CharDL.P3'('")', O, R)
        ),
        'Strings.StringInts.P2'(B, S),
        'Lists.Append.P3'(S, J, R).
'SharedPrograms.UpdateDelays.P6'('ProgDefs.Export.C0', A, B, C, [A|B], C).
'SharedPrograms.UpdateDelays.P6'('ProgDefs.Closed.C0', A, B, C, [A|B], C).
'SharedPrograms.UpdateDelays.P6'('ProgDefs.Local.C0', A, B, C, B, [A|C]).
'SharedPrograms.UpdateDelays.P6'('ProgDefs.Module.C0', A, B, C, B, [A|C]).
'SharedPrograms.VarListToIntDL.P6'([A|B], C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('"[', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'SharedPrograms.RestVarListToIntDL.P6'(B, C, D, E, I, J),
        'SharedPrograms.CharDL.P3'('"]', J, G).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% Prolog routines shared by the parser and Programs and Scripts system modules.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% Useful routines
%------------------------------------------------------------------------------
 
'SharedPrograms.FindMember.P2'(X, Ys) :-
   ( var(X) ->
     Ys = [Z|Zs],
     member_in(Zs, Z, X)
   ; user:'Lists.MemberCheck.P2'(X, Ys)
   ).

member_in([], X, X).

member_in([Y|Ys], Z, X) :-
   ( X = Z ; member_in(Ys, Y, X)).

'~SharedPrograms.FindMember.P2'(X, Ys) :-
   'SharedPrograms.FindMember.P2'(X, Ys).


'SharedPrograms.PickOne.P3'(X, Ys, Rest) :-
   ( var(X) ->
     delete_one(Ys, X, Rest)
   ; user:'Lists.DeleteFirst.P3'(X, Ys, Rest)
   ).

'~SharedPrograms.PickOne.P3'(X, Ys, Rest) :-
   'SharedPrograms.PickOne.P3'(X, Ys, Rest).


delete_one([Y|Ys], X, Rest) :-
   ( Ys = [] ->
     X = Y,
     Rest = Ys
   ;
     ( X = Y, 
       Rest = Ys
     ; delete_one(Ys, X, Rest1),
       Rest = [Y|Rest1]
     )
   ).

%------------------------------------------------------------------------------
% Support for access to symbols in language
%------------------------------------------------------------------------------ 

'SharedPrograms.FindDescriptor.P5'([Desc|Descs], Access, Category, Arity, Decl) :-
   ( nonvar(Arity), nonvar(Category) ->
     one_descriptor_in([Desc|Descs], Access, Category, Arity, Decl)
   ; descriptor_in(Descs, Desc, Access, Category, Arity, Decl) 
   ).

'~SharedPrograms.FindDescriptor.P5'(Descriptors, Access, Category, Arity, Decl) :-
   'SharedPrograms.FindDescriptor.P5'(Descriptors, Access, Category, Arity, Decl).


descriptor_in([], Desc, Access, Cat, Arity, Decl) :-
   'SharedPrograms.MatchingDescriptor.P5'(Desc, Access, Cat, Arity, Decl).

descriptor_in([D|Ds], Desc, Access, Cat, Arity, Decl) :-
   ( 'SharedPrograms.MatchingDescriptor.P5'(Desc, Access, Cat, Arity, Decl)
   ; descriptor_in(Ds, D, Access, Cat, Arity, Decl)
   ).


one_descriptor_in([Desc|Descs], Access, Cat, Arity, Decl) :-
   ( 'SharedPrograms.MatchingDescriptor.P5'(Desc, Access, Cat, Arity, Decl) ->
     true
   ; one_descriptor_in(Descs, Access, Cat, Arity, Decl)
   ).

%------------------------------------------------------------------------------
% Support for access to Program code
%------------------------------------------------------------------------------

'SharedPrograms.SystemModule.P1'(S) :-
%   (S == S), !.
   (S == '"' ; user:system_module_name(S)), !.

'~SharedPrograms.SystemModule.P1'(S) :-
   'SharedPrograms.SystemModule.P1'(S).

 
'SharedPrograms.AVLFind.P3'(Tree, Key, Item) :-
   ( var(Key) ->
     user:'AVLTrees.AVLMember.P3'(Tree, Key, Item)
   ; user:'AVLTrees.AVLSearch.P3'(Tree, Key, Item)
   ).

'~SharedPrograms.AVLFind.P3'(Tree, Key, Item) :-
   'SharedPrograms.AVLFind.P3'(Tree, Key, Item).


'SharedPrograms.FindPredDef.P3'(Arity, PredDefs
      , 'ProgDefs.PredDef.F4'(Arity, Statements, Delays1, Delays2)
      ) :-
   ( var(Arity) ->
     user:'Lists.Member.P2'('ProgDefs.PredDef.F4'(Arity, Statements
           , Delays1, Delays2)
        , PredDefs)
   ; user:'Lists.MemberCheck.P2'('ProgDefs.PredDef.F4'(Arity, Statements
           , Delays1, Delays2)
        , PredDefs)
   ).

'~SharedPrograms.FindPredDef.P3'(Arity, PredDefs, Program) :-
   'SharedPrograms.FindPredDef.P3'(Arity, PredDefs, Program).


'SharedPrograms.PickPredDef.P4'(Arity, PredDefs
      , 'ProgDefs.PredDef.F4'(Arity, Statements, Delays1, Delays2), RestDefs
      ) :-
   ( var(Arity) ->
     'Lists':'Lists.Delete.P3'('ProgDefs.PredDef.F4'(Arity, Statements
           , Delays1, Delays2)
        , PredDefs, RestDefs)
   ; 'Lists':'Lists.DeleteFirst.P3'('ProgDefs.PredDef.F4'(Arity, Statements
           , Delays1, Delays2)
        , PredDefs, RestDefs)
   ).

'~SharedPrograms.PickPredDef.P4'(Arity, PredDefs, PredDef, RestDefs) :-
   'SharedPrograms.PickPredDef.P4'(Arity, PredDefs, PredDef, RestDefs).

%------------------------------------------------------------------------------
% Support for autorecompilation (Succeed etc.)
%------------------------------------------------------------------------------

:- dynamic used_module_version/2.

'SharedPrograms.NextModuleVersion.P2'(Module, Version) :-
   ( retract(used_module_version(Module, Old)) ->
     Version is Old + 1
   ; Version is 1
   ),
   assert(used_module_version(Module, Version)).

'~SharedPrograms.NextModuleVersion.P2'(Module, Version) :-
   'SharedPrograms.NextModuleVersion.P2'(Module, Version).


'SharedPrograms.SystemModuleRep.P4'(Module, Definition, Descriptor, PredDefs) :-
   user:system_module(Module, Definition, Descriptor, PredDefs).

'~SharedPrograms.SystemModuleRep.P4'(Module, Definition, Descriptor, PredDefs) :-
   'SharedPrograms.SystemModuleRep.P4'(Module, Definition, Descriptor, PredDefs).
%------------------------------------------------------------------------------
% Support for Type Checking
%------------------------------------------------------------------------------

'SharedPrograms.FixType.P3'(Type, N, NewN) :-
   ( var(Type) ->
     Type = 'MetaDefs.Par.F1'(N), NewN is N + 1
   ; nonvar_fix_type(Type, N, NewN)
   ).

'~SharedPrograms.FixType.P3'(Type, N, NewN) :-
   'SharedPrograms.FixType.P3'(Type, N, NewN).

nonvar_fix_type('MetaDefs.Par.F1'(_), N, N).

nonvar_fix_type('MetaDefs.Par.F2'(_, _), N, N).

nonvar_fix_type('MetaDefs.BType.F1'(_), N, N).

nonvar_fix_type('MetaDefs.Type.F2'(_, Args), N, NewN) :-
   list_fix_type(Args, N, NewN).


list_fix_type([], N, N).

list_fix_type([Type|Types], N, NewN) :-
   'SharedPrograms.FixType.P3'(Type, N, N1),
   list_fix_type(Types, N1, NewN).


'SharedPrograms.FixRationals.P2'([], 0).

'SharedPrograms.FixRationals.P2'([NumType|NumTypes], Count) :-
   fix_rational(NumType, R),
   'SharedPrograms.FixRationals.P2'(NumTypes, Count1),
   Count is Count1 + R.

'~SharedPrograms.FixRationals.P2'(NumTypes, Count) :-
   'SharedPrograms.FixRationals.P2'(NumTypes, Count).


fix_rational('SharedPrograms.NumType.F3'(Value, 'MetaDefs.BType.F1'(Name), Term)
      , Rational
      ) :-
   ( Name = 'MetaDefs.Name.F4'('"Rationals', '"Rational', 'MetaDefs.Base.C0', 0) ->
     'SharedPrograms.RationalTerm.P2'(Value, Term),
     Rational = 1
   ; Term = 'MetaDefs.Int.F1'(Value),
     Rational = 0
   ). 

%------------------------------------------------------------------------------
% Type Unification
%------------------------------------------------------------------------------
 
'SharedPrograms.UnifyMeltedTypes.P4'(ContextType, ArgType, Term, Errors) :-
   ( unify_types(ContextType, ArgType) ->
     Errors = []
   ; copy_term((ContextType, ArgType), (ContextTypeCopy, ArgTypeCopy)),
     'SharedPrograms.FixType.P3'(ArgTypeCopy, 0, N),
     'SharedPrograms.FixType.P3'(ContextTypeCopy, N, _),
     Errors = ['SharedPrograms.TypeError.F3'(Term, ArgTypeCopy, ContextTypeCopy)]
   ).

'~SharedPrograms.UnifyMeltedTypes.P4'(ContextType, ArgType, Term, Errors) :-
   'SharedPrograms.UnifyMeltedTypes.P4'(ContextType, ArgType, Term, Errors).

unify_types(Type1, Type2) :-
   ( nonvar(Type1) ->
     ( nonvar(Type2) ->
       nonvar_unify_types(Type1, Type2)
     ; not_occurs_in(Type1, Type2), Type2 = Type1
     )
   ; ( nonvar(Type2) ->
       not_occurs_in(Type2, Type1), Type1 = Type2
     ; Type1 = Type2
     )
   ).


nonvar_unify_types('MetaDefs.BType.F1'(Name), 'MetaDefs.BType.F1'(Name)).

nonvar_unify_types('MetaDefs.Type.F2'(Name, Args1), 
                   'MetaDefs.Type.F2'(Name, Args2)
                  ) :-   
   unify_type_lists(Args1, Args2).


unify_type_lists([], []).

unify_type_lists([Type1|Types1], [Type2|Types2]) :-
   unify_types(Type1, Type2),
   unify_type_lists(Types1, Types2).

 
not_occurs_in('MetaDefs.BType.F1'(_), _).

not_occurs_in('MetaDefs.Type.F2'(_, Args), Var) :-
   not_occurs_in_list(Args, Var).


not_occurs_in_list([], _).

not_occurs_in_list([Term|Terms], Var) :-
   ( var(Term) ->
     Term \== Var
   ; not_occurs_in(Term, Var)
   ),
   not_occurs_in_list(Terms, Var).

%------------------------------------------------------------------------------
% Conversion between delay conditions and strings
%------------------------------------------------------------------------------
 
'SharedPrograms.SStringToCondition.P2'(String, Condition) :-
   user:'Strings.StringInts.P2'(String, IntList),
   user:append(IntList, [0'.], Chars),
   user:get_one_item_2nd(Chars, [], Item),
   user:cond(Item, [], Condition, [], Error, 1, 2, local),
   var(Error).

'~SharedPrograms.SStringToCondition.P2'(String, Condition) :-
   'SharedPrograms.SStringToCondition.P2'(String, Condition).

%------------------------------------------------------------------------------
% Support for ground representation to String conversion
%------------------------------------------------------------------------------

'SharedPrograms.CharDL.P3'(Gstr, [Char|Chars], Chars) :-
   name(Gstr, [0'", Char]).

'~SharedPrograms.CharDL.P3'(Gstr, Chars, Chars1) :-
   'SharedPrograms.CharDL.P3'(Gstr, Chars, Chars1).


'SharedPrograms.ClassifyToken.P2'(Token, Class) :-
   name(Token, [0'", Sample|_]),
   ( 0'A =< Sample, Sample =< 0'Z ->
     Class = 'SharedPrograms.AlphaNum.C0'
   ; Class = 'SharedPrograms.Graphic.C0'
   ).

'~SharedPrograms.ClassifyToken.P2'(Token, Class) :-
   'SharedPrograms.ClassifyToken.P2'(Token, Class).

%------------------------------------------------------------------------------
% Support for symbol insertion routines
%------------------------------------------------------------------------------
 
'SharedPrograms.AddToDescriptorList.P6'(Descriptors, ModAccess, Category, Arity
      , NewDescriptor, [NewDescriptor|Descriptors]
      ) :-
   \+ 'SharedPrograms.FindDescriptor.P5'(Descriptors, ModAccess, Category, Arity
         , _Declaration).

'~SharedPrograms.AddToDescriptorList.P6'(Descriptors, ModAccess, Category, Arity
      , NewDescriptor, NewDescriptors
      ) :-
   'SharedPrograms.AddToDescriptorList.P6'(Descriptors, ModAccess, Category, Arity
      , NewDescriptor, NewDescriptors).

%------------------------------------------------------------------------------
% Variant types (used to check head condition)
%------------------------------------------------------------------------------
 
'SharedPrograms.VariantTypes.P4'(ContextType, ArgType, Term, Errors) :-
   ( variant_types(ContextType, ArgType, [], _) ->
     Errors = []
   ;
     copy_term((ContextType, ArgType), (ContextTypeCopy, ArgTypeCopy)),
     'SharedPrograms.FixType.P3'(ArgTypeCopy, 0, N),
     'SharedPrograms.FixType.P3'(ContextTypeCopy, N, _),
     Errors = ['SharedPrograms.HeadError.F3'(Term, ArgTypeCopy, ContextTypeCopy)]
   ).

'~SharedPrograms.VariantTypes.P4'(ContextType, ArgType, Term, Errors) :-
   'SharedPrograms.VariantTypes.P4'(ContextType, ArgType, Term, Errors).

variant_types(Type1, Type2, ParDict, NewParDict) :-
   ( var(Type1) ->
     var(Type2),
     ( in_par_dict(Type1, ParDict, Type1Alias) ->
       Type1Alias == Type2,
       NewParDict = ParDict
     ; NewParDict = [Type1/Type2|ParDict]
     )
   ; nonvar(Type2),
     nonvar_variant_types(Type1, Type2, ParDict, NewParDict)
   ).

nonvar_variant_types('MetaDefs.BType.F1'(Name), 'MetaDefs.BType.F1'(Name)
      , ParDict, ParDict).

nonvar_variant_types('MetaDefs.Type.F2'(Name, Args1)
      , 'MetaDefs.Type.F2'(Name, Args2), ParDict, NewParDict
      ) :-
   list_variant_types(Args1, Args2, ParDict, NewParDict).


list_variant_types([], [], ParDict, ParDict).

list_variant_types([Type1|Types1], [Type2|Types2], ParDict, NewParDict) :-
   variant_types(Type1, Type2, ParDict, ParDict1),
   list_variant_types(Types1, Types2, ParDict1, NewParDict).

in_par_dict(P1, [A1/A2|Entries], P2) :-
   ( P1 == A1 ->
     P2 = A2
   ; in_par_dict(P1, Entries, P2)
   ).
:- op(500, yfx, and).
:- op(400, yfx, or).

'Substs.FullDerefType.P4'('MetaDefs.Par.F1'(A), B, _, C) :-
        (   B='MetaDefs.Par.F1'(A) ->
            C='MetaDefs.Occ.F1'('MetaDefs.Par.F1'(A))
        ;   C='MetaDefs.Par.F1'(A)
        ).
'Substs.FullDerefType.P4'('MetaDefs.Par.F2'(A,B), C, _, D) :-
        (   C='MetaDefs.Par.F2'(A,B) ->
            D='MetaDefs.Occ.F1'('MetaDefs.Par.F2'(A,B))
        ;   D='MetaDefs.Par.F2'(A,B)
        ).
'Substs.FullDerefType.P4'('MetaDefs.BType.F1'(A), _, _, 'MetaDefs.BType.F1'(A)).
'Substs.FullDerefType.P4'('MetaDefs.Type.F2'(A,B), C, D, 'MetaDefs.Type.F2'(A,E)) :-
        'Substs.FullDerefType1.P4'(B, C, D, E).
'Substs.FullDerefType.P4'('MetaDefs.XBType.F1'(A), _, _, 'MetaDefs.XBType.F1'(A)).
'Substs.FullDerefType.P4'('MetaDefs.XType.F2'(A,B), C, D, 'MetaDefs.XType.F2'(A,E)) :-
        'Substs.FullDerefType1.P4'(B, C, D, E).
'Substs.FullDerefType.P3'('MetaDefs.Par.F2'(A,B), _, 'MetaDefs.Par.F2'(A,B)).
'Substs.FullDerefType.P3'('MetaDefs.Par.F1'(A), _, 'MetaDefs.Par.F1'(A)).
'Substs.FullDerefType.P3'('MetaDefs.Type.F2'(A,B), C, 'MetaDefs.Type.F2'(A,D)) :-
        'Substs.ApplyTypeSubst.P3'(B, C, D).
'Substs.FullDerefType.P3'('MetaDefs.BType.F1'(A), _, 'MetaDefs.BType.F1'(A)).
'Substs.FullDerefType.P3'('MetaDefs.XType.F2'(A,B), C, 'MetaDefs.XType.F2'(A,D)) :-
        'Substs.ApplyTypeSubst.P3'(B, C, D).
'Substs.FullDerefType.P3'('MetaDefs.XBType.F1'(A), _, 'MetaDefs.XBType.F1'(A)).
'Substs.ComposeTypeSubsts2.P3'('Substs.TypeSubst.F2'(A,B), 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,F)) :-
        'Substs.ComposeHeaps.P3'(A, C, E),
        'Substs.ComposeLists.P3'(D, B, F).
'Substs.ApplyTypeSubst.P3'([], _, []).
'Substs.ApplyTypeSubst.P3'([A|B], C, [D|E]) :-
        'Substs.SubstApplyToType.P3'(A, C, D),
        'Substs.ApplyTypeSubst.P3'(B, C, E).
'Substs.AddTypeBinding1.P4'('MetaDefs.Par.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.V.F1'('MetaDefs.Par.F2'(A,B)), D, E).
'Substs.AddTypeBinding1.P4'('MetaDefs.Par.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.R.F1'(A), C, D).
'Substs.AddTypeBinding1.P4'('MetaDefs.BType.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.BType.F1'(A)), C, D).
'Substs.AddTypeBinding1.P4'('MetaDefs.Type.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.Type.F2'(A,B)), D, E).
'Substs.AddTypeBinding1.P4'('MetaDefs.XType.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.XType.F2'(A,B)), D, E).
'Substs.AddTypeBinding1.P4'('MetaDefs.XBType.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.XBType.F1'(A)), C, D).
'Substs.AddTermBinding1.P4'('MetaDefs.Var.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.V.F1'('MetaDefs.Var.F2'(A,B)), D, E).
'Substs.AddTermBinding1.P4'('MetaDefs.Var.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.R.F1'(A), C, D).
'Substs.AddTermBinding1.P4'('MetaDefs.CTerm.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.CTerm.F1'(A)), C, D).
'Substs.AddTermBinding1.P4'('MetaDefs.Term.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.Term.F2'(A,B)), D, E).
'Substs.AddTermBinding1.P4'('MetaDefs.XTerm.F3'(A,B,C), D, E, F) :-
        'Substs.AddBinding.P4'(D, 'Substs.T.F1'('MetaDefs.XTerm.F3'(A,B,C)), E, F).
'Substs.AddTermBinding1.P4'('MetaDefs.XCTerm.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.XCTerm.F2'(A,B)), D, E).
'Substs.AddTermBinding1.P4'('MetaDefs.Int.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.Int.F1'(A)), C, D).
'Substs.AddTermBinding1.P4'('MetaDefs.Prm.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.Prm.F1'(A)), C, D).
'Substs.AddTermBinding1.P4'('MetaDefs.Str.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.Str.F1'(A)), C, D).
'Substs.AddTermBinding1.P4'('MetaDefs.SuchThat.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.SuchThat.F2'(A,B)), D, E).
'Substs.AddNewBinding.P5'(15, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D)) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(14, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(13, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(12, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(11, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(10, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(9, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(8, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(7, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(6, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(5, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(4, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(3, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(2, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(1, A, B, C, 'Substs.H.F16'('Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(0, A, B, C, 'Substs.H.F16'(D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddBinding2.P21'(15, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,T)) :-
        'Substs.AddBinding1.P5'(S, A, B, C, T).
'Substs.AddBinding2.P21'(14, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,O,P,Q,T,S)) :-
        'Substs.AddBinding1.P5'(R, A, B, C, T).
'Substs.AddBinding2.P21'(13, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,O,P,T,R,S)) :-
        'Substs.AddBinding1.P5'(Q, A, B, C, T).
'Substs.AddBinding2.P21'(12, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,O,T,Q,R,S)) :-
        'Substs.AddBinding1.P5'(P, A, B, C, T).
'Substs.AddBinding2.P21'(11, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,T,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(O, A, B, C, T).
'Substs.AddBinding2.P21'(10, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,T,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(N, A, B, C, T).
'Substs.AddBinding2.P21'(9, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,T,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(M, A, B, C, T).
'Substs.AddBinding2.P21'(8, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,T,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(L, A, B, C, T).
'Substs.AddBinding2.P21'(7, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,T,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(K, A, B, C, T).
'Substs.AddBinding2.P21'(6, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,T,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(J, A, B, C, T).
'Substs.AddBinding2.P21'(5, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,T,J,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(I, A, B, C, T).
'Substs.AddBinding2.P21'(4, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,T,I,J,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(H, A, B, C, T).
'Substs.AddBinding2.P21'(3, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,T,H,I,J,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(G, A, B, C, T).
'Substs.AddBinding2.P21'(2, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,T,G,H,I,J,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(F, A, B, C, T).
'Substs.AddBinding2.P21'(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,T,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(E, A, B, C, T).
'Substs.AddBinding2.P21'(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(T,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(D, A, B, C, T).
'Substs.AddTermBinding.P4'(A, B, 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,D)) :-
        'Substs.AddTermBinding1.P4'(A, B, C, E).
'Substs.AddTypeBinding.P4'(A, B, 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,D)) :-
        'Substs.AddTypeBinding1.P4'(A, B, C, E).
'Substs.ApplySubstToType.P4'(A, B, C, D) :-
        'Substs.DerefType.P3'(A, C, E),
        'Substs.FullDerefType.P4'(E, B, C, D).
'Substs.ApplySubstToTerm.P4'(A, B, C, D) :-
        'Substs.Dereference.P3'(A, C, E),
        'Substs.FullDerefTerm.P4'(E, B, C, D).
'Substs.ApplyTermSubst.P3'([], _, []).
'Substs.ApplyTermSubst.P3'([A|B], C, [D|E]) :-
        'Substs.SubstApplyToTerm.P3'(A, C, D),
        'Substs.ApplyTermSubst.P3'(B, C, E).
'Substs.ComposeTermSubsts1.P3'('Substs.TermSubst.F2'('Substs.Heap.F2'(A,B),C), D, 'Substs.TermSubst.F2'(E,F)) :-
        'Substs.RationaliseTermList.P3'(C, F, D),
        'Substs.EmptyHeap.P1'(G),
        'Substs.RationaliseTermHeap.P6'(B, A, 0, D, G, E).
'Substs.ComposeHeaps.P3'(A, 'Substs.Heap.F2'(B,C), D) :-
        'Substs.ComposeHeaps1.P5'(C, B, 0, A, D).
'Substs.BindVariable.P4'('MetaDefs.Var.F2'(A,B), C, 'Substs.TermSubst.F2'(D,E), 'Substs.TermSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),C)|E])).
'Substs.BindVariable.P4'('MetaDefs.Var.F1'(A), B, C, D) :-
        'Substs.AddTermBinding.P4'(B, A, C, D).
'Substs.BindParameter.P4'('MetaDefs.Par.F2'(A,B), C, 'Substs.TypeSubst.F2'(D,E), 'Substs.TypeSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),C)|E])).
'Substs.BindParameter.P4'('MetaDefs.Par.F1'(A), B, C, D) :-
        'Substs.AddTypeBinding.P4'(B, A, C, D).
'Substs.BindingInHeap.P3'('Substs.Heap.F2'(A,B), C, D) :-
        'Substs.Address.P5'(B, A, 0, C, D).
'Substs.ComposeLists.P3'([], A, A).
'Substs.ComposeLists.P3'(['MetaDefs.!.F2'(B,C)|A], D, ['MetaDefs.!.F2'(B,C)|E]) :-
        'Substs.DB.P3'(D, B, F),
        'Substs.ComposeLists.P3'(A, F, E).
'Substs.ComposeHeaps1.P5'('Substs.N.C0', _, _, A, A).
'Substs.ComposeHeaps1.P5'('Substs.R.F1'(A), _, B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.R.F1'(A), C, D).
'Substs.ComposeHeaps1.P5'('Substs.V.F1'(A), _, B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.V.F1'(A), C, D).
'Substs.ComposeHeaps1.P5'('Substs.T.F1'(A), _, B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'(A), C, D).
'Substs.ComposeHeaps1.P5'('Substs.H.F16'(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), Q, R, S, T) :-
        minus(Q, 1, V),
        U=V,
        power(16, Q, X),
        W=X,
        'Substs.ComposeHeaps1.P5'(A, U, R, S, Y),
        plus(W, R, Z),
        'Substs.ComposeHeaps1.P5'(B, U, Z, Y, A1),
        times(2, W, D1),
        plus(D1, R, B1),
        'Substs.ComposeHeaps1.P5'(C, U, B1, A1, C1),
        times(3, W, G1),
        plus(G1, R, E1),
        'Substs.ComposeHeaps1.P5'(D, U, E1, C1, F1),
        times(4, W, J1),
        plus(J1, R, H1),
        'Substs.ComposeHeaps1.P5'(E, U, H1, F1, I1),
        times(5, W, M1),
        plus(M1, R, K1),
        'Substs.ComposeHeaps1.P5'(F, U, K1, I1, L1),
        times(6, W, P1),
        plus(P1, R, N1),
        'Substs.ComposeHeaps1.P5'(G, U, N1, L1, O1),
        times(7, W, S1),
        plus(S1, R, Q1),
        'Substs.ComposeHeaps1.P5'(H, U, Q1, O1, R1),
        times(8, W, V1),
        plus(V1, R, T1),
        'Substs.ComposeHeaps1.P5'(I, U, T1, R1, U1),
        times(9, W, Y1),
        plus(Y1, R, W1),
        'Substs.ComposeHeaps1.P5'(J, U, W1, U1, X1),
        times(10, W, B2),
        plus(B2, R, Z1),
        'Substs.ComposeHeaps1.P5'(K, U, Z1, X1, A2),
        times(11, W, E2),
        plus(E2, R, C2),
        'Substs.ComposeHeaps1.P5'(L, U, C2, A2, D2),
        times(12, W, H2),
        plus(H2, R, F2),
        'Substs.ComposeHeaps1.P5'(M, U, F2, D2, G2),
        times(13, W, K2),
        plus(K2, R, I2),
        'Substs.ComposeHeaps1.P5'(N, U, I2, G2, J2),
        times(14, W, N2),
        plus(N2, R, L2),
        'Substs.ComposeHeaps1.P5'(O, U, L2, J2, M2),
        times(15, W, P2),
        plus(P2, R, O2),
        'Substs.ComposeHeaps1.P5'(P, U, O2, M2, T).
'Substs.ComposeTypeSubsts1.P3'('Substs.TypeSubst.F2'('Substs.Heap.F2'(A,B),C), D, 'Substs.TypeSubst.F2'(E,F)) :-
        'Substs.RationaliseTypeList.P3'(C, F, D),
        'Substs.EmptyHeap.P1'(G),
        'Substs.RationaliseTypeHeap.P6'(B, A, 0, D, G, E).
'Substs.ComposeTermSubsts2.P3'('Substs.TermSubst.F2'(A,B), 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,F)) :-
        'Substs.ComposeHeaps.P3'(A, C, E),
        'Substs.ComposeLists.P3'(D, B, F).
'Substs.DerefType1.P6'('Substs.N.C0', A, _, _, _, A).
'Substs.DerefType1.P6'('Substs.R.F1'(A), _, B, C, D, E) :-
        'Substs.Contents.P4'(A, B, C, F),
        'Substs.DerefType1.P6'(F, 'MetaDefs.Par.F1'(A), B, C, D, E).
'Substs.DerefType1.P6'('Substs.V.F1'(A), _, B, C, D, E) :-
        'Substs.DerefType.P3'(A, 'Substs.TypeSubst.F2'('Substs.Heap.F2'(B,C),D), E).
'Substs.DerefType1.P6'('Substs.T.F1'(A), _, _, _, _, A).
'Substs.DelTypeBinding.P4'('MetaDefs.Par.F2'(A,B), C, 'Substs.TypeSubst.F2'(D,E), 'Substs.TypeSubst.F2'(D,F)) :-
        'Lists.DeleteFirst.P3'('MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),G), E, F),
        'Substs.SubstApplyToType.P3'(G, 'Substs.TypeSubst.F2'(D,E), C).
'Substs.DelTypeBinding.P4'('MetaDefs.Par.F1'(A), B, 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,D)) :-
        'Substs.AddBinding.P4'(A, 'Substs.N.C0', C, E),
        'Substs.SubstApplyToType.P3'('MetaDefs.Par.F1'(A), 'Substs.TypeSubst.F2'(C,D), B),
        user:not_equal([], [B,A], B, 'MetaDefs.Par.F1'(A)).
'Substs.DB.P3'([], _, []).
'Substs.DB.P3'(['MetaDefs.!.F2'(B,C)|A], D, E) :-
        (   B=D ->
            E=A
        ;   E=['MetaDefs.!.F2'(B,C)|F],
            'Substs.DB.P3'(A, D, F)
        ).
'Substs.DelTermBinding.P4'('MetaDefs.Var.F2'(A,B), C, 'Substs.TermSubst.F2'(D,E), 'Substs.TermSubst.F2'(D,F)) :-
        'Lists.DeleteFirst.P3'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),G), E, F),
        'Substs.SubstApplyToTerm.P3'(G, 'Substs.TermSubst.F2'(D,E), C).
'Substs.DelTermBinding.P4'('MetaDefs.Var.F1'(A), B, 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,D)) :-
        'Substs.AddBinding.P4'(A, 'Substs.N.C0', C, E),
        'Substs.SubstApplyToTerm.P3'('MetaDefs.Var.F1'(A), 'Substs.TermSubst.F2'(C,D), B),
        user:not_equal([], [B,A], B, 'MetaDefs.Var.F1'(A)).
'Substs.DerefType.P3'('MetaDefs.Occ.F1'(A), _, A).
'Substs.DerefType.P3'('MetaDefs.Par.F1'(A), 'Substs.TypeSubst.F2'('Substs.Heap.F2'(B,C),D), E) :-
        'Substs.Contents.P4'(A, B, C, F),
        'Substs.DerefType1.P6'(F, 'MetaDefs.Par.F1'(A), B, C, D, E).
'Substs.DerefType.P3'('MetaDefs.Type.F2'(A,B), _, 'MetaDefs.Type.F2'(A,B)).
'Substs.DerefType.P3'('MetaDefs.BType.F1'(A), _, 'MetaDefs.BType.F1'(A)).
'Substs.DerefType.P3'('MetaDefs.XType.F2'(A,B), _, 'MetaDefs.XType.F2'(A,B)).
'Substs.DerefType.P3'('MetaDefs.XBType.F1'(A), _, 'MetaDefs.XBType.F1'(A)).
'Substs.DerefType.P3'('MetaDefs.Par.F2'(A,B), 'Substs.TypeSubst.F2'(C,D), E) :-
        user:one_solution(('Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),F),D)->'Substs.DerefType.P3'(F,'Substs.TypeSubst.F2'(C,D),E);E='MetaDefs.Par.F2'(A,B))).
'Substs.Dereference1.P6'('Substs.N.C0', A, _, _, _, A).
'Substs.Dereference1.P6'('Substs.R.F1'(A), _, B, C, D, E) :-
        'Substs.Contents.P4'(A, B, C, F),
        'Substs.Dereference1.P6'(F, 'MetaDefs.Var.F1'(A), B, C, D, E).
'Substs.Dereference1.P6'('Substs.V.F1'(A), _, B, C, D, E) :-
        'Substs.Dereference.P3'(A, 'Substs.TermSubst.F2'('Substs.Heap.F2'(B,C),D), E).
'Substs.Dereference1.P6'('Substs.T.F1'(A), _, _, _, _, A).
'Substs.Dereference.P3'('MetaDefs.Occ.F1'(A), _, A).
'Substs.Dereference.P3'('MetaDefs.Var.F1'(A), 'Substs.TermSubst.F2'('Substs.Heap.F2'(B,C),D), E) :-
        'Substs.Contents.P4'(A, B, C, F),
        'Substs.Dereference1.P6'(F, 'MetaDefs.Var.F1'(A), B, C, D, E).
'Substs.Dereference.P3'('MetaDefs.Term.F2'(A,B), _, 'MetaDefs.Term.F2'(A,B)).
'Substs.Dereference.P3'('MetaDefs.CTerm.F1'(A), _, 'MetaDefs.CTerm.F1'(A)).
'Substs.Dereference.P3'('MetaDefs.XTerm.F3'(A,B,C), _, 'MetaDefs.XTerm.F3'(A,B,C)).
'Substs.Dereference.P3'('MetaDefs.XCTerm.F2'(A,B), _, 'MetaDefs.XCTerm.F2'(A,B)).
'Substs.Dereference.P3'('MetaDefs.Str.F1'(A), _, 'MetaDefs.Str.F1'(A)).
'Substs.Dereference.P3'('MetaDefs.Int.F1'(A), _, 'MetaDefs.Int.F1'(A)).
'Substs.Dereference.P3'('MetaDefs.Prm.F1'(A), _, 'MetaDefs.Prm.F1'(A)).
'Substs.Dereference.P3'('MetaDefs.SuchThat.F2'(A,B), _, 'MetaDefs.SuchThat.F2'(A,B)).
'Substs.Dereference.P3'('MetaDefs.Var.F2'(A,B), 'Substs.TermSubst.F2'(C,D), E) :-
        user:one_solution(('Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),F),D)->'Substs.Dereference.P3'(F,'Substs.TermSubst.F2'(C,D),E);E='MetaDefs.Var.F2'(A,B))).
'Substs.FullDerefTerm.P4'('MetaDefs.Var.F1'(A), B, _, C) :-
        (   B='MetaDefs.Var.F1'(A) ->
            C='MetaDefs.Occ.F1'(B)
        ;   C='MetaDefs.Var.F1'(A)
        ).
'Substs.FullDerefTerm.P4'('MetaDefs.Var.F2'(A,B), C, _, D) :-
        (   C='MetaDefs.Var.F2'(A,B) ->
            D='MetaDefs.Occ.F1'(C)
        ;   D='MetaDefs.Var.F2'(A,B)
        ).
'Substs.FullDerefTerm.P4'('MetaDefs.CTerm.F1'(A), _, _, 'MetaDefs.CTerm.F1'(A)).
'Substs.FullDerefTerm.P4'('MetaDefs.Term.F2'(A,B), C, D, 'MetaDefs.Term.F2'(A,E)) :-
        'Substs.FullDerefTerm1.P4'(B, C, D, E).
'Substs.FullDerefTerm.P4'('MetaDefs.XCTerm.F2'(A,B), _, _, 'MetaDefs.XCTerm.F2'(A,B)).
'Substs.FullDerefTerm.P4'('MetaDefs.XTerm.F3'(A,B,C), D, E, 'MetaDefs.XTerm.F3'(A,F,C)) :-
        'Substs.FullDerefTerm1.P4'(B, D, E, F).
'Substs.FullDerefTerm.P4'('MetaDefs.Int.F1'(A), _, _, 'MetaDefs.Int.F1'(A)).
'Substs.FullDerefTerm.P4'('MetaDefs.Prm.F1'(A), _, _, 'MetaDefs.Prm.F1'(A)).
'Substs.FullDerefTerm.P4'('MetaDefs.Str.F1'(A), _, _, 'MetaDefs.Str.F1'(A)).
'Substs.FullDerefTerm.P4'('MetaDefs.SuchThat.F2'(A,B), _, _, 'MetaDefs.SuchThat.F2'(A,B)).
'Substs.EmptyHeap.P1'('Substs.Heap.F2'(0,'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0'))).
'Substs.FullDerefTerm1.P4'([], _, _, []).
'Substs.FullDerefTerm1.P4'([A|B], C, D, [E|F]) :-
        'Substs.ApplySubstToTerm.P4'(A, C, D, E),
        'Substs.FullDerefTerm1.P4'(B, C, D, F).
'Substs.RationaliseTypeHeap.P6'('Substs.N.C0', _, _, _, A, A).
'Substs.RationaliseTypeHeap.P6'('Substs.R.F1'(A), _, B, C, D, E) :-
        'Substs.ApplySubstToType.P4'('MetaDefs.Par.F1'(A), 'MetaDefs.Par.F1'(B), C, F),
        (   user:not_equal([], [F,B], F, 'MetaDefs.Occ.F1'('MetaDefs.Par.F1'(B))) ->
            'Substs.AddTypeBinding1.P4'(F, B, D, E)
        ;   E=D
        ).
'Substs.RationaliseTypeHeap.P6'('Substs.V.F1'(A), _, B, C, D, E) :-
        'Substs.ApplySubstToType.P4'(A, 'MetaDefs.Par.F1'(B), C, F),
        (   user:not_equal([], [F,B], F, 'MetaDefs.Occ.F1'('MetaDefs.Par.F1'(B))) ->
            'Substs.AddTypeBinding1.P4'(F, B, D, E)
        ;   E=D
        ).
'Substs.RationaliseTypeHeap.P6'('Substs.T.F1'(A), _, B, C, D, E) :-
        'Substs.ApplySubstToType.P4'(A, 'MetaDefs.Par.F1'(B), C, F),
        'Substs.AddTypeBinding1.P4'(F, B, D, E).
'Substs.RationaliseTypeHeap.P6'('Substs.H.F16'(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), Q, R, S, T, U) :-
        minus(Q, 1, W),
        V=W,
        power(16, Q, Y),
        X=Y,
        'Substs.RationaliseTypeHeap.P6'(A, V, R, S, T, Z),
        plus(X, R, A1),
        'Substs.RationaliseTypeHeap.P6'(B, V, A1, S, Z, B1),
        times(2, X, E1),
        plus(E1, R, C1),
        'Substs.RationaliseTypeHeap.P6'(C, V, C1, S, B1, D1),
        times(3, X, H1),
        plus(H1, R, F1),
        'Substs.RationaliseTypeHeap.P6'(D, V, F1, S, D1, G1),
        times(4, X, K1),
        plus(K1, R, I1),
        'Substs.RationaliseTypeHeap.P6'(E, V, I1, S, G1, J1),
        times(5, X, N1),
        plus(N1, R, L1),
        'Substs.RationaliseTypeHeap.P6'(F, V, L1, S, J1, M1),
        times(6, X, Q1),
        plus(Q1, R, O1),
        'Substs.RationaliseTypeHeap.P6'(G, V, O1, S, M1, P1),
        times(7, X, T1),
        plus(T1, R, R1),
        'Substs.RationaliseTypeHeap.P6'(H, V, R1, S, P1, S1),
        times(8, X, W1),
        plus(W1, R, U1),
        'Substs.RationaliseTypeHeap.P6'(I, V, U1, S, S1, V1),
        times(9, X, Z1),
        plus(Z1, R, X1),
        'Substs.RationaliseTypeHeap.P6'(J, V, X1, S, V1, Y1),
        times(10, X, C2),
        plus(C2, R, A2),
        'Substs.RationaliseTypeHeap.P6'(K, V, A2, S, Y1, B2),
        times(11, X, F2),
        plus(F2, R, D2),
        'Substs.RationaliseTypeHeap.P6'(L, V, D2, S, B2, E2),
        times(12, X, I2),
        plus(I2, R, G2),
        'Substs.RationaliseTypeHeap.P6'(M, V, G2, S, E2, H2),
        times(13, X, L2),
        plus(L2, R, J2),
        'Substs.RationaliseTypeHeap.P6'(N, V, J2, S, H2, K2),
        times(14, X, O2),
        plus(O2, R, M2),
        'Substs.RationaliseTypeHeap.P6'(O, V, M2, S, K2, N2),
        times(15, X, Q2),
        plus(Q2, R, P2),
        'Substs.RationaliseTypeHeap.P6'(P, V, P2, S, N2, U).
'Substs.HeapTerm.P2'('Substs.R.F1'(A), 'MetaDefs.Var.F1'(A)).
'Substs.HeapTerm.P2'('Substs.V.F1'(A), A).
'Substs.HeapTerm.P2'('Substs.T.F1'(A), A).
'Substs.GetConstant1.P4'('MetaDefs.Var.F1'(A), B, 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,D)) :-
        'Substs.AddBinding.P4'(A, 'Substs.T.F1'(B), C, E).
'Substs.GetConstant1.P4'('MetaDefs.Var.F2'(A,B), C, 'Substs.TermSubst.F2'(D,E), 'Substs.TermSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),C)|E])).
'Substs.GetConstant1.P4'('MetaDefs.CTerm.F1'(A), 'MetaDefs.CTerm.F1'(A), B, B).
'Substs.GetConstant1.P4'('MetaDefs.XCTerm.F2'(A,B), 'MetaDefs.XCTerm.F2'(A,B), C, C).
'Substs.GetConstant1.P4'('MetaDefs.Int.F1'(A), 'MetaDefs.Int.F1'(A), B, B).
'Substs.GetConstant1.P4'('MetaDefs.Prm.F1'(A), 'MetaDefs.Prm.F1'(A), B, B).
'Substs.GetConstant1.P4'('MetaDefs.Str.F1'(A), 'MetaDefs.Str.F1'(A), B, B).
'Substs.GetBase.P4'(A, B, C, D) :-
        'Substs.DerefType.P3'(A, C, E),
        'Substs.GetBase1.P4'(E, B, C, D).
'Substs.FullDereference.P3'('MetaDefs.Var.F2'(A,B), _, 'MetaDefs.Var.F2'(A,B)).
'Substs.FullDereference.P3'('MetaDefs.Var.F1'(A), _, 'MetaDefs.Var.F1'(A)).
'Substs.FullDereference.P3'('MetaDefs.Term.F2'(A,B), C, 'MetaDefs.Term.F2'(A,D)) :-
        'Substs.ApplyTermSubst.P3'(B, C, D).
'Substs.FullDereference.P3'('MetaDefs.CTerm.F1'(A), _, 'MetaDefs.CTerm.F1'(A)).
'Substs.FullDereference.P3'('MetaDefs.XTerm.F3'(A,B,C), D, 'MetaDefs.XTerm.F3'(A,E,C)) :-
        'Substs.ApplyTermSubst.P3'(B, D, E).
'Substs.FullDereference.P3'('MetaDefs.XCTerm.F2'(A,B), _, 'MetaDefs.XCTerm.F2'(A,B)).
'Substs.FullDereference.P3'('MetaDefs.Str.F1'(A), _, 'MetaDefs.Str.F1'(A)).
'Substs.FullDereference.P3'('MetaDefs.Int.F1'(A), _, 'MetaDefs.Int.F1'(A)).
'Substs.FullDereference.P3'('MetaDefs.Prm.F1'(A), _, 'MetaDefs.Prm.F1'(A)).
'Substs.FullDereference.P3'('MetaDefs.SuchThat.F2'(A,B), _, 'MetaDefs.SuchThat.F2'(A,B)).
'Substs.FullDerefType1.P4'([], _, _, []).
'Substs.FullDerefType1.P4'([A|B], C, D, [E|F]) :-
        'Substs.ApplySubstToType.P4'(A, C, D, E),
        'Substs.FullDerefType1.P4'(B, C, D, F).
'Substs.GetConstant.P4'(A, B, C, D) :-
        'Substs.Dereference.P3'(A, C, E),
        'Substs.GetConstant1.P4'(E, B, C, D).
'Substs.GetBase1.P4'('MetaDefs.Par.F1'(A), B, 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,D)) :-
        'Substs.AddBinding.P4'(A, 'Substs.T.F1'(B), C, E).
'Substs.GetBase1.P4'('MetaDefs.Par.F2'(A,B), C, 'Substs.TypeSubst.F2'(D,E), 'Substs.TypeSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),C)|E])).
'Substs.GetBase1.P4'('MetaDefs.BType.F1'(A), 'MetaDefs.BType.F1'(A), B, B).
'Substs.GetBase1.P4'('MetaDefs.XBType.F1'(A), 'MetaDefs.XBType.F1'(A), B, B).
'Substs.GetType.P5'(A, B, C, D, E) :-
        'Substs.DerefType.P3'(A, D, F),
        'Substs.GetType1.P5'(F, B, C, D, E).
'Substs.GetFunction.P5'(A, B, C, D, E) :-
        'Substs.Dereference.P3'(A, D, F),
        'Substs.GetFunction1.P5'(F, B, C, D, E).
'Substs.GetFunction1.P5'('MetaDefs.Var.F1'(A), B, 'Substs.Write.C0', 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,D)) :-
        'Substs.AddBinding.P4'(A, 'Substs.T.F1'(B), C, E).
'Substs.GetFunction1.P5'('MetaDefs.Var.F2'(A,B), C, 'Substs.Write.C0', 'Substs.TermSubst.F2'(D,E), 'Substs.TermSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),C)|E])).
'Substs.GetFunction1.P5'('MetaDefs.Term.F2'(A,B), 'MetaDefs.Term.F2'(A,B), 'Substs.Read.C0', C, C).
'Substs.GetFunction1.P5'('MetaDefs.XTerm.F3'(A,B,C), 'MetaDefs.XTerm.F3'(A,B,C), 'Substs.Read.C0', D, D).
'Substs.GetType1.P5'('MetaDefs.Par.F1'(A), B, 'Substs.Write.C0', 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,D)) :-
        'Substs.AddBinding.P4'(A, 'Substs.T.F1'(B), C, E).
'Substs.GetType1.P5'('MetaDefs.Par.F2'(A,B), C, 'Substs.Write.C0', 'Substs.TypeSubst.F2'(D,E), 'Substs.TypeSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),C)|E])).
'Substs.GetType1.P5'('MetaDefs.Type.F2'(A,B), 'MetaDefs.Type.F2'(A,B), 'Substs.Read.C0', C, C).
'Substs.GetType1.P5'('MetaDefs.XType.F2'(A,B), 'MetaDefs.XType.F2'(A,B), 'Substs.Read.C0', C, C).
'Substs.ParameterInSubst.P3'('Substs.TypeSubst.F2'(_,A), B, C) :-
        'Lists.Member.P2'('MetaDefs.!.F2'(B,C), A).
'Substs.ParameterInSubst.P3'('Substs.TypeSubst.F2'(A,_), 'MetaDefs.Par.F1'(B), C) :-
        'Substs.BindingInHeap.P3'(A, B, D),
        'Substs.HeapType.P2'(D, C).
'Substs.HeapType.P2'('Substs.R.F1'(A), 'MetaDefs.Par.F1'(A)).
'Substs.HeapType.P2'('Substs.V.F1'(A), A).
'Substs.HeapType.P2'('Substs.T.F1'(A), A).
'Substs.RationaliseTermHeap.P6'('Substs.N.C0', _, _, _, A, A).
'Substs.RationaliseTermHeap.P6'('Substs.R.F1'(A), _, B, C, D, E) :-
        'Substs.ApplySubstToTerm.P4'('MetaDefs.Var.F1'(A), 'MetaDefs.Var.F1'(B), C, F),
        (   user:not_equal([], [F,B], F, 'MetaDefs.Occ.F1'('MetaDefs.Var.F1'(B))) ->
            'Substs.AddTermBinding1.P4'(F, B, D, E)
        ;   E=D
        ).
'Substs.RationaliseTermHeap.P6'('Substs.V.F1'(A), _, B, C, D, E) :-
        'Substs.ApplySubstToTerm.P4'(A, 'MetaDefs.Var.F1'(B), C, F),
        (   user:not_equal([], [F,B], F, 'MetaDefs.Occ.F1'('MetaDefs.Var.F1'(B))) ->
            'Substs.AddTermBinding1.P4'(F, B, D, E)
        ;   E=D
        ).
'Substs.RationaliseTermHeap.P6'('Substs.T.F1'(A), _, B, C, D, E) :-
        'Substs.ApplySubstToTerm.P4'(A, 'MetaDefs.Var.F1'(B), C, F),
        'Substs.AddTermBinding1.P4'(F, B, D, E).
'Substs.RationaliseTermHeap.P6'('Substs.H.F16'(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), Q, R, S, T, U) :-
        minus(Q, 1, W),
        V=W,
        power(16, Q, Y),
        X=Y,
        'Substs.RationaliseTermHeap.P6'(A, V, R, S, T, Z),
        plus(X, R, A1),
        'Substs.RationaliseTermHeap.P6'(B, V, A1, S, Z, B1),
        times(2, X, E1),
        plus(E1, R, C1),
        'Substs.RationaliseTermHeap.P6'(C, V, C1, S, B1, D1),
        times(3, X, H1),
        plus(H1, R, F1),
        'Substs.RationaliseTermHeap.P6'(D, V, F1, S, D1, G1),
        times(4, X, K1),
        plus(K1, R, I1),
        'Substs.RationaliseTermHeap.P6'(E, V, I1, S, G1, J1),
        times(5, X, N1),
        plus(N1, R, L1),
        'Substs.RationaliseTermHeap.P6'(F, V, L1, S, J1, M1),
        times(6, X, Q1),
        plus(Q1, R, O1),
        'Substs.RationaliseTermHeap.P6'(G, V, O1, S, M1, P1),
        times(7, X, T1),
        plus(T1, R, R1),
        'Substs.RationaliseTermHeap.P6'(H, V, R1, S, P1, S1),
        times(8, X, W1),
        plus(W1, R, U1),
        'Substs.RationaliseTermHeap.P6'(I, V, U1, S, S1, V1),
        times(9, X, Z1),
        plus(Z1, R, X1),
        'Substs.RationaliseTermHeap.P6'(J, V, X1, S, V1, Y1),
        times(10, X, C2),
        plus(C2, R, A2),
        'Substs.RationaliseTermHeap.P6'(K, V, A2, S, Y1, B2),
        times(11, X, F2),
        plus(F2, R, D2),
        'Substs.RationaliseTermHeap.P6'(L, V, D2, S, B2, E2),
        times(12, X, I2),
        plus(I2, R, G2),
        'Substs.RationaliseTermHeap.P6'(M, V, G2, S, E2, H2),
        times(13, X, L2),
        plus(L2, R, J2),
        'Substs.RationaliseTermHeap.P6'(N, V, J2, S, H2, K2),
        times(14, X, O2),
        plus(O2, R, M2),
        'Substs.RationaliseTermHeap.P6'(O, V, M2, S, K2, N2),
        times(15, X, Q2),
        plus(Q2, R, P2),
        'Substs.RationaliseTermHeap.P6'(P, V, P2, S, N2, U).
'Substs.RationaliseTermList.P3'([], [], _).
'Substs.RationaliseTermList.P3'(['MetaDefs.!.F2'(B,C)|A], D, E) :-
        'Substs.ApplySubstToTerm.P4'(C, B, E, F),
        (   user:not_equal([], [C,B], C, 'MetaDefs.Occ.F1'(B)) ->
            D=['MetaDefs.!.F2'(B,F)|G]
        ;   D=G
        ),
        'Substs.RationaliseTermList.P3'(A, G, E).
'Substs.SubstsComposeType.P3'(A, B, C) :-
        'Substs.ComposeTypeSubsts1.P3'(A, A, D),
        'Substs.ComposeTypeSubsts1.P3'(D, B, E),
        'Substs.ComposeTypeSubsts1.P3'(B, B, F),
        'Substs.ComposeTypeSubsts2.P3'(F, E, C).
'Substs.SubstApplyToType.P3'(A, B, C) :-
        'Substs.DerefType.P3'(A, B, D),
        'Substs.FullDerefType.P3'(D, B, C).
'Substs.RationaliseTypeList.P3'([], [], _).
'Substs.RationaliseTypeList.P3'(['MetaDefs.!.F2'(B,C)|A], D, E) :-
        'Substs.ApplySubstToType.P4'(C, B, E, F),
        (   user:not_equal([], [C,B], C, 'MetaDefs.Occ.F1'(B)) ->
            D=['MetaDefs.!.F2'(B,F)|G]
        ;   D=G
        ),
        'Substs.RationaliseTypeList.P3'(A, G, E).
'Substs.SubstApplyToTerm.P3'(A, B, C) :-
        'Substs.Dereference.P3'(A, B, D),
        'Substs.FullDereference.P3'(D, B, C).
'Substs.SubstsComposeTerm.P3'(A, B, C) :-
        'Substs.ComposeTermSubsts1.P3'(A, A, D),
        'Substs.ComposeTermSubsts1.P3'(D, B, E),
        'Substs.ComposeTermSubsts1.P3'(B, B, F),
        'Substs.ComposeTermSubsts2.P3'(F, E, C).
'Substs.UnifyParameter.P5'('Substs.Write.C0', A, A, B, B).
'Substs.UnifyParameter.P5'('Substs.Read.C0', A, B, C, D) :-
        (   user:not_equal([], [A,B], A, B) ->
            'Substs.BindParameter.P4'(B, A, C, D)
        ;   D=C
        ).
'Substs.UnifyConstant.P5'('Substs.Write.C0', A, A, B, B).
'Substs.UnifyConstant.P5'('Substs.Read.C0', A, B, C, D) :-
        'Substs.Dereference.P3'(A, C, E),
        'Substs.GetConstant1.P4'(E, B, C, D).
'Substs.UnifyBase.P5'('Substs.Write.C0', A, A, B, B).
'Substs.UnifyBase.P5'('Substs.Read.C0', A, B, C, D) :-
        'Substs.DerefType.P3'(A, C, E),
        'Substs.GetBase1.P4'(E, B, C, D).
'Substs.UnifyKnownVariable.P5'('Substs.Write.C0', A, A, B, B).
'Substs.UnifyKnownVariable.P5'('Substs.Read.C0', A, B, C, D) :-
        (   user:not_equal([], [A,B], A, B) ->
            'Substs.BindVariable.P4'(B, A, C, D)
        ;   D=C
        ).
'Substs.UnifyFunction.P6'('Substs.Write.C0', A, A, 'Substs.Write.C0', B, B).
'Substs.UnifyFunction.P6'('Substs.Read.C0', A, B, C, D, E) :-
        'Substs.Dereference.P3'(A, D, F),
        'Substs.GetFunction1.P5'(F, B, C, D, E).
'Substs.UnifyVariable.P4'('Substs.Write.C0', 'MetaDefs.Var.F1'(A), A, B) :-
        plus(A, 1, B),
        true.
'Substs.UnifyVariable.P4'('Substs.Read.C0', _, A, A).
'Substs.UnifyType.P6'('Substs.Write.C0', A, A, 'Substs.Write.C0', B, B).
'Substs.UnifyType.P6'('Substs.Read.C0', A, B, C, D, E) :-
        'Substs.DerefType.P3'(A, D, F),
        'Substs.GetType1.P5'(F, B, C, D, E).
'Substs.VariableInSubst.P3'('Substs.TermSubst.F2'(_,A), B, C) :-
        'Lists.Member.P2'('MetaDefs.!.F2'(B,C), A).
'Substs.VariableInSubst.P3'('Substs.TermSubst.F2'(A,_), 'MetaDefs.Var.F1'(B), C) :-
        'Substs.BindingInHeap.P3'(A, B, D),
        'Substs.HeapTerm.P2'(D, C).
%	---------------------------------------------------
%	---------------------------------------------------
%	Fast Prolog code for Substs bindings.
%	---------------------------------------------------
%	---------------------------------------------------


'Substs.AddBinding.P4'(Index,Term,'Substs.Heap.F2'(Exp,Heap)
				,'Substs.Heap.F2'(Exp1,Heap1)) :-
	Exp2 is Exp+1,
	power16(Exp2,Pow),
	( Index < Pow ->
		Exp1 = Exp,
		'Substs.AddBinding1.P5'(Heap,Index,Exp,Term,Heap1)
	;	newbinding(Index,Exp2,Exp1,Term,Heap,Heap1)).

'~Substs.AddBinding.P4'(A, B, C, D) :-
	'Substs.AddBinding.P4'(A, B, C, D).


newbinding(Index,Exp,Exp1,Term,Heap,Heap1) :-
	Exp2 is Exp+1,
	power16(Exp2,Pow),
	( Index < Pow ->
		'Substs.AddBinding1.P5'('Substs.H.F16'(Heap,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0'),Index,Exp,Term,Heap1),
		Exp1 = Exp
	;	newbinding(Index,Exp2,Exp1,Term,'Substs.H.F16'(Heap,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0'),Heap1)).


'Substs.AddBinding1.P5'('Substs.H.F16'(H0,H1,H2,H3,H4,H5,H6,H7,H8,H9,HA,HB,HC,HD,HE,HF),Index,Exp,Term,Heap1) :-
	power16(Exp,Pow),
	Div is Index // Pow,
	Mod is Index mod Pow,
	Exp1 is Exp-1,
	'Substs.AddBinding2.P21'(Div,Mod,Exp1,Term,H0,H1,H2,H3,H4,H5,H6,H7,H8,H9,HA,HB,HC,HD,HE,HF,Heap1).

'Substs.AddBinding1.P5'('Substs.N.C0',Index,Exp,Term,Heap1) :-
	'Substs.AddNewBinding1.P4'(Index,Exp,Term,Heap1).

'Substs.AddBinding1.P5'('Substs.R.F1'(_),_,_,Term,Term).
	       
'Substs.AddBinding1.P5'('Substs.V.F1'(_),_,_,Term,Term).
	       
'Substs.AddBinding1.P5'('Substs.T.F1'(_),_,_,Term,Term).
	       
'~Substs.AddBinding1.P5'(Heap,Index,Exp,Term,Heap1) :-
	'Substs.AddBinding1.P5'(Heap,Index,Exp,Term,Heap1).


'Substs.AddNewBinding1.P4'(Index,Exp,Term,Heap) :-
	( Exp =  -1 ->
		Heap = Term
	;	power16(Exp,Pow),
		Div is Index // Pow,
		Mod is Index mod Pow,
		Exp1 is Exp-1,
		'Substs.AddNewBinding.P5'(Div,Mod,Exp1,Term,Heap)).

'~Substs.AddNewBinding1.P4'(A, B, C, D) :-
	'Substs.AddNewBinding1.P4'(A, B, C, D).


'Substs.Contents.P4'(Index,Exp,Heap,Value) :-
	Index1 is Index+1,
	Exp1 is Exp+1,
	power16(Exp1,Pow),
	( Index1 > Pow ->
		Value = 'Substs.N.C0'
	;	contents(Heap,Index,Exp,Value)).

'~Substs.Contents.P4'(Index,Exp,Heap,Value) :-
	'Substs.Contents.P4'(Index,Exp,Heap,Value).


contents(Heap,Indx,Exp,Val):-
	( Heap = 'Substs.N.C0' ->
		Val = 'Substs.N.C0'
	;	( Exp = 0 ->
			Indx1 is Indx+1,
			arg(Indx1,Heap,Val)
		;	power16(Exp,Pow),
			Div is Indx // Pow,
			Div1 is Div+1,
			arg(Div1,Heap,V1),
			Mod is Indx mod Pow,
			Exp1 is Exp-1,
			contents(V1,Mod,Exp1,Val))).
	

'Substs.Address.P5'(Heap, Exp, A, A1, Term) :-
  (  Heap = 'Substs.N.C0' ->
       fail
  ;    ( Heap = 'Substs.V.F1'(_) ->
            A = A1,
            Term = Heap
       ;    ( Heap = 'Substs.R.F1'(_) ->
                 A = A1,
                 Term = Heap
            ;    ( Heap = 'Substs.T.F1'(_) ->
                     A = A1,
                     Term = Heap
                 ;   power16(Exp,Pow),
                     Exp1 is Exp-1,
                     member3(Arg,1,[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]),
                     Arg1 is Arg-1,
                     A2 is A + (Pow * Arg1),
                     arg(Arg,Heap,Heap1),
                     'Substs.Address.P5'(Heap1,Exp1,A2,A1,Term) )))).

'~Substs.Address.P5'(Heap, Exp, A, A1, Term) :-
	'Substs.Address.P5'(Heap, Exp, A, A1, Term).

power16(0,1).
power16(1,16).
power16(2,256).
power16(3,4096).
power16(4,65536).
%power16(5,1048576).
%power10(6,16777216).
%power10(7,268435456).
%power10(8,4294967296).

member3(X,X,_).

member3(X,_,[Y|L]) :-
	member3(X,Y,L).

%------------------------------------------------------------------------------
% Predicates required by Syntax (moved here from Syntax.sup)
%------------------------------------------------------------------------------
 
% Avoid unnecessary choicepoints

'Substs.SBindingInTypeSubst.P3'(Subst, Var, Type) :-
        ( var(Var) ->
                'Substs':'Substs.ParameterInSubst.P3'(Subst, Var, Type1),
                'Substs':'Substs.SubstApplyToType.P3'(Type1, Subst, Type)
                ; 'Substs':'Substs.SubstApplyToType.P3'(Var, Subst, Type),
		  Type \== Var
        ).

'~Substs.SBindingInTypeSubst.P3'(Subst, Param, Type) :-
   'Substs.SBindingInTypeSubst.P3'(Subst, Param, Type).


'Substs.SBindingInTermSubst.P3'(Subst, Var, Term) :-
        ( var(Var) ->
                'Substs':'Substs.VariableInSubst.P3'(Subst, Var, Term1),
                'Substs':'Substs.SubstApplyToTerm.P3'(Term1, Subst, Term)
                ; 'Substs':'Substs.SubstApplyToTerm.P3'(Var, Subst, Term),
		  Term \== Var
        ).

'~Substs.SBindingInTermSubst.P3'(Subst, Param, Term) :-
   'Substs.SBindingInTermSubst.P3'(Subst, Param, Term).


'Substs.SDelBindingInTypeSubst.P4'(Subst, Var, Type, NewSubst) :-
   ( var(Var) ->
	'Substs':'Substs.ParameterInSubst.P3'(Subst, Var, _),
	'Substs':'Substs.DelTypeBinding.P4'(Var, Type, Subst, NewSubst)
     ;  'Substs':'Substs.DelTypeBinding.P4'(Var, Type, Subst, NewSubst)
   ).

'~Substs.SDelBindingInTypeSubst.P4'(Subst, Var, Type, NewSubst) :-
   'Substs.SDelBindingInTypeSubst.P4'(Subst, Var, Type, NewSubst).


'Substs.SDelBindingInTermSubst.P4'(Subst, Var, Term, NewSubst) :-
   ( var(Var) ->
	'Substs':'Substs.VariableInSubst.P3'(Subst, Var, _),
	'Substs':'Substs.DelTermBinding.P4'(Var, Term, Subst, NewSubst)
     ;  'Substs':'Substs.DelTermBinding.P4'(Var, Term, Subst, NewSubst)
   ).

'~Substs.SDelBindingInTermSubst.P4'(Subst, Var, Term, NewSubst) :-
   'Substs.SDelBindingInTermSubst.P4'(Subst, Var, Term, NewSubst).

