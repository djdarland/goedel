:- module('SharedSyntax', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

'SharedSyntax.STermMaxVarIndex.P2'(A, B) :-
        'Integers':plus(C, 1, B),
        'Integers':negative(1, D),
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, D, C).
'~SharedSyntax.STermMaxVarIndex.P2'(A, B) :-
        'Integers':plus(C, 1, B),
        'Integers':negative(1, D),
        '~SharedSyntax.MaxVarIndexInTerms.P3'(A, D, C).
'SharedSyntax.MaxVarIndexInTerms.P3'([], A, A).
'~SharedSyntax.MaxVarIndexInTerms.P3'([], A, A).
'SharedSyntax.MaxVarIndexInTerms.P3'([A|B], C, D) :-
        'SharedSyntax.MaxVarIndexInTerm.P3'(A, C, E),
        'SharedSyntax.MaxVarIndexInTerms.P3'(B, E, D).
'~SharedSyntax.MaxVarIndexInTerms.P3'([A|B], C, D) :-
        '~SharedSyntax.MaxVarIndexInTerm.P3'(A, C, E),
        '~SharedSyntax.MaxVarIndexInTerms.P3'(B, E, D).
'SharedSyntax.MaxVarIndex.P3'([], A, A).
'~SharedSyntax.MaxVarIndex.P3'([], A, A).
'SharedSyntax.MaxVarIndex.P3'([A|B], C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex.P3'(B, E, D).
'~SharedSyntax.MaxVarIndex.P3'([A|B], C, D) :-
        '~SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        '~SharedSyntax.MaxVarIndex.P3'(B, E, D).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Par.F1'(A), B, C, 'MetaDefs.Par.F2'(D,E)) :-
        'SharedSyntax.SNotNewBinding.P4'(B, C, 'MetaDefs.Par.F1'(A), 'MetaDefs.Par.F2'(D,E)).
'~SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Par.F1'(A), B, C, 'MetaDefs.Par.F2'(D,E)) :-
        '~SharedSyntax.SNotNewBinding.P4'(B, C, 'MetaDefs.Par.F1'(A), 'MetaDefs.Par.F2'(D,E)).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Par.F1'(A), B, C, 'MetaDefs.Par.F1'(D)) :-
        'SharedSyntax.SNotNewBinding.P4'(B, C, 'MetaDefs.Par.F1'(A), 'MetaDefs.Par.F1'(D)).
'~SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Par.F1'(A), B, C, 'MetaDefs.Par.F1'(D)) :-
        '~SharedSyntax.SNotNewBinding.P4'(B, C, 'MetaDefs.Par.F1'(A), 'MetaDefs.Par.F1'(D)).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Par.F2'(A,B), C, D, 'MetaDefs.Par.F1'(E)) :-
        'SharedSyntax.SNotNewBinding.P4'(C, D, 'MetaDefs.Par.F2'(A,B), 'MetaDefs.Par.F1'(E)).
'~SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Par.F2'(A,B), C, D, 'MetaDefs.Par.F1'(E)) :-
        '~SharedSyntax.SNotNewBinding.P4'(C, D, 'MetaDefs.Par.F2'(A,B), 'MetaDefs.Par.F1'(E)).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Par.F2'(A,B), C, D, 'MetaDefs.Par.F2'(E,F)) :-
        'SharedSyntax.SNotNewBinding.P4'(C, D, 'MetaDefs.Par.F2'(A,B), 'MetaDefs.Par.F2'(E,F)).
'~SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Par.F2'(A,B), C, D, 'MetaDefs.Par.F2'(E,F)) :-
        '~SharedSyntax.SNotNewBinding.P4'(C, D, 'MetaDefs.Par.F2'(A,B), 'MetaDefs.Par.F2'(E,F)).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.BType.F1'(A), B, B, 'MetaDefs.BType.F1'(A)).
'~SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.BType.F1'(A), B, B, 'MetaDefs.BType.F1'(A)).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Type.F2'(A,B), C, D, 'MetaDefs.Type.F2'(A,E)) :-
        'SharedSyntax.SCheckVariantTypes.P4'(B, C, D, E).
'~SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.Type.F2'(A,B), C, D, 'MetaDefs.Type.F2'(A,E)) :-
        '~SharedSyntax.SCheckVariantTypes.P4'(B, C, D, E).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.XBType.F1'(A), B, B, 'MetaDefs.XBType.F1'(A)).
'~SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.XBType.F1'(A), B, B, 'MetaDefs.XBType.F1'(A)).
'SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.XType.F2'(A,B), C, D, 'MetaDefs.XType.F2'(A,E)) :-
        'SharedSyntax.SCheckVariantTypes.P4'(B, C, D, E).
'~SharedSyntax.CheckVariantTypes1.P4'('MetaDefs.XType.F2'(A,B), C, D, 'MetaDefs.XType.F2'(A,E)) :-
        '~SharedSyntax.SCheckVariantTypes.P4'(B, C, D, E).
'SharedSyntax.ArgFreeVars.P2'([], []).
'~SharedSyntax.ArgFreeVars.P2'([], []).
'SharedSyntax.ArgFreeVars.P2'([A|B], C) :-
        'SharedSyntax.STermFreeVars.P2'(A, D),
        'SharedSyntax.ArgFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'~SharedSyntax.ArgFreeVars.P2'([A|B], C) :-
        '~SharedSyntax.STermFreeVars.P2'(A, D),
        '~SharedSyntax.ArgFreeVars.P2'(B, E),
        '~SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.GetVariable.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), (user:not_equal([],[A,B],A,B),true->'SharedSyntax':'SharedSyntax.TermNotOccur.P2'(A,B),'Substs':'Substs.BindVariable.P4'(B,A,C,D);D=C)).
'~SharedSyntax.GetVariable.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), (user:not_equal([],[A,B],A,B),true->'SharedSyntax':'~SharedSyntax.TermNotOccur.P2'(A,B),'Substs':'~Substs.BindVariable.P4'(B,A,C,D);D=C)).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.Empty.C0', A, A).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.Empty.C0', A, A).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.PAtom.F1'(_), A, A).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.PAtom.F1'(_), A, A).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.Atom.F2'(_,A), B, C) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, B, C).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.Atom.F2'(_,A), B, C) :-
        '~SharedSyntax.MaxVarIndexInTerms.P3'(A, B, C).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.&''.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.&''.F2'(A,B), C, D) :-
        '~SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        '~SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.\/''.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.\/''.F2'(A,B), C, D) :-
        '~SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        '~SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.->''.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.->''.F2'(A,B), C, D) :-
        '~SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        '~SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.<-''.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.<-''.F2'(A,B), C, D) :-
        '~SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        '~SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.<->''.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.<->''.F2'(A,B), C, D) :-
        '~SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        '~SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.~''.F1'(A), B, C) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, B, C).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.~''.F1'(A), B, C) :-
        '~SharedSyntax.MaxVarIndex1.P3'(A, B, C).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.Some.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.Some.F2'(A,B), C, D) :-
        '~SharedSyntax.MaxVarIndexInTerms.P3'(A, C, E),
        '~SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.All.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.All.F2'(A,B), C, D) :-
        '~SharedSyntax.MaxVarIndexInTerms.P3'(A, C, E),
        '~SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.IT.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.IT.F2'(A,B), C, D) :-
        '~SharedSyntax.MaxVarIndex1.P3'(A, C, E),
        '~SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.IST.F3'(A,B,C), D, E) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, D, F),
        'SharedSyntax.MaxVarIndex1.P3'(B, F, G),
        'SharedSyntax.MaxVarIndex1.P3'(C, G, E).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.IST.F3'(A,B,C), D, E) :-
        '~SharedSyntax.MaxVarIndexInTerms.P3'(A, D, F),
        '~SharedSyntax.MaxVarIndex1.P3'(B, F, G),
        '~SharedSyntax.MaxVarIndex1.P3'(C, G, E).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.ITE.F3'(A,B,C), D, E) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, D, F),
        'SharedSyntax.MaxVarIndex1.P3'(B, F, G),
        'SharedSyntax.MaxVarIndex1.P3'(C, G, E).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.ITE.F3'(A,B,C), D, E) :-
        '~SharedSyntax.MaxVarIndex1.P3'(A, D, F),
        '~SharedSyntax.MaxVarIndex1.P3'(B, F, G),
        '~SharedSyntax.MaxVarIndex1.P3'(C, G, E).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.ISTE.F4'(A,B,C,D), E, F) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, E, G),
        'SharedSyntax.MaxVarIndex1.P3'(B, G, H),
        'SharedSyntax.MaxVarIndex1.P3'(C, H, I),
        'SharedSyntax.MaxVarIndex1.P3'(D, I, F).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.ISTE.F4'(A,B,C,D), E, F) :-
        '~SharedSyntax.MaxVarIndexInTerms.P3'(A, E, G),
        '~SharedSyntax.MaxVarIndex1.P3'(B, G, H),
        '~SharedSyntax.MaxVarIndex1.P3'(C, H, I),
        '~SharedSyntax.MaxVarIndex1.P3'(D, I, F).
'SharedSyntax.MaxVarIndex1.P3'('MetaDefs.Commit.F2'(_,A), B, C) :-
        'SharedSyntax.MaxVarIndex1.P3'(A, B, C).
'~SharedSyntax.MaxVarIndex1.P3'('MetaDefs.Commit.F2'(_,A), B, C) :-
        '~SharedSyntax.MaxVarIndex1.P3'(A, B, C).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Var.F1'(A), B, C) :-
        user:goedel_freeze(ground([B,A]), ('Integers':'~Integers.>.P2'(A,B),true->C=A;C=B)).
'~SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Var.F1'(A), B, C) :-
        user:goedel_freeze(ground([B,A]), ('Integers':'~Integers.>.P2'(A,B),true->C=A;C=B)).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Var.F2'(_,A), B, C) :-
        user:goedel_freeze(ground([B,A]), ('Integers':'~Integers.>.P2'(A,B),true->C=A;C=B)).
'~SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Var.F2'(_,A), B, C) :-
        user:goedel_freeze(ground([B,A]), ('Integers':'~Integers.>.P2'(A,B),true->C=A;C=B)).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.CTerm.F1'(_), A, A).
'~SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.CTerm.F1'(_), A, A).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.XCTerm.F2'(_,_), A, A).
'~SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.XCTerm.F2'(_,_), A, A).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Term.F2'(_,A), B, C) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, B, C).
'~SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Term.F2'(_,A), B, C) :-
        '~SharedSyntax.MaxVarIndexInTerms.P3'(A, B, C).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.XTerm.F3'(_,A,_), B, C) :-
        'SharedSyntax.MaxVarIndexInTerms.P3'(A, B, C).
'~SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.XTerm.F3'(_,A,_), B, C) :-
        '~SharedSyntax.MaxVarIndexInTerms.P3'(A, B, C).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Int.F1'(_), A, A).
'~SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Int.F1'(_), A, A).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Str.F1'(_), A, A).
'~SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Str.F1'(_), A, A).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Prm.F1'(_), A, A).
'~SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.Prm.F1'(_), A, A).
'SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.SuchThat.F2'(A,B), C, D) :-
        'SharedSyntax.MaxVarIndexInTerm.P3'(A, C, E),
        'SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'~SharedSyntax.MaxVarIndexInTerm.P3'('MetaDefs.SuchThat.F2'(A,B), C, D) :-
        '~SharedSyntax.MaxVarIndexInTerm.P3'(A, C, E),
        '~SharedSyntax.MaxVarIndex1.P3'(B, E, D).
'SharedSyntax.SFormulaMaxVarIndex.P2'(A, B) :-
        'Integers':plus(C, 1, B),
        'Integers':negative(1, D),
        'SharedSyntax.MaxVarIndex.P3'(A, D, C).
'~SharedSyntax.SFormulaMaxVarIndex.P2'(A, B) :-
        'Integers':plus(C, 1, B),
        'Integers':negative(1, D),
        '~SharedSyntax.MaxVarIndex.P3'(A, D, C).
'SharedSyntax.SEmptyTermSubst.P1'('Substs.TermSubst.F2'(A,[])) :-
        'Substs':'Substs.EmptyHeap.P1'(A).
'~SharedSyntax.SEmptyTermSubst.P1'('Substs.TermSubst.F2'(A,[])) :-
        'Substs':'~Substs.EmptyHeap.P1'(A).
'SharedSyntax.SCheckVariantTypes.P4'([], A, A, []).
'~SharedSyntax.SCheckVariantTypes.P4'([], A, A, []).
'SharedSyntax.SCheckVariantTypes.P4'([A|B], C, D, [E|F]) :-
        'SharedSyntax.CheckVariantTypes1.P4'(A, C, G, E),
        'SharedSyntax.SCheckVariantTypes.P4'(B, G, D, F).
'~SharedSyntax.SCheckVariantTypes.P4'([A|B], C, D, [E|F]) :-
        '~SharedSyntax.CheckVariantTypes1.P4'(A, C, G, E),
        '~SharedSyntax.SCheckVariantTypes.P4'(B, G, D, F).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.Empty.C0', []).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.Empty.C0', []).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.PAtom.F1'(_), []).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.PAtom.F1'(_), []).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.Atom.F2'(_,A), B) :-
        'SharedSyntax.ArgFreeVars.P2'(A, B).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.Atom.F2'(_,A), B) :-
        '~SharedSyntax.ArgFreeVars.P2'(A, B).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.XPAtom.F1'(_), []).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.XPAtom.F1'(_), []).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.XAtom.F2'(_,A), B) :-
        'SharedSyntax.ArgFreeVars.P2'(A, B).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.XAtom.F2'(_,A), B) :-
        '~SharedSyntax.ArgFreeVars.P2'(A, B).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.~''.F1'(A), B) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, B).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.~''.F1'(A), B) :-
        '~SharedSyntax.SFormulaFreeVars.P2'(A, B).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.&''.F2'(A,B), C) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.&''.F2'(A,B), C) :-
        '~SharedSyntax.SFormulaFreeVars.P2'(A, D),
        '~SharedSyntax.SFormulaFreeVars.P2'(B, E),
        '~SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.\/''.F2'(A,B), C) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.\/''.F2'(A,B), C) :-
        '~SharedSyntax.SFormulaFreeVars.P2'(A, D),
        '~SharedSyntax.SFormulaFreeVars.P2'(B, E),
        '~SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.->''.F2'(A,B), C) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.->''.F2'(A,B), C) :-
        '~SharedSyntax.SFormulaFreeVars.P2'(A, D),
        '~SharedSyntax.SFormulaFreeVars.P2'(B, E),
        '~SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.<-''.F2'(A,B), C) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.<-''.F2'(A,B), C) :-
        '~SharedSyntax.SFormulaFreeVars.P2'(A, D),
        '~SharedSyntax.SFormulaFreeVars.P2'(B, E),
        '~SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.<->''.F2'(A,B), C) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.<->''.F2'(A,B), C) :-
        '~SharedSyntax.SFormulaFreeVars.P2'(A, D),
        '~SharedSyntax.SFormulaFreeVars.P2'(B, E),
        '~SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.All.F2'(A,B), C) :-
        'SharedSyntax.Order.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Difference.P3'(E, D, C).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.All.F2'(A,B), C) :-
        '~SharedSyntax.Order.P2'(A, D),
        '~SharedSyntax.SFormulaFreeVars.P2'(B, E),
        '~SharedSyntax.Difference.P3'(E, D, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.Some.F2'(A,B), C) :-
        'SharedSyntax.Order.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Difference.P3'(E, D, C).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.Some.F2'(A,B), C) :-
        '~SharedSyntax.Order.P2'(A, D),
        '~SharedSyntax.SFormulaFreeVars.P2'(B, E),
        '~SharedSyntax.Difference.P3'(E, D, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.ISTE.F4'(A,B,C,D), E) :-
        'SharedSyntax.Order.P2'(A, F),
        'SharedSyntax.SFormulaFreeVars.P2'(B, G),
        'SharedSyntax.SFormulaFreeVars.P2'(C, H),
        'SharedSyntax.SFormulaFreeVars.P2'(D, I),
        'SharedSyntax.Union.P3'(G, H, J),
        'SharedSyntax.Difference.P3'(J, F, K),
        'SharedSyntax.Union.P3'(K, I, E).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.ISTE.F4'(A,B,C,D), E) :-
        '~SharedSyntax.Order.P2'(A, F),
        '~SharedSyntax.SFormulaFreeVars.P2'(B, G),
        '~SharedSyntax.SFormulaFreeVars.P2'(C, H),
        '~SharedSyntax.SFormulaFreeVars.P2'(D, I),
        '~SharedSyntax.Union.P3'(G, H, J),
        '~SharedSyntax.Difference.P3'(J, F, K),
        '~SharedSyntax.Union.P3'(K, I, E).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.ITE.F3'(A,B,C), D) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, E),
        'SharedSyntax.SFormulaFreeVars.P2'(B, F),
        'SharedSyntax.SFormulaFreeVars.P2'(C, G),
        'SharedSyntax.Union.P3'(E, F, H),
        'SharedSyntax.Union.P3'(H, G, D).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.ITE.F3'(A,B,C), D) :-
        '~SharedSyntax.SFormulaFreeVars.P2'(A, E),
        '~SharedSyntax.SFormulaFreeVars.P2'(B, F),
        '~SharedSyntax.SFormulaFreeVars.P2'(C, G),
        '~SharedSyntax.Union.P3'(E, F, H),
        '~SharedSyntax.Union.P3'(H, G, D).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.IST.F3'(A,B,C), D) :-
        'SharedSyntax.Order.P2'(A, E),
        'SharedSyntax.SFormulaFreeVars.P2'(B, F),
        'SharedSyntax.SFormulaFreeVars.P2'(C, G),
        'SharedSyntax.Union.P3'(F, G, H),
        'SharedSyntax.Difference.P3'(H, E, D).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.IST.F3'(A,B,C), D) :-
        '~SharedSyntax.Order.P2'(A, E),
        '~SharedSyntax.SFormulaFreeVars.P2'(B, F),
        '~SharedSyntax.SFormulaFreeVars.P2'(C, G),
        '~SharedSyntax.Union.P3'(F, G, H),
        '~SharedSyntax.Difference.P3'(H, E, D).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.IT.F2'(A,B), C) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Union.P3'(D, E, C).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.IT.F2'(A,B), C) :-
        '~SharedSyntax.SFormulaFreeVars.P2'(A, D),
        '~SharedSyntax.SFormulaFreeVars.P2'(B, E),
        '~SharedSyntax.Union.P3'(D, E, C).
'SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.Commit.F2'(_,A), B) :-
        'SharedSyntax.SFormulaFreeVars.P2'(A, B).
'~SharedSyntax.SFormulaFreeVars.P2'('MetaDefs.Commit.F2'(_,A), B) :-
        '~SharedSyntax.SFormulaFreeVars.P2'(A, B).
'SharedSyntax.SNotNewBinding.P4'([], ['MetaDefs.!.F2'(A,B)], A, B).
'~SharedSyntax.SNotNewBinding.P4'([], ['MetaDefs.!.F2'(A,B)], A, B).
'SharedSyntax.SNotNewBinding.P4'(['MetaDefs.!.F2'(B,C)|A], D, E, F) :-
        user:goedel_freeze(ground([B,E]), (B=E,true->C=F,D=G;user:not_equal([],[C,F],C,F),D=['MetaDefs.!.F2'(B,C)|G])),
        'SharedSyntax.SNotNewBinding.P4'(A, G, E, F).
'~SharedSyntax.SNotNewBinding.P4'(['MetaDefs.!.F2'(B,C)|A], D, E, F) :-
        user:goedel_freeze(ground([B,E]), (B=E,true->C=F,D=G;user:not_equal([],[C,F],C,F),D=['MetaDefs.!.F2'(B,C)|G])),
        '~SharedSyntax.SNotNewBinding.P4'(A, G, E, F).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Var.F2'(A,B), ['MetaDefs.Var.F2'(A,B)]).
'~SharedSyntax.STermFreeVars.P2'('MetaDefs.Var.F2'(A,B), ['MetaDefs.Var.F2'(A,B)]).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Var.F1'(A), ['MetaDefs.Var.F1'(A)]).
'~SharedSyntax.STermFreeVars.P2'('MetaDefs.Var.F1'(A), ['MetaDefs.Var.F1'(A)]).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Term.F2'(_,A), B) :-
        'SharedSyntax.ArgFreeVars.P2'(A, B).
'~SharedSyntax.STermFreeVars.P2'('MetaDefs.Term.F2'(_,A), B) :-
        '~SharedSyntax.ArgFreeVars.P2'(A, B).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.XTerm.F3'(_,A,_), B) :-
        'SharedSyntax.ArgFreeVars.P2'(A, B).
'~SharedSyntax.STermFreeVars.P2'('MetaDefs.XTerm.F3'(_,A,_), B) :-
        '~SharedSyntax.ArgFreeVars.P2'(A, B).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.CTerm.F1'(_), []).
'~SharedSyntax.STermFreeVars.P2'('MetaDefs.CTerm.F1'(_), []).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.XCTerm.F2'(_,_), []).
'~SharedSyntax.STermFreeVars.P2'('MetaDefs.XCTerm.F2'(_,_), []).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Int.F1'(_), []).
'~SharedSyntax.STermFreeVars.P2'('MetaDefs.Int.F1'(_), []).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Num.F1'(_), []).
'~SharedSyntax.STermFreeVars.P2'('MetaDefs.Num.F1'(_), []).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Str.F1'(_), []).
'~SharedSyntax.STermFreeVars.P2'('MetaDefs.Str.F1'(_), []).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.Prm.F1'(_), []).
'~SharedSyntax.STermFreeVars.P2'('MetaDefs.Prm.F1'(_), []).
'SharedSyntax.STermFreeVars.P2'('MetaDefs.SuchThat.F2'(A,B), C) :-
        'SharedSyntax.STermFreeVars.P2'(A, D),
        'SharedSyntax.SFormulaFreeVars.P2'(B, E),
        'SharedSyntax.Difference.P3'(E, D, C).
'~SharedSyntax.STermFreeVars.P2'('MetaDefs.SuchThat.F2'(A,B), C) :-
        '~SharedSyntax.STermFreeVars.P2'(A, D),
        '~SharedSyntax.SFormulaFreeVars.P2'(B, E),
        '~SharedSyntax.Difference.P3'(E, D, C).
'SharedSyntax.UnifyTermArgs.P5'([], [], _, A, A).
'~SharedSyntax.UnifyTermArgs.P5'([], [], _, A, A).
'SharedSyntax.UnifyTermArgs.P5'([A|B], [C|D], E, F, G) :-
        'SharedSyntax.UnifyTerms1.P5'(A, C, E, F, H),
        'SharedSyntax.UnifyTermArgs.P5'(B, D, E, H, G).
'~SharedSyntax.UnifyTermArgs.P5'([A|B], [C|D], E, F, G) :-
        '~SharedSyntax.UnifyTerms1.P5'(A, C, E, F, H),
        '~SharedSyntax.UnifyTermArgs.P5'(B, D, E, H, G).
'SharedSyntax.SVariableName.P3'('MetaDefs.Var.F2'(A,B), A, B) :-
        call_residue(user:not_equal([],[A],A,'"v'), C),
        (   C=[] ->
            !
        ;   user:release_suspended(C)
        ).
'~SharedSyntax.SVariableName.P3'('MetaDefs.Var.F2'(A,B), A, B) :-
        user:not_equal([], [A], A, '"v').
'SharedSyntax.SVariableName.P3'('MetaDefs.Var.F1'(A), '"v', A) :- !.
'~SharedSyntax.SVariableName.P3'('MetaDefs.Var.F1'(A), '"v', A).
'SharedSyntax.SUnifyTerms.P4'(A, B, C, D) :-
        'Substs':'Substs.SubstApplyToTerm.P3'(A, C, E),
        'SharedSyntax.UnifyTerms0.P4'(E, B, C, D).
'~SharedSyntax.SUnifyTerms.P4'(A, B, C, D) :-
        'Substs':'~Substs.SubstApplyToTerm.P3'(A, C, E),
        '~SharedSyntax.UnifyTerms0.P4'(E, B, C, D).
'SharedSyntax.SUnifyAtoms.P4'('MetaDefs.PAtom.F1'(A), 'MetaDefs.PAtom.F1'(A), B, B).
'~SharedSyntax.SUnifyAtoms.P4'('MetaDefs.PAtom.F1'(A), 'MetaDefs.PAtom.F1'(A), B, B).
'SharedSyntax.SUnifyAtoms.P4'('MetaDefs.XPAtom.F1'(A), 'MetaDefs.XPAtom.F1'(A), B, B).
'~SharedSyntax.SUnifyAtoms.P4'('MetaDefs.XPAtom.F1'(A), 'MetaDefs.XPAtom.F1'(A), B, B).
'SharedSyntax.SUnifyAtoms.P4'('MetaDefs.Atom.F2'(A,B), 'MetaDefs.Atom.F2'(A,C), D, E) :-
        'SharedSyntax.UnifyingTermSubst.P4'(B, C, D, E).
'~SharedSyntax.SUnifyAtoms.P4'('MetaDefs.Atom.F2'(A,B), 'MetaDefs.Atom.F2'(A,C), D, E) :-
        '~SharedSyntax.UnifyingTermSubst.P4'(B, C, D, E).
'SharedSyntax.SUnifyAtoms.P4'('MetaDefs.XAtom.F2'(A,B), 'MetaDefs.XAtom.F2'(A,C), D, E) :-
        'SharedSyntax.UnifyingTermSubst.P4'(B, C, D, E).
'~SharedSyntax.SUnifyAtoms.P4'('MetaDefs.XAtom.F2'(A,B), 'MetaDefs.XAtom.F2'(A,C), D, E) :-
        '~SharedSyntax.UnifyingTermSubst.P4'(B, C, D, E).
'SharedSyntax.TermNotOccur1.P2'([], _).
'~SharedSyntax.TermNotOccur1.P2'([], _).
'SharedSyntax.TermNotOccur1.P2'([A|B], C) :-
        'SharedSyntax.TermNotOccur.P2'(A, C),
        'SharedSyntax.TermNotOccur1.P2'(B, C).
'~SharedSyntax.TermNotOccur1.P2'([A|B], C) :-
        '~SharedSyntax.TermNotOccur.P2'(A, C),
        '~SharedSyntax.TermNotOccur1.P2'(B, C).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.Var.F2'(A,B), C) :-
        user:not_equal([], [C,A,B], C, 'MetaDefs.Var.F2'(A,B)).
'~SharedSyntax.TermNotOccur.P2'('MetaDefs.Var.F2'(A,B), C) :-
        user:not_equal([], [C,A,B], C, 'MetaDefs.Var.F2'(A,B)).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.Var.F1'(A), B) :-
        user:not_equal([], [B,A], B, 'MetaDefs.Var.F1'(A)).
'~SharedSyntax.TermNotOccur.P2'('MetaDefs.Var.F1'(A), B) :-
        user:not_equal([], [B,A], B, 'MetaDefs.Var.F1'(A)).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.CTerm.F1'(_), _).
'~SharedSyntax.TermNotOccur.P2'('MetaDefs.CTerm.F1'(_), _).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.Term.F2'(_,A), B) :-
        'SharedSyntax.TermNotOccur1.P2'(A, B).
'~SharedSyntax.TermNotOccur.P2'('MetaDefs.Term.F2'(_,A), B) :-
        '~SharedSyntax.TermNotOccur1.P2'(A, B).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.XCTerm.F2'(_,_), _).
'~SharedSyntax.TermNotOccur.P2'('MetaDefs.XCTerm.F2'(_,_), _).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.Int.F1'(_), _).
'~SharedSyntax.TermNotOccur.P2'('MetaDefs.Int.F1'(_), _).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.Str.F1'(_), _).
'~SharedSyntax.TermNotOccur.P2'('MetaDefs.Str.F1'(_), _).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.Prm.F1'(_), _).
'~SharedSyntax.TermNotOccur.P2'('MetaDefs.Prm.F1'(_), _).
'SharedSyntax.TermNotOccur.P2'('MetaDefs.XTerm.F3'(_,A,_), B) :-
        'SharedSyntax.TermNotOccur1.P2'(A, B).
'~SharedSyntax.TermNotOccur.P2'('MetaDefs.XTerm.F3'(_,A,_), B) :-
        '~SharedSyntax.TermNotOccur1.P2'(A, B).
'SharedSyntax.TermOccurCheck.P3'('Substs.Read.C0', A, B) :-
        'SharedSyntax.TermNotOccur1.P2'(B, A).
'~SharedSyntax.TermOccurCheck.P3'('Substs.Read.C0', A, B) :-
        '~SharedSyntax.TermNotOccur1.P2'(B, A).
'SharedSyntax.TermOccurCheck.P3'('Substs.Write.C0', _, _).
'~SharedSyntax.TermOccurCheck.P3'('Substs.Write.C0', _, _).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.Var.F2'(A,B), C, D, E, F) :-
        'Substs':'Substs.UnifyKnownVariable.P5'(D, C, 'MetaDefs.Var.F2'(A,B), E, F).
'~SharedSyntax.UnifyTerms1.P5'('MetaDefs.Var.F2'(A,B), C, D, E, F) :-
        'Substs':'~Substs.UnifyKnownVariable.P5'(D, C, 'MetaDefs.Var.F2'(A,B), E, F).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.Var.F1'(A), B, C, D, E) :-
        'Substs':'Substs.UnifyKnownVariable.P5'(C, B, 'MetaDefs.Var.F1'(A), D, E).
'~SharedSyntax.UnifyTerms1.P5'('MetaDefs.Var.F1'(A), B, C, D, E) :-
        'Substs':'~Substs.UnifyKnownVariable.P5'(C, B, 'MetaDefs.Var.F1'(A), D, E).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.CTerm.F1'(A), B, C, D, E) :-
        'Substs':'Substs.UnifyConstant.P5'(C, B, 'MetaDefs.CTerm.F1'(A), D, E).
'~SharedSyntax.UnifyTerms1.P5'('MetaDefs.CTerm.F1'(A), B, C, D, E) :-
        'Substs':'~Substs.UnifyConstant.P5'(C, B, 'MetaDefs.CTerm.F1'(A), D, E).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.Term.F2'(A,B), C, D, E, F) :-
        'Substs':'Substs.UnifyFunction.P6'(D, C, 'MetaDefs.Term.F2'(A,G), H, E, I),
        'SharedSyntax.TermOccurCheck.P3'(H, C, B),
        'SharedSyntax.UnifyTermArgs.P5'(B, G, H, I, F).
'~SharedSyntax.UnifyTerms1.P5'('MetaDefs.Term.F2'(A,B), C, D, E, F) :-
        'Substs':'~Substs.UnifyFunction.P6'(D, C, 'MetaDefs.Term.F2'(A,G), H, E, I),
        '~SharedSyntax.TermOccurCheck.P3'(H, C, B),
        '~SharedSyntax.UnifyTermArgs.P5'(B, G, H, I, F).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.XTerm.F3'(A,B,C), D, E, F, G) :-
        'Substs':'Substs.UnifyFunction.P6'(E, D, 'MetaDefs.XTerm.F3'(A,H,C), I, F, J),
        'SharedSyntax.TermOccurCheck.P3'(I, D, B),
        'SharedSyntax.UnifyTermArgs.P5'(B, H, I, J, G).
'~SharedSyntax.UnifyTerms1.P5'('MetaDefs.XTerm.F3'(A,B,C), D, E, F, G) :-
        'Substs':'~Substs.UnifyFunction.P6'(E, D, 'MetaDefs.XTerm.F3'(A,H,C), I, F, J),
        '~SharedSyntax.TermOccurCheck.P3'(I, D, B),
        '~SharedSyntax.UnifyTermArgs.P5'(B, H, I, J, G).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.XCTerm.F2'(A,B), C, D, E, F) :-
        'Substs':'Substs.UnifyConstant.P5'(D, C, 'MetaDefs.XCTerm.F2'(A,B), E, F).
'~SharedSyntax.UnifyTerms1.P5'('MetaDefs.XCTerm.F2'(A,B), C, D, E, F) :-
        'Substs':'~Substs.UnifyConstant.P5'(D, C, 'MetaDefs.XCTerm.F2'(A,B), E, F).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.Int.F1'(A), B, C, D, E) :-
        'Substs':'Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Int.F1'(A), D, E).
'~SharedSyntax.UnifyTerms1.P5'('MetaDefs.Int.F1'(A), B, C, D, E) :-
        'Substs':'~Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Int.F1'(A), D, E).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.Str.F1'(A), B, C, D, E) :-
        'Substs':'Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Str.F1'(A), D, E).
'~SharedSyntax.UnifyTerms1.P5'('MetaDefs.Str.F1'(A), B, C, D, E) :-
        'Substs':'~Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Str.F1'(A), D, E).
'SharedSyntax.UnifyTerms1.P5'('MetaDefs.Prm.F1'(A), B, C, D, E) :-
        'Substs':'Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Prm.F1'(A), D, E).
'~SharedSyntax.UnifyTerms1.P5'('MetaDefs.Prm.F1'(A), B, C, D, E) :-
        'Substs':'~Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Prm.F1'(A), D, E).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.Var.F2'(A,B), C, D, E) :-
        'SharedSyntax.GetVariable.P4'(C, 'MetaDefs.Var.F2'(A,B), D, E).
'~SharedSyntax.UnifyTerms0.P4'('MetaDefs.Var.F2'(A,B), C, D, E) :-
        '~SharedSyntax.GetVariable.P4'(C, 'MetaDefs.Var.F2'(A,B), D, E).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.Var.F1'(A), B, C, D) :-
        'SharedSyntax.GetVariable.P4'(B, 'MetaDefs.Var.F1'(A), C, D).
'~SharedSyntax.UnifyTerms0.P4'('MetaDefs.Var.F1'(A), B, C, D) :-
        '~SharedSyntax.GetVariable.P4'(B, 'MetaDefs.Var.F1'(A), C, D).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.CTerm.F1'(A), B, C, D) :-
        'Substs':'Substs.GetConstant.P4'(B, 'MetaDefs.CTerm.F1'(A), C, D).
'~SharedSyntax.UnifyTerms0.P4'('MetaDefs.CTerm.F1'(A), B, C, D) :-
        'Substs':'~Substs.GetConstant.P4'(B, 'MetaDefs.CTerm.F1'(A), C, D).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.Term.F2'(A,B), C, D, E) :-
        'Substs':'Substs.GetFunction.P5'(C, 'MetaDefs.Term.F2'(A,F), G, D, H),
        'SharedSyntax.TermOccurCheck.P3'(G, C, B),
        'SharedSyntax.UnifyTermArgs.P5'(B, F, G, H, E).
'~SharedSyntax.UnifyTerms0.P4'('MetaDefs.Term.F2'(A,B), C, D, E) :-
        'Substs':'~Substs.GetFunction.P5'(C, 'MetaDefs.Term.F2'(A,F), G, D, H),
        '~SharedSyntax.TermOccurCheck.P3'(G, C, B),
        '~SharedSyntax.UnifyTermArgs.P5'(B, F, G, H, E).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.XTerm.F3'(A,B,C), D, E, F) :-
        'Substs':'Substs.GetFunction.P5'(D, 'MetaDefs.XTerm.F3'(A,G,C), H, E, I),
        'SharedSyntax.TermOccurCheck.P3'(H, D, B),
        'SharedSyntax.UnifyTermArgs.P5'(B, G, H, I, F).
'~SharedSyntax.UnifyTerms0.P4'('MetaDefs.XTerm.F3'(A,B,C), D, E, F) :-
        'Substs':'~Substs.GetFunction.P5'(D, 'MetaDefs.XTerm.F3'(A,G,C), H, E, I),
        '~SharedSyntax.TermOccurCheck.P3'(H, D, B),
        '~SharedSyntax.UnifyTermArgs.P5'(B, G, H, I, F).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.XCTerm.F2'(A,B), C, D, E) :-
        'Substs':'Substs.GetConstant.P4'(C, 'MetaDefs.XCTerm.F2'(A,B), D, E).
'~SharedSyntax.UnifyTerms0.P4'('MetaDefs.XCTerm.F2'(A,B), C, D, E) :-
        'Substs':'~Substs.GetConstant.P4'(C, 'MetaDefs.XCTerm.F2'(A,B), D, E).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.Int.F1'(A), B, C, D) :-
        'Substs':'Substs.GetConstant.P4'(B, 'MetaDefs.Int.F1'(A), C, D).
'~SharedSyntax.UnifyTerms0.P4'('MetaDefs.Int.F1'(A), B, C, D) :-
        'Substs':'~Substs.GetConstant.P4'(B, 'MetaDefs.Int.F1'(A), C, D).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.Str.F1'(A), B, C, D) :-
        'Substs':'Substs.GetConstant.P4'(B, 'MetaDefs.Str.F1'(A), C, D).
'~SharedSyntax.UnifyTerms0.P4'('MetaDefs.Str.F1'(A), B, C, D) :-
        'Substs':'~Substs.GetConstant.P4'(B, 'MetaDefs.Str.F1'(A), C, D).
'SharedSyntax.UnifyTerms0.P4'('MetaDefs.Prm.F1'(A), B, C, D) :-
        'Substs':'Substs.GetConstant.P4'(B, 'MetaDefs.Prm.F1'(A), C, D).
'~SharedSyntax.UnifyTerms0.P4'('MetaDefs.Prm.F1'(A), B, C, D) :-
        'Substs':'~Substs.GetConstant.P4'(B, 'MetaDefs.Prm.F1'(A), C, D).
'SharedSyntax.UnifyingTermSubst.P4'([], [], A, A).
'~SharedSyntax.UnifyingTermSubst.P4'([], [], A, A).
'SharedSyntax.UnifyingTermSubst.P4'([A|B], [C|D], E, F) :-
        'SharedSyntax.SUnifyTerms.P4'(A, C, E, G),
        'SharedSyntax.UnifyingTermSubst.P4'(B, D, G, F).
'~SharedSyntax.UnifyingTermSubst.P4'([A|B], [C|D], E, F) :-
        '~SharedSyntax.SUnifyTerms.P4'(A, C, E, G),
        '~SharedSyntax.UnifyingTermSubst.P4'(B, D, G, F).
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

