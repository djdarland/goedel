:- module('Syntax', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

'Syntax.GroundType.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.GroundType.P1.0'(A)).
'~Syntax.GroundType.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.GroundType.P1.0'(A)).
'Syntax.GroundType.P1.0'('MetaDefs.BType.F1'(_)).
'~Syntax.GroundType.P1.0'('MetaDefs.BType.F1'(_)).
'Syntax.GroundType.P1.0'('MetaDefs.Type.F2'(_,A)) :-
        'Syntax.GroundType1.P1'(A).
'~Syntax.GroundType.P1.0'('MetaDefs.Type.F2'(_,A)) :-
        '~Syntax.GroundType1.P1'(A).
'Syntax.GroundType.P1.0'('MetaDefs.XBType.F1'(_)).
'~Syntax.GroundType.P1.0'('MetaDefs.XBType.F1'(_)).
'Syntax.GroundType.P1.0'('MetaDefs.XType.F2'(_,A)) :-
        'Syntax.GroundType1.P1'(A).
'~Syntax.GroundType.P1.0'('MetaDefs.XType.F2'(_,A)) :-
        '~Syntax.GroundType1.P1'(A).
'Syntax.CommitFreeFormula.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.CommitFreeFormula.P1.0'(A)).
'~Syntax.CommitFreeFormula.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.CommitFreeFormula.P1.0'(A)).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.Empty.C0').
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.Empty.C0').
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.PAtom.F1'(_)).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.PAtom.F1'(_)).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.Atom.F2'(_,_)).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.Atom.F2'(_,_)).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.&''.F2'(A,B)) :-
        'Syntax.CommitFreeFormula.P1'(A),
        'Syntax.CommitFreeFormula.P1'(B).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.&''.F2'(A,B)) :-
        '~Syntax.CommitFreeFormula.P1'(A),
        '~Syntax.CommitFreeFormula.P1'(B).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.\\/''.F2'(A,B)) :-
        'Syntax.CommitFreeFormula.P1'(A),
        'Syntax.CommitFreeFormula.P1'(B).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.\\/''.F2'(A,B)) :-
        '~Syntax.CommitFreeFormula.P1'(A),
        '~Syntax.CommitFreeFormula.P1'(B).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.->''.F2'(A,B)) :-
        'Syntax.CommitFreeFormula.P1'(A),
        'Syntax.CommitFreeFormula.P1'(B).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.->''.F2'(A,B)) :-
        '~Syntax.CommitFreeFormula.P1'(A),
        '~Syntax.CommitFreeFormula.P1'(B).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.<-''.F2'(A,B)) :-
        'Syntax.CommitFreeFormula.P1'(A),
        'Syntax.CommitFreeFormula.P1'(B).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.<-''.F2'(A,B)) :-
        '~Syntax.CommitFreeFormula.P1'(A),
        '~Syntax.CommitFreeFormula.P1'(B).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.<->''.F2'(A,B)) :-
        'Syntax.CommitFreeFormula.P1'(A),
        'Syntax.CommitFreeFormula.P1'(B).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.<->''.F2'(A,B)) :-
        '~Syntax.CommitFreeFormula.P1'(A),
        '~Syntax.CommitFreeFormula.P1'(B).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.~''.F1'(A)) :-
        'Syntax.CommitFreeFormula.P1'(A).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.~''.F1'(A)) :-
        '~Syntax.CommitFreeFormula.P1'(A).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.Some.F2'(_,A)) :-
        'Syntax.CommitFreeFormula.P1'(A).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.Some.F2'(_,A)) :-
        '~Syntax.CommitFreeFormula.P1'(A).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.All.F2'(_,A)) :-
        'Syntax.CommitFreeFormula.P1'(A).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.All.F2'(_,A)) :-
        '~Syntax.CommitFreeFormula.P1'(A).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.IT.F2'(A,B)) :-
        'Syntax.CommitFreeFormula.P1'(A),
        'Syntax.CommitFreeFormula.P1'(B).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.IT.F2'(A,B)) :-
        '~Syntax.CommitFreeFormula.P1'(A),
        '~Syntax.CommitFreeFormula.P1'(B).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.IST.F3'(_,A,B)) :-
        'Syntax.CommitFreeFormula.P1'(A),
        'Syntax.CommitFreeFormula.P1'(B).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.IST.F3'(_,A,B)) :-
        '~Syntax.CommitFreeFormula.P1'(A),
        '~Syntax.CommitFreeFormula.P1'(B).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.ITE.F3'(A,B,C)) :-
        'Syntax.CommitFreeFormula.P1'(A),
        'Syntax.CommitFreeFormula.P1'(B),
        'Syntax.CommitFreeFormula.P1'(C).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.ITE.F3'(A,B,C)) :-
        '~Syntax.CommitFreeFormula.P1'(A),
        '~Syntax.CommitFreeFormula.P1'(B),
        '~Syntax.CommitFreeFormula.P1'(C).
'Syntax.CommitFreeFormula.P1.0'('MetaDefs.ISTE.F4'(_,A,B,C)) :-
        'Syntax.CommitFreeFormula.P1'(A),
        'Syntax.CommitFreeFormula.P1'(B),
        'Syntax.CommitFreeFormula.P1'(C).
'~Syntax.CommitFreeFormula.P1.0'('MetaDefs.ISTE.F4'(_,A,B,C)) :-
        '~Syntax.CommitFreeFormula.P1'(A),
        '~Syntax.CommitFreeFormula.P1'(B),
        '~Syntax.CommitFreeFormula.P1'(C).
'Syntax.BindingToTermSubst.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.BindingToTermSubst.P3.0'(A,B,C)).
'~Syntax.BindingToTermSubst.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.BindingToTermSubst.P3.0'(A,B,C)).
'Syntax.BindingToTermSubst.P3.0'(A, B, C) :-
        'Syntax.Variable.P1'(A),
        user:goedel_freeze(ground([B,A]), (A=B,true->'Syntax':'Syntax.EmptyTermSubst.P1'(C);'SharedSyntax':'SharedSyntax.TermNotOccur.P2'(B,A),'Syntax':'Syntax.EmptyTermSubst.P1'(D),'Substs':'Substs.BindVariable.P4'(A,B,D,C))).
'~Syntax.BindingToTermSubst.P3.0'(A, B, C) :-
        '~Syntax.Variable.P1'(A),
        user:goedel_freeze(ground([B,A]), (A=B,true->'Syntax':'~Syntax.EmptyTermSubst.P1'(C);'SharedSyntax':'~SharedSyntax.TermNotOccur.P2'(B,A),'Syntax':'~Syntax.EmptyTermSubst.P1'(D),'Substs':'~Substs.BindVariable.P4'(A,B,D,C))).
'Syntax.Atom.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.Atom.P1.0'(A)).
'~Syntax.Atom.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.Atom.P1.0'(A)).
'Syntax.Atom.P1.0'('MetaDefs.Atom.F2'(_,_)).
'~Syntax.Atom.P1.0'('MetaDefs.Atom.F2'(_,_)).
'Syntax.Atom.P1.0'('MetaDefs.PAtom.F1'(_)).
'~Syntax.Atom.P1.0'('MetaDefs.PAtom.F1'(_)).
'Syntax.AppendTypeBindings.P3'([], A, A).
'~Syntax.AppendTypeBindings.P3'([], A, A).
'Syntax.AppendTypeBindings.P3'(['MetaDefs.!.F2'(B,C)|A], D, E) :-
        'Substs':'Substs.BindParameter.P4'(B, C, D, F),
        'Syntax.AppendTypeBindings.P3'(A, F, E).
'~Syntax.AppendTypeBindings.P3'(['MetaDefs.!.F2'(B,C)|A], D, E) :-
        'Substs':'~Substs.BindParameter.P4'(B, C, D, F),
        '~Syntax.AppendTypeBindings.P3'(A, F, E).
'Syntax.AndWithEmpty.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.AndWithEmpty.P3.0'(A,B,C)).
'~Syntax.AndWithEmpty.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.AndWithEmpty.P3.0'(A,B,C)).
'Syntax.AndWithEmpty.P3.0'(A, B, C) :-
        user:goedel_freeze(ground([A]), ('Syntax':'~Syntax.EmptyFormula.P1'(A),true->C=B;user:goedel_freeze(ground([B]),('Syntax':'~Syntax.EmptyFormula.P1'(B),true->C=A;C='MetaDefs.&''.F2'(A,B))))).
'~Syntax.AndWithEmpty.P3.0'(A, B, C) :-
        user:goedel_freeze(ground([A]), ('Syntax':'~Syntax.EmptyFormula.P1'(A),true->C=B;user:goedel_freeze(ground([B]),('Syntax':'~Syntax.EmptyFormula.P1'(B),true->C=A;C='MetaDefs.&''.F2'(A,B))))).
'Syntax.And.P3'(A, B, 'MetaDefs.&''.F2'(A,B)).
'~Syntax.And.P3'(A, B, 'MetaDefs.&''.F2'(A,B)).
'Syntax.All.P3'(A, B, 'MetaDefs.All.F2'(A,B)).
'~Syntax.All.P3'(A, B, 'MetaDefs.All.F2'(A,B)).
'Syntax.AppendTermBindings.P3'([], A, A).
'~Syntax.AppendTermBindings.P3'([], A, A).
'Syntax.AppendTermBindings.P3'(['MetaDefs.!.F2'(B,C)|A], D, E) :-
        'Substs':'Substs.BindVariable.P4'(B, C, D, F),
        'Syntax.AppendTermBindings.P3'(A, F, E).
'~Syntax.AppendTermBindings.P3'(['MetaDefs.!.F2'(B,C)|A], D, E) :-
        'Substs':'~Substs.BindVariable.P4'(B, C, D, F),
        '~Syntax.AppendTermBindings.P3'(A, F, E).
'Syntax.ApplySubstToTerm.P8'('MetaDefs.Var.F1'(A), B, C, D, E, F, G, H) :-
        user:goedel_freeze(ground([A,D]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),I),D),true),(C=B,G=F,H=I),user:goedel_freeze(ground([A,F]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),J),F),true),(C=B,G=F,H=J),user:goedel_freeze(ground([A,E]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),K),E),true),(C=B,G=F,H=K),(('Integers':plus(B,1,L),C=L),G=['MetaDefs.!.F2'('MetaDefs.Var.F1'(A),H)|F],H='MetaDefs.Var.F1'(B)))))))).
'~Syntax.ApplySubstToTerm.P8'('MetaDefs.Var.F1'(A), B, C, D, E, F, G, H) :-
        user:goedel_freeze(ground([A,D]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),I),D),true),(C=B,G=F,H=I),user:goedel_freeze(ground([A,F]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),J),F),true),(C=B,G=F,H=J),user:goedel_freeze(ground([A,E]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),K),E),true),(C=B,G=F,H=K),(('Integers':plus(B,1,L),C=L),G=['MetaDefs.!.F2'('MetaDefs.Var.F1'(A),H)|F],H='MetaDefs.Var.F1'(B)))))))).
'Syntax.ApplySubstToTerm.P8'('MetaDefs.Var.F2'(A,B), C, D, E, F, G, H, I) :-
        user:goedel_freeze(ground([B,E,A]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),J),E),true),(D=C,H=G,I=J),user:goedel_freeze(ground([B,G,A]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),K),G),true),(D=C,H=G,I=K),user:goedel_freeze(ground([B,F,A]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),L),F),true),(D=C,H=G,I=L),(('Integers':plus(C,1,M),D=M),H=['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),I)|G],I='MetaDefs.Var.F1'(C)))))))).
'~Syntax.ApplySubstToTerm.P8'('MetaDefs.Var.F2'(A,B), C, D, E, F, G, H, I) :-
        user:goedel_freeze(ground([B,E,A]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),J),E),true),(D=C,H=G,I=J),user:goedel_freeze(ground([B,G,A]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),K),G),true),(D=C,H=G,I=K),user:goedel_freeze(ground([B,F,A]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),L),F),true),(D=C,H=G,I=L),(('Integers':plus(C,1,M),D=M),H=['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),I)|G],I='MetaDefs.Var.F1'(C)))))))).
'Syntax.ApplySubstToTerm.P8'('MetaDefs.Term.F2'(A,B), C, D, E, F, G, H, 'MetaDefs.Term.F2'(A,I)) :-
        'Syntax.ApplySubstToArgs.P8'(B, C, D, E, F, G, H, I).
'~Syntax.ApplySubstToTerm.P8'('MetaDefs.Term.F2'(A,B), C, D, E, F, G, H, 'MetaDefs.Term.F2'(A,I)) :-
        '~Syntax.ApplySubstToArgs.P8'(B, C, D, E, F, G, H, I).
'Syntax.ApplySubstToTerm.P8'('MetaDefs.XTerm.F3'(A,B,C), D, E, F, G, H, I, 'MetaDefs.XTerm.F3'(A,J,C)) :-
        'Syntax.ApplySubstToArgs.P8'(B, D, E, F, G, H, I, J).
'~Syntax.ApplySubstToTerm.P8'('MetaDefs.XTerm.F3'(A,B,C), D, E, F, G, H, I, 'MetaDefs.XTerm.F3'(A,J,C)) :-
        '~Syntax.ApplySubstToArgs.P8'(B, D, E, F, G, H, I, J).
'Syntax.ApplySubstToTerm.P8'('MetaDefs.CTerm.F1'(A), B, B, _, _, C, C, 'MetaDefs.CTerm.F1'(A)).
'~Syntax.ApplySubstToTerm.P8'('MetaDefs.CTerm.F1'(A), B, B, _, _, C, C, 'MetaDefs.CTerm.F1'(A)).
'Syntax.ApplySubstToTerm.P8'('MetaDefs.XCTerm.F2'(A,B), C, C, _, _, D, D, 'MetaDefs.XCTerm.F2'(A,B)).
'~Syntax.ApplySubstToTerm.P8'('MetaDefs.XCTerm.F2'(A,B), C, C, _, _, D, D, 'MetaDefs.XCTerm.F2'(A,B)).
'Syntax.ApplySubstToTerm.P8'('MetaDefs.Int.F1'(A), B, B, _, _, C, C, 'MetaDefs.Int.F1'(A)).
'~Syntax.ApplySubstToTerm.P8'('MetaDefs.Int.F1'(A), B, B, _, _, C, C, 'MetaDefs.Int.F1'(A)).
'Syntax.ApplySubstToTerm.P8'('MetaDefs.Str.F1'(A), B, B, _, _, C, C, 'MetaDefs.Str.F1'(A)).
'~Syntax.ApplySubstToTerm.P8'('MetaDefs.Str.F1'(A), B, B, _, _, C, C, 'MetaDefs.Str.F1'(A)).
'Syntax.ApplySubstToTerm.P8'('MetaDefs.Prm.F1'(A), B, B, _, _, C, C, 'MetaDefs.Prm.F1'(A)).
'~Syntax.ApplySubstToTerm.P8'('MetaDefs.Prm.F1'(A), B, B, _, _, C, C, 'MetaDefs.Prm.F1'(A)).
'Syntax.ApplySubstToTerm.P8'('MetaDefs.SuchThat.F2'(A,B), C, D, E, F, G, H, 'MetaDefs.SuchThat.F2'(I,J)) :-
        'SharedSyntax':'SharedSyntax.STermFreeVars.P2'(A, K),
        'Syntax.StandardiseApartQuants.P6'(K, _, E, L, C, M),
        'Syntax.ApplySubstToTerm.P8'(A, M, N, L, F, G, O, I),
        'Syntax.ApplySubstToFormula.P9'(B, N, D, L, F, O, H, [], J).
'~Syntax.ApplySubstToTerm.P8'('MetaDefs.SuchThat.F2'(A,B), C, D, E, F, G, H, 'MetaDefs.SuchThat.F2'(I,J)) :-
        'SharedSyntax':'~SharedSyntax.STermFreeVars.P2'(A, K),
        '~Syntax.StandardiseApartQuants.P6'(K, _, E, L, C, M),
        '~Syntax.ApplySubstToTerm.P8'(A, M, N, L, F, G, O, I),
        '~Syntax.ApplySubstToFormula.P9'(B, N, D, L, F, O, H, [], J).
'Syntax.ApplySubstToTerm.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.ApplySubstToTerm.P3.0'(A,B,C)).
'~Syntax.ApplySubstToTerm.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.ApplySubstToTerm.P3.0'(A,B,C)).
'Syntax.ApplySubstToTerm.P3.0'(A, B, C) :-
        'Substs':'Substs.SubstApplyToTerm.P3'(A, B, C).
'~Syntax.ApplySubstToTerm.P3.0'(A, B, C) :-
        'Substs':'~Substs.SubstApplyToTerm.P3'(A, B, C).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.&''.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.&''.F2'(J,K)) :-
        'Syntax.ApplySubstToFormula.P9'(A, C, L, E, F, G, M, I, J),
        'Syntax.ApplySubstToFormula.P9'(B, L, D, E, F, M, H, I, K).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.&''.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.&''.F2'(J,K)) :-
        '~Syntax.ApplySubstToFormula.P9'(A, C, L, E, F, G, M, I, J),
        '~Syntax.ApplySubstToFormula.P9'(B, L, D, E, F, M, H, I, K).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.Atom.F2'(A,B), C, D, E, F, G, H, _, 'MetaDefs.Atom.F2'(A,I)) :-
        'Syntax.ApplySubstToArgs.P8'(B, C, D, E, F, G, H, I).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.Atom.F2'(A,B), C, D, E, F, G, H, _, 'MetaDefs.Atom.F2'(A,I)) :-
        '~Syntax.ApplySubstToArgs.P8'(B, C, D, E, F, G, H, I).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.XAtom.F2'(A,B), C, D, E, F, G, H, _, 'MetaDefs.XAtom.F2'(A,I)) :-
        'Syntax.ApplySubstToArgs.P8'(B, C, D, E, F, G, H, I).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.XAtom.F2'(A,B), C, D, E, F, G, H, _, 'MetaDefs.XAtom.F2'(A,I)) :-
        '~Syntax.ApplySubstToArgs.P8'(B, C, D, E, F, G, H, I).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.Some.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.Some.F2'(J,K)) :-
        'Syntax.StandardiseApartQuants.P6'(A, J, E, L, C, M),
        'Syntax.ApplySubstToFormula.P9'(B, M, D, L, F, G, H, I, K).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.Some.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.Some.F2'(J,K)) :-
        '~Syntax.StandardiseApartQuants.P6'(A, J, E, L, C, M),
        '~Syntax.ApplySubstToFormula.P9'(B, M, D, L, F, G, H, I, K).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.ISTE.F4'(A,B,C,D), E, F, G, H, I, J, K, 'MetaDefs.ISTE.F4'(L,M,N,O)) :-
        'Syntax.StandardiseApartQuants.P6'(A, L, G, P, E, Q),
        'Syntax.ApplySubstToFormula.P9'(B, Q, R, P, H, I, S, K, M),
        'Syntax.ApplySubstToFormula.P9'(C, R, T, P, H, S, U, K, N),
        'Syntax.ApplySubstToFormula.P9'(D, T, F, G, H, U, J, K, O).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.ISTE.F4'(A,B,C,D), E, F, G, H, I, J, K, 'MetaDefs.ISTE.F4'(L,M,N,O)) :-
        '~Syntax.StandardiseApartQuants.P6'(A, L, G, P, E, Q),
        '~Syntax.ApplySubstToFormula.P9'(B, Q, R, P, H, I, S, K, M),
        '~Syntax.ApplySubstToFormula.P9'(C, R, T, P, H, S, U, K, N),
        '~Syntax.ApplySubstToFormula.P9'(D, T, F, G, H, U, J, K, O).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.Commit.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.Commit.F2'(J,K)) :-
        'Lists':'Lists.MemberCheck.P2'('Syntax.LabelPair.F2'(A,J), I),
        'Syntax.ApplySubstToFormula.P9'(B, C, D, E, F, G, H, I, K).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.Commit.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.Commit.F2'(J,K)) :-
        'Lists':'~Lists.MemberCheck.P2'('Syntax.LabelPair.F2'(A,J), I),
        '~Syntax.ApplySubstToFormula.P9'(B, C, D, E, F, G, H, I, K).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.Empty.C0', A, A, _, _, B, B, _, 'MetaDefs.Empty.C0').
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.Empty.C0', A, A, _, _, B, B, _, 'MetaDefs.Empty.C0').
'Syntax.ApplySubstToFormula.P9'('MetaDefs.PAtom.F1'(A), B, B, _, _, C, C, _, 'MetaDefs.PAtom.F1'(A)).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.PAtom.F1'(A), B, B, _, _, C, C, _, 'MetaDefs.PAtom.F1'(A)).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.XPAtom.F1'(A), B, B, _, _, C, C, _, 'MetaDefs.XPAtom.F1'(A)).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.XPAtom.F1'(A), B, B, _, _, C, C, _, 'MetaDefs.XPAtom.F1'(A)).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.\\/''.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.\\/''.F2'(J,K)) :-
        'Syntax.ApplySubstToFormula.P9'(A, C, L, E, F, G, M, I, J),
        'Syntax.ApplySubstToFormula.P9'(B, L, D, E, F, M, H, I, K).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.\\/''.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.\\/''.F2'(J,K)) :-
        '~Syntax.ApplySubstToFormula.P9'(A, C, L, E, F, G, M, I, J),
        '~Syntax.ApplySubstToFormula.P9'(B, L, D, E, F, M, H, I, K).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.ITE.F3'(A,B,C), D, E, F, G, H, I, J, 'MetaDefs.ITE.F3'(K,L,M)) :-
        'Syntax.ApplySubstToFormula.P9'(A, D, N, F, G, H, O, J, K),
        'Syntax.ApplySubstToFormula.P9'(B, N, P, F, G, O, Q, J, L),
        'Syntax.ApplySubstToFormula.P9'(C, P, E, F, G, Q, I, J, M).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.ITE.F3'(A,B,C), D, E, F, G, H, I, J, 'MetaDefs.ITE.F3'(K,L,M)) :-
        '~Syntax.ApplySubstToFormula.P9'(A, D, N, F, G, H, O, J, K),
        '~Syntax.ApplySubstToFormula.P9'(B, N, P, F, G, O, Q, J, L),
        '~Syntax.ApplySubstToFormula.P9'(C, P, E, F, G, Q, I, J, M).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.IST.F3'(A,B,C), D, E, F, G, H, I, J, 'MetaDefs.IST.F3'(K,L,M)) :-
        'Syntax.StandardiseApartQuants.P6'(A, K, F, N, D, O),
        'Syntax.ApplySubstToFormula.P9'(B, O, P, N, G, H, Q, J, L),
        'Syntax.ApplySubstToFormula.P9'(C, P, E, N, G, Q, I, J, M).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.IST.F3'(A,B,C), D, E, F, G, H, I, J, 'MetaDefs.IST.F3'(K,L,M)) :-
        '~Syntax.StandardiseApartQuants.P6'(A, K, F, N, D, O),
        '~Syntax.ApplySubstToFormula.P9'(B, O, P, N, G, H, Q, J, L),
        '~Syntax.ApplySubstToFormula.P9'(C, P, E, N, G, Q, I, J, M).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.IT.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.IT.F2'(J,K)) :-
        'Syntax.ApplySubstToFormula.P9'(A, C, L, E, F, G, M, I, J),
        'Syntax.ApplySubstToFormula.P9'(B, L, D, E, F, M, H, I, K).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.IT.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.IT.F2'(J,K)) :-
        '~Syntax.ApplySubstToFormula.P9'(A, C, L, E, F, G, M, I, J),
        '~Syntax.ApplySubstToFormula.P9'(B, L, D, E, F, M, H, I, K).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.~''.F1'(A), B, C, D, E, F, G, H, 'MetaDefs.~''.F1'(I)) :-
        'Syntax.ApplySubstToFormula.P9'(A, B, C, D, E, F, G, H, I).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.~''.F1'(A), B, C, D, E, F, G, H, 'MetaDefs.~''.F1'(I)) :-
        '~Syntax.ApplySubstToFormula.P9'(A, B, C, D, E, F, G, H, I).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.->''.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.->''.F2'(J,K)) :-
        'Syntax.ApplySubstToFormula.P9'(A, C, L, E, F, G, M, I, J),
        'Syntax.ApplySubstToFormula.P9'(B, L, D, E, F, M, H, I, K).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.->''.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.->''.F2'(J,K)) :-
        '~Syntax.ApplySubstToFormula.P9'(A, C, L, E, F, G, M, I, J),
        '~Syntax.ApplySubstToFormula.P9'(B, L, D, E, F, M, H, I, K).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.<-''.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.<-''.F2'(J,K)) :-
        'Syntax.ApplySubstToFormula.P9'(A, C, L, E, F, G, M, I, J),
        'Syntax.ApplySubstToFormula.P9'(B, L, D, E, F, M, H, I, K).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.<-''.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.<-''.F2'(J,K)) :-
        '~Syntax.ApplySubstToFormula.P9'(A, C, L, E, F, G, M, I, J),
        '~Syntax.ApplySubstToFormula.P9'(B, L, D, E, F, M, H, I, K).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.<->''.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.<->''.F2'(J,K)) :-
        'Syntax.ApplySubstToFormula.P9'(A, C, L, E, F, G, M, I, J),
        'Syntax.ApplySubstToFormula.P9'(B, L, D, E, F, M, H, I, K).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.<->''.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.<->''.F2'(J,K)) :-
        '~Syntax.ApplySubstToFormula.P9'(A, C, L, E, F, G, M, I, J),
        '~Syntax.ApplySubstToFormula.P9'(B, L, D, E, F, M, H, I, K).
'Syntax.ApplySubstToFormula.P9'('MetaDefs.All.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.All.F2'(J,K)) :-
        'Syntax.StandardiseApartQuants.P6'(A, J, E, L, C, M),
        'Syntax.ApplySubstToFormula.P9'(B, M, D, L, F, G, H, I, K).
'~Syntax.ApplySubstToFormula.P9'('MetaDefs.All.F2'(A,B), C, D, E, F, G, H, I, 'MetaDefs.All.F2'(J,K)) :-
        '~Syntax.StandardiseApartQuants.P6'(A, J, E, L, C, M),
        '~Syntax.ApplySubstToFormula.P9'(B, M, D, L, F, G, H, I, K).
'Syntax.ApplySubstToFormula.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.ApplySubstToFormula.P3.0'(A,B,C)).
'~Syntax.ApplySubstToFormula.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.ApplySubstToFormula.P3.0'(A,B,C)).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.Empty.C0', _, 'MetaDefs.Empty.C0').
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.Empty.C0', _, 'MetaDefs.Empty.C0').
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.PAtom.F1'(A), _, 'MetaDefs.PAtom.F1'(A)).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.PAtom.F1'(A), _, 'MetaDefs.PAtom.F1'(A)).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.XPAtom.F1'(A), _, 'MetaDefs.XPAtom.F1'(A)).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.XPAtom.F1'(A), _, 'MetaDefs.XPAtom.F1'(A)).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.Atom.F2'(A,B), C, 'MetaDefs.Atom.F2'(A,D)) :-
        'Substs':'Substs.ApplyTermSubst.P3'(B, C, D).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.Atom.F2'(A,B), C, 'MetaDefs.Atom.F2'(A,D)) :-
        'Substs':'~Substs.ApplyTermSubst.P3'(B, C, D).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.XAtom.F2'(A,B), C, 'MetaDefs.XAtom.F2'(A,D)) :-
        'Substs':'Substs.ApplyTermSubst.P3'(B, C, D).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.XAtom.F2'(A,B), C, 'MetaDefs.XAtom.F2'(A,D)) :-
        'Substs':'~Substs.ApplyTermSubst.P3'(B, C, D).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.&''.F2'(A,B), C, 'MetaDefs.&''.F2'(D,E)) :-
        'Syntax.ApplySubstToFormula.P3'(A, C, D),
        'Syntax.ApplySubstToFormula.P3'(B, C, E).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.&''.F2'(A,B), C, 'MetaDefs.&''.F2'(D,E)) :-
        '~Syntax.ApplySubstToFormula.P3'(A, C, D),
        '~Syntax.ApplySubstToFormula.P3'(B, C, E).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.\\/''.F2'(A,B), C, 'MetaDefs.\\/''.F2'(D,E)) :-
        'Syntax.ApplySubstToFormula.P3'(A, C, D),
        'Syntax.ApplySubstToFormula.P3'(B, C, E).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.\\/''.F2'(A,B), C, 'MetaDefs.\\/''.F2'(D,E)) :-
        '~Syntax.ApplySubstToFormula.P3'(A, C, D),
        '~Syntax.ApplySubstToFormula.P3'(B, C, E).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.~''.F1'(A), B, 'MetaDefs.~''.F1'(C)) :-
        'Syntax.ApplySubstToFormula.P3'(A, B, C).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.~''.F1'(A), B, 'MetaDefs.~''.F1'(C)) :-
        '~Syntax.ApplySubstToFormula.P3'(A, B, C).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.<-''.F2'(A,B), C, 'MetaDefs.<-''.F2'(D,E)) :-
        'Syntax.ApplySubstToFormula.P3'(A, C, D),
        'Syntax.ApplySubstToFormula.P3'(B, C, E).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.<-''.F2'(A,B), C, 'MetaDefs.<-''.F2'(D,E)) :-
        '~Syntax.ApplySubstToFormula.P3'(A, C, D),
        '~Syntax.ApplySubstToFormula.P3'(B, C, E).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.->''.F2'(A,B), C, 'MetaDefs.->''.F2'(D,E)) :-
        'Syntax.ApplySubstToFormula.P3'(A, C, D),
        'Syntax.ApplySubstToFormula.P3'(B, C, E).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.->''.F2'(A,B), C, 'MetaDefs.->''.F2'(D,E)) :-
        '~Syntax.ApplySubstToFormula.P3'(A, C, D),
        '~Syntax.ApplySubstToFormula.P3'(B, C, E).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.<->''.F2'(A,B), C, 'MetaDefs.<->''.F2'(D,E)) :-
        'Syntax.ApplySubstToFormula.P3'(A, C, D),
        'Syntax.ApplySubstToFormula.P3'(B, C, E).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.<->''.F2'(A,B), C, 'MetaDefs.<->''.F2'(D,E)) :-
        '~Syntax.ApplySubstToFormula.P3'(A, C, D),
        '~Syntax.ApplySubstToFormula.P3'(B, C, E).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.All.F2'(A,B), C, 'MetaDefs.All.F2'(A,D)) :-
        'Syntax.DeleteBindings.P3'(A, C, E),
        'Syntax.ApplySubstToFormula.P3'(B, E, D).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.All.F2'(A,B), C, 'MetaDefs.All.F2'(A,D)) :-
        '~Syntax.DeleteBindings.P3'(A, C, E),
        '~Syntax.ApplySubstToFormula.P3'(B, E, D).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.Some.F2'(A,B), C, 'MetaDefs.Some.F2'(A,D)) :-
        'Syntax.DeleteBindings.P3'(A, C, E),
        'Syntax.ApplySubstToFormula.P3'(B, E, D).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.Some.F2'(A,B), C, 'MetaDefs.Some.F2'(A,D)) :-
        '~Syntax.DeleteBindings.P3'(A, C, E),
        '~Syntax.ApplySubstToFormula.P3'(B, E, D).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.IT.F2'(A,B), C, 'MetaDefs.IT.F2'(D,E)) :-
        'Syntax.ApplySubstToFormula.P3'(A, C, D),
        'Syntax.ApplySubstToFormula.P3'(B, C, E).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.IT.F2'(A,B), C, 'MetaDefs.IT.F2'(D,E)) :-
        '~Syntax.ApplySubstToFormula.P3'(A, C, D),
        '~Syntax.ApplySubstToFormula.P3'(B, C, E).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.ITE.F3'(A,B,C), D, 'MetaDefs.ITE.F3'(E,F,G)) :-
        'Syntax.ApplySubstToFormula.P3'(A, D, E),
        'Syntax.ApplySubstToFormula.P3'(B, D, F),
        'Syntax.ApplySubstToFormula.P3'(C, D, G).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.ITE.F3'(A,B,C), D, 'MetaDefs.ITE.F3'(E,F,G)) :-
        '~Syntax.ApplySubstToFormula.P3'(A, D, E),
        '~Syntax.ApplySubstToFormula.P3'(B, D, F),
        '~Syntax.ApplySubstToFormula.P3'(C, D, G).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.IST.F3'(A,B,C), D, 'MetaDefs.IST.F3'(A,E,F)) :-
        'Syntax.DeleteBindings.P3'(A, D, G),
        'Syntax.ApplySubstToFormula.P3'(B, G, E),
        'Syntax.ApplySubstToFormula.P3'(C, G, F).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.IST.F3'(A,B,C), D, 'MetaDefs.IST.F3'(A,E,F)) :-
        '~Syntax.DeleteBindings.P3'(A, D, G),
        '~Syntax.ApplySubstToFormula.P3'(B, G, E),
        '~Syntax.ApplySubstToFormula.P3'(C, G, F).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.ISTE.F4'(A,B,C,D), E, 'MetaDefs.ISTE.F4'(A,F,G,H)) :-
        'Syntax.DeleteBindings.P3'(A, E, I),
        'Syntax.ApplySubstToFormula.P3'(B, I, F),
        'Syntax.ApplySubstToFormula.P3'(C, I, G),
        'Syntax.ApplySubstToFormula.P3'(D, I, H).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.ISTE.F4'(A,B,C,D), E, 'MetaDefs.ISTE.F4'(A,F,G,H)) :-
        '~Syntax.DeleteBindings.P3'(A, E, I),
        '~Syntax.ApplySubstToFormula.P3'(B, I, F),
        '~Syntax.ApplySubstToFormula.P3'(C, I, G),
        '~Syntax.ApplySubstToFormula.P3'(D, I, H).
'Syntax.ApplySubstToFormula.P3.0'('MetaDefs.Commit.F2'(A,B), C, 'MetaDefs.Commit.F2'(A,D)) :-
        'Syntax.ApplySubstToFormula.P3'(B, C, D).
'~Syntax.ApplySubstToFormula.P3.0'('MetaDefs.Commit.F2'(A,B), C, 'MetaDefs.Commit.F2'(A,D)) :-
        '~Syntax.ApplySubstToFormula.P3'(B, C, D).
'Syntax.ApplySubstToArgs.P8'([], A, A, _, _, B, B, []).
'~Syntax.ApplySubstToArgs.P8'([], A, A, _, _, B, B, []).
'Syntax.ApplySubstToArgs.P8'([A|B], C, D, E, F, G, H, [I|J]) :-
        'Syntax.ApplySubstToTerm.P8'(A, C, K, E, F, G, L, I),
        'Syntax.ApplySubstToArgs.P8'(B, K, D, E, F, L, H, J).
'~Syntax.ApplySubstToArgs.P8'([A|B], C, D, E, F, G, H, [I|J]) :-
        '~Syntax.ApplySubstToTerm.P8'(A, C, K, E, F, G, L, I),
        '~Syntax.ApplySubstToArgs.P8'(B, K, D, E, F, L, H, J).
'Syntax.ApplySubstToType.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.ApplySubstToType.P3.0'(A,B,C)).
'~Syntax.ApplySubstToType.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.ApplySubstToType.P3.0'(A,B,C)).
'Syntax.ApplySubstToType.P3.0'(A, B, C) :-
        'Substs':'Substs.SubstApplyToType.P3'(A, B, C).
'~Syntax.ApplySubstToType.P3.0'(A, B, C) :-
        'Substs':'~Substs.SubstApplyToType.P3'(A, B, C).
'Syntax.ApplySubstToVarTypes.P3'([], _, []).
'~Syntax.ApplySubstToVarTypes.P3'([], _, []).
'Syntax.ApplySubstToVarTypes.P3'(['MetaDefs.@.F2'(B,C)|A], D, ['MetaDefs.@.F2'(B,F)|E]) :-
        'Syntax.ApplySubstToType.P3'(C, D, F),
        'Syntax.ApplySubstToVarTypes.P3'(A, D, E).
'~Syntax.ApplySubstToVarTypes.P3'(['MetaDefs.@.F2'(B,C)|A], D, ['MetaDefs.@.F2'(B,F)|E]) :-
        '~Syntax.ApplySubstToType.P3'(C, D, F),
        '~Syntax.ApplySubstToVarTypes.P3'(A, D, E).
'Syntax.BindingInTypeSubst.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.BindingInTypeSubst.P3.0'(A,B,C)).
'~Syntax.BindingInTypeSubst.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.BindingInTypeSubst.P3.0'(A,B,C)).
'Syntax.BindingInTypeSubst.P3.0'(A, B, C) :-
        'Substs':'Substs.SBindingInTypeSubst.P3'(A, B, C).
'~Syntax.BindingInTypeSubst.P3.0'(A, B, C) :-
        'Substs':'~Substs.SBindingInTypeSubst.P3'(A, B, C).
'Syntax.BindingInTermSubst.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.BindingInTermSubst.P3.0'(A,B,C)).
'~Syntax.BindingInTermSubst.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.BindingInTermSubst.P3.0'(A,B,C)).
'Syntax.BindingInTermSubst.P3.0'(A, B, C) :-
        'Substs':'Substs.SBindingInTermSubst.P3'(A, B, C).
'~Syntax.BindingInTermSubst.P3.0'(A, B, C) :-
        'Substs':'~Substs.SBindingInTermSubst.P3'(A, B, C).
'Syntax.BaseType.P2'('MetaDefs.BType.F1'(A), A).
'~Syntax.BaseType.P2'('MetaDefs.BType.F1'(A), A).
'Syntax.BindingInVarTyping.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.BindingInVarTyping.P3.0'(A,B,C)).
'~Syntax.BindingInVarTyping.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.BindingInVarTyping.P3.0'(A,B,C)).
'Syntax.BindingInVarTyping.P3.0'('MetaDefs.VarTyping.F1'(A), B, C) :-
        'ExtraSyntax':'ExtraSyntax.EBindingInVarTyping.P3'(A, B, C).
'~Syntax.BindingInVarTyping.P3.0'('MetaDefs.VarTyping.F1'(A), B, C) :-
        'ExtraSyntax':'~ExtraSyntax.EBindingInVarTyping.P3'(A, B, C).
'Syntax.CheckVariantTerms1.P4'('MetaDefs.Var.F1'(A), B, C, 'MetaDefs.Var.F2'(D,E)) :-
        'SharedSyntax':'SharedSyntax.SNotNewBinding.P4'(B, C, 'MetaDefs.Var.F1'(A), 'MetaDefs.Var.F2'(D,E)).
'~Syntax.CheckVariantTerms1.P4'('MetaDefs.Var.F1'(A), B, C, 'MetaDefs.Var.F2'(D,E)) :-
        'SharedSyntax':'~SharedSyntax.SNotNewBinding.P4'(B, C, 'MetaDefs.Var.F1'(A), 'MetaDefs.Var.F2'(D,E)).
'Syntax.CheckVariantTerms1.P4'('MetaDefs.Var.F1'(A), B, C, 'MetaDefs.Var.F1'(D)) :-
        'SharedSyntax':'SharedSyntax.SNotNewBinding.P4'(B, C, 'MetaDefs.Var.F1'(A), 'MetaDefs.Var.F1'(D)).
'~Syntax.CheckVariantTerms1.P4'('MetaDefs.Var.F1'(A), B, C, 'MetaDefs.Var.F1'(D)) :-
        'SharedSyntax':'~SharedSyntax.SNotNewBinding.P4'(B, C, 'MetaDefs.Var.F1'(A), 'MetaDefs.Var.F1'(D)).
'Syntax.CheckVariantTerms1.P4'('MetaDefs.Var.F2'(A,B), C, D, 'MetaDefs.Var.F1'(E)) :-
        'SharedSyntax':'SharedSyntax.SNotNewBinding.P4'(C, D, 'MetaDefs.Var.F2'(A,B), 'MetaDefs.Var.F1'(E)).
'~Syntax.CheckVariantTerms1.P4'('MetaDefs.Var.F2'(A,B), C, D, 'MetaDefs.Var.F1'(E)) :-
        'SharedSyntax':'~SharedSyntax.SNotNewBinding.P4'(C, D, 'MetaDefs.Var.F2'(A,B), 'MetaDefs.Var.F1'(E)).
'Syntax.CheckVariantTerms1.P4'('MetaDefs.Var.F2'(A,B), C, D, 'MetaDefs.Var.F2'(E,F)) :-
        'SharedSyntax':'SharedSyntax.SNotNewBinding.P4'(C, D, 'MetaDefs.Var.F2'(A,B), 'MetaDefs.Var.F2'(E,F)).
'~Syntax.CheckVariantTerms1.P4'('MetaDefs.Var.F2'(A,B), C, D, 'MetaDefs.Var.F2'(E,F)) :-
        'SharedSyntax':'~SharedSyntax.SNotNewBinding.P4'(C, D, 'MetaDefs.Var.F2'(A,B), 'MetaDefs.Var.F2'(E,F)).
'Syntax.CheckVariantTerms1.P4'('MetaDefs.CTerm.F1'(A), B, B, 'MetaDefs.CTerm.F1'(A)).
'~Syntax.CheckVariantTerms1.P4'('MetaDefs.CTerm.F1'(A), B, B, 'MetaDefs.CTerm.F1'(A)).
'Syntax.CheckVariantTerms1.P4'('MetaDefs.Term.F2'(A,B), C, D, 'MetaDefs.Term.F2'(A,E)) :-
        'Syntax.CheckVariantTerms.P4'(B, C, D, E).
'~Syntax.CheckVariantTerms1.P4'('MetaDefs.Term.F2'(A,B), C, D, 'MetaDefs.Term.F2'(A,E)) :-
        '~Syntax.CheckVariantTerms.P4'(B, C, D, E).
'Syntax.CheckVariantTerms1.P4'('MetaDefs.XCTerm.F2'(A,B), C, C, 'MetaDefs.XCTerm.F2'(A,B)).
'~Syntax.CheckVariantTerms1.P4'('MetaDefs.XCTerm.F2'(A,B), C, C, 'MetaDefs.XCTerm.F2'(A,B)).
'Syntax.CheckVariantTerms1.P4'('MetaDefs.Int.F1'(A), B, B, 'MetaDefs.Int.F1'(A)).
'~Syntax.CheckVariantTerms1.P4'('MetaDefs.Int.F1'(A), B, B, 'MetaDefs.Int.F1'(A)).
'Syntax.CheckVariantTerms1.P4'('MetaDefs.Str.F1'(A), B, B, 'MetaDefs.Str.F1'(A)).
'~Syntax.CheckVariantTerms1.P4'('MetaDefs.Str.F1'(A), B, B, 'MetaDefs.Str.F1'(A)).
'Syntax.CheckVariantTerms1.P4'('MetaDefs.Prm.F1'(A), B, B, 'MetaDefs.Prm.F1'(A)).
'~Syntax.CheckVariantTerms1.P4'('MetaDefs.Prm.F1'(A), B, B, 'MetaDefs.Prm.F1'(A)).
'Syntax.CheckVariantTerms1.P4'('MetaDefs.XTerm.F3'(A,B,C), D, E, 'MetaDefs.XTerm.F3'(A,F,C)) :-
        'Syntax.CheckVariantTerms.P4'(B, D, E, F).
'~Syntax.CheckVariantTerms1.P4'('MetaDefs.XTerm.F3'(A,B,C), D, E, 'MetaDefs.XTerm.F3'(A,F,C)) :-
        '~Syntax.CheckVariantTerms.P4'(B, D, E, F).
'Syntax.CheckVariantTerms1.P4'('MetaDefs.SuchThat.F2'(A,B), C, D, 'MetaDefs.SuchThat.F2'(E,F)) :-
        'Syntax.CheckVariantTerms1.P4'(A, C, G, E),
        'Syntax.CheckVariantFormulas1.P4'(B, F, G, D).
'~Syntax.CheckVariantTerms1.P4'('MetaDefs.SuchThat.F2'(A,B), C, D, 'MetaDefs.SuchThat.F2'(E,F)) :-
        '~Syntax.CheckVariantTerms1.P4'(A, C, G, E),
        '~Syntax.CheckVariantFormulas1.P4'(B, F, G, D).
'Syntax.BindingToTypeSubst.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.BindingToTypeSubst.P3.0'(A,B,C)).
'~Syntax.BindingToTypeSubst.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.BindingToTypeSubst.P3.0'(A,B,C)).
'Syntax.BindingToTypeSubst.P3.0'(A, B, C) :-
        'Syntax.Parameter.P1'(A),
        user:goedel_freeze(ground([B,A]), (A=B,true->'Syntax':'Syntax.EmptyTypeSubst.P1'(C);'Syntax':'Syntax.TypeNotOccur.P2'(B,A),'Syntax':'Syntax.EmptyTypeSubst.P1'(D),'Substs':'Substs.BindParameter.P4'(A,B,D,C))).
'~Syntax.BindingToTypeSubst.P3.0'(A, B, C) :-
        '~Syntax.Parameter.P1'(A),
        user:goedel_freeze(ground([B,A]), (A=B,true->'Syntax':'~Syntax.EmptyTypeSubst.P1'(C);'Syntax':'~Syntax.TypeNotOccur.P2'(B,A),'Syntax':'~Syntax.EmptyTypeSubst.P1'(D),'Substs':'~Substs.BindParameter.P4'(A,B,D,C))).
'Syntax.BindingToVarTyping.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.BindingToVarTyping.P3.0'(A,B,C)).
'~Syntax.BindingToVarTyping.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.BindingToVarTyping.P3.0'(A,B,C)).
'Syntax.BindingToVarTyping.P3.0'(A, B, 'MetaDefs.VarTyping.F1'(['MetaDefs.@.F2'(A,B)])) :-
        'Syntax.Variable.P1'(A).
'~Syntax.BindingToVarTyping.P3.0'(A, B, 'MetaDefs.VarTyping.F1'(['MetaDefs.@.F2'(A,B)])) :-
        '~Syntax.Variable.P1'(A).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.Empty.C0', 'MetaDefs.Empty.C0', A, A).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.Empty.C0', 'MetaDefs.Empty.C0', A, A).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.PAtom.F1'(A), 'MetaDefs.PAtom.F1'(A), B, B).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.PAtom.F1'(A), 'MetaDefs.PAtom.F1'(A), B, B).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.Atom.F2'(A,B), 'MetaDefs.Atom.F2'(A,C), D, E) :-
        'Syntax.CheckVariantTerms.P4'(B, D, E, C).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.Atom.F2'(A,B), 'MetaDefs.Atom.F2'(A,C), D, E) :-
        '~Syntax.CheckVariantTerms.P4'(B, D, E, C).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.XPAtom.F1'(A), 'MetaDefs.XPAtom.F1'(A), B, B).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.XPAtom.F1'(A), 'MetaDefs.XPAtom.F1'(A), B, B).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.XAtom.F2'(A,B), 'MetaDefs.XAtom.F2'(A,C), D, E) :-
        'Syntax.CheckVariantTerms.P4'(B, D, E, C).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.XAtom.F2'(A,B), 'MetaDefs.XAtom.F2'(A,C), D, E) :-
        '~Syntax.CheckVariantTerms.P4'(B, D, E, C).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.&''.F2'(A,B), 'MetaDefs.&''.F2'(C,D), E, F) :-
        'Syntax.CheckVariantFormulas1.P4'(A, C, E, G),
        'Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.&''.F2'(A,B), 'MetaDefs.&''.F2'(C,D), E, F) :-
        '~Syntax.CheckVariantFormulas1.P4'(A, C, E, G),
        '~Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.\\/''.F2'(A,B), 'MetaDefs.\\/''.F2'(C,D), E, F) :-
        'Syntax.CheckVariantFormulas1.P4'(A, C, E, G),
        'Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.\\/''.F2'(A,B), 'MetaDefs.\\/''.F2'(C,D), E, F) :-
        '~Syntax.CheckVariantFormulas1.P4'(A, C, E, G),
        '~Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.~''.F1'(A), 'MetaDefs.~''.F1'(B), C, D) :-
        'Syntax.CheckVariantFormulas1.P4'(A, B, C, D).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.~''.F1'(A), 'MetaDefs.~''.F1'(B), C, D) :-
        '~Syntax.CheckVariantFormulas1.P4'(A, B, C, D).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.<-''.F2'(A,B), 'MetaDefs.<-''.F2'(C,D), E, F) :-
        'Syntax.CheckVariantFormulas1.P4'(A, C, E, G),
        'Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.<-''.F2'(A,B), 'MetaDefs.<-''.F2'(C,D), E, F) :-
        '~Syntax.CheckVariantFormulas1.P4'(A, C, E, G),
        '~Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.->''.F2'(A,B), 'MetaDefs.->''.F2'(C,D), E, F) :-
        'Syntax.CheckVariantFormulas1.P4'(A, C, E, G),
        'Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.->''.F2'(A,B), 'MetaDefs.->''.F2'(C,D), E, F) :-
        '~Syntax.CheckVariantFormulas1.P4'(A, C, E, G),
        '~Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.<->''.F2'(A,B), 'MetaDefs.<->''.F2'(C,D), E, F) :-
        'Syntax.CheckVariantFormulas1.P4'(A, C, E, G),
        'Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.<->''.F2'(A,B), 'MetaDefs.<->''.F2'(C,D), E, F) :-
        '~Syntax.CheckVariantFormulas1.P4'(A, C, E, G),
        '~Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.All.F2'(A,B), 'MetaDefs.All.F2'(C,D), E, F) :-
        'Syntax.CheckVariantTerms.P4'(A, E, G, C),
        'Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.All.F2'(A,B), 'MetaDefs.All.F2'(C,D), E, F) :-
        '~Syntax.CheckVariantTerms.P4'(A, E, G, C),
        '~Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.Some.F2'(A,B), 'MetaDefs.Some.F2'(C,D), E, F) :-
        'Syntax.CheckVariantTerms.P4'(A, E, G, C),
        'Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.Some.F2'(A,B), 'MetaDefs.Some.F2'(C,D), E, F) :-
        '~Syntax.CheckVariantTerms.P4'(A, E, G, C),
        '~Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.IT.F2'(A,B), 'MetaDefs.IT.F2'(C,D), E, F) :-
        'Syntax.CheckVariantFormulas1.P4'(A, C, E, G),
        'Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.IT.F2'(A,B), 'MetaDefs.IT.F2'(C,D), E, F) :-
        '~Syntax.CheckVariantFormulas1.P4'(A, C, E, G),
        '~Syntax.CheckVariantFormulas1.P4'(B, D, G, F).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.ITE.F3'(A,B,C), 'MetaDefs.ITE.F3'(D,E,F), G, H) :-
        'Syntax.CheckVariantFormulas1.P4'(A, D, G, I),
        'Syntax.CheckVariantFormulas1.P4'(B, E, I, J),
        'Syntax.CheckVariantFormulas1.P4'(C, F, J, H).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.ITE.F3'(A,B,C), 'MetaDefs.ITE.F3'(D,E,F), G, H) :-
        '~Syntax.CheckVariantFormulas1.P4'(A, D, G, I),
        '~Syntax.CheckVariantFormulas1.P4'(B, E, I, J),
        '~Syntax.CheckVariantFormulas1.P4'(C, F, J, H).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.IST.F3'(A,B,C), 'MetaDefs.IST.F3'(D,E,F), G, H) :-
        'Syntax.CheckVariantTerms.P4'(A, G, I, D),
        'Syntax.CheckVariantFormulas1.P4'(B, E, I, J),
        'Syntax.CheckVariantFormulas1.P4'(C, F, J, H).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.IST.F3'(A,B,C), 'MetaDefs.IST.F3'(D,E,F), G, H) :-
        '~Syntax.CheckVariantTerms.P4'(A, G, I, D),
        '~Syntax.CheckVariantFormulas1.P4'(B, E, I, J),
        '~Syntax.CheckVariantFormulas1.P4'(C, F, J, H).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.ISTE.F4'(A,B,C,D), 'MetaDefs.ISTE.F4'(E,F,G,H), I, J) :-
        'Syntax.CheckVariantTerms.P4'(A, I, K, E),
        'Syntax.CheckVariantFormulas1.P4'(B, F, K, L),
        'Syntax.CheckVariantFormulas1.P4'(C, G, L, M),
        'Syntax.CheckVariantFormulas1.P4'(D, H, M, J).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.ISTE.F4'(A,B,C,D), 'MetaDefs.ISTE.F4'(E,F,G,H), I, J) :-
        '~Syntax.CheckVariantTerms.P4'(A, I, K, E),
        '~Syntax.CheckVariantFormulas1.P4'(B, F, K, L),
        '~Syntax.CheckVariantFormulas1.P4'(C, G, L, M),
        '~Syntax.CheckVariantFormulas1.P4'(D, H, M, J).
'Syntax.CheckVariantFormulas1.P4'('MetaDefs.Commit.F2'(A,B), 'MetaDefs.Commit.F2'(A,C), D, E) :-
        'Syntax.CheckVariantFormulas1.P4'(B, C, D, E).
'~Syntax.CheckVariantFormulas1.P4'('MetaDefs.Commit.F2'(A,B), 'MetaDefs.Commit.F2'(A,C), D, E) :-
        '~Syntax.CheckVariantFormulas1.P4'(B, C, D, E).
'Syntax.CheckFunctionTerm1.P10'('MetaDefs.Var.F2'(A,B), C, D, E, F, G, H, I, J, K) :-
        user:goedel_freeze(ground([B,G,A]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),L),G),true),('Syntax':'Syntax.UnifyValue.P5'(D,L,C,J,K),F=E,I=H),user:goedel_freeze(ground([B,H,A]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),M),H),true),('Syntax':'Syntax.UnifyValue.P5'(D,M,C,J,K),F=E,I=H),('Substs':'Substs.UnifyVariable.P4'(D,C,E,F),I=['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),C)|H],K=J))))).
'~Syntax.CheckFunctionTerm1.P10'('MetaDefs.Var.F2'(A,B), C, D, E, F, G, H, I, J, K) :-
        user:goedel_freeze(ground([B,G,A]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),L),G),true),('Syntax':'~Syntax.UnifyValue.P5'(D,L,C,J,K),F=E,I=H),user:goedel_freeze(ground([B,H,A]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),M),H),true),('Syntax':'~Syntax.UnifyValue.P5'(D,M,C,J,K),F=E,I=H),('Substs':'~Substs.UnifyVariable.P4'(D,C,E,F),I=['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),C)|H],K=J))))).
'Syntax.CheckFunctionTerm1.P10'('MetaDefs.Var.F1'(A), B, C, D, E, F, G, H, I, J) :-
        user:goedel_freeze(ground([A,F]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),K),F),true),('Syntax':'Syntax.UnifyValue.P5'(C,K,B,I,J),E=D,H=G),user:goedel_freeze(ground([A,G]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),L),G),true),('Syntax':'Syntax.UnifyValue.P5'(C,L,B,I,J),E=D,H=G),('Substs':'Substs.UnifyVariable.P4'(C,B,D,E),H=['MetaDefs.!.F2'('MetaDefs.Var.F1'(A),B)|G],J=I))))).
'~Syntax.CheckFunctionTerm1.P10'('MetaDefs.Var.F1'(A), B, C, D, E, F, G, H, I, J) :-
        user:goedel_freeze(ground([A,F]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),K),F),true),('Syntax':'~Syntax.UnifyValue.P5'(C,K,B,I,J),E=D,H=G),user:goedel_freeze(ground([A,G]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),L),G),true),('Syntax':'~Syntax.UnifyValue.P5'(C,L,B,I,J),E=D,H=G),('Substs':'~Substs.UnifyVariable.P4'(C,B,D,E),H=['MetaDefs.!.F2'('MetaDefs.Var.F1'(A),B)|G],J=I))))).
'Syntax.CheckFunctionTerm1.P10'('MetaDefs.Term.F2'(A,B), C, D, E, F, G, H, I, J, K) :-
        'Substs':'Substs.UnifyFunction.P6'(D, C, 'MetaDefs.Term.F2'(A,L), M, J, N),
        'Syntax.CheckFunctionTerm.P10'(B, L, M, E, F, G, H, I, N, K).
'~Syntax.CheckFunctionTerm1.P10'('MetaDefs.Term.F2'(A,B), C, D, E, F, G, H, I, J, K) :-
        'Substs':'~Substs.UnifyFunction.P6'(D, C, 'MetaDefs.Term.F2'(A,L), M, J, N),
        '~Syntax.CheckFunctionTerm.P10'(B, L, M, E, F, G, H, I, N, K).
'Syntax.CheckFunctionTerm1.P10'('MetaDefs.XTerm.F3'(A,B,C), D, E, F, G, H, I, J, K, L) :-
        'Substs':'Substs.UnifyFunction.P6'(E, D, 'MetaDefs.XTerm.F3'(A,M,C), N, K, O),
        'Syntax.CheckFunctionTerm.P10'(B, M, N, F, G, H, I, J, O, L).
'~Syntax.CheckFunctionTerm1.P10'('MetaDefs.XTerm.F3'(A,B,C), D, E, F, G, H, I, J, K, L) :-
        'Substs':'~Substs.UnifyFunction.P6'(E, D, 'MetaDefs.XTerm.F3'(A,M,C), N, K, O),
        '~Syntax.CheckFunctionTerm.P10'(B, M, N, F, G, H, I, J, O, L).
'Syntax.CheckFunctionTerm1.P10'('MetaDefs.CTerm.F1'(A), B, C, D, D, _, E, E, F, G) :-
        'Substs':'Substs.UnifyConstant.P5'(C, B, 'MetaDefs.CTerm.F1'(A), F, G).
'~Syntax.CheckFunctionTerm1.P10'('MetaDefs.CTerm.F1'(A), B, C, D, D, _, E, E, F, G) :-
        'Substs':'~Substs.UnifyConstant.P5'(C, B, 'MetaDefs.CTerm.F1'(A), F, G).
'Syntax.CheckFunctionTerm1.P10'('MetaDefs.XCTerm.F2'(A,B), C, D, E, E, _, F, F, G, H) :-
        'Substs':'Substs.UnifyConstant.P5'(D, C, 'MetaDefs.XCTerm.F2'(A,B), G, H).
'~Syntax.CheckFunctionTerm1.P10'('MetaDefs.XCTerm.F2'(A,B), C, D, E, E, _, F, F, G, H) :-
        'Substs':'~Substs.UnifyConstant.P5'(D, C, 'MetaDefs.XCTerm.F2'(A,B), G, H).
'Syntax.CheckFunctionTerm1.P10'('MetaDefs.Int.F1'(A), B, C, D, D, _, E, E, F, G) :-
        'Substs':'Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Int.F1'(A), F, G).
'~Syntax.CheckFunctionTerm1.P10'('MetaDefs.Int.F1'(A), B, C, D, D, _, E, E, F, G) :-
        'Substs':'~Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Int.F1'(A), F, G).
'Syntax.CheckFunctionTerm1.P10'('MetaDefs.Str.F1'(A), B, C, D, D, _, E, E, F, G) :-
        'Substs':'Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Str.F1'(A), F, G).
'~Syntax.CheckFunctionTerm1.P10'('MetaDefs.Str.F1'(A), B, C, D, D, _, E, E, F, G) :-
        'Substs':'~Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Str.F1'(A), F, G).
'Syntax.CheckFunctionTerm1.P10'('MetaDefs.Prm.F1'(A), B, C, D, D, _, E, E, F, G) :-
        'Substs':'Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Prm.F1'(A), F, G).
'~Syntax.CheckFunctionTerm1.P10'('MetaDefs.Prm.F1'(A), B, C, D, D, _, E, E, F, G) :-
        'Substs':'~Substs.UnifyConstant.P5'(C, B, 'MetaDefs.Prm.F1'(A), F, G).
'Syntax.CheckFunctionTerm1.P10'('MetaDefs.SuchThat.F2'(A,B), C, D, E, E, _, F, F, G, H) :-
        'Substs':'Substs.UnifyConstant.P5'(D, C, 'MetaDefs.SuchThat.F2'(A,B), G, H).
'~Syntax.CheckFunctionTerm1.P10'('MetaDefs.SuchThat.F2'(A,B), C, D, E, E, _, F, F, G, H) :-
        'Substs':'~Substs.UnifyConstant.P5'(D, C, 'MetaDefs.SuchThat.F2'(A,B), G, H).
'Syntax.CheckFunctionTerm.P10'([], [], _, A, A, _, B, B, C, C).
'~Syntax.CheckFunctionTerm.P10'([], [], _, A, A, _, B, B, C, C).
'Syntax.CheckFunctionTerm.P10'([A|B], [C|D], E, F, G, H, I, J, K, L) :-
        'Syntax.CheckFunctionTerm1.P10'(A, C, E, F, M, H, I, N, K, O),
        'Syntax.CheckFunctionTerm.P10'(B, D, E, M, G, H, N, J, O, L).
'~Syntax.CheckFunctionTerm.P10'([A|B], [C|D], E, F, G, H, I, J, K, L) :-
        '~Syntax.CheckFunctionTerm1.P10'(A, C, E, F, M, H, I, N, K, O),
        '~Syntax.CheckFunctionTerm.P10'(B, D, E, M, G, H, N, J, O, L).
'Syntax.CheckVariantFormulas.P4'([], A, A, []).
'~Syntax.CheckVariantFormulas.P4'([], A, A, []).
'Syntax.CheckVariantFormulas.P4'([A|B], C, D, [E|F]) :-
        'Syntax.CheckVariantFormulas1.P4'(A, E, C, G),
        'Syntax.CheckVariantFormulas.P4'(B, G, D, F).
'~Syntax.CheckVariantFormulas.P4'([A|B], C, D, [E|F]) :-
        '~Syntax.CheckVariantFormulas1.P4'(A, E, C, G),
        '~Syntax.CheckVariantFormulas.P4'(B, G, D, F).
'Syntax.CheckVariantTerms.P4'([], A, A, []).
'~Syntax.CheckVariantTerms.P4'([], A, A, []).
'Syntax.CheckVariantTerms.P4'([A|B], C, D, [E|F]) :-
        'Syntax.CheckVariantTerms1.P4'(A, C, G, E),
        'Syntax.CheckVariantTerms.P4'(B, G, D, F).
'~Syntax.CheckVariantTerms.P4'([A|B], C, D, [E|F]) :-
        '~Syntax.CheckVariantTerms1.P4'(A, C, G, E),
        '~Syntax.CheckVariantTerms.P4'(B, G, D, F).
'Syntax.CombineVarTypings.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.CombineVarTypings.P3.0'(A,B,C)).
'~Syntax.CombineVarTypings.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.CombineVarTypings.P3.0'(A,B,C)).
'Syntax.CombineVarTypings.P3.0'('MetaDefs.VarTyping.F1'(A), 'MetaDefs.VarTyping.F1'(B), 'MetaDefs.VarTyping.F1'(C)) :-
        'Substs':'Substs.EmptyHeap.P1'(D),
        'Syntax.CombineVarTypings1.P6'(A, [], 'Substs.TypeSubst.F2'(D,[]), E, B, F),
        'Syntax.ApplySubstToVarTypes.P3'(F, E, C).
'~Syntax.CombineVarTypings.P3.0'('MetaDefs.VarTyping.F1'(A), 'MetaDefs.VarTyping.F1'(B), 'MetaDefs.VarTyping.F1'(C)) :-
        'Substs':'~Substs.EmptyHeap.P1'(D),
        '~Syntax.CombineVarTypings1.P6'(A, [], 'Substs.TypeSubst.F2'(D,[]), E, B, F),
        '~Syntax.ApplySubstToVarTypes.P3'(F, E, C).
'Syntax.ClosedFormula.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.ClosedFormula.P1.0'(A)).
'~Syntax.ClosedFormula.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.ClosedFormula.P1.0'(A)).
'Syntax.ClosedFormula.P1.0'(A) :-
        'SharedSyntax':'SharedSyntax.SFormulaFreeVars.P2'(A, []).
'~Syntax.ClosedFormula.P1.0'(A) :-
        'SharedSyntax':'~SharedSyntax.SFormulaFreeVars.P2'(A, []).
'Syntax.Commit.P3'(A, B, 'MetaDefs.Commit.F2'(A,B)).
'~Syntax.Commit.P3'(A, B, 'MetaDefs.Commit.F2'(A,B)).
'Syntax.CombineVarTypings1.P6'([], A, B, B, C, D) :-
        'Syntax.DeleteVarTypeBindings.P3'(A, C, D).
'~Syntax.CombineVarTypings1.P6'([], A, B, B, C, D) :-
        '~Syntax.DeleteVarTypeBindings.P3'(A, C, D).
'Syntax.CombineVarTypings1.P6'(['MetaDefs.@.F2'(B,C)|A], D, E, F, G, ['MetaDefs.@.F2'(B,I)|H]) :-
        'Syntax.ApplySubstToType.P3'(C, E, J),
        user:goedel_freeze(ground([G,B]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.@.F2'(B,K),G),true),('Syntax':'Syntax.ApplySubstToType.P3'(K,E,L),'Substs':'Substs.EmptyHeap.P1'(M),'Syntax':'Syntax.UnifyTypes.P4'(J,L,'Substs.TypeSubst.F2'(M,[]),N),'Syntax':'Syntax.ApplySubstToType.P3'(J,N,I),'Syntax':'Syntax.ComposeTypeSubsts.P3'(N,E,O)),(I=J,O=E))),
        'Syntax.CombineVarTypings1.P6'(A, [B|D], O, F, G, H).
'~Syntax.CombineVarTypings1.P6'(['MetaDefs.@.F2'(B,C)|A], D, E, F, G, ['MetaDefs.@.F2'(B,I)|H]) :-
        '~Syntax.ApplySubstToType.P3'(C, E, J),
        user:goedel_freeze(ground([G,B]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.@.F2'(B,K),G),true),('Syntax':'~Syntax.ApplySubstToType.P3'(K,E,L),'Substs':'~Substs.EmptyHeap.P1'(M),'Syntax':'~Syntax.UnifyTypes.P4'(J,L,'Substs.TypeSubst.F2'(M,[]),N),'Syntax':'~Syntax.ApplySubstToType.P3'(J,N,I),'Syntax':'~Syntax.ComposeTypeSubsts.P3'(N,E,O)),(I=J,O=E))),
        '~Syntax.CombineVarTypings1.P6'(A, [B|D], O, F, G, H).
'Syntax.DelBindingInTypeSubst.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.DelBindingInTypeSubst.P4.0'(A,B,C,D)).
'~Syntax.DelBindingInTypeSubst.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.DelBindingInTypeSubst.P4.0'(A,B,C,D)).
'Syntax.DelBindingInTypeSubst.P4.0'(A, B, C, D) :-
        'Substs':'Substs.ComposeTypeSubsts1.P3'(A, A, E),
        'Substs':'Substs.SDelBindingInTypeSubst.P4'(E, B, C, D).
'~Syntax.DelBindingInTypeSubst.P4.0'(A, B, C, D) :-
        'Substs':'~Substs.ComposeTypeSubsts1.P3'(A, A, E),
        'Substs':'~Substs.SDelBindingInTypeSubst.P4'(E, B, C, D).
'Syntax.ConstructorType.P3'('MetaDefs.Type.F2'(A,B), A, B).
'~Syntax.ConstructorType.P3'('MetaDefs.Type.F2'(A,B), A, B).
'Syntax.ConjunctionOfAtoms.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.ConjunctionOfAtoms.P1.0'(A)).
'~Syntax.ConjunctionOfAtoms.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.ConjunctionOfAtoms.P1.0'(A)).
'Syntax.ConjunctionOfAtoms.P1.0'('MetaDefs.PAtom.F1'(_)).
'~Syntax.ConjunctionOfAtoms.P1.0'('MetaDefs.PAtom.F1'(_)).
'Syntax.ConjunctionOfAtoms.P1.0'('MetaDefs.Atom.F2'(_,_)).
'~Syntax.ConjunctionOfAtoms.P1.0'('MetaDefs.Atom.F2'(_,_)).
'Syntax.ConjunctionOfAtoms.P1.0'('MetaDefs.&''.F2'(A,B)) :-
        'Syntax.ConjunctionOfAtoms.P1'(A),
        'Syntax.ConjunctionOfAtoms.P1'(B).
'~Syntax.ConjunctionOfAtoms.P1.0'('MetaDefs.&''.F2'(A,B)) :-
        '~Syntax.ConjunctionOfAtoms.P1'(A),
        '~Syntax.ConjunctionOfAtoms.P1'(B).
'Syntax.ComposeTypeSubsts.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.ComposeTypeSubsts.P3.0'(A,B,C)).
'~Syntax.ComposeTypeSubsts.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.ComposeTypeSubsts.P3.0'(A,B,C)).
'Syntax.ComposeTypeSubsts.P3.0'(A, B, C) :-
        'Substs':'Substs.SubstsComposeType.P3'(A, B, C).
'~Syntax.ComposeTypeSubsts.P3.0'(A, B, C) :-
        'Substs':'~Substs.SubstsComposeType.P3'(A, B, C).
'Syntax.ComposeTermSubsts.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.ComposeTermSubsts.P3.0'(A,B,C)).
'~Syntax.ComposeTermSubsts.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.ComposeTermSubsts.P3.0'(A,B,C)).
'Syntax.ComposeTermSubsts.P3.0'(A, B, C) :-
        'Substs':'Substs.SubstsComposeTerm.P3'(A, B, C).
'~Syntax.ComposeTermSubsts.P3.0'(A, B, C) :-
        'Substs':'~Substs.SubstsComposeTerm.P3'(A, B, C).
'Syntax.ConditionalFreeFormula.P1'('MetaDefs.Empty.C0').
'~Syntax.ConditionalFreeFormula.P1'('MetaDefs.Empty.C0').
'Syntax.ConditionalFreeFormula.P1'('MetaDefs.PAtom.F1'(_)).
'~Syntax.ConditionalFreeFormula.P1'('MetaDefs.PAtom.F1'(_)).
'Syntax.ConditionalFreeFormula.P1'('MetaDefs.Atom.F2'(_,_)).
'~Syntax.ConditionalFreeFormula.P1'('MetaDefs.Atom.F2'(_,_)).
'Syntax.ConditionalFreeFormula.P1'('MetaDefs.&''.F2'(A,B)) :-
        'Syntax.ConditionalFreeFormula.P1'(A),
        'Syntax.ConditionalFreeFormula.P1'(B).
'~Syntax.ConditionalFreeFormula.P1'('MetaDefs.&''.F2'(A,B)) :-
        '~Syntax.ConditionalFreeFormula.P1'(A),
        '~Syntax.ConditionalFreeFormula.P1'(B).
'Syntax.ConditionalFreeFormula.P1'('MetaDefs.\\/''.F2'(A,B)) :-
        'Syntax.ConditionalFreeFormula.P1'(A),
        'Syntax.ConditionalFreeFormula.P1'(B).
'~Syntax.ConditionalFreeFormula.P1'('MetaDefs.\\/''.F2'(A,B)) :-
        '~Syntax.ConditionalFreeFormula.P1'(A),
        '~Syntax.ConditionalFreeFormula.P1'(B).
'Syntax.ConditionalFreeFormula.P1'('MetaDefs.->''.F2'(A,B)) :-
        'Syntax.ConditionalFreeFormula.P1'(A),
        'Syntax.ConditionalFreeFormula.P1'(B).
'~Syntax.ConditionalFreeFormula.P1'('MetaDefs.->''.F2'(A,B)) :-
        '~Syntax.ConditionalFreeFormula.P1'(A),
        '~Syntax.ConditionalFreeFormula.P1'(B).
'Syntax.ConditionalFreeFormula.P1'('MetaDefs.<-''.F2'(A,B)) :-
        'Syntax.ConditionalFreeFormula.P1'(A),
        'Syntax.ConditionalFreeFormula.P1'(B).
'~Syntax.ConditionalFreeFormula.P1'('MetaDefs.<-''.F2'(A,B)) :-
        '~Syntax.ConditionalFreeFormula.P1'(A),
        '~Syntax.ConditionalFreeFormula.P1'(B).
'Syntax.ConditionalFreeFormula.P1'('MetaDefs.<->''.F2'(A,B)) :-
        'Syntax.ConditionalFreeFormula.P1'(A),
        'Syntax.ConditionalFreeFormula.P1'(B).
'~Syntax.ConditionalFreeFormula.P1'('MetaDefs.<->''.F2'(A,B)) :-
        '~Syntax.ConditionalFreeFormula.P1'(A),
        '~Syntax.ConditionalFreeFormula.P1'(B).
'Syntax.ConditionalFreeFormula.P1'('MetaDefs.~''.F1'(A)) :-
        'Syntax.ConditionalFreeFormula.P1'(A).
'~Syntax.ConditionalFreeFormula.P1'('MetaDefs.~''.F1'(A)) :-
        '~Syntax.ConditionalFreeFormula.P1'(A).
'Syntax.ConditionalFreeFormula.P1'('MetaDefs.Some.F2'(_,A)) :-
        'Syntax.ConditionalFreeFormula.P1'(A).
'~Syntax.ConditionalFreeFormula.P1'('MetaDefs.Some.F2'(_,A)) :-
        '~Syntax.ConditionalFreeFormula.P1'(A).
'Syntax.ConditionalFreeFormula.P1'('MetaDefs.All.F2'(_,A)) :-
        'Syntax.ConditionalFreeFormula.P1'(A).
'~Syntax.ConditionalFreeFormula.P1'('MetaDefs.All.F2'(_,A)) :-
        '~Syntax.ConditionalFreeFormula.P1'(A).
'Syntax.ConditionalFreeFormula.P1'('MetaDefs.Commit.F2'(_,A)) :-
        'Syntax.ConditionalFreeFormula.P1'(A).
'~Syntax.ConditionalFreeFormula.P1'('MetaDefs.Commit.F2'(_,A)) :-
        '~Syntax.ConditionalFreeFormula.P1'(A).
'Syntax.ConjunctionOfLiterals.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.ConjunctionOfLiterals.P1.0'(A)).
'~Syntax.ConjunctionOfLiterals.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.ConjunctionOfLiterals.P1.0'(A)).
'Syntax.ConjunctionOfLiterals.P1.0'(A) :-
        user:goedel_freeze(ground([A]), ('Syntax':'~Syntax.Literal.P1'(A),true->true;A='MetaDefs.&''.F2'(B,C),'Syntax':'Syntax.ConjunctionOfLiterals.P1'(B),'Syntax':'Syntax.ConjunctionOfLiterals.P1'(C))).
'~Syntax.ConjunctionOfLiterals.P1.0'(A) :-
        user:goedel_freeze(ground([A]), ('Syntax':'~Syntax.Literal.P1'(A),true->true;A='MetaDefs.&''.F2'(B,C),'Syntax':'~Syntax.ConjunctionOfLiterals.P1'(B),'Syntax':'~Syntax.ConjunctionOfLiterals.P1'(C))).
'Syntax.ConstantTerm.P2'('MetaDefs.CTerm.F1'(A), A).
'~Syntax.ConstantTerm.P2'('MetaDefs.CTerm.F1'(A), A).
'Syntax.ConstantTerm.P2'('MetaDefs.Int.F1'(A), 'MetaDefs.Name.F4'('"Integers',B,'MetaDefs.Constant.C0',0)) :-
        'SharedSyntax':'SharedSyntax.IntegerToCharDL.P3'(A, C, []),
        'Strings':'Strings.StringInts.P2'(B, C).
'~Syntax.ConstantTerm.P2'('MetaDefs.Int.F1'(A), 'MetaDefs.Name.F4'('"Integers',B,'MetaDefs.Constant.C0',0)) :-
        'SharedSyntax':'~SharedSyntax.IntegerToCharDL.P3'(A, C, []),
        'Strings':'~Strings.StringInts.P2'(B, C).
'Syntax.DefiniteGoal.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.DefiniteGoal.P1.0'(A)).
'~Syntax.DefiniteGoal.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.DefiniteGoal.P1.0'(A)).
'Syntax.DefiniteGoal.P1.0'('MetaDefs.<-''.F2'('MetaDefs.Empty.C0',A)) :-
        'Syntax.DefiniteBody.P1'(A).
'~Syntax.DefiniteGoal.P1.0'('MetaDefs.<-''.F2'('MetaDefs.Empty.C0',A)) :-
        '~Syntax.DefiniteBody.P1'(A).
'Syntax.DefiniteBody.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.DefiniteBody.P1.0'(A)).
'~Syntax.DefiniteBody.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.DefiniteBody.P1.0'(A)).
'Syntax.DefiniteBody.P1.0'('MetaDefs.Empty.C0').
'~Syntax.DefiniteBody.P1.0'('MetaDefs.Empty.C0').
'Syntax.DefiniteBody.P1.0'('MetaDefs.PAtom.F1'(_)).
'~Syntax.DefiniteBody.P1.0'('MetaDefs.PAtom.F1'(_)).
'Syntax.DefiniteBody.P1.0'('MetaDefs.Atom.F2'(_,_)).
'~Syntax.DefiniteBody.P1.0'('MetaDefs.Atom.F2'(_,_)).
'Syntax.DefiniteBody.P1.0'('MetaDefs.XPAtom.F1'(_)).
'~Syntax.DefiniteBody.P1.0'('MetaDefs.XPAtom.F1'(_)).
'Syntax.DefiniteBody.P1.0'('MetaDefs.XAtom.F2'(_,_)).
'~Syntax.DefiniteBody.P1.0'('MetaDefs.XAtom.F2'(_,_)).
'Syntax.DefiniteBody.P1.0'('MetaDefs.&''.F2'(A,B)) :-
        'Syntax.DefiniteBody.P1'(A),
        'Syntax.DefiniteBody.P1'(B).
'~Syntax.DefiniteBody.P1.0'('MetaDefs.&''.F2'(A,B)) :-
        '~Syntax.DefiniteBody.P1'(A),
        '~Syntax.DefiniteBody.P1'(B).
'Syntax.DefiniteBody.P1.0'('MetaDefs.Commit.F2'(_,A)) :-
        'Syntax.DefiniteBody.P1'(A).
'~Syntax.DefiniteBody.P1.0'('MetaDefs.Commit.F2'(_,A)) :-
        '~Syntax.DefiniteBody.P1'(A).
'Syntax.DefiniteStatement.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.DefiniteStatement.P1.0'(A)).
'~Syntax.DefiniteStatement.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.DefiniteStatement.P1.0'(A)).
'Syntax.DefiniteStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.PAtom.F1'(_),A)) :-
        'Syntax.DefiniteBody.P1'(A).
'~Syntax.DefiniteStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.PAtom.F1'(_),A)) :-
        '~Syntax.DefiniteBody.P1'(A).
'Syntax.DefiniteStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.Atom.F2'(_,_),A)) :-
        'Syntax.DefiniteBody.P1'(A).
'~Syntax.DefiniteStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.Atom.F2'(_,_),A)) :-
        '~Syntax.DefiniteBody.P1'(A).
'Syntax.DefiniteStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.XPAtom.F1'(_),A)) :-
        'Syntax.DefiniteBody.P1'(A).
'~Syntax.DefiniteStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.XPAtom.F1'(_),A)) :-
        '~Syntax.DefiniteBody.P1'(A).
'Syntax.DefiniteStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.XAtom.F2'(_,_),A)) :-
        'Syntax.DefiniteBody.P1'(A).
'~Syntax.DefiniteStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.XAtom.F2'(_,_),A)) :-
        '~Syntax.DefiniteBody.P1'(A).
'Syntax.DefiniteResultant.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.DefiniteResultant.P1.0'(A)).
'~Syntax.DefiniteResultant.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.DefiniteResultant.P1.0'(A)).
'Syntax.DefiniteResultant.P1.0'('MetaDefs.<-''.F2'(A,B)) :-
        'Syntax.DefiniteBody.P1'(A),
        'Syntax.DefiniteBody.P1'(B).
'~Syntax.DefiniteResultant.P1.0'('MetaDefs.<-''.F2'(A,B)) :-
        '~Syntax.DefiniteBody.P1'(A),
        '~Syntax.DefiniteBody.P1'(B).
'Syntax.DelBindingInTermSubst.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.DelBindingInTermSubst.P4.0'(A,B,C,D)).
'~Syntax.DelBindingInTermSubst.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.DelBindingInTermSubst.P4.0'(A,B,C,D)).
'Syntax.DelBindingInTermSubst.P4.0'(A, B, C, D) :-
        'Substs':'Substs.ComposeTermSubsts1.P3'(A, A, E),
        'Substs':'Substs.SDelBindingInTermSubst.P4'(E, B, C, D).
'~Syntax.DelBindingInTermSubst.P4.0'(A, B, C, D) :-
        'Substs':'~Substs.ComposeTermSubsts1.P3'(A, A, E),
        'Substs':'~Substs.SDelBindingInTermSubst.P4'(E, B, C, D).
'Syntax.FormulaMaxVarIndex.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.FormulaMaxVarIndex.P2.0'(A,B)).
'~Syntax.FormulaMaxVarIndex.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.FormulaMaxVarIndex.P2.0'(A,B)).
'Syntax.FormulaMaxVarIndex.P2.0'(A, B) :-
        'SharedSyntax':'SharedSyntax.SFormulaMaxVarIndex.P2'(A, B).
'~Syntax.FormulaMaxVarIndex.P2.0'(A, B) :-
        'SharedSyntax':'~SharedSyntax.SFormulaMaxVarIndex.P2'(A, B).
'Syntax.EmptyFormula.P1'('MetaDefs.Empty.C0').
'~Syntax.EmptyFormula.P1'('MetaDefs.Empty.C0').
'Syntax.DeleteBindings.P3'([], A, A).
'~Syntax.DeleteBindings.P3'([], A, A).
'Syntax.DeleteBindings.P3'([A|B], C, D) :-
        user:goedel_freeze(ground([C,A]), if(('Syntax':'~Syntax.DelBindingInTermSubst.P4'(C,A,_,E),true),'Syntax':'Syntax.DeleteBindings.P3'(B,E,D),'Syntax':'Syntax.DeleteBindings.P3'(B,C,D))).
'~Syntax.DeleteBindings.P3'([A|B], C, D) :-
        user:goedel_freeze(ground([C,A]), if(('Syntax':'~Syntax.DelBindingInTermSubst.P4'(C,A,_,E),true),'Syntax':'~Syntax.DeleteBindings.P3'(B,E,D),'Syntax':'~Syntax.DeleteBindings.P3'(B,C,D))).
'Syntax.DelBindingInVarTyping.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.DelBindingInVarTyping.P4.0'(A,B,C,D)).
'~Syntax.DelBindingInVarTyping.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.DelBindingInVarTyping.P4.0'(A,B,C,D)).
'Syntax.DelBindingInVarTyping.P4.0'('MetaDefs.VarTyping.F1'(A), B, C, 'MetaDefs.VarTyping.F1'(D)) :-
        'ExtraSyntax':'ExtraSyntax.EDelBindingInVarTyping.P4'(A, B, C, D).
'~Syntax.DelBindingInVarTyping.P4.0'('MetaDefs.VarTyping.F1'(A), B, C, 'MetaDefs.VarTyping.F1'(D)) :-
        'ExtraSyntax':'~ExtraSyntax.EDelBindingInVarTyping.P4'(A, B, C, D).
'Syntax.DeleteAllVars.P3'([], _, []).
'~Syntax.DeleteAllVars.P3'([], _, []).
'Syntax.DeleteAllVars.P3'([A|B], C, D) :-
        user:goedel_freeze(ground([C,A]), ('Lists':'~Lists.MemberCheck.P2'(A,C),true->'Syntax':'Syntax.DeleteAllVars.P3'(B,C,D);D=[A|E],'Syntax':'Syntax.DeleteAllVars.P3'(B,C,E))).
'~Syntax.DeleteAllVars.P3'([A|B], C, D) :-
        user:goedel_freeze(ground([C,A]), ('Lists':'~Lists.MemberCheck.P2'(A,C),true->'Syntax':'~Syntax.DeleteAllVars.P3'(B,C,D);D=[A|E],'Syntax':'~Syntax.DeleteAllVars.P3'(B,C,E))).
'Syntax.Derive.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C,D,E]), 'Syntax':'Syntax.Derive.P7.0'(A,B,C,D,E,F,G)).
'~Syntax.Derive.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C,D,E]), 'Syntax':'~Syntax.Derive.P7.0'(A,B,C,D,E,F,G)).
'Syntax.Derive.P7.0'(A, B, 'MetaDefs.PAtom.F1'(C), D, 'MetaDefs.<-''.F2'('MetaDefs.PAtom.F1'(C),E), F, 'MetaDefs.<-''.F2'(A,G)) :- !,
        'Syntax.NormalBody.P1'(E),
        'Syntax.AndWithEmpty.P3'(B, E, H),
        'Syntax.AndWithEmpty.P3'(H, D, G),
        'Syntax.EmptyTermSubst.P1'(F).
'~Syntax.Derive.P7.0'(A, B, 'MetaDefs.PAtom.F1'(C), D, 'MetaDefs.<-''.F2'('MetaDefs.PAtom.F1'(C),E), F, 'MetaDefs.<-''.F2'(A,G)) :-
        true,
        '~Syntax.NormalBody.P1'(E),
        '~Syntax.AndWithEmpty.P3'(B, E, H),
        '~Syntax.AndWithEmpty.P3'(H, D, G),
        '~Syntax.EmptyTermSubst.P1'(F).
'Syntax.Derive.P7.0'(A, B, 'MetaDefs.Atom.F2'(C,D), E, 'MetaDefs.<-''.F2'('MetaDefs.Atom.F2'(C,F),G), H, 'MetaDefs.<-''.F2'(I,J)) :- !,
        'Syntax.NormalBody.P1'(G),
        'Syntax.EmptyTermSubst.P1'(K),
        'SharedSyntax':'SharedSyntax.UnifyingTermSubst.P4'(D, F, K, H),
        'Syntax.ApplySubstToFormula.P3'(A, H, I),
        'Syntax.ApplySubstToFormula.P3'(B, H, L),
        'Syntax.ApplySubstToFormula.P3'(E, H, M),
        'Syntax.ApplySubstToFormula.P3'(G, H, N),
        'Syntax.AndWithEmpty.P3'(L, N, O),
        'Syntax.AndWithEmpty.P3'(O, M, J).
'~Syntax.Derive.P7.0'(A, B, 'MetaDefs.Atom.F2'(C,D), E, 'MetaDefs.<-''.F2'('MetaDefs.Atom.F2'(C,F),G), H, 'MetaDefs.<-''.F2'(I,J)) :-
        true,
        '~Syntax.NormalBody.P1'(G),
        '~Syntax.EmptyTermSubst.P1'(K),
        'SharedSyntax':'~SharedSyntax.UnifyingTermSubst.P4'(D, F, K, H),
        '~Syntax.ApplySubstToFormula.P3'(A, H, I),
        '~Syntax.ApplySubstToFormula.P3'(B, H, L),
        '~Syntax.ApplySubstToFormula.P3'(E, H, M),
        '~Syntax.ApplySubstToFormula.P3'(G, H, N),
        '~Syntax.AndWithEmpty.P3'(L, N, O),
        '~Syntax.AndWithEmpty.P3'(O, M, J).
'Syntax.Derive.P7.0'(A, B, 'MetaDefs.XPAtom.F1'(C), D, 'MetaDefs.<-''.F2'('MetaDefs.XPAtom.F1'(C),E), F, 'MetaDefs.<-''.F2'(A,G)) :- !,
        'Syntax.NormalBody.P1'(E),
        'Syntax.AndWithEmpty.P3'(B, E, H),
        'Syntax.AndWithEmpty.P3'(H, D, G),
        'Syntax.EmptyTermSubst.P1'(F).
'~Syntax.Derive.P7.0'(A, B, 'MetaDefs.XPAtom.F1'(C), D, 'MetaDefs.<-''.F2'('MetaDefs.XPAtom.F1'(C),E), F, 'MetaDefs.<-''.F2'(A,G)) :-
        true,
        '~Syntax.NormalBody.P1'(E),
        '~Syntax.AndWithEmpty.P3'(B, E, H),
        '~Syntax.AndWithEmpty.P3'(H, D, G),
        '~Syntax.EmptyTermSubst.P1'(F).
'Syntax.Derive.P7.0'(A, B, 'MetaDefs.XAtom.F2'(C,D), E, 'MetaDefs.<-''.F2'('MetaDefs.XAtom.F2'(C,F),G), H, 'MetaDefs.<-''.F2'(I,J)) :- !,
        'Syntax.NormalBody.P1'(G),
        'Syntax.EmptyTermSubst.P1'(K),
        'SharedSyntax':'SharedSyntax.UnifyingTermSubst.P4'(D, F, K, H),
        'Syntax.ApplySubstToFormula.P3'(A, H, I),
        'Syntax.ApplySubstToFormula.P3'(B, H, L),
        'Syntax.ApplySubstToFormula.P3'(E, H, M),
        'Syntax.ApplySubstToFormula.P3'(G, H, N),
        'Syntax.AndWithEmpty.P3'(L, N, O),
        'Syntax.AndWithEmpty.P3'(O, M, J).
'~Syntax.Derive.P7.0'(A, B, 'MetaDefs.XAtom.F2'(C,D), E, 'MetaDefs.<-''.F2'('MetaDefs.XAtom.F2'(C,F),G), H, 'MetaDefs.<-''.F2'(I,J)) :-
        true,
        '~Syntax.NormalBody.P1'(G),
        '~Syntax.EmptyTermSubst.P1'(K),
        'SharedSyntax':'~SharedSyntax.UnifyingTermSubst.P4'(D, F, K, H),
        '~Syntax.ApplySubstToFormula.P3'(A, H, I),
        '~Syntax.ApplySubstToFormula.P3'(B, H, L),
        '~Syntax.ApplySubstToFormula.P3'(E, H, M),
        '~Syntax.ApplySubstToFormula.P3'(G, H, N),
        '~Syntax.AndWithEmpty.P3'(L, N, O),
        '~Syntax.AndWithEmpty.P3'(O, M, J).
'Syntax.DeleteVarTypeBindings.P3'([], A, A).
'~Syntax.DeleteVarTypeBindings.P3'([], A, A).
'Syntax.DeleteVarTypeBindings.P3'([A|B], C, D) :-
        user:goedel_freeze(ground([C,A]), if(('Lists':'~Lists.DeleteFirst.P3'('MetaDefs.@.F2'(A,_),C,E),true),'Syntax':'Syntax.DeleteVarTypeBindings.P3'(B,E,D),'Syntax':'Syntax.DeleteVarTypeBindings.P3'(B,C,D))).
'~Syntax.DeleteVarTypeBindings.P3'([A|B], C, D) :-
        user:goedel_freeze(ground([C,A]), if(('Lists':'~Lists.DeleteFirst.P3'('MetaDefs.@.F2'(A,_),C,E),true),'Syntax':'~Syntax.DeleteVarTypeBindings.P3'(B,E,D),'Syntax':'~Syntax.DeleteVarTypeBindings.P3'(B,C,D))).
'Syntax.Equivalent.P3'(A, B, 'MetaDefs.<->''.F2'(A,B)).
'~Syntax.Equivalent.P3'(A, B, 'MetaDefs.<->''.F2'(A,B)).
'Syntax.EmptyTypeSubst.P1'('Substs.TypeSubst.F2'(A,[])) :-
        'Substs':'Substs.EmptyHeap.P1'(A).
'~Syntax.EmptyTypeSubst.P1'('Substs.TypeSubst.F2'(A,[])) :-
        'Substs':'~Substs.EmptyHeap.P1'(A).
'Syntax.EmptyTermSubst.P1'(A) :-
        'SharedSyntax':'SharedSyntax.SEmptyTermSubst.P1'(A).
'~Syntax.EmptyTermSubst.P1'(A) :-
        'SharedSyntax':'~SharedSyntax.SEmptyTermSubst.P1'(A).
'Syntax.EmptyVarTyping.P1'('MetaDefs.VarTyping.F1'([])).
'~Syntax.EmptyVarTyping.P1'('MetaDefs.VarTyping.F1'([])).
'Syntax.FindTypeBindings.P3'([], _, []).
'~Syntax.FindTypeBindings.P3'([], _, []).
'Syntax.FindTypeBindings.P3'([A|B], C, D) :-
        user:goedel_freeze(ground([C,A]), if(('Syntax':'~Syntax.BindingInTypeSubst.P3'(C,A,E),true),D=['MetaDefs.!.F2'(A,E)|F],D=F)),
        'Syntax.FindTypeBindings.P3'(B, C, F).
'~Syntax.FindTypeBindings.P3'([A|B], C, D) :-
        user:goedel_freeze(ground([C,A]), if(('Syntax':'~Syntax.BindingInTypeSubst.P3'(C,A,E),true),D=['MetaDefs.!.F2'(A,E)|F],D=F)),
        '~Syntax.FindTypeBindings.P3'(B, C, F).
'Syntax.FindTermBindings.P3'([], _, []).
'~Syntax.FindTermBindings.P3'([], _, []).
'Syntax.FindTermBindings.P3'([A|B], C, D) :-
        user:goedel_freeze(ground([C,A]), if(('Syntax':'~Syntax.BindingInTermSubst.P3'(C,A,E),true),D=['MetaDefs.!.F2'(A,E)|F],D=F)),
        'Syntax.FindTermBindings.P3'(B, C, F).
'~Syntax.FindTermBindings.P3'([A|B], C, D) :-
        user:goedel_freeze(ground([C,A]), if(('Syntax':'~Syntax.BindingInTermSubst.P3'(C,A,E),true),D=['MetaDefs.!.F2'(A,E)|F],D=F)),
        '~Syntax.FindTermBindings.P3'(B, C, F).
'Syntax.GroundAtom.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.GroundAtom.P1.0'(A)).
'~Syntax.GroundAtom.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.GroundAtom.P1.0'(A)).
'Syntax.GroundAtom.P1.0'('MetaDefs.PAtom.F1'(_)).
'~Syntax.GroundAtom.P1.0'('MetaDefs.PAtom.F1'(_)).
'Syntax.GroundAtom.P1.0'('MetaDefs.Atom.F2'(_,A)) :-
        'Syntax.GroundTerm1.P1'(A).
'~Syntax.GroundAtom.P1.0'('MetaDefs.Atom.F2'(_,A)) :-
        '~Syntax.GroundTerm1.P1'(A).
'Syntax.GroundAtom.P1.0'('MetaDefs.XPAtom.F1'(_)).
'~Syntax.GroundAtom.P1.0'('MetaDefs.XPAtom.F1'(_)).
'Syntax.GroundAtom.P1.0'('MetaDefs.XAtom.F2'(_,A)) :-
        'Syntax.GroundTerm1.P1'(A).
'~Syntax.GroundAtom.P1.0'('MetaDefs.XAtom.F2'(_,A)) :-
        '~Syntax.GroundTerm1.P1'(A).
'Syntax.FunctionTerm.P3'('MetaDefs.Term.F2'(A,B), A, B).
'~Syntax.FunctionTerm.P3'('MetaDefs.Term.F2'(A,B), A, B).
'Syntax.FormulaVariables.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.FormulaVariables.P2.0'(A,B)).
'~Syntax.FormulaVariables.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.FormulaVariables.P2.0'(A,B)).
'Syntax.FormulaVariables.P2.0'(A, B) :-
        'SharedSyntax':'SharedSyntax.SFormulaFreeVars.P2'(A, B).
'~Syntax.FormulaVariables.P2.0'(A, B) :-
        'SharedSyntax':'~SharedSyntax.SFormulaFreeVars.P2'(A, B).
'Syntax.Goal.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.Goal.P1.0'(A)).
'~Syntax.Goal.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.Goal.P1.0'(A)).
'Syntax.Goal.P1.0'('MetaDefs.<-''.F2'('MetaDefs.Empty.C0',_)).
'~Syntax.Goal.P1.0'('MetaDefs.<-''.F2'('MetaDefs.Empty.C0',_)).
'Syntax.GetParameter.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), (user:not_equal([],[A,B],A,B),true->'Syntax':'Syntax.TypeNotOccur.P2'(A,B),'Substs':'Substs.BindParameter.P4'(B,A,C,D);D=C)).
'~Syntax.GetParameter.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), (user:not_equal([],[A,B],A,B),true->'Syntax':'~Syntax.TypeNotOccur.P2'(A,B),'Substs':'~Substs.BindParameter.P4'(B,A,C,D);D=C)).
'Syntax.GroundTerm.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.GroundTerm.P1.0'(A)).
'~Syntax.GroundTerm.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.GroundTerm.P1.0'(A)).
'Syntax.GroundTerm.P1.0'('MetaDefs.CTerm.F1'(_)).
'~Syntax.GroundTerm.P1.0'('MetaDefs.CTerm.F1'(_)).
'Syntax.GroundTerm.P1.0'('MetaDefs.Term.F2'(_,A)) :-
        'Syntax.GroundTerm1.P1'(A).
'~Syntax.GroundTerm.P1.0'('MetaDefs.Term.F2'(_,A)) :-
        '~Syntax.GroundTerm1.P1'(A).
'Syntax.GroundTerm.P1.0'('MetaDefs.XCTerm.F2'(_,_)).
'~Syntax.GroundTerm.P1.0'('MetaDefs.XCTerm.F2'(_,_)).
'Syntax.GroundTerm.P1.0'('MetaDefs.Int.F1'(_)).
'~Syntax.GroundTerm.P1.0'('MetaDefs.Int.F1'(_)).
'Syntax.GroundTerm.P1.0'('MetaDefs.Str.F1'(_)).
'~Syntax.GroundTerm.P1.0'('MetaDefs.Str.F1'(_)).
'Syntax.GroundTerm.P1.0'('MetaDefs.Prm.F1'(_)).
'~Syntax.GroundTerm.P1.0'('MetaDefs.Prm.F1'(_)).
'Syntax.GroundTerm.P1.0'('MetaDefs.XTerm.F3'(_,A,_)) :-
        'Syntax.GroundTerm1.P1'(A).
'~Syntax.GroundTerm.P1.0'('MetaDefs.XTerm.F3'(_,A,_)) :-
        '~Syntax.GroundTerm1.P1'(A).
'Syntax.GroundTerm1.P1'([]).
'~Syntax.GroundTerm1.P1'([]).
'Syntax.GroundTerm1.P1'([A|B]) :-
        'Syntax.GroundTerm.P1'(A),
        'Syntax.GroundTerm1.P1'(B).
'~Syntax.GroundTerm1.P1'([A|B]) :-
        '~Syntax.GroundTerm.P1'(A),
        '~Syntax.GroundTerm1.P1'(B).
'Syntax.Resultant.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.Resultant.P1.0'(A)).
'~Syntax.Resultant.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.Resultant.P1.0'(A)).
'Syntax.Resultant.P1.0'('MetaDefs.<-''.F2'(_,_)).
'~Syntax.Resultant.P1.0'('MetaDefs.<-''.F2'(_,_)).
'Syntax.Parameter.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.Parameter.P1.0'(A)).
'~Syntax.Parameter.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.Parameter.P1.0'(A)).
'Syntax.Parameter.P1.0'('MetaDefs.Par.F1'(_)).
'~Syntax.Parameter.P1.0'('MetaDefs.Par.F1'(_)).
'Syntax.Parameter.P1.0'('MetaDefs.Par.F2'(_,_)).
'~Syntax.Parameter.P1.0'('MetaDefs.Par.F2'(_,_)).
'Syntax.NormalBody.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.NormalBody.P1.0'(A)).
'~Syntax.NormalBody.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.NormalBody.P1.0'(A)).
'Syntax.NormalBody.P1.0'('MetaDefs.Empty.C0').
'~Syntax.NormalBody.P1.0'('MetaDefs.Empty.C0').
'Syntax.NormalBody.P1.0'('MetaDefs.PAtom.F1'(_)).
'~Syntax.NormalBody.P1.0'('MetaDefs.PAtom.F1'(_)).
'Syntax.NormalBody.P1.0'('MetaDefs.Atom.F2'(_,_)).
'~Syntax.NormalBody.P1.0'('MetaDefs.Atom.F2'(_,_)).
'Syntax.NormalBody.P1.0'('MetaDefs.XPAtom.F1'(_)).
'~Syntax.NormalBody.P1.0'('MetaDefs.XPAtom.F1'(_)).
'Syntax.NormalBody.P1.0'('MetaDefs.XAtom.F2'(_,_)).
'~Syntax.NormalBody.P1.0'('MetaDefs.XAtom.F2'(_,_)).
'Syntax.NormalBody.P1.0'('MetaDefs.&''.F2'(A,B)) :-
        'Syntax.NormalBody.P1'(A),
        'Syntax.NormalBody.P1'(B).
'~Syntax.NormalBody.P1.0'('MetaDefs.&''.F2'(A,B)) :-
        '~Syntax.NormalBody.P1'(A),
        '~Syntax.NormalBody.P1'(B).
'Syntax.NormalBody.P1.0'('MetaDefs.~''.F1'(A)) :-
        'Syntax.NormalBody.P1'(A).
'~Syntax.NormalBody.P1.0'('MetaDefs.~''.F1'(A)) :-
        '~Syntax.NormalBody.P1'(A).
'Syntax.NormalBody.P1.0'('MetaDefs.IT.F2'(A,B)) :-
        'Syntax.ConditionalFreeFormula.P1'(A),
        'Syntax.ConditionalFreeFormula.P1'(B).
'~Syntax.NormalBody.P1.0'('MetaDefs.IT.F2'(A,B)) :-
        '~Syntax.ConditionalFreeFormula.P1'(A),
        '~Syntax.ConditionalFreeFormula.P1'(B).
'Syntax.NormalBody.P1.0'('MetaDefs.ITE.F3'(A,B,_)) :-
        'Syntax.ConditionalFreeFormula.P1'(A),
        'Syntax.ConditionalFreeFormula.P1'(B).
'~Syntax.NormalBody.P1.0'('MetaDefs.ITE.F3'(A,B,_)) :-
        '~Syntax.ConditionalFreeFormula.P1'(A),
        '~Syntax.ConditionalFreeFormula.P1'(B).
'Syntax.NormalBody.P1.0'('MetaDefs.IST.F3'(_,A,B)) :-
        'Syntax.ConditionalFreeFormula.P1'(A),
        'Syntax.ConditionalFreeFormula.P1'(B).
'~Syntax.NormalBody.P1.0'('MetaDefs.IST.F3'(_,A,B)) :-
        '~Syntax.ConditionalFreeFormula.P1'(A),
        '~Syntax.ConditionalFreeFormula.P1'(B).
'Syntax.NormalBody.P1.0'('MetaDefs.ISTE.F4'(_,A,B,_)) :-
        'Syntax.ConditionalFreeFormula.P1'(A),
        'Syntax.ConditionalFreeFormula.P1'(B).
'~Syntax.NormalBody.P1.0'('MetaDefs.ISTE.F4'(_,A,B,_)) :-
        '~Syntax.ConditionalFreeFormula.P1'(A),
        '~Syntax.ConditionalFreeFormula.P1'(B).
'Syntax.NormalBody.P1.0'('MetaDefs.Commit.F2'(_,A)) :-
        'Syntax.NormalBody.P1'(A).
'~Syntax.NormalBody.P1.0'('MetaDefs.Commit.F2'(_,A)) :-
        '~Syntax.NormalBody.P1'(A).
'Syntax.IsImpliedBy.P3'(A, B, 'MetaDefs.<-''.F2'(A,B)).
'~Syntax.IsImpliedBy.P3'(A, B, 'MetaDefs.<-''.F2'(A,B)).
'Syntax.IfThen.P3'(A, B, 'MetaDefs.IT.F2'(A,B)).
'~Syntax.IfThen.P3'(A, B, 'MetaDefs.IT.F2'(A,B)).
'Syntax.IfSomeThen.P4'(A, B, C, 'MetaDefs.IST.F3'(A,B,C)).
'~Syntax.IfSomeThen.P4'(A, B, C, 'MetaDefs.IST.F3'(A,B,C)).
'Syntax.GroundType1.P1'([]).
'~Syntax.GroundType1.P1'([]).
'Syntax.GroundType1.P1'([A|B]) :-
        'Syntax.GroundType.P1'(A),
        'Syntax.GroundType1.P1'(B).
'~Syntax.GroundType1.P1'([A|B]) :-
        '~Syntax.GroundType.P1'(A),
        '~Syntax.GroundType1.P1'(B).
'Syntax.IfSomeThenElse.P5'(A, B, C, D, 'MetaDefs.ISTE.F4'(A,B,C,D)).
'~Syntax.IfSomeThenElse.P5'(A, B, C, D, 'MetaDefs.ISTE.F4'(A,B,C,D)).
'Syntax.Implies.P3'(A, B, 'MetaDefs.->''.F2'(A,B)).
'~Syntax.Implies.P3'(A, B, 'MetaDefs.->''.F2'(A,B)).
'Syntax.IfThenElse.P4'(A, B, C, 'MetaDefs.ITE.F3'(A,B,C)).
'~Syntax.IfThenElse.P4'(A, B, C, 'MetaDefs.ITE.F3'(A,B,C)).
'Syntax.IntensionalSet.P3'(A, B, 'MetaDefs.SuchThat.F2'(A,B)).
'~Syntax.IntensionalSet.P3'(A, B, 'MetaDefs.SuchThat.F2'(A,B)).
'Syntax.NewCommit.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,D]), ('Lists':'~Lists.MemberCheck.P2'('Syntax.LabelPair.F2'(A,_),D),true->C=B,E=D;user:goedel_freeze(ground([B]),('Integers':'~Integers.>=.P2'(B,0),true->E=['Syntax.LabelPair.F2'(A,B)|D],'Integers':plus(B,1,F),C=F;E=['Syntax.LabelPair.F2'(A,A)|D],C=B)))).
'~Syntax.NewCommit.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,D]), ('Lists':'~Lists.MemberCheck.P2'('Syntax.LabelPair.F2'(A,_),D),true->C=B,E=D;user:goedel_freeze(ground([B]),('Integers':'~Integers.>=.P2'(B,0),true->E=['Syntax.LabelPair.F2'(A,B)|D],'Integers':plus(B,1,F),C=F;E=['Syntax.LabelPair.F2'(A,A)|D],C=B)))).
'Syntax.MaxParIndex.P3'([], A, A).
'~Syntax.MaxParIndex.P3'([], A, A).
'Syntax.MaxParIndex.P3'([A|B], C, D) :-
        'Syntax.MaxParIndex1.P3'(A, C, E),
        'Syntax.MaxParIndex.P3'(B, E, D).
'~Syntax.MaxParIndex.P3'([A|B], C, D) :-
        '~Syntax.MaxParIndex1.P3'(A, C, E),
        '~Syntax.MaxParIndex.P3'(B, E, D).
'Syntax.Literal.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.Literal.P1.0'(A)).
'~Syntax.Literal.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.Literal.P1.0'(A)).
'Syntax.Literal.P1.0'(A) :-
        user:goedel_freeze(ground([A]), ('Syntax':'~Syntax.Atom.P1'(A),true->true;A='MetaDefs.~''.F1'(B),'Syntax':'Syntax.Atom.P1'(B))).
'~Syntax.Literal.P1.0'(A) :-
        user:goedel_freeze(ground([A]), ('Syntax':'~Syntax.Atom.P1'(A),true->true;A='MetaDefs.~''.F1'(B),'Syntax':'~Syntax.Atom.P1'(B))).
'Syntax.MaxParIndex1.P3'('MetaDefs.Par.F1'(A), B, C) :-
        user:goedel_freeze(ground([B,A]), ('Integers':'~Integers.>.P2'(A,B),true->C=A;C=B)).
'~Syntax.MaxParIndex1.P3'('MetaDefs.Par.F1'(A), B, C) :-
        user:goedel_freeze(ground([B,A]), ('Integers':'~Integers.>.P2'(A,B),true->C=A;C=B)).
'Syntax.MaxParIndex1.P3'('MetaDefs.Par.F2'(_,A), B, C) :-
        user:goedel_freeze(ground([B,A]), ('Integers':'~Integers.>.P2'(A,B),true->C=A;C=B)).
'~Syntax.MaxParIndex1.P3'('MetaDefs.Par.F2'(_,A), B, C) :-
        user:goedel_freeze(ground([B,A]), ('Integers':'~Integers.>.P2'(A,B),true->C=A;C=B)).
'Syntax.MaxParIndex1.P3'('MetaDefs.BType.F1'(_), A, A).
'~Syntax.MaxParIndex1.P3'('MetaDefs.BType.F1'(_), A, A).
'Syntax.MaxParIndex1.P3'('MetaDefs.XBType.F1'(_), A, A).
'~Syntax.MaxParIndex1.P3'('MetaDefs.XBType.F1'(_), A, A).
'Syntax.MaxParIndex1.P3'('MetaDefs.Type.F2'(_,A), B, C) :-
        'Syntax.MaxParIndex.P3'(A, B, C).
'~Syntax.MaxParIndex1.P3'('MetaDefs.Type.F2'(_,A), B, C) :-
        '~Syntax.MaxParIndex.P3'(A, B, C).
'Syntax.MaxParIndex1.P3'('MetaDefs.XType.F2'(_,A), B, C) :-
        'Syntax.MaxParIndex.P3'(A, B, C).
'~Syntax.MaxParIndex1.P3'('MetaDefs.XType.F2'(_,A), B, C) :-
        '~Syntax.MaxParIndex.P3'(A, B, C).
'Syntax.NonParameter.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.NonParameter.P1.0'(A)).
'~Syntax.NonParameter.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.NonParameter.P1.0'(A)).
'Syntax.NonParameter.P1.0'('MetaDefs.BType.F1'(_)).
'~Syntax.NonParameter.P1.0'('MetaDefs.BType.F1'(_)).
'Syntax.NonParameter.P1.0'('MetaDefs.Type.F2'(_,_)).
'~Syntax.NonParameter.P1.0'('MetaDefs.Type.F2'(_,_)).
'Syntax.NonParameter.P1.0'('MetaDefs.XBType.F1'(_)).
'~Syntax.NonParameter.P1.0'('MetaDefs.XBType.F1'(_)).
'Syntax.NonParameter.P1.0'('MetaDefs.XType.F2'(_,_)).
'~Syntax.NonParameter.P1.0'('MetaDefs.XType.F2'(_,_)).
'Syntax.NonVariable.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.NonVariable.P1.0'(A)).
'~Syntax.NonVariable.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.NonVariable.P1.0'(A)).
'Syntax.NonVariable.P1.0'('MetaDefs.CTerm.F1'(_)).
'~Syntax.NonVariable.P1.0'('MetaDefs.CTerm.F1'(_)).
'Syntax.NonVariable.P1.0'('MetaDefs.Term.F2'(_,_)).
'~Syntax.NonVariable.P1.0'('MetaDefs.Term.F2'(_,_)).
'Syntax.NonVariable.P1.0'('MetaDefs.XCTerm.F2'(_,_)).
'~Syntax.NonVariable.P1.0'('MetaDefs.XCTerm.F2'(_,_)).
'Syntax.NonVariable.P1.0'('MetaDefs.XTerm.F3'(_,_,_)).
'~Syntax.NonVariable.P1.0'('MetaDefs.XTerm.F3'(_,_,_)).
'Syntax.NonVariable.P1.0'('MetaDefs.Int.F1'(_)).
'~Syntax.NonVariable.P1.0'('MetaDefs.Int.F1'(_)).
'Syntax.NonVariable.P1.0'('MetaDefs.Str.F1'(_)).
'~Syntax.NonVariable.P1.0'('MetaDefs.Str.F1'(_)).
'Syntax.NonVariable.P1.0'('MetaDefs.Prm.F1'(_)).
'~Syntax.NonVariable.P1.0'('MetaDefs.Prm.F1'(_)).
'Syntax.NormalStatement.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.NormalStatement.P1.0'(A)).
'~Syntax.NormalStatement.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.NormalStatement.P1.0'(A)).
'Syntax.NormalStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.PAtom.F1'(_),A)) :-
        'Syntax.NormalBody.P1'(A).
'~Syntax.NormalStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.PAtom.F1'(_),A)) :-
        '~Syntax.NormalBody.P1'(A).
'Syntax.NormalStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.Atom.F2'(_,_),A)) :-
        'Syntax.NormalBody.P1'(A).
'~Syntax.NormalStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.Atom.F2'(_,_),A)) :-
        '~Syntax.NormalBody.P1'(A).
'Syntax.NormalStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.XPAtom.F1'(_),A)) :-
        'Syntax.NormalBody.P1'(A).
'~Syntax.NormalStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.XPAtom.F1'(_),A)) :-
        '~Syntax.NormalBody.P1'(A).
'Syntax.NormalStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.XAtom.F2'(_,_),A)) :-
        'Syntax.NormalBody.P1'(A).
'~Syntax.NormalStatement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.XAtom.F2'(_,_),A)) :-
        '~Syntax.NormalBody.P1'(A).
'Syntax.NormalResultant.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.NormalResultant.P1.0'(A)).
'~Syntax.NormalResultant.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.NormalResultant.P1.0'(A)).
'Syntax.NormalResultant.P1.0'('MetaDefs.<-''.F2'(A,B)) :-
        'Syntax.NormalBody.P1'(A),
        'Syntax.NormalBody.P1'(B).
'~Syntax.NormalResultant.P1.0'('MetaDefs.<-''.F2'(A,B)) :-
        '~Syntax.NormalBody.P1'(A),
        '~Syntax.NormalBody.P1'(B).
'Syntax.NormalGoal.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.NormalGoal.P1.0'(A)).
'~Syntax.NormalGoal.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.NormalGoal.P1.0'(A)).
'Syntax.NormalGoal.P1.0'('MetaDefs.<-''.F2'('MetaDefs.Empty.C0',A)) :-
        'Syntax.NormalBody.P1'(A).
'~Syntax.NormalGoal.P1.0'('MetaDefs.<-''.F2'('MetaDefs.Empty.C0',A)) :-
        '~Syntax.NormalBody.P1'(A).
'Syntax.OpaqueTerm.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.OpaqueTerm.P1.0'(A)).
'~Syntax.OpaqueTerm.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.OpaqueTerm.P1.0'(A)).
'Syntax.OpaqueTerm.P1.0'('MetaDefs.XCTerm.F2'(_,_)).
'~Syntax.OpaqueTerm.P1.0'('MetaDefs.XCTerm.F2'(_,_)).
'Syntax.OpaqueTerm.P1.0'('MetaDefs.Str.F1'(_)).
'~Syntax.OpaqueTerm.P1.0'('MetaDefs.Str.F1'(_)).
'Syntax.OpaqueTerm.P1.0'('MetaDefs.Prm.F1'(_)).
'~Syntax.OpaqueTerm.P1.0'('MetaDefs.Prm.F1'(_)).
'Syntax.OpaqueTerm.P1.0'('MetaDefs.XTerm.F3'(_,_,_)).
'~Syntax.OpaqueTerm.P1.0'('MetaDefs.XTerm.F3'(_,_,_)).
'Syntax.NotMember.P2'([], _).
'~Syntax.NotMember.P2'([], _).
'Syntax.NotMember.P2'([A|B], C) :-
        user:not_equal([], [C,A], C, A),
        'Syntax.NotMember.P2'(B, C).
'~Syntax.NotMember.P2'([A|B], C) :-
        user:not_equal([], [C,A], C, A),
        '~Syntax.NotMember.P2'(B, C).
'Syntax.Not.P2'(A, 'MetaDefs.~''.F1'(A)).
'~Syntax.Not.P2'(A, 'MetaDefs.~''.F1'(A)).
'Syntax.OpaqueAtom.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.OpaqueAtom.P1.0'(A)).
'~Syntax.OpaqueAtom.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.OpaqueAtom.P1.0'(A)).
'Syntax.OpaqueAtom.P1.0'('MetaDefs.XPAtom.F1'(_)).
'~Syntax.OpaqueAtom.P1.0'('MetaDefs.XPAtom.F1'(_)).
'Syntax.OpaqueAtom.P1.0'('MetaDefs.XAtom.F2'(_,_)).
'~Syntax.OpaqueAtom.P1.0'('MetaDefs.XAtom.F2'(_,_)).
'Syntax.OpaqueType.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.OpaqueType.P1.0'(A)).
'~Syntax.OpaqueType.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.OpaqueType.P1.0'(A)).
'Syntax.OpaqueType.P1.0'('MetaDefs.XBType.F1'(_)).
'~Syntax.OpaqueType.P1.0'('MetaDefs.XBType.F1'(_)).
'Syntax.OpaqueType.P1.0'('MetaDefs.XType.F2'(_,_)).
'~Syntax.OpaqueType.P1.0'('MetaDefs.XType.F2'(_,_)).
'Syntax.Or.P3'(A, B, 'MetaDefs.\\/''.F2'(A,B)).
'~Syntax.Or.P3'(A, B, 'MetaDefs.\\/''.F2'(A,B)).
'Syntax.RenameTypes.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.RenameTypes.P3.0'(A,B,C)).
'~Syntax.RenameTypes.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.RenameTypes.P3.0'(A,B,C)).
'Syntax.RenameTypes.P3.0'(A, B, C) :-
        'Syntax.TypeMaxParIndex.P2'(A, D),
        'Syntax.RenameAllType2.P4'(B, D, [], C).
'~Syntax.RenameTypes.P3.0'(A, B, C) :-
        '~Syntax.TypeMaxParIndex.P2'(A, D),
        '~Syntax.RenameAllType2.P4'(B, D, [], C).
'Syntax.RenameAllTerm2.P4'([], _, _, []).
'~Syntax.RenameAllTerm2.P4'([], _, _, []).
'Syntax.RenameAllTerm2.P4'([A|B], C, D, [E|F]) :-
        'Syntax.RenameAllTerm3.P6'(A, C, G, D, H, E),
        'Syntax.RenameAllTerm2.P4'(B, G, H, F).
'~Syntax.RenameAllTerm2.P4'([A|B], C, D, [E|F]) :-
        '~Syntax.RenameAllTerm3.P6'(A, C, G, D, H, E),
        '~Syntax.RenameAllTerm2.P4'(B, G, H, F).
'Syntax.PredicateAtom.P3'('MetaDefs.Atom.F2'(A,B), A, B).
'~Syntax.PredicateAtom.P3'('MetaDefs.Atom.F2'(A,B), A, B).
'Syntax.ParamsInType.P3'('MetaDefs.Par.F2'(A,B), C, D) :-
        user:goedel_freeze(ground([B,A,C]), ('Lists':'~Lists.MemberCheck.P2'('MetaDefs.Par.F2'(A,B),C),true->D=C;D=['MetaDefs.Par.F2'(A,B)|C])).
'~Syntax.ParamsInType.P3'('MetaDefs.Par.F2'(A,B), C, D) :-
        user:goedel_freeze(ground([B,A,C]), ('Lists':'~Lists.MemberCheck.P2'('MetaDefs.Par.F2'(A,B),C),true->D=C;D=['MetaDefs.Par.F2'(A,B)|C])).
'Syntax.ParamsInType.P3'('MetaDefs.Par.F1'(A), B, C) :-
        user:goedel_freeze(ground([A,B]), ('Lists':'~Lists.MemberCheck.P2'('MetaDefs.Par.F1'(A),B),true->C=B;C=['MetaDefs.Par.F1'(A)|B])).
'~Syntax.ParamsInType.P3'('MetaDefs.Par.F1'(A), B, C) :-
        user:goedel_freeze(ground([A,B]), ('Lists':'~Lists.MemberCheck.P2'('MetaDefs.Par.F1'(A),B),true->C=B;C=['MetaDefs.Par.F1'(A)|B])).
'Syntax.ParamsInType.P3'('MetaDefs.BType.F1'(_), A, A).
'~Syntax.ParamsInType.P3'('MetaDefs.BType.F1'(_), A, A).
'Syntax.ParamsInType.P3'('MetaDefs.Type.F2'(_,A), B, C) :-
        'Syntax.ParamsInType1.P3'(A, B, C).
'~Syntax.ParamsInType.P3'('MetaDefs.Type.F2'(_,A), B, C) :-
        '~Syntax.ParamsInType1.P3'(A, B, C).
'Syntax.ParamsInType.P3'('MetaDefs.XBType.F1'(_), A, A).
'~Syntax.ParamsInType.P3'('MetaDefs.XBType.F1'(_), A, A).
'Syntax.ParamsInType.P3'('MetaDefs.XType.F2'(_,A), B, C) :-
        'Syntax.ParamsInType1.P3'(A, B, C).
'~Syntax.ParamsInType.P3'('MetaDefs.XType.F2'(_,A), B, C) :-
        '~Syntax.ParamsInType1.P3'(A, B, C).
'Syntax.ParameterName.P3'('MetaDefs.Par.F2'(A,B), A, B) :-
        user:not_equal([], [A], A, '"p').
'~Syntax.ParameterName.P3'('MetaDefs.Par.F2'(A,B), A, B) :-
        user:not_equal([], [A], A, '"p').
'Syntax.ParameterName.P3'('MetaDefs.Par.F1'(A), '"p', A).
'~Syntax.ParameterName.P3'('MetaDefs.Par.F1'(A), '"p', A).
'Syntax.ParamsInType1.P3'([], A, A).
'~Syntax.ParamsInType1.P3'([], A, A).
'Syntax.ParamsInType1.P3'([A|B], C, D) :-
        'Syntax.ParamsInType.P3'(A, C, E),
        'Syntax.ParamsInType1.P3'(B, E, D).
'~Syntax.ParamsInType1.P3'([A|B], C, D) :-
        '~Syntax.ParamsInType.P3'(A, C, E),
        '~Syntax.ParamsInType1.P3'(B, E, D).
'Syntax.RenameAllFormula2.P4'([], _, _, []).
'~Syntax.RenameAllFormula2.P4'([], _, _, []).
'Syntax.RenameAllFormula2.P4'([A|B], C, D, [E|F]) :-
        'Syntax.RenameAllFormula3.P6'(A, C, G, D, H, E),
        'Syntax.RenameAllFormula2.P4'(B, G, H, F).
'~Syntax.RenameAllFormula2.P4'([A|B], C, D, [E|F]) :-
        '~Syntax.RenameAllFormula3.P6'(A, C, G, D, H, E),
        '~Syntax.RenameAllFormula2.P4'(B, G, H, F).
'Syntax.PropositionAtom.P2'('MetaDefs.PAtom.F1'(A), A).
'~Syntax.PropositionAtom.P2'('MetaDefs.PAtom.F1'(A), A).
'Syntax.RenameAllFormula3.P6'('MetaDefs.Empty.C0', A, A, B, B, 'MetaDefs.Empty.C0').
'~Syntax.RenameAllFormula3.P6'('MetaDefs.Empty.C0', A, A, B, B, 'MetaDefs.Empty.C0').
'Syntax.RenameAllFormula3.P6'('MetaDefs.PAtom.F1'(A), B, B, C, C, 'MetaDefs.PAtom.F1'(A)).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.PAtom.F1'(A), B, B, C, C, 'MetaDefs.PAtom.F1'(A)).
'Syntax.RenameAllFormula3.P6'('MetaDefs.Atom.F2'(A,B), C, D, E, F, 'MetaDefs.Atom.F2'(A,G)) :-
        'Syntax.RenameAllTerm4.P6'(B, C, D, E, F, G).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.Atom.F2'(A,B), C, D, E, F, 'MetaDefs.Atom.F2'(A,G)) :-
        '~Syntax.RenameAllTerm4.P6'(B, C, D, E, F, G).
'Syntax.RenameAllFormula3.P6'('MetaDefs.XPAtom.F1'(A), B, B, C, C, 'MetaDefs.XPAtom.F1'(A)).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.XPAtom.F1'(A), B, B, C, C, 'MetaDefs.XPAtom.F1'(A)).
'Syntax.RenameAllFormula3.P6'('MetaDefs.XAtom.F2'(A,B), C, D, E, F, 'MetaDefs.XAtom.F2'(A,G)) :-
        'Syntax.RenameAllTerm4.P6'(B, C, D, E, F, G).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.XAtom.F2'(A,B), C, D, E, F, 'MetaDefs.XAtom.F2'(A,G)) :-
        '~Syntax.RenameAllTerm4.P6'(B, C, D, E, F, G).
'Syntax.RenameAllFormula3.P6'('MetaDefs.&''.F2'(A,B), C, D, E, F, 'MetaDefs.&''.F2'(G,H)) :-
        'Syntax.RenameAllFormula3.P6'(A, C, I, E, J, G),
        'Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.&''.F2'(A,B), C, D, E, F, 'MetaDefs.&''.F2'(G,H)) :-
        '~Syntax.RenameAllFormula3.P6'(A, C, I, E, J, G),
        '~Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'Syntax.RenameAllFormula3.P6'('MetaDefs.\\/''.F2'(A,B), C, D, E, F, 'MetaDefs.\\/''.F2'(G,H)) :-
        'Syntax.RenameAllFormula3.P6'(A, C, I, E, J, G),
        'Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.\\/''.F2'(A,B), C, D, E, F, 'MetaDefs.\\/''.F2'(G,H)) :-
        '~Syntax.RenameAllFormula3.P6'(A, C, I, E, J, G),
        '~Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'Syntax.RenameAllFormula3.P6'('MetaDefs.~''.F1'(A), B, C, D, E, 'MetaDefs.~''.F1'(F)) :-
        'Syntax.RenameAllFormula3.P6'(A, B, C, D, E, F).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.~''.F1'(A), B, C, D, E, 'MetaDefs.~''.F1'(F)) :-
        '~Syntax.RenameAllFormula3.P6'(A, B, C, D, E, F).
'Syntax.RenameAllFormula3.P6'('MetaDefs.<-''.F2'(A,B), C, D, E, F, 'MetaDefs.<-''.F2'(G,H)) :-
        'Syntax.RenameAllFormula3.P6'(A, C, I, E, J, G),
        'Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.<-''.F2'(A,B), C, D, E, F, 'MetaDefs.<-''.F2'(G,H)) :-
        '~Syntax.RenameAllFormula3.P6'(A, C, I, E, J, G),
        '~Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'Syntax.RenameAllFormula3.P6'('MetaDefs.->''.F2'(A,B), C, D, E, F, 'MetaDefs.->''.F2'(G,H)) :-
        'Syntax.RenameAllFormula3.P6'(A, C, I, E, J, G),
        'Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.->''.F2'(A,B), C, D, E, F, 'MetaDefs.->''.F2'(G,H)) :-
        '~Syntax.RenameAllFormula3.P6'(A, C, I, E, J, G),
        '~Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'Syntax.RenameAllFormula3.P6'('MetaDefs.<->''.F2'(A,B), C, D, E, F, 'MetaDefs.<->''.F2'(G,H)) :-
        'Syntax.RenameAllFormula3.P6'(A, C, I, E, J, G),
        'Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.<->''.F2'(A,B), C, D, E, F, 'MetaDefs.<->''.F2'(G,H)) :-
        '~Syntax.RenameAllFormula3.P6'(A, C, I, E, J, G),
        '~Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'Syntax.RenameAllFormula3.P6'('MetaDefs.All.F2'(A,B), C, D, E, F, 'MetaDefs.All.F2'(G,H)) :-
        'Syntax.RenameAllTerm4.P6'(A, C, I, E, J, G),
        'Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.All.F2'(A,B), C, D, E, F, 'MetaDefs.All.F2'(G,H)) :-
        '~Syntax.RenameAllTerm4.P6'(A, C, I, E, J, G),
        '~Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'Syntax.RenameAllFormula3.P6'('MetaDefs.Some.F2'(A,B), C, D, E, F, 'MetaDefs.Some.F2'(G,H)) :-
        'Syntax.RenameAllTerm4.P6'(A, C, I, E, J, G),
        'Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.Some.F2'(A,B), C, D, E, F, 'MetaDefs.Some.F2'(G,H)) :-
        '~Syntax.RenameAllTerm4.P6'(A, C, I, E, J, G),
        '~Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'Syntax.RenameAllFormula3.P6'('MetaDefs.IT.F2'(A,B), C, D, E, F, 'MetaDefs.IT.F2'(G,H)) :-
        'Syntax.RenameAllFormula3.P6'(A, C, I, E, J, G),
        'Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.IT.F2'(A,B), C, D, E, F, 'MetaDefs.IT.F2'(G,H)) :-
        '~Syntax.RenameAllFormula3.P6'(A, C, I, E, J, G),
        '~Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'Syntax.RenameAllFormula3.P6'('MetaDefs.ITE.F3'(A,B,C), D, E, F, G, 'MetaDefs.ITE.F3'(H,I,J)) :-
        'Syntax.RenameAllFormula3.P6'(A, D, K, F, L, H),
        'Syntax.RenameAllFormula3.P6'(B, K, M, L, N, I),
        'Syntax.RenameAllFormula3.P6'(C, M, E, N, G, J).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.ITE.F3'(A,B,C), D, E, F, G, 'MetaDefs.ITE.F3'(H,I,J)) :-
        '~Syntax.RenameAllFormula3.P6'(A, D, K, F, L, H),
        '~Syntax.RenameAllFormula3.P6'(B, K, M, L, N, I),
        '~Syntax.RenameAllFormula3.P6'(C, M, E, N, G, J).
'Syntax.RenameAllFormula3.P6'('MetaDefs.IST.F3'(A,B,C), D, E, F, G, 'MetaDefs.IST.F3'(H,I,J)) :-
        'Syntax.RenameAllTerm4.P6'(A, D, K, F, L, H),
        'Syntax.RenameAllFormula3.P6'(B, K, M, L, N, I),
        'Syntax.RenameAllFormula3.P6'(C, M, E, N, G, J).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.IST.F3'(A,B,C), D, E, F, G, 'MetaDefs.IST.F3'(H,I,J)) :-
        '~Syntax.RenameAllTerm4.P6'(A, D, K, F, L, H),
        '~Syntax.RenameAllFormula3.P6'(B, K, M, L, N, I),
        '~Syntax.RenameAllFormula3.P6'(C, M, E, N, G, J).
'Syntax.RenameAllFormula3.P6'('MetaDefs.ISTE.F4'(A,B,C,D), E, F, G, H, 'MetaDefs.ISTE.F4'(I,J,K,L)) :-
        'Syntax.RenameAllTerm4.P6'(A, E, M, G, N, I),
        'Syntax.RenameAllFormula3.P6'(B, M, O, N, P, J),
        'Syntax.RenameAllFormula3.P6'(C, O, Q, P, R, K),
        'Syntax.RenameAllFormula3.P6'(D, Q, F, R, H, L).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.ISTE.F4'(A,B,C,D), E, F, G, H, 'MetaDefs.ISTE.F4'(I,J,K,L)) :-
        '~Syntax.RenameAllTerm4.P6'(A, E, M, G, N, I),
        '~Syntax.RenameAllFormula3.P6'(B, M, O, N, P, J),
        '~Syntax.RenameAllFormula3.P6'(C, O, Q, P, R, K),
        '~Syntax.RenameAllFormula3.P6'(D, Q, F, R, H, L).
'Syntax.RenameAllFormula3.P6'('MetaDefs.Commit.F2'(A,B), C, D, E, F, 'MetaDefs.Commit.F2'(A,G)) :-
        'Syntax.RenameAllFormula3.P6'(B, C, D, E, F, G).
'~Syntax.RenameAllFormula3.P6'('MetaDefs.Commit.F2'(A,B), C, D, E, F, 'MetaDefs.Commit.F2'(A,G)) :-
        '~Syntax.RenameAllFormula3.P6'(B, C, D, E, F, G).
'Syntax.RenameAllType3.P6'('MetaDefs.Par.F2'(A,B), C, D, E, F, 'MetaDefs.Par.F1'(G)) :-
        user:goedel_freeze(ground([B,A,E]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),'MetaDefs.Par.F1'(H)),E),true),(G=H,F=E,D=C),(G=C,F=['MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),'MetaDefs.Par.F1'(C))|E],'Integers':plus(C,1,I),D=I))).
'~Syntax.RenameAllType3.P6'('MetaDefs.Par.F2'(A,B), C, D, E, F, 'MetaDefs.Par.F1'(G)) :-
        user:goedel_freeze(ground([B,A,E]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),'MetaDefs.Par.F1'(H)),E),true),(G=H,F=E,D=C),(G=C,F=['MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),'MetaDefs.Par.F1'(C))|E],'Integers':plus(C,1,I),D=I))).
'Syntax.RenameAllType3.P6'('MetaDefs.Par.F1'(A), B, C, D, E, 'MetaDefs.Par.F1'(F)) :-
        user:goedel_freeze(ground([A,D]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Par.F1'(A),'MetaDefs.Par.F1'(G)),D),true),(F=G,E=D,C=B),(F=B,E=['MetaDefs.!.F2'('MetaDefs.Par.F1'(A),'MetaDefs.Par.F1'(B))|D],'Integers':plus(B,1,H),C=H))).
'~Syntax.RenameAllType3.P6'('MetaDefs.Par.F1'(A), B, C, D, E, 'MetaDefs.Par.F1'(F)) :-
        user:goedel_freeze(ground([A,D]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Par.F1'(A),'MetaDefs.Par.F1'(G)),D),true),(F=G,E=D,C=B),(F=B,E=['MetaDefs.!.F2'('MetaDefs.Par.F1'(A),'MetaDefs.Par.F1'(B))|D],'Integers':plus(B,1,H),C=H))).
'Syntax.RenameAllType3.P6'('MetaDefs.BType.F1'(A), B, B, C, C, 'MetaDefs.BType.F1'(A)).
'~Syntax.RenameAllType3.P6'('MetaDefs.BType.F1'(A), B, B, C, C, 'MetaDefs.BType.F1'(A)).
'Syntax.RenameAllType3.P6'('MetaDefs.Type.F2'(A,B), C, D, E, F, 'MetaDefs.Type.F2'(A,G)) :-
        'Syntax.RenameAllType4.P6'(B, C, D, E, F, G).
'~Syntax.RenameAllType3.P6'('MetaDefs.Type.F2'(A,B), C, D, E, F, 'MetaDefs.Type.F2'(A,G)) :-
        '~Syntax.RenameAllType4.P6'(B, C, D, E, F, G).
'Syntax.RenameAllType3.P6'('MetaDefs.XBType.F1'(A), B, B, C, C, 'MetaDefs.XBType.F1'(A)).
'~Syntax.RenameAllType3.P6'('MetaDefs.XBType.F1'(A), B, B, C, C, 'MetaDefs.XBType.F1'(A)).
'Syntax.RenameAllType3.P6'('MetaDefs.XType.F2'(A,B), C, D, E, F, 'MetaDefs.XType.F2'(A,G)) :-
        'Syntax.RenameAllType4.P6'(B, C, D, E, F, G).
'~Syntax.RenameAllType3.P6'('MetaDefs.XType.F2'(A,B), C, D, E, F, 'MetaDefs.XType.F2'(A,G)) :-
        '~Syntax.RenameAllType4.P6'(B, C, D, E, F, G).
'Syntax.RenameAllTerm4.P6'([], A, A, B, B, []).
'~Syntax.RenameAllTerm4.P6'([], A, A, B, B, []).
'Syntax.RenameAllTerm4.P6'([A|B], C, D, E, F, [G|H]) :-
        'Syntax.RenameAllTerm3.P6'(A, C, I, E, J, G),
        'Syntax.RenameAllTerm4.P6'(B, I, D, J, F, H).
'~Syntax.RenameAllTerm4.P6'([A|B], C, D, E, F, [G|H]) :-
        '~Syntax.RenameAllTerm3.P6'(A, C, I, E, J, G),
        '~Syntax.RenameAllTerm4.P6'(B, I, D, J, F, H).
'Syntax.RenameAllTerm3.P6'('MetaDefs.Var.F2'(A,B), C, D, E, F, 'MetaDefs.Var.F1'(G)) :-
        user:goedel_freeze(ground([B,A,E]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),'MetaDefs.Var.F1'(H)),E),true),(G=H,F=E,D=C),(G=C,F=['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),'MetaDefs.Var.F1'(C))|E],'Integers':plus(C,1,I),D=I))).
'~Syntax.RenameAllTerm3.P6'('MetaDefs.Var.F2'(A,B), C, D, E, F, 'MetaDefs.Var.F1'(G)) :-
        user:goedel_freeze(ground([B,A,E]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),'MetaDefs.Var.F1'(H)),E),true),(G=H,F=E,D=C),(G=C,F=['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),'MetaDefs.Var.F1'(C))|E],'Integers':plus(C,1,I),D=I))).
'Syntax.RenameAllTerm3.P6'('MetaDefs.Var.F1'(A), B, C, D, E, 'MetaDefs.Var.F1'(F)) :-
        user:goedel_freeze(ground([A,D]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),'MetaDefs.Var.F1'(G)),D),true),(F=G,E=D,C=B),(F=B,E=['MetaDefs.!.F2'('MetaDefs.Var.F1'(A),'MetaDefs.Var.F1'(B))|D],'Integers':plus(B,1,H),C=H))).
'~Syntax.RenameAllTerm3.P6'('MetaDefs.Var.F1'(A), B, C, D, E, 'MetaDefs.Var.F1'(F)) :-
        user:goedel_freeze(ground([A,D]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),'MetaDefs.Var.F1'(G)),D),true),(F=G,E=D,C=B),(F=B,E=['MetaDefs.!.F2'('MetaDefs.Var.F1'(A),'MetaDefs.Var.F1'(B))|D],'Integers':plus(B,1,H),C=H))).
'Syntax.RenameAllTerm3.P6'('MetaDefs.CTerm.F1'(A), B, B, C, C, 'MetaDefs.CTerm.F1'(A)).
'~Syntax.RenameAllTerm3.P6'('MetaDefs.CTerm.F1'(A), B, B, C, C, 'MetaDefs.CTerm.F1'(A)).
'Syntax.RenameAllTerm3.P6'('MetaDefs.Term.F2'(A,B), C, D, E, F, 'MetaDefs.Term.F2'(A,G)) :-
        'Syntax.RenameAllTerm4.P6'(B, C, D, E, F, G).
'~Syntax.RenameAllTerm3.P6'('MetaDefs.Term.F2'(A,B), C, D, E, F, 'MetaDefs.Term.F2'(A,G)) :-
        '~Syntax.RenameAllTerm4.P6'(B, C, D, E, F, G).
'Syntax.RenameAllTerm3.P6'('MetaDefs.XCTerm.F2'(A,B), C, C, D, D, 'MetaDefs.XCTerm.F2'(A,B)).
'~Syntax.RenameAllTerm3.P6'('MetaDefs.XCTerm.F2'(A,B), C, C, D, D, 'MetaDefs.XCTerm.F2'(A,B)).
'Syntax.RenameAllTerm3.P6'('MetaDefs.Int.F1'(A), B, B, C, C, 'MetaDefs.Int.F1'(A)).
'~Syntax.RenameAllTerm3.P6'('MetaDefs.Int.F1'(A), B, B, C, C, 'MetaDefs.Int.F1'(A)).
'Syntax.RenameAllTerm3.P6'('MetaDefs.Str.F1'(A), B, B, C, C, 'MetaDefs.Str.F1'(A)).
'~Syntax.RenameAllTerm3.P6'('MetaDefs.Str.F1'(A), B, B, C, C, 'MetaDefs.Str.F1'(A)).
'Syntax.RenameAllTerm3.P6'('MetaDefs.Prm.F1'(A), B, B, C, C, 'MetaDefs.Prm.F1'(A)).
'~Syntax.RenameAllTerm3.P6'('MetaDefs.Prm.F1'(A), B, B, C, C, 'MetaDefs.Prm.F1'(A)).
'Syntax.RenameAllTerm3.P6'('MetaDefs.SuchThat.F2'(A,B), C, D, E, F, 'MetaDefs.SuchThat.F2'(G,H)) :-
        'Syntax.RenameAllTerm3.P6'(A, C, I, E, J, G),
        'Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'~Syntax.RenameAllTerm3.P6'('MetaDefs.SuchThat.F2'(A,B), C, D, E, F, 'MetaDefs.SuchThat.F2'(G,H)) :-
        '~Syntax.RenameAllTerm3.P6'(A, C, I, E, J, G),
        '~Syntax.RenameAllFormula3.P6'(B, I, D, J, F, H).
'Syntax.RenameAllTerm3.P6'('MetaDefs.XTerm.F3'(A,B,C), D, E, F, G, 'MetaDefs.XTerm.F3'(A,H,C)) :-
        'Syntax.RenameAllTerm4.P6'(B, D, E, F, G, H).
'~Syntax.RenameAllTerm3.P6'('MetaDefs.XTerm.F3'(A,B,C), D, E, F, G, 'MetaDefs.XTerm.F3'(A,H,C)) :-
        '~Syntax.RenameAllTerm4.P6'(B, D, E, F, G, H).
'Syntax.RenameAllType2.P4'([], _, _, []).
'~Syntax.RenameAllType2.P4'([], _, _, []).
'Syntax.RenameAllType2.P4'([A|B], C, D, [E|F]) :-
        'Syntax.RenameAllType3.P6'(A, C, G, D, H, E),
        'Syntax.RenameAllType2.P4'(B, G, H, F).
'~Syntax.RenameAllType2.P4'([A|B], C, D, [E|F]) :-
        '~Syntax.RenameAllType3.P6'(A, C, G, D, H, E),
        '~Syntax.RenameAllType2.P4'(B, G, H, F).
'Syntax.RenameFormulas.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.RenameFormulas.P3.0'(A,B,C)).
'~Syntax.RenameFormulas.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.RenameFormulas.P3.0'(A,B,C)).
'Syntax.RenameFormulas.P3.0'(A, B, C) :-
        'Syntax.FormulaMaxVarIndex.P2'(A, D),
        'Syntax.RenameAllFormula2.P4'(B, D, [], C).
'~Syntax.RenameFormulas.P3.0'(A, B, C) :-
        '~Syntax.FormulaMaxVarIndex.P2'(A, D),
        '~Syntax.RenameAllFormula2.P4'(B, D, [], C).
'Syntax.RenameAllType4.P6'([], A, A, B, B, []).
'~Syntax.RenameAllType4.P6'([], A, A, B, B, []).
'Syntax.RenameAllType4.P6'([A|B], C, D, E, F, [G|H]) :-
        'Syntax.RenameAllType3.P6'(A, C, I, E, J, G),
        'Syntax.RenameAllType4.P6'(B, I, D, J, F, H).
'~Syntax.RenameAllType4.P6'([A|B], C, D, E, F, [G|H]) :-
        '~Syntax.RenameAllType3.P6'(A, C, I, E, J, G),
        '~Syntax.RenameAllType4.P6'(B, I, D, J, F, H).
'Syntax.RenameTerms.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.RenameTerms.P3.0'(A,B,C)).
'~Syntax.RenameTerms.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.RenameTerms.P3.0'(A,B,C)).
'Syntax.RenameTerms.P3.0'(A, B, C) :-
        'Syntax.TermMaxVarIndex.P2'(A, D),
        'Syntax.RenameAllTerm2.P4'(B, D, [], C).
'~Syntax.RenameTerms.P3.0'(A, B, C) :-
        '~Syntax.TermMaxVarIndex.P2'(A, D),
        '~Syntax.RenameAllTerm2.P4'(B, D, [], C).
'Syntax.RestrictSubstToFormula.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.RestrictSubstToFormula.P3.0'(A,B,C)).
'~Syntax.RestrictSubstToFormula.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.RestrictSubstToFormula.P3.0'(A,B,C)).
'Syntax.RestrictSubstToFormula.P3.0'(A, B, C) :-
        'Syntax.FormulaVariables.P2'(A, D),
        'Syntax.FindTermBindings.P3'(D, B, E),
        'Syntax.EmptyTermSubst.P1'(F),
        'Syntax.AppendTermBindings.P3'(E, F, C).
'~Syntax.RestrictSubstToFormula.P3.0'(A, B, C) :-
        '~Syntax.FormulaVariables.P2'(A, D),
        '~Syntax.FindTermBindings.P3'(D, B, E),
        '~Syntax.EmptyTermSubst.P1'(F),
        '~Syntax.AppendTermBindings.P3'(E, F, C).
'Syntax.ResolveAll.P9'(A, B, C, D, E, F, G, H, I) :-
        user:goedel_freeze(ground([A,B,C,E,G]), 'Syntax':'Syntax.ResolveAll.P9.0'(A,B,C,D,E,F,G,H,I)).
'~Syntax.ResolveAll.P9'(A, B, C, D, E, F, G, H, I) :-
        user:goedel_freeze(ground([A,B,C,E,G]), 'Syntax':'~Syntax.ResolveAll.P9.0'(A,B,C,D,E,F,G,H,I)).
'Syntax.ResolveAll.P9.0'(A, B, C, D, E, F, G, H, I) :-
        'Integers':'Integers.>=.P2'(C, 0),
        'Integers':'Integers.>=.P2'(E, 0),
        'Syntax.ResolveAll1.P10'(B, A, C, D, E, F, [], G, H, I).
'~Syntax.ResolveAll.P9.0'(A, B, C, D, E, F, G, H, I) :-
        'Integers':'~Integers.>=.P2'(C, 0),
        'Integers':'~Integers.>=.P2'(E, 0),
        '~Syntax.ResolveAll1.P10'(B, A, C, D, E, F, [], G, H, I).
'Syntax.Resolve.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C,E]), 'Syntax':'Syntax.Resolve.P7.0'(A,B,C,D,E,F,G)).
'~Syntax.Resolve.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C,E]), 'Syntax':'~Syntax.Resolve.P7.0'(A,B,C,D,E,F,G)).
'Syntax.Resolve.P7.0'(A, 'MetaDefs.<-''.F2'(B,C), D, E, F, G, H) :-
        'Integers':negative(1, I),
        'Syntax.StandardiseCommits.P5'(C, I, _, [], J),
        'Syntax.Resolve1.P9'(A, B, C, D, E, J, F, G, H).
'~Syntax.Resolve.P7.0'(A, 'MetaDefs.<-''.F2'(B,C), D, E, F, G, H) :-
        'Integers':negative(1, I),
        '~Syntax.StandardiseCommits.P5'(C, I, _, [], J),
        '~Syntax.Resolve1.P9'(A, B, C, D, E, J, F, G, H).
'Syntax.Resolve1.P9'('MetaDefs.Atom.F2'(A,B), 'MetaDefs.Atom.F2'(A,C), D, E, F, G, H, I, J) :-
        'Syntax.UnifyArgs.P11'(B, C, E, F, [], [], H, I, G, D, J).
'~Syntax.Resolve1.P9'('MetaDefs.Atom.F2'(A,B), 'MetaDefs.Atom.F2'(A,C), D, E, F, G, H, I, J) :-
        '~Syntax.UnifyArgs.P11'(B, C, E, F, [], [], H, I, G, D, J).
'Syntax.Resolve1.P9'('MetaDefs.XAtom.F2'(A,B), 'MetaDefs.XAtom.F2'(A,C), D, E, F, G, H, I, J) :-
        'Syntax.UnifyArgs.P11'(B, C, E, F, [], [], H, I, G, D, J).
'~Syntax.Resolve1.P9'('MetaDefs.XAtom.F2'(A,B), 'MetaDefs.XAtom.F2'(A,C), D, E, F, G, H, I, J) :-
        '~Syntax.UnifyArgs.P11'(B, C, E, F, [], [], H, I, G, D, J).
'Syntax.Resolve1.P9'('MetaDefs.PAtom.F1'(A), 'MetaDefs.PAtom.F1'(A), B, C, D, E, F, F, G) :-
        'Syntax.ApplySubstToFormula.P9'(B, C, D, [], [], [], _, E, G).
'~Syntax.Resolve1.P9'('MetaDefs.PAtom.F1'(A), 'MetaDefs.PAtom.F1'(A), B, C, D, E, F, F, G) :-
        '~Syntax.ApplySubstToFormula.P9'(B, C, D, [], [], [], _, E, G).
'Syntax.Resolve1.P9'('MetaDefs.XPAtom.F1'(A), 'MetaDefs.XPAtom.F1'(A), B, C, D, E, F, F, G) :-
        'Syntax.ApplySubstToFormula.P9'(B, C, D, [], [], [], _, E, G).
'~Syntax.Resolve1.P9'('MetaDefs.XPAtom.F1'(A), 'MetaDefs.XPAtom.F1'(A), B, C, D, E, F, F, G) :-
        '~Syntax.ApplySubstToFormula.P9'(B, C, D, [], [], [], _, E, G).
'Syntax.ResolveAll1.P10'([], _, A, A, B, B, _, _, [], []).
'~Syntax.ResolveAll1.P10'([], _, A, A, B, B, _, _, [], []).
'Syntax.ResolveAll1.P10'(['MetaDefs.<-''.F2'(B,C)|A], D, E, F, G, H, I, J, K, L) :-
        'Integers':maximum(M, N, F),
        'Syntax.StandardiseCommits.P5'(C, G, O, I, P),
        user:goedel_freeze(ground([E,D,C,B,P,J]), if(('Syntax':'~Syntax.Resolve1.P9'(D,B,C,E,Q,P,J,R,S),true),(L=[S|T],K=[R|U],M=Q),(L=T,K=U,M=E))),
        'Syntax.ResolveAll1.P10'(A, D, E, N, O, H, P, J, U, T).
'~Syntax.ResolveAll1.P10'(['MetaDefs.<-''.F2'(B,C)|A], D, E, F, G, H, I, J, K, L) :-
        'Integers':maximum(M, N, F),
        '~Syntax.StandardiseCommits.P5'(C, G, O, I, P),
        user:goedel_freeze(ground([E,D,C,B,P,J]), if(('Syntax':'~Syntax.Resolve1.P9'(D,B,C,E,Q,P,J,R,S),true),(L=[S|T],K=[R|U],M=Q),(L=T,K=U,M=E))),
        '~Syntax.ResolveAll1.P10'(A, D, E, N, O, H, P, J, U, T).
'Syntax.RestrictSubstToTerm.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.RestrictSubstToTerm.P3.0'(A,B,C)).
'~Syntax.RestrictSubstToTerm.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.RestrictSubstToTerm.P3.0'(A,B,C)).
'Syntax.RestrictSubstToTerm.P3.0'(A, B, C) :-
        'Syntax.TermVariables.P2'(A, D),
        'Syntax.FindTermBindings.P3'(D, B, E),
        'Syntax.EmptyTermSubst.P1'(F),
        'Syntax.AppendTermBindings.P3'(E, F, C).
'~Syntax.RestrictSubstToTerm.P3.0'(A, B, C) :-
        '~Syntax.TermVariables.P2'(A, D),
        '~Syntax.FindTermBindings.P3'(D, B, E),
        '~Syntax.EmptyTermSubst.P1'(F),
        '~Syntax.AppendTermBindings.P3'(E, F, C).
'Syntax.RestrictSubstToType.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.RestrictSubstToType.P3.0'(A,B,C)).
'~Syntax.RestrictSubstToType.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.RestrictSubstToType.P3.0'(A,B,C)).
'Syntax.RestrictSubstToType.P3.0'(A, B, C) :-
        'Syntax.TypeParameters.P2'(A, D),
        'Syntax.FindTypeBindings.P3'(D, B, E),
        'Syntax.EmptyTypeSubst.P1'(F),
        'Syntax.AppendTypeBindings.P3'(E, F, C).
'~Syntax.RestrictSubstToType.P3.0'(A, B, C) :-
        '~Syntax.TypeParameters.P2'(A, D),
        '~Syntax.FindTypeBindings.P3'(D, B, E),
        '~Syntax.EmptyTypeSubst.P1'(F),
        '~Syntax.AppendTypeBindings.P3'(E, F, C).
'Syntax.TypeParameters.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.TypeParameters.P2.0'(A,B)).
'~Syntax.TypeParameters.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.TypeParameters.P2.0'(A,B)).
'Syntax.TypeParameters.P2.0'(A, B) :-
        'Syntax.ParamsInType.P3'(A, [], B).
'~Syntax.TypeParameters.P2.0'(A, B) :-
        '~Syntax.ParamsInType.P3'(A, [], B).
'Syntax.TypeMaxParIndex.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.TypeMaxParIndex.P2.0'(A,B)).
'~Syntax.TypeMaxParIndex.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.TypeMaxParIndex.P2.0'(A,B)).
'Syntax.TypeMaxParIndex.P2.0'(A, B) :-
        'Integers':negative(1, C),
        'Syntax.MaxParIndex.P3'(A, C, B).
'~Syntax.TypeMaxParIndex.P2.0'(A, B) :-
        'Integers':negative(1, C),
        '~Syntax.MaxParIndex.P3'(A, C, B).
'Syntax.StandardiseFormula.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.StandardiseFormula.P4.0'(A,B,C,D)).
'~Syntax.StandardiseFormula.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.StandardiseFormula.P4.0'(A,B,C,D)).
'Syntax.StandardiseFormula.P4.0'(A, B, C, D) :-
        'Syntax.RenameAllFormula3.P6'(A, B, C, [], _, D).
'~Syntax.StandardiseFormula.P4.0'(A, B, C, D) :-
        '~Syntax.RenameAllFormula3.P6'(A, B, C, [], _, D).
'Syntax.StandardiseApartQuants.P6'([], [], A, A, B, B).
'~Syntax.StandardiseApartQuants.P6'([], [], A, A, B, B).
'Syntax.StandardiseApartQuants.P6'([A|B], ['MetaDefs.Var.F1'(D)|C], E, F, D, G) :-
        'Integers':plus(D, 1, H),
        'Syntax.StandardiseApartQuants.P6'(B, C, ['MetaDefs.!.F2'(A,'MetaDefs.Var.F1'(D))|E], F, H, G).
'~Syntax.StandardiseApartQuants.P6'([A|B], ['MetaDefs.Var.F1'(D)|C], E, F, D, G) :-
        'Integers':plus(D, 1, H),
        '~Syntax.StandardiseApartQuants.P6'(B, C, ['MetaDefs.!.F2'(A,'MetaDefs.Var.F1'(D))|E], F, H, G).
'Syntax.Some.P3'(A, B, 'MetaDefs.Some.F2'(A,B)).
'~Syntax.Some.P3'(A, B, 'MetaDefs.Some.F2'(A,B)).
'Syntax.StandardiseCommits.P5'('MetaDefs.&''.F2'(A,B), C, D, E, F) :-
        'Syntax.StandardiseCommits.P5'(A, C, G, E, H),
        'Syntax.StandardiseCommits.P5'(B, G, D, H, F).
'~Syntax.StandardiseCommits.P5'('MetaDefs.&''.F2'(A,B), C, D, E, F) :-
        '~Syntax.StandardiseCommits.P5'(A, C, G, E, H),
        '~Syntax.StandardiseCommits.P5'(B, G, D, H, F).
'Syntax.StandardiseCommits.P5'('MetaDefs.Commit.F2'(A,B), C, D, E, F) :-
        'Syntax.NewCommit.P5'(A, C, G, E, H),
        'Syntax.StandardiseCommits.P5'(B, G, D, H, F).
'~Syntax.StandardiseCommits.P5'('MetaDefs.Commit.F2'(A,B), C, D, E, F) :-
        '~Syntax.NewCommit.P5'(A, C, G, E, H),
        '~Syntax.StandardiseCommits.P5'(B, G, D, H, F).
'Syntax.StandardiseCommits.P5'('MetaDefs.Empty.C0', A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.Empty.C0', A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.\\/''.F2'(_,_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.\\/''.F2'(_,_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.~''.F1'(_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.~''.F1'(_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.->''.F2'(_,_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.->''.F2'(_,_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.<-''.F2'(_,_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.<-''.F2'(_,_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.<->''.F2'(_,_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.<->''.F2'(_,_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.Some.F2'(_,_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.Some.F2'(_,_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.All.F2'(_,_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.All.F2'(_,_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.IT.F2'(_,_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.IT.F2'(_,_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.ITE.F3'(_,_,_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.ITE.F3'(_,_,_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.IST.F3'(_,_,_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.IST.F3'(_,_,_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.ISTE.F4'(_,_,_,_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.ISTE.F4'(_,_,_,_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.Atom.F2'(_,_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.Atom.F2'(_,_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.PAtom.F1'(_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.PAtom.F1'(_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.XAtom.F2'(_,_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.XAtom.F2'(_,_), A, A, B, B).
'Syntax.StandardiseCommits.P5'('MetaDefs.XPAtom.F1'(_), A, A, B, B).
'~Syntax.StandardiseCommits.P5'('MetaDefs.XPAtom.F1'(_), A, A, B, B).
'Syntax.TermMaxVarIndex.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.TermMaxVarIndex.P2.0'(A,B)).
'~Syntax.TermMaxVarIndex.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.TermMaxVarIndex.P2.0'(A,B)).
'Syntax.TermMaxVarIndex.P2.0'(A, B) :-
        'SharedSyntax':'SharedSyntax.STermMaxVarIndex.P2'(A, B).
'~Syntax.TermMaxVarIndex.P2.0'(A, B) :-
        'SharedSyntax':'~SharedSyntax.STermMaxVarIndex.P2'(A, B).
'Syntax.Statement.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.Statement.P1.0'(A)).
'~Syntax.Statement.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.Statement.P1.0'(A)).
'Syntax.Statement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.PAtom.F1'(_),_)).
'~Syntax.Statement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.PAtom.F1'(_),_)).
'Syntax.Statement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.Atom.F2'(_,_),_)).
'~Syntax.Statement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.Atom.F2'(_,_),_)).
'Syntax.Statement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.XPAtom.F1'(_),_)).
'~Syntax.Statement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.XPAtom.F1'(_),_)).
'Syntax.Statement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.XAtom.F2'(_,_),_)).
'~Syntax.Statement.P1.0'('MetaDefs.<-''.F2'('MetaDefs.XAtom.F2'(_,_),_)).
'Syntax.TermVariables.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.TermVariables.P2.0'(A,B)).
'~Syntax.TermVariables.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.TermVariables.P2.0'(A,B)).
'Syntax.TermVariables.P2.0'(A, B) :-
        'SharedSyntax':'SharedSyntax.STermFreeVars.P2'(A, B).
'~Syntax.TermVariables.P2.0'(A, B) :-
        'SharedSyntax':'~SharedSyntax.STermFreeVars.P2'(A, B).
'Syntax.TypeNotOccur1.P2'([], _).
'~Syntax.TypeNotOccur1.P2'([], _).
'Syntax.TypeNotOccur1.P2'([A|B], C) :-
        'Syntax.TypeNotOccur.P2'(A, C),
        'Syntax.TypeNotOccur1.P2'(B, C).
'~Syntax.TypeNotOccur1.P2'([A|B], C) :-
        '~Syntax.TypeNotOccur.P2'(A, C),
        '~Syntax.TypeNotOccur1.P2'(B, C).
'Syntax.TypeNotOccur.P2'('MetaDefs.Par.F1'(A), B) :-
        user:not_equal([], [B,A], B, 'MetaDefs.Par.F1'(A)).
'~Syntax.TypeNotOccur.P2'('MetaDefs.Par.F1'(A), B) :-
        user:not_equal([], [B,A], B, 'MetaDefs.Par.F1'(A)).
'Syntax.TypeNotOccur.P2'('MetaDefs.Par.F2'(A,B), C) :-
        user:not_equal([], [C,A,B], C, 'MetaDefs.Par.F2'(A,B)).
'~Syntax.TypeNotOccur.P2'('MetaDefs.Par.F2'(A,B), C) :-
        user:not_equal([], [C,A,B], C, 'MetaDefs.Par.F2'(A,B)).
'Syntax.TypeNotOccur.P2'('MetaDefs.BType.F1'(_), _).
'~Syntax.TypeNotOccur.P2'('MetaDefs.BType.F1'(_), _).
'Syntax.TypeNotOccur.P2'('MetaDefs.Type.F2'(_,A), B) :-
        'Syntax.TypeNotOccur1.P2'(A, B).
'~Syntax.TypeNotOccur.P2'('MetaDefs.Type.F2'(_,A), B) :-
        '~Syntax.TypeNotOccur1.P2'(A, B).
'Syntax.TypeNotOccur.P2'('MetaDefs.XBType.F1'(_), _).
'~Syntax.TypeNotOccur.P2'('MetaDefs.XBType.F1'(_), _).
'Syntax.TypeNotOccur.P2'('MetaDefs.XType.F2'(_,A), B) :-
        'Syntax.TypeNotOccur1.P2'(A, B).
'~Syntax.TypeNotOccur.P2'('MetaDefs.XType.F2'(_,A), B) :-
        '~Syntax.TypeNotOccur1.P2'(A, B).
'Syntax.TypeOccurCheck.P3'('Substs.Read.C0', A, B) :-
        'Syntax.TypeNotOccur1.P2'(B, A).
'~Syntax.TypeOccurCheck.P3'('Substs.Read.C0', A, B) :-
        '~Syntax.TypeNotOccur1.P2'(B, A).
'Syntax.TypeOccurCheck.P3'('Substs.Write.C0', _, _).
'~Syntax.TypeOccurCheck.P3'('Substs.Write.C0', _, _).
'Syntax.UnifyTypes.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Syntax':'Syntax.UnifyTypes.P4.0'(A,B,C,D)).
'~Syntax.UnifyTypes.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Syntax':'~Syntax.UnifyTypes.P4.0'(A,B,C,D)).
'Syntax.UnifyTypes.P4.0'(A, B, C, D) :-
        'Syntax.ApplySubstToType.P3'(A, C, E),
        'Syntax.UnifyTypes0.P4'(E, B, C, D).
'~Syntax.UnifyTypes.P4.0'(A, B, C, D) :-
        '~Syntax.ApplySubstToType.P3'(A, C, E),
        '~Syntax.UnifyTypes0.P4'(E, B, C, D).
'Syntax.UnifyTerms.P13'('MetaDefs.Var.F2'(A,B), C, D, E, F, G, H, I, J, K, L, M, N) :-
        user:goedel_freeze(ground([B,F,A]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),O),F),true),('Syntax':'Syntax.UnifyTerms.P4'(O,C,H,P),'Syntax':'Syntax.UnifyArgs.P11'(J,K,D,E,F,G,P,I,L,M,N)),user:goedel_freeze(ground([B,G,A]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),Q),G),true),('Syntax':'Syntax.UnifyTerms.P4'(Q,C,H,P),'Syntax':'Syntax.UnifyArgs.P11'(J,K,D,E,F,G,P,I,L,M,N)),'Syntax':'Syntax.UnifyArgs.P11'(J,K,D,E,['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),C)|F],G,H,I,L,M,N))))).
'~Syntax.UnifyTerms.P13'('MetaDefs.Var.F2'(A,B), C, D, E, F, G, H, I, J, K, L, M, N) :-
        user:goedel_freeze(ground([B,F,A]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),O),F),true),('Syntax':'~Syntax.UnifyTerms.P4'(O,C,H,P),'Syntax':'~Syntax.UnifyArgs.P11'(J,K,D,E,F,G,P,I,L,M,N)),user:goedel_freeze(ground([B,G,A]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),Q),G),true),('Syntax':'~Syntax.UnifyTerms.P4'(Q,C,H,P),'Syntax':'~Syntax.UnifyArgs.P11'(J,K,D,E,F,G,P,I,L,M,N)),'Syntax':'~Syntax.UnifyArgs.P11'(J,K,D,E,['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),C)|F],G,H,I,L,M,N))))).
'Syntax.UnifyTerms.P13'('MetaDefs.Var.F1'(A), B, C, D, E, F, G, H, I, J, K, L, M) :-
        user:goedel_freeze(ground([A,E]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),N),E),true),('Syntax':'Syntax.UnifyTerms.P4'(N,B,G,O),'Syntax':'Syntax.UnifyArgs.P11'(I,J,C,D,E,F,O,H,K,L,M)),user:goedel_freeze(ground([A,F]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),P),F),true),('Syntax':'Syntax.UnifyTerms.P4'(P,B,G,O),'Syntax':'Syntax.UnifyArgs.P11'(I,J,C,D,E,F,O,H,K,L,M)),'Syntax':'Syntax.UnifyArgs.P11'(I,J,C,D,['MetaDefs.!.F2'('MetaDefs.Var.F1'(A),B)|E],F,G,H,K,L,M))))).
'~Syntax.UnifyTerms.P13'('MetaDefs.Var.F1'(A), B, C, D, E, F, G, H, I, J, K, L, M) :-
        user:goedel_freeze(ground([A,E]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),N),E),true),('Syntax':'~Syntax.UnifyTerms.P4'(N,B,G,O),'Syntax':'~Syntax.UnifyArgs.P11'(I,J,C,D,E,F,O,H,K,L,M)),user:goedel_freeze(ground([A,F]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F1'(A),P),F),true),('Syntax':'~Syntax.UnifyTerms.P4'(P,B,G,O),'Syntax':'~Syntax.UnifyArgs.P11'(I,J,C,D,E,F,O,H,K,L,M)),'Syntax':'~Syntax.UnifyArgs.P11'(I,J,C,D,['MetaDefs.!.F2'('MetaDefs.Var.F1'(A),B)|E],F,G,H,K,L,M))))).
'Syntax.UnifyTerms.P13'('MetaDefs.Term.F2'(A,B), C, D, E, F, G, H, I, J, K, L, M, N) :-
        'Substs':'Substs.GetFunction.P5'(C, 'MetaDefs.Term.F2'(A,O), P, H, Q),
        'Syntax.CheckFunctionTerm.P10'(B, O, P, D, R, F, G, S, Q, T),
        'Syntax.UnifyArgs.P11'(J, K, R, E, F, S, T, I, L, M, N).
'~Syntax.UnifyTerms.P13'('MetaDefs.Term.F2'(A,B), C, D, E, F, G, H, I, J, K, L, M, N) :-
        'Substs':'~Substs.GetFunction.P5'(C, 'MetaDefs.Term.F2'(A,O), P, H, Q),
        '~Syntax.CheckFunctionTerm.P10'(B, O, P, D, R, F, G, S, Q, T),
        '~Syntax.UnifyArgs.P11'(J, K, R, E, F, S, T, I, L, M, N).
'Syntax.UnifyTerms.P13'('MetaDefs.XTerm.F3'(A,B,C), D, E, F, G, H, I, J, K, L, M, N, O) :-
        'Substs':'Substs.GetFunction.P5'(D, 'MetaDefs.XTerm.F3'(A,P,C), Q, I, R),
        'Syntax.CheckFunctionTerm.P10'(B, P, Q, E, S, G, H, T, R, U),
        'Syntax.UnifyArgs.P11'(K, L, S, F, G, T, U, J, M, N, O).
'~Syntax.UnifyTerms.P13'('MetaDefs.XTerm.F3'(A,B,C), D, E, F, G, H, I, J, K, L, M, N, O) :-
        'Substs':'~Substs.GetFunction.P5'(D, 'MetaDefs.XTerm.F3'(A,P,C), Q, I, R),
        '~Syntax.CheckFunctionTerm.P10'(B, P, Q, E, S, G, H, T, R, U),
        '~Syntax.UnifyArgs.P11'(K, L, S, F, G, T, U, J, M, N, O).
'Syntax.UnifyTerms.P13'('MetaDefs.CTerm.F1'(A), B, C, D, E, F, G, H, I, J, K, L, M) :-
        'Substs':'Substs.GetConstant.P4'(B, 'MetaDefs.CTerm.F1'(A), G, N),
        'Syntax.UnifyArgs.P11'(I, J, C, D, E, F, N, H, K, L, M).
'~Syntax.UnifyTerms.P13'('MetaDefs.CTerm.F1'(A), B, C, D, E, F, G, H, I, J, K, L, M) :-
        'Substs':'~Substs.GetConstant.P4'(B, 'MetaDefs.CTerm.F1'(A), G, N),
        '~Syntax.UnifyArgs.P11'(I, J, C, D, E, F, N, H, K, L, M).
'Syntax.UnifyTerms.P13'('MetaDefs.XCTerm.F2'(A,B), C, D, E, F, G, H, I, J, K, L, M, N) :-
        'Substs':'Substs.GetConstant.P4'(C, 'MetaDefs.XCTerm.F2'(A,B), H, O),
        'Syntax.UnifyArgs.P11'(J, K, D, E, F, G, O, I, L, M, N).
'~Syntax.UnifyTerms.P13'('MetaDefs.XCTerm.F2'(A,B), C, D, E, F, G, H, I, J, K, L, M, N) :-
        'Substs':'~Substs.GetConstant.P4'(C, 'MetaDefs.XCTerm.F2'(A,B), H, O),
        '~Syntax.UnifyArgs.P11'(J, K, D, E, F, G, O, I, L, M, N).
'Syntax.UnifyTerms.P13'('MetaDefs.Int.F1'(A), B, C, D, E, F, G, H, I, J, K, L, M) :-
        'Substs':'Substs.GetConstant.P4'(B, 'MetaDefs.Int.F1'(A), G, N),
        'Syntax.UnifyArgs.P11'(I, J, C, D, E, F, N, H, K, L, M).
'~Syntax.UnifyTerms.P13'('MetaDefs.Int.F1'(A), B, C, D, E, F, G, H, I, J, K, L, M) :-
        'Substs':'~Substs.GetConstant.P4'(B, 'MetaDefs.Int.F1'(A), G, N),
        '~Syntax.UnifyArgs.P11'(I, J, C, D, E, F, N, H, K, L, M).
'Syntax.UnifyTerms.P13'('MetaDefs.Str.F1'(A), B, C, D, E, F, G, H, I, J, K, L, M) :-
        'Substs':'Substs.GetConstant.P4'(B, 'MetaDefs.Str.F1'(A), G, N),
        'Syntax.UnifyArgs.P11'(I, J, C, D, E, F, N, H, K, L, M).
'~Syntax.UnifyTerms.P13'('MetaDefs.Str.F1'(A), B, C, D, E, F, G, H, I, J, K, L, M) :-
        'Substs':'~Substs.GetConstant.P4'(B, 'MetaDefs.Str.F1'(A), G, N),
        '~Syntax.UnifyArgs.P11'(I, J, C, D, E, F, N, H, K, L, M).
'Syntax.UnifyTerms.P13'('MetaDefs.Prm.F1'(A), B, C, D, E, F, G, H, I, J, K, L, M) :-
        'Substs':'Substs.GetConstant.P4'(B, 'MetaDefs.Prm.F1'(A), G, N),
        'Syntax.UnifyArgs.P11'(I, J, C, D, E, F, N, H, K, L, M).
'~Syntax.UnifyTerms.P13'('MetaDefs.Prm.F1'(A), B, C, D, E, F, G, H, I, J, K, L, M) :-
        'Substs':'~Substs.GetConstant.P4'(B, 'MetaDefs.Prm.F1'(A), G, N),
        '~Syntax.UnifyArgs.P11'(I, J, C, D, E, F, N, H, K, L, M).
'Syntax.UnifyTerms.P13'('MetaDefs.SuchThat.F2'(A,B), C, D, E, F, G, H, I, J, K, L, M, N) :-
        'Substs':'Substs.GetConstant.P4'(C, 'MetaDefs.SuchThat.F2'(A,B), H, O),
        'Syntax.UnifyArgs.P11'(J, K, D, E, F, G, O, I, L, M, N).
'~Syntax.UnifyTerms.P13'('MetaDefs.SuchThat.F2'(A,B), C, D, E, F, G, H, I, J, K, L, M, N) :-
        'Substs':'~Substs.GetConstant.P4'(C, 'MetaDefs.SuchThat.F2'(A,B), H, O),
        '~Syntax.UnifyArgs.P11'(J, K, D, E, F, G, O, I, L, M, N).
'Syntax.UnifyTerms.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Syntax':'Syntax.UnifyTerms.P4.0'(A,B,C,D)).
'~Syntax.UnifyTerms.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Syntax':'~Syntax.UnifyTerms.P4.0'(A,B,C,D)).
'Syntax.UnifyTerms.P4.0'(A, B, C, D) :-
        'SharedSyntax':'SharedSyntax.SUnifyTerms.P4'(A, B, C, D).
'~Syntax.UnifyTerms.P4.0'(A, B, C, D) :-
        'SharedSyntax':'~SharedSyntax.SUnifyTerms.P4'(A, B, C, D).
'Syntax.UnifyAtoms.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Syntax':'Syntax.UnifyAtoms.P4.0'(A,B,C,D)).
'~Syntax.UnifyAtoms.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Syntax':'~Syntax.UnifyAtoms.P4.0'(A,B,C,D)).
'Syntax.UnifyAtoms.P4.0'(A, B, C, D) :-
        'SharedSyntax':'SharedSyntax.SUnifyAtoms.P4'(A, B, C, D).
'~Syntax.UnifyAtoms.P4.0'(A, B, C, D) :-
        'SharedSyntax':'~SharedSyntax.SUnifyAtoms.P4'(A, B, C, D).
'Syntax.UnifyArgs.P11'([], [], A, B, C, D, E, E, F, G, H) :-
        'Syntax.ApplySubstToFormula.P9'(G, A, B, [], D, C, _, F, H).
'~Syntax.UnifyArgs.P11'([], [], A, B, C, D, E, E, F, G, H) :-
        '~Syntax.ApplySubstToFormula.P9'(G, A, B, [], D, C, _, F, H).
'Syntax.UnifyArgs.P11'([A|B], [C|D], E, F, G, H, I, J, K, L, M) :-
        'Syntax.UnifyTerms.P13'(C, A, E, F, G, H, I, J, B, D, K, L, M).
'~Syntax.UnifyArgs.P11'([A|B], [C|D], E, F, G, H, I, J, K, L, M) :-
        '~Syntax.UnifyTerms.P13'(C, A, E, F, G, H, I, J, B, D, K, L, M).
'Syntax.UnifyTypeArgs.P5'([], [], _, A, A).
'~Syntax.UnifyTypeArgs.P5'([], [], _, A, A).
'Syntax.UnifyTypeArgs.P5'([A|B], [C|D], E, F, G) :-
        'Syntax.UnifyTypes1.P5'(A, C, E, F, H),
        'Syntax.UnifyTypeArgs.P5'(B, D, E, H, G).
'~Syntax.UnifyTypeArgs.P5'([A|B], [C|D], E, F, G) :-
        '~Syntax.UnifyTypes1.P5'(A, C, E, F, H),
        '~Syntax.UnifyTypeArgs.P5'(B, D, E, H, G).
'Syntax.VariableName.P3'(A, B, C) :-
        'SharedSyntax':'SharedSyntax.SVariableName.P3'(A, B, C).
'~Syntax.VariableName.P3'(A, B, C) :-
        'SharedSyntax':'~SharedSyntax.SVariableName.P3'(A, B, C).
'Syntax.UnifyTypes1.P5'('MetaDefs.Par.F2'(A,B), C, D, E, F) :-
        'Substs':'Substs.UnifyParameter.P5'(D, C, 'MetaDefs.Par.F2'(A,B), E, F).
'~Syntax.UnifyTypes1.P5'('MetaDefs.Par.F2'(A,B), C, D, E, F) :-
        'Substs':'~Substs.UnifyParameter.P5'(D, C, 'MetaDefs.Par.F2'(A,B), E, F).
'Syntax.UnifyTypes1.P5'('MetaDefs.Par.F1'(A), B, C, D, E) :-
        'Substs':'Substs.UnifyParameter.P5'(C, B, 'MetaDefs.Par.F1'(A), D, E).
'~Syntax.UnifyTypes1.P5'('MetaDefs.Par.F1'(A), B, C, D, E) :-
        'Substs':'~Substs.UnifyParameter.P5'(C, B, 'MetaDefs.Par.F1'(A), D, E).
'Syntax.UnifyTypes1.P5'('MetaDefs.BType.F1'(A), B, C, D, E) :-
        'Substs':'Substs.UnifyBase.P5'(C, B, 'MetaDefs.BType.F1'(A), D, E).
'~Syntax.UnifyTypes1.P5'('MetaDefs.BType.F1'(A), B, C, D, E) :-
        'Substs':'~Substs.UnifyBase.P5'(C, B, 'MetaDefs.BType.F1'(A), D, E).
'Syntax.UnifyTypes1.P5'('MetaDefs.Type.F2'(A,B), C, D, E, F) :-
        'Substs':'Substs.UnifyType.P6'(D, C, 'MetaDefs.Type.F2'(A,G), H, E, I),
        'Syntax.TypeOccurCheck.P3'(H, C, B),
        'Syntax.UnifyTypeArgs.P5'(B, G, H, I, F).
'~Syntax.UnifyTypes1.P5'('MetaDefs.Type.F2'(A,B), C, D, E, F) :-
        'Substs':'~Substs.UnifyType.P6'(D, C, 'MetaDefs.Type.F2'(A,G), H, E, I),
        '~Syntax.TypeOccurCheck.P3'(H, C, B),
        '~Syntax.UnifyTypeArgs.P5'(B, G, H, I, F).
'Syntax.UnifyTypes1.P5'('MetaDefs.XType.F2'(A,B), C, D, E, F) :-
        'Substs':'Substs.UnifyType.P6'(D, C, 'MetaDefs.XType.F2'(A,G), H, E, I),
        'Syntax.TypeOccurCheck.P3'(H, C, B),
        'Syntax.UnifyTypeArgs.P5'(B, G, H, I, F).
'~Syntax.UnifyTypes1.P5'('MetaDefs.XType.F2'(A,B), C, D, E, F) :-
        'Substs':'~Substs.UnifyType.P6'(D, C, 'MetaDefs.XType.F2'(A,G), H, E, I),
        '~Syntax.TypeOccurCheck.P3'(H, C, B),
        '~Syntax.UnifyTypeArgs.P5'(B, G, H, I, F).
'Syntax.UnifyTypes1.P5'('MetaDefs.XBType.F1'(A), B, C, D, E) :-
        'Substs':'Substs.UnifyBase.P5'(C, B, 'MetaDefs.XBType.F1'(A), D, E).
'~Syntax.UnifyTypes1.P5'('MetaDefs.XBType.F1'(A), B, C, D, E) :-
        'Substs':'~Substs.UnifyBase.P5'(C, B, 'MetaDefs.XBType.F1'(A), D, E).
'Syntax.UnifyTypes0.P4'('MetaDefs.Par.F2'(A,B), C, D, E) :-
        'Syntax.GetParameter.P4'(C, 'MetaDefs.Par.F2'(A,B), D, E).
'~Syntax.UnifyTypes0.P4'('MetaDefs.Par.F2'(A,B), C, D, E) :-
        '~Syntax.GetParameter.P4'(C, 'MetaDefs.Par.F2'(A,B), D, E).
'Syntax.UnifyTypes0.P4'('MetaDefs.Par.F1'(A), B, C, D) :-
        'Syntax.GetParameter.P4'(B, 'MetaDefs.Par.F1'(A), C, D).
'~Syntax.UnifyTypes0.P4'('MetaDefs.Par.F1'(A), B, C, D) :-
        '~Syntax.GetParameter.P4'(B, 'MetaDefs.Par.F1'(A), C, D).
'Syntax.UnifyTypes0.P4'('MetaDefs.BType.F1'(A), B, C, D) :-
        'Substs':'Substs.GetBase.P4'(B, 'MetaDefs.BType.F1'(A), C, D).
'~Syntax.UnifyTypes0.P4'('MetaDefs.BType.F1'(A), B, C, D) :-
        'Substs':'~Substs.GetBase.P4'(B, 'MetaDefs.BType.F1'(A), C, D).
'Syntax.UnifyTypes0.P4'('MetaDefs.Type.F2'(A,B), C, D, E) :-
        'Substs':'Substs.GetType.P5'(C, 'MetaDefs.Type.F2'(A,F), G, D, H),
        'Syntax.TypeOccurCheck.P3'(G, C, B),
        'Syntax.UnifyTypeArgs.P5'(B, F, G, H, E).
'~Syntax.UnifyTypes0.P4'('MetaDefs.Type.F2'(A,B), C, D, E) :-
        'Substs':'~Substs.GetType.P5'(C, 'MetaDefs.Type.F2'(A,F), G, D, H),
        '~Syntax.TypeOccurCheck.P3'(G, C, B),
        '~Syntax.UnifyTypeArgs.P5'(B, F, G, H, E).
'Syntax.UnifyTypes0.P4'('MetaDefs.XType.F2'(A,B), C, D, E) :-
        'Substs':'Substs.GetType.P5'(C, 'MetaDefs.XType.F2'(A,F), G, D, H),
        'Syntax.TypeOccurCheck.P3'(G, C, B),
        'Syntax.UnifyTypeArgs.P5'(B, F, G, H, E).
'~Syntax.UnifyTypes0.P4'('MetaDefs.XType.F2'(A,B), C, D, E) :-
        'Substs':'~Substs.GetType.P5'(C, 'MetaDefs.XType.F2'(A,F), G, D, H),
        '~Syntax.TypeOccurCheck.P3'(G, C, B),
        '~Syntax.UnifyTypeArgs.P5'(B, F, G, H, E).
'Syntax.UnifyTypes0.P4'('MetaDefs.XBType.F1'(A), B, C, D) :-
        'Substs':'Substs.GetBase.P4'(B, 'MetaDefs.XBType.F1'(A), C, D).
'~Syntax.UnifyTypes0.P4'('MetaDefs.XBType.F1'(A), B, C, D) :-
        'Substs':'~Substs.GetBase.P4'(B, 'MetaDefs.XBType.F1'(A), C, D).
'Syntax.Variable.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'Syntax.Variable.P1.0'(A)).
'~Syntax.Variable.P1'(A) :-
        user:goedel_freeze(ground([A]), 'Syntax':'~Syntax.Variable.P1.0'(A)).
'Syntax.Variable.P1.0'('MetaDefs.Var.F2'(_,_)).
'~Syntax.Variable.P1.0'('MetaDefs.Var.F2'(_,_)).
'Syntax.Variable.P1.0'('MetaDefs.Var.F1'(_)).
'~Syntax.Variable.P1.0'('MetaDefs.Var.F1'(_)).
'Syntax.UnifyValue.P5'('Substs.Write.C0', A, A, B, B).
'~Syntax.UnifyValue.P5'('Substs.Write.C0', A, A, B, B).
'Syntax.UnifyValue.P5'('Substs.Read.C0', A, B, C, D) :-
        'Syntax.UnifyTerms.P4'(A, B, C, D).
'~Syntax.UnifyValue.P5'('Substs.Read.C0', A, B, C, D) :-
        '~Syntax.UnifyTerms.P4'(A, B, C, D).
'Syntax.VariantTerms.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.VariantTerms.P2.0'(A,B)).
'~Syntax.VariantTerms.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.VariantTerms.P2.0'(A,B)).
'Syntax.VariantTerms.P2.0'(A, B) :-
        'Syntax.CheckVariantTerms.P4'(A, [], _, B).
'~Syntax.VariantTerms.P2.0'(A, B) :-
        '~Syntax.CheckVariantTerms.P4'(A, [], _, B).
'Syntax.VariantFormulas.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.VariantFormulas.P2.0'(A,B)).
'~Syntax.VariantFormulas.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.VariantFormulas.P2.0'(A,B)).
'Syntax.VariantFormulas.P2.0'(A, B) :-
        'Syntax.CheckVariantFormulas.P4'(A, [], _, B).
'~Syntax.VariantFormulas.P2.0'(A, B) :-
        '~Syntax.CheckVariantFormulas.P4'(A, [], _, B).
'Syntax.VariantTypes.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'Syntax.VariantTypes.P2.0'(A,B)).
'~Syntax.VariantTypes.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Syntax':'~Syntax.VariantTypes.P2.0'(A,B)).
'Syntax.VariantTypes.P2.0'(A, B) :-
        'SharedSyntax':'SharedSyntax.SCheckVariantTypes.P4'(A, [], _, B).
'~Syntax.VariantTypes.P2.0'(A, B) :-
        'SharedSyntax':'~SharedSyntax.SCheckVariantTypes.P4'(A, [], _, B).
