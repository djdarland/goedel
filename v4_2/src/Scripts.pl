:- module('Scripts', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

'Scripts.FormulaInScript.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Scripts':'Scripts.FormulaInScript.P4.0'(A,B,C,D)).
'~Scripts.FormulaInScript.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Scripts':'~Scripts.FormulaInScript.P4.0'(A,B,C,D)).
'Scripts.FormulaInScript.P4.0'('Scripts.Script.F5'(_,_,A,_,_), B, C, D) :-
        'SharedPrograms':'SharedPrograms.FormulaInLanguage.P4'(C, A, B, D).
'~Scripts.FormulaInScript.P4.0'('Scripts.Script.F5'(_,_,A,_,_), B, C, D) :-
        'SharedPrograms':'~SharedPrograms.FormulaInLanguage.P4'(C, A, B, D).
'Scripts.ControlInScript.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.ControlInScript.P4.0'(A,B,C,D)).
'~Scripts.ControlInScript.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.ControlInScript.P4.0'(A,B,C,D)).
'Scripts.ControlInScript.P4.0'('Scripts.Script.F5'(_,A,_,_,B), 'MetaDefs.Name.F4'(C,D,'MetaDefs.Predicate.C0',E), F, G) :-
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(B, C, 'ProgDefs.Code.F2'(_,H)),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(H, D, I),
        'SharedPrograms':'SharedPrograms.FindPredDef.P3'(E, I, 'ProgDefs.PredDef.F4'(E,_,J,K)),
        user:goedel_freeze(ground([C,A]), ('Scripts':'~Scripts.ClosedModule.P2'(C,A),true->L=J;'Lists':'Lists.Append.P3'(J,K,L))),
        'Scripts.ReformatDelays.P3'(L, F, G).
'~Scripts.ControlInScript.P4.0'('Scripts.Script.F5'(_,A,_,_,B), 'MetaDefs.Name.F4'(C,D,'MetaDefs.Predicate.C0',E), F, G) :-
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(B, C, 'ProgDefs.Code.F2'(_,H)),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(H, D, I),
        'SharedPrograms':'~SharedPrograms.FindPredDef.P3'(E, I, 'ProgDefs.PredDef.F4'(E,_,J,K)),
        user:goedel_freeze(ground([C,A]), ('Scripts':'~Scripts.ClosedModule.P2'(C,A),true->L=J;'Lists':'~Lists.Append.P3'(J,K,L))),
        '~Scripts.ReformatDelays.P3'(L, F, G).
'Scripts.ConstantInScript.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.ConstantInScript.P3.0'(A,B,C)).
'~Scripts.ConstantInScript.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.ConstantInScript.P3.0'(A,B,C)).
'Scripts.ConstantInScript.P3.0'('Scripts.Script.F5'(_,A,B,_,_), C, D) :-
        'Scripts.SymbolInScript.P4'(C, B, A, 'ProgDefs.ConstantDecl.F1'(D)).
'~Scripts.ConstantInScript.P3.0'('Scripts.Script.F5'(_,A,B,_,_), C, D) :-
        '~Scripts.SymbolInScript.P4'(C, B, A, 'ProgDefs.ConstantDecl.F1'(D)).
'Scripts.BaseInScript.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.BaseInScript.P2.0'(A,B)).
'~Scripts.BaseInScript.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.BaseInScript.P2.0'(A,B)).
'Scripts.BaseInScript.P2.0'('Scripts.Script.F5'(_,A,B,_,_), C) :-
        'Scripts.TypeSymbolInScriptLanguage.P4'(C, B, A, 'ProgDefs.BaseDecl.C0').
'~Scripts.BaseInScript.P2.0'('Scripts.Script.F5'(_,A,B,_,_), C) :-
        '~Scripts.TypeSymbolInScriptLanguage.P4'(C, B, A, 'ProgDefs.BaseDecl.C0').
'Scripts.ClosedModule.P2'(A, B) :-
        'AVLTrees':'AVLTrees.AVLSearch.P3'(B, A, 'ProgDefs.ModDef.F4'(C,_,_,_)),
        C='ProgDefs.ClosedKind.C0'.
'~Scripts.ClosedModule.P2'(A, B) :-
        'AVLTrees':'~AVLTrees.AVLSearch.P3'(B, A, 'ProgDefs.ModDef.F4'(C,_,_,_)),
        C='ProgDefs.ClosedKind.C0'.
'Scripts.ConstructorInScript.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.ConstructorInScript.P3.0'(A,B,C)).
'~Scripts.ConstructorInScript.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.ConstructorInScript.P3.0'(A,B,C)).
'Scripts.ConstructorInScript.P3.0'('Scripts.Script.F5'(_,A,B,_,_), C, D) :-
        'Scripts.TypeSymbolInScriptLanguage.P4'(C, B, A, 'ProgDefs.ConstructorDecl.F1'(D)).
'~Scripts.ConstructorInScript.P3.0'('Scripts.Script.F5'(_,A,B,_,_), C, D) :-
        '~Scripts.TypeSymbolInScriptLanguage.P4'(C, B, A, 'ProgDefs.ConstructorDecl.F1'(D)).
'Scripts.DeleteScriptPredicate.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.DeleteScriptPredicate.P5.0'(A,B,C,D,E)).
'~Scripts.DeleteScriptPredicate.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.DeleteScriptPredicate.P5.0'(A,B,C,D,E)).
'Scripts.DeleteScriptPredicate.P5.0'(A, B, C, D, E) :-
        'Scripts.DelDeclaration.P4'(E, B, 'ProgDefs.PredicateDecl.F3'(_,C,D), A).
'~Scripts.DeleteScriptPredicate.P5.0'(A, B, C, D, E) :-
        '~Scripts.DelDeclaration.P4'(E, B, 'ProgDefs.PredicateDecl.F3'(_,C,D), A).
'Scripts.DelayInScript.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.DelayInScript.P3.0'(A,B,C)).
'~Scripts.DelayInScript.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.DelayInScript.P3.0'(A,B,C)).
'Scripts.DelayInScript.P3.0'('Scripts.Script.F5'(_,A,_,_,B), C, D) :-
        C='MetaDefs.Atom.F2'('MetaDefs.Name.F4'(E,F,_,G),_),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(B, E, 'ProgDefs.Code.F2'(_,H)),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(H, F, I),
        'SharedPrograms':'SharedPrograms.FindPredDef.P3'(G, I, 'ProgDefs.PredDef.F4'(G,_,J,K)),
        'AVLTrees':'AVLTrees.AVLSearch.P3'(A, E, 'ProgDefs.ModDef.F4'(L,_,_,_)),
        'Scripts.DelayInKind.P4'(L, J, K, 'ProgDefs.Delay.F2'(C,D)).
'~Scripts.DelayInScript.P3.0'('Scripts.Script.F5'(_,A,_,_,B), C, D) :-
        C='MetaDefs.Atom.F2'('MetaDefs.Name.F4'(E,F,_,G),_),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(B, E, 'ProgDefs.Code.F2'(_,H)),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(H, F, I),
        'SharedPrograms':'~SharedPrograms.FindPredDef.P3'(G, I, 'ProgDefs.PredDef.F4'(G,_,J,K)),
        'AVLTrees':'~AVLTrees.AVLSearch.P3'(A, E, 'ProgDefs.ModDef.F4'(L,_,_,_)),
        '~Scripts.DelayInKind.P4'(L, J, K, 'ProgDefs.Delay.F2'(C,D)).
'Scripts.DelDeclaration.P4'('Scripts.Script.F5'(A,B,C,D,E), F, G, 'Scripts.Script.F5'(A,B,H,D,E)) :-
        'Scripts.DeleteSymbol.P5'(F, H, B, G, C),
        'Scripts.FreeSymbol.P2'(F, E).
'~Scripts.DelDeclaration.P4'('Scripts.Script.F5'(A,B,C,D,E), F, G, 'Scripts.Script.F5'(A,B,H,D,E)) :-
        '~Scripts.DeleteSymbol.P5'(F, H, B, G, C),
        '~Scripts.FreeSymbol.P2'(F, E).
'Scripts.DefinitionInScript.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.DefinitionInScript.P3.0'(A,B,C)).
'~Scripts.DefinitionInScript.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.DefinitionInScript.P3.0'(A,B,C)).
'Scripts.DefinitionInScript.P3.0'('Scripts.Script.F5'(_,A,_,_,B), 'MetaDefs.Name.F4'(C,D,E,F), G) :-
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(B, C, 'ProgDefs.Code.F2'(_,H)),
        user:goedel_freeze(ground([C,A]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(C,A))),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(H, D, I),
        'SharedPrograms':'SharedPrograms.FindPredDef.P3'(F, I, 'ProgDefs.PredDef.F4'(F,G,_,_)),
        user:goedel_freeze(ground([F]), (F=0,true->E='MetaDefs.Proposition.C0';E='MetaDefs.Predicate.C0')).
'~Scripts.DefinitionInScript.P3.0'('Scripts.Script.F5'(_,A,_,_,B), 'MetaDefs.Name.F4'(C,D,E,F), G) :-
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(B, C, 'ProgDefs.Code.F2'(_,H)),
        user:goedel_freeze(ground([C,A]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(C,A))),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(H, D, I),
        'SharedPrograms':'~SharedPrograms.FindPredDef.P3'(F, I, 'ProgDefs.PredDef.F4'(F,G,_,_)),
        user:goedel_freeze(ground([F]), (F=0,true->E='MetaDefs.Proposition.C0';E='MetaDefs.Predicate.C0')).
'Scripts.DelayInKind.P4'('ProgDefs.NormalKind.C0', A, B, C) :-
        (   'Lists':'Lists.Member.P2'(C, A)
        ;   'Lists':'Lists.Member.P2'(C, B)
        ).
'~Scripts.DelayInKind.P4'('ProgDefs.NormalKind.C0', A, B, C) :-
        (   'Lists':'~Lists.Member.P2'(C, A)
        ;   'Lists':'~Lists.Member.P2'(C, B)
        ).
'Scripts.DelayInKind.P4'('ProgDefs.ClosedKind.C0', A, _, B) :-
        'Lists':'Lists.Member.P2'(B, A).
'~Scripts.DelayInKind.P4'('ProgDefs.ClosedKind.C0', A, _, B) :-
        'Lists':'~Lists.Member.P2'(B, A).
'Scripts.DelayInKind.P4'('ProgDefs.ModuleKind.C0', _, A, B) :-
        'Lists':'Lists.Member.P2'(B, A).
'~Scripts.DelayInKind.P4'('ProgDefs.ModuleKind.C0', _, A, B) :-
        'Lists':'~Lists.Member.P2'(B, A).
'Scripts.DeleteDelay.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.DeleteDelay.P4.0'(A,B,C,D)).
'~Scripts.DeleteDelay.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.DeleteDelay.P4.0'(A,B,C,D)).
'Scripts.DeleteDelay.P4.0'('Scripts.Script.F5'(A,B,C,D,E), F, G, 'Scripts.Script.F5'(A,B,C,D,H)) :-
        'Syntax':'Syntax.PredicateAtom.P3'(F, 'MetaDefs.Name.F4'(I,J,_,K), _),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(E, I, 'ProgDefs.Code.F2'(L,M)),
        user:goedel_freeze(ground([I,B]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(I,B))),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(M, J, N),
        'SharedPrograms':'SharedPrograms.PickPredDef.P4'(K, N, 'ProgDefs.PredDef.F4'(K,O,P,Q), R),
        (   'SharedPrograms':'SharedPrograms.PickOne.P3'('ProgDefs.Delay.F2'(F,G), P, S),
            T=Q
        ;   'SharedPrograms':'SharedPrograms.PickOne.P3'('ProgDefs.Delay.F2'(F,G), Q, T),
            S=P
        ),
        user:goedel_freeze(ground([S,T,O]), ((O=[],S=[],T=[]),true->U=R;U=['ProgDefs.PredDef.F4'(K,O,S,T)|R])),
        user:goedel_freeze(ground([U]), (U=[],true->'AVLTrees':'AVLTrees.AVLDelete.P3'(M,J,V);'AVLTrees':'AVLTrees.AVLUpdate.P5'(M,J,U,V,_))),
        'AVLTrees':'AVLTrees.AVLUpdate.P5'(E, I, 'ProgDefs.Code.F2'(L,V), H, _).
'~Scripts.DeleteDelay.P4.0'('Scripts.Script.F5'(A,B,C,D,E), F, G, 'Scripts.Script.F5'(A,B,C,D,H)) :-
        'Syntax':'~Syntax.PredicateAtom.P3'(F, 'MetaDefs.Name.F4'(I,J,_,K), _),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(E, I, 'ProgDefs.Code.F2'(L,M)),
        user:goedel_freeze(ground([I,B]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(I,B))),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(M, J, N),
        'SharedPrograms':'~SharedPrograms.PickPredDef.P4'(K, N, 'ProgDefs.PredDef.F4'(K,O,P,Q), R),
        (   'SharedPrograms':'~SharedPrograms.PickOne.P3'('ProgDefs.Delay.F2'(F,G), P, S),
            T=Q
        ;   'SharedPrograms':'~SharedPrograms.PickOne.P3'('ProgDefs.Delay.F2'(F,G), Q, T),
            S=P
        ),
        user:goedel_freeze(ground([S,T,O]), ((O=[],S=[],T=[]),true->U=R;U=['ProgDefs.PredDef.F4'(K,O,S,T)|R])),
        user:goedel_freeze(ground([U]), (U=[],true->'AVLTrees':'~AVLTrees.AVLDelete.P3'(M,J,V);'AVLTrees':'~AVLTrees.AVLUpdate.P5'(M,J,U,V,_))),
        'AVLTrees':'~AVLTrees.AVLUpdate.P5'(E, I, 'ProgDefs.Code.F2'(L,V), H, _).
'Scripts.DeleteStatement.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.DeleteStatement.P3.0'(A,B,C)).
'~Scripts.DeleteStatement.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.DeleteStatement.P3.0'(A,B,C)).
'Scripts.DeleteStatement.P3.0'('Scripts.Script.F5'(A,B,C,D,E), F, 'Scripts.Script.F5'(A,B,C,D,G)) :-
        'Scripts.OpenStatementPredicate.P2'(F, 'MetaDefs.Name.F4'(H,I,_,J)),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(E, H, 'ProgDefs.Code.F2'(K,L)),
        user:goedel_freeze(ground([H,B]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(H,B))),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(L, I, M),
        'SharedPrograms':'SharedPrograms.PickPredDef.P4'(J, M, 'ProgDefs.PredDef.F4'(J,N,O,P), Q),
        'Lists':'Lists.Delete.P3'(F, N, R),
        user:goedel_freeze(ground([O,P,R]), ((R=[],O=[],P=[]),true->S=Q;S=['ProgDefs.PredDef.F4'(J,R,O,P)|Q])),
        user:goedel_freeze(ground([S]), (S=[],true->'AVLTrees':'AVLTrees.AVLDelete.P3'(L,I,T);'AVLTrees':'AVLTrees.AVLUpdate.P5'(L,I,S,T,_))),
        'AVLTrees':'AVLTrees.AVLUpdate.P5'(E, H, 'ProgDefs.Code.F2'(K,T), G, _).
'~Scripts.DeleteStatement.P3.0'('Scripts.Script.F5'(A,B,C,D,E), F, 'Scripts.Script.F5'(A,B,C,D,G)) :-
        '~Scripts.OpenStatementPredicate.P2'(F, 'MetaDefs.Name.F4'(H,I,_,J)),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(E, H, 'ProgDefs.Code.F2'(K,L)),
        user:goedel_freeze(ground([H,B]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(H,B))),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(L, I, M),
        'SharedPrograms':'~SharedPrograms.PickPredDef.P4'(J, M, 'ProgDefs.PredDef.F4'(J,N,O,P), Q),
        'Lists':'~Lists.Delete.P3'(F, N, R),
        user:goedel_freeze(ground([O,P,R]), ((R=[],O=[],P=[]),true->S=Q;S=['ProgDefs.PredDef.F4'(J,R,O,P)|Q])),
        user:goedel_freeze(ground([S]), (S=[],true->'AVLTrees':'~AVLTrees.AVLDelete.P3'(L,I,T);'AVLTrees':'~AVLTrees.AVLUpdate.P5'(L,I,S,T,_))),
        'AVLTrees':'~AVLTrees.AVLUpdate.P5'(E, H, 'ProgDefs.Code.F2'(K,T), G, _).
'Scripts.DeleteScriptProposition.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.DeleteScriptProposition.P3.0'(A,B,C)).
'~Scripts.DeleteScriptProposition.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.DeleteScriptProposition.P3.0'(A,B,C)).
'Scripts.DeleteScriptProposition.P3.0'(A, B, C) :-
        'Scripts.DelDeclaration.P4'(C, B, 'ProgDefs.PropositionDecl.C0', A).
'~Scripts.DeleteScriptProposition.P3.0'(A, B, C) :-
        '~Scripts.DelDeclaration.P4'(C, B, 'ProgDefs.PropositionDecl.C0', A).
'Scripts.DeleteSymbol.P5'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G, 'ProgDefs.Language.F1'(H)) :-
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(I,'ProgDefs.Categories.F2'(J,K),L)),
        user:goedel_freeze(ground([A,F]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(A,F))),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(K, B, M),
        'SharedPrograms':'SharedPrograms.FindDescriptor.P5'(M, I, C, D, G),
        'Lists':'Lists.DeleteFirst.P3'('ProgDefs.Symbol.F2'(_,G), M, N),
        user:goedel_freeze(ground([N]), (N=[],true->'AVLTrees':'AVLTrees.AVLDelete.P3'(K,B,O);'AVLTrees':'AVLTrees.AVLUpdate.P5'(K,B,N,O,_))),
        'AVLTrees':'AVLTrees.AVLUpdate.P5'(E, A, 'ProgDefs.Module.F3'(I,'ProgDefs.Categories.F2'(J,O),L), H, _).
'~Scripts.DeleteSymbol.P5'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G, 'ProgDefs.Language.F1'(H)) :-
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(I,'ProgDefs.Categories.F2'(J,K),L)),
        user:goedel_freeze(ground([A,F]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(A,F))),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(K, B, M),
        'SharedPrograms':'~SharedPrograms.FindDescriptor.P5'(M, I, C, D, G),
        'Lists':'~Lists.DeleteFirst.P3'('ProgDefs.Symbol.F2'(_,G), M, N),
        user:goedel_freeze(ground([N]), (N=[],true->'AVLTrees':'~AVLTrees.AVLDelete.P3'(K,B,O);'AVLTrees':'~AVLTrees.AVLUpdate.P5'(K,B,N,O,_))),
        'AVLTrees':'~AVLTrees.AVLUpdate.P5'(E, A, 'ProgDefs.Module.F3'(I,'ProgDefs.Categories.F2'(J,O),L), H, _).
'Scripts.ProgramToScript.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.ProgramToScript.P2.0'(A,B)).
'~Scripts.ProgramToScript.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.ProgramToScript.P2.0'(A,B)).
'Scripts.ProgramToScript.P2.0'(A, 'Scripts.Script.F5'(B,C,D,E,F)) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, G),
        'SharedPrograms':'SharedPrograms.ModuleLanguage.P3'(G, B, E),
        G='ProgDefs.Program.F4'(B,C,D,F).
'~Scripts.ProgramToScript.P2.0'(A, 'Scripts.Script.F5'(B,C,D,E,F)) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, G),
        'SharedPrograms':'~SharedPrograms.ModuleLanguage.P3'(G, B, E),
        G='ProgDefs.Program.F4'(B,C,D,F).
'Scripts.InsertScriptProposition.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Scripts':'Scripts.InsertScriptProposition.P3.0'(A,B,C)).
'~Scripts.InsertScriptProposition.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Scripts':'~Scripts.InsertScriptProposition.P3.0'(A,B,C)).
'Scripts.InsertScriptProposition.P3.0'(A, B, C) :-
        'Scripts.InsDeclaration.P4'(A, B, 'ProgDefs.PropositionDecl.C0', C).
'~Scripts.InsertScriptProposition.P3.0'(A, B, C) :-
        '~Scripts.InsDeclaration.P4'(A, B, 'ProgDefs.PropositionDecl.C0', C).
'Scripts.InsDeclaration.P4'('Scripts.Script.F5'(A,B,C,D,E), F, G, 'Scripts.Script.F5'(A,B,H,D,E)) :-
        'Scripts.ValidDeclaration.P2'(G, C),
        F='MetaDefs.Name.F4'(I,_,_,_),
        user:goedel_freeze(ground([I,B]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(I,B))),
        'SharedPrograms':'SharedPrograms.InsertSymbol.P5'(F, C, 'ProgDefs.Exported.C0', G, H).
'~Scripts.InsDeclaration.P4'('Scripts.Script.F5'(A,B,C,D,E), F, G, 'Scripts.Script.F5'(A,B,H,D,E)) :-
        '~Scripts.ValidDeclaration.P2'(G, C),
        F='MetaDefs.Name.F4'(I,_,_,_),
        user:goedel_freeze(ground([I,B]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(I,B))),
        'SharedPrograms':'~SharedPrograms.InsertSymbol.P5'(F, C, 'ProgDefs.Exported.C0', G, H).
'Scripts.FunctionInScript.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.FunctionInScript.P5.0'(A,B,C,D,E)).
'~Scripts.FunctionInScript.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.FunctionInScript.P5.0'(A,B,C,D,E)).
'Scripts.FunctionInScript.P5.0'('Scripts.Script.F5'(_,A,B,_,_), C, D, E, F) :-
        'Scripts.SymbolInScript.P4'(C, B, A, 'ProgDefs.FunctionDecl.F4'(_,D,E,F)).
'~Scripts.FunctionInScript.P5.0'('Scripts.Script.F5'(_,A,B,_,_), C, D, E, F) :-
        '~Scripts.SymbolInScript.P4'(C, B, A, 'ProgDefs.FunctionDecl.F4'(_,D,E,F)).
'Scripts.FreeSymbol.P2'(A, B) :-
        user:goedel_freeze(ground([B,A]), user:goedel_not(('AVLTrees':'~AVLTrees.AVLMember.P3'(B,_,C),user:goedel_freeze(ground([C,A]),user:goedel_not('Scripts':'~Scripts.SymbolFreeInModuleCode.P2'(A,C)))))).
'~Scripts.FreeSymbol.P2'(A, B) :-
        user:goedel_freeze(ground([B,A]), user:goedel_not(('AVLTrees':'~AVLTrees.AVLMember.P3'(B,_,C),user:goedel_freeze(ground([C,A]),user:goedel_not('Scripts':'~Scripts.SymbolFreeInModuleCode.P2'(A,C)))))).
'Scripts.HasIntensionalSet.P1'('MetaDefs.Term.F2'(_,A)) :-
        'Scripts.HasIntensionalSet1.P1'(A).
'~Scripts.HasIntensionalSet.P1'('MetaDefs.Term.F2'(_,A)) :-
        '~Scripts.HasIntensionalSet1.P1'(A).
'Scripts.HasIntensionalSet.P1'('MetaDefs.SuchThat.F2'(_,_)).
'~Scripts.HasIntensionalSet.P1'('MetaDefs.SuchThat.F2'(_,_)).
'Scripts.HasIntensionalSet1.P1'([A|B]) :-
        (   'Scripts.HasIntensionalSet.P1'(A)
        ;   'Scripts.HasIntensionalSet1.P1'(B)
        ).
'~Scripts.HasIntensionalSet1.P1'([A|B]) :-
        (   '~Scripts.HasIntensionalSet.P1'(A)
        ;   '~Scripts.HasIntensionalSet1.P1'(B)
        ).
'Scripts.InsertDelay.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Scripts':'Scripts.InsertDelay.P4.0'(A,B,C,D)).
'~Scripts.InsertDelay.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Scripts':'~Scripts.InsertDelay.P4.0'(A,B,C,D)).
'Scripts.InsertDelay.P4.0'('Scripts.Script.F5'(A,B,C,D,E), F, G, 'Scripts.Script.F5'(A,B,C,D,H)) :-
        'Syntax':'Syntax.PredicateAtom.P3'(F, 'MetaDefs.Name.F4'(I,J,_,K), _),
        user:goedel_freeze(ground([I,B]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(I,B))),
        'SharedPrograms':'SharedPrograms.FormulaInLanguage.P4'(F, C, 'MetaDefs.VarTyping.F1'([]), _),
        'Scripts.IntensionalSetFree.P1'(F),
        'AVLTrees':'AVLTrees.AVLIsEmpty.P1'(L),
        'AVLTrees':'AVLTrees.AVLAmend.P6'(E, I, 'ProgDefs.Code.F2'(M,N), 'ProgDefs.Code.F2'(0,L), H, 'ProgDefs.Code.F2'(M,O)),
        'AVLTrees':'AVLTrees.AVLAmend.P6'(O, J, P, [], N, Q),
        user:goedel_freeze(ground([K,Q]), if(('Lists':'~Lists.DeleteFirst.P3'('ProgDefs.PredDef.F4'(K,R,S,T),Q,U),true),P=['ProgDefs.PredDef.F4'(K,R,S,['ProgDefs.Delay.F2'(F,G)|T])|U],P=['ProgDefs.PredDef.F4'(K,[],[],['ProgDefs.Delay.F2'(F,G)])|Q])).
'~Scripts.InsertDelay.P4.0'('Scripts.Script.F5'(A,B,C,D,E), F, G, 'Scripts.Script.F5'(A,B,C,D,H)) :-
        'Syntax':'~Syntax.PredicateAtom.P3'(F, 'MetaDefs.Name.F4'(I,J,_,K), _),
        user:goedel_freeze(ground([I,B]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(I,B))),
        'SharedPrograms':'~SharedPrograms.FormulaInLanguage.P4'(F, C, 'MetaDefs.VarTyping.F1'([]), _),
        '~Scripts.IntensionalSetFree.P1'(F),
        'AVLTrees':'~AVLTrees.AVLIsEmpty.P1'(L),
        'AVLTrees':'~AVLTrees.AVLAmend.P6'(E, I, 'ProgDefs.Code.F2'(M,N), 'ProgDefs.Code.F2'(0,L), H, 'ProgDefs.Code.F2'(M,O)),
        'AVLTrees':'~AVLTrees.AVLAmend.P6'(O, J, P, [], N, Q),
        user:goedel_freeze(ground([K,Q]), if(('Lists':'~Lists.DeleteFirst.P3'('ProgDefs.PredDef.F4'(K,R,S,T),Q,U),true),P=['ProgDefs.PredDef.F4'(K,R,S,['ProgDefs.Delay.F2'(F,G)|T])|U],P=['ProgDefs.PredDef.F4'(K,[],[],['ProgDefs.Delay.F2'(F,G)])|Q])).
'Scripts.InsertScriptPredicate.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C,D]), 'Scripts':'Scripts.InsertScriptPredicate.P5.0'(A,B,C,D,E)).
'~Scripts.InsertScriptPredicate.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C,D]), 'Scripts':'~Scripts.InsertScriptPredicate.P5.0'(A,B,C,D,E)).
'Scripts.InsertScriptPredicate.P5.0'(A, B, C, D, E) :-
        'Scripts.InsDeclaration.P4'(A, B, 'ProgDefs.PredicateDecl.F3'(_,C,D), E).
'~Scripts.InsertScriptPredicate.P5.0'(A, B, C, D, E) :-
        '~Scripts.InsDeclaration.P4'(A, B, 'ProgDefs.PredicateDecl.F3'(_,C,D), E).
'Scripts.OpenHeadPred.P2'(A, B) :-
        user:goedel_freeze(nonvar(A), 'Scripts':'Scripts.OpenHeadPred.P2.0'(A,B)).
'~Scripts.OpenHeadPred.P2'(A, B) :-
        user:goedel_freeze(nonvar(A), 'Scripts':'~Scripts.OpenHeadPred.P2.0'(A,B)).
'Scripts.OpenHeadPred.P2.0'('MetaDefs.PAtom.F1'(A), A).
'~Scripts.OpenHeadPred.P2.0'('MetaDefs.PAtom.F1'(A), A).
'Scripts.OpenHeadPred.P2.0'('MetaDefs.Atom.F2'(A,_), A).
'~Scripts.OpenHeadPred.P2.0'('MetaDefs.Atom.F2'(A,_), A).
'Scripts.InsertStatement.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Scripts':'Scripts.InsertStatement.P3.0'(A,B,C)).
'~Scripts.InsertStatement.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Scripts':'~Scripts.InsertStatement.P3.0'(A,B,C)).
'Scripts.InsertStatement.P3.0'('Scripts.Script.F5'(A,B,C,D,E), F, 'Scripts.Script.F5'(A,B,C,D,G)) :-
        'SharedPrograms':'SharedPrograms.StatementInLanguage.P5'(F, C, _, _, []),
        'Scripts.OpenStatementPredicate.P2'(F, 'MetaDefs.Name.F4'(H,I,_,J)),
        user:goedel_freeze(ground([H,B]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(H,B))),
        'AVLTrees':'AVLTrees.AVLIsEmpty.P1'(K),
        'AVLTrees':'AVLTrees.AVLAmend.P6'(E, H, 'ProgDefs.Code.F2'(L,M), 'ProgDefs.Code.F2'(0,K), G, 'ProgDefs.Code.F2'(L,N)),
        'AVLTrees':'AVLTrees.AVLAmend.P6'(N, I, O, [], M, P),
        user:goedel_freeze(ground([J,P]), if(('Lists':'~Lists.DeleteFirst.P3'('ProgDefs.PredDef.F4'(J,Q,R,S),P,T),true),O=['ProgDefs.PredDef.F4'(J,[F|Q],R,S)|T],O=['ProgDefs.PredDef.F4'(J,[F],[],[])|P])).
'~Scripts.InsertStatement.P3.0'('Scripts.Script.F5'(A,B,C,D,E), F, 'Scripts.Script.F5'(A,B,C,D,G)) :-
        'SharedPrograms':'~SharedPrograms.StatementInLanguage.P5'(F, C, _, _, []),
        '~Scripts.OpenStatementPredicate.P2'(F, 'MetaDefs.Name.F4'(H,I,_,J)),
        user:goedel_freeze(ground([H,B]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(H,B))),
        'AVLTrees':'~AVLTrees.AVLIsEmpty.P1'(K),
        'AVLTrees':'~AVLTrees.AVLAmend.P6'(E, H, 'ProgDefs.Code.F2'(L,M), 'ProgDefs.Code.F2'(0,K), G, 'ProgDefs.Code.F2'(L,N)),
        'AVLTrees':'~AVLTrees.AVLAmend.P6'(N, I, O, [], M, P),
        user:goedel_freeze(ground([J,P]), if(('Lists':'~Lists.DeleteFirst.P3'('ProgDefs.PredDef.F4'(J,Q,R,S),P,T),true),O=['ProgDefs.PredDef.F4'(J,[F|Q],R,S)|T],O=['ProgDefs.PredDef.F4'(J,[F],[],[])|P])).
'Scripts.IntensionalSetFree.P1'('MetaDefs.Atom.F2'(_,A)) :-
        user:goedel_freeze(ground([A]), user:goedel_not('Scripts':'~Scripts.HasIntensionalSet1.P1'(A))).
'~Scripts.IntensionalSetFree.P1'('MetaDefs.Atom.F2'(_,A)) :-
        user:goedel_freeze(ground([A]), user:goedel_not('Scripts':'~Scripts.HasIntensionalSet1.P1'(A))).
'Scripts.PredicateInScript.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.PredicateInScript.P4.0'(A,B,C,D)).
'~Scripts.PredicateInScript.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.PredicateInScript.P4.0'(A,B,C,D)).
'Scripts.PredicateInScript.P4.0'('Scripts.Script.F5'(_,A,B,_,_), C, D, E) :-
        'Scripts.SymbolInScript.P4'(C, B, A, 'ProgDefs.PredicateDecl.F3'(_,D,E)).
'~Scripts.PredicateInScript.P4.0'('Scripts.Script.F5'(_,A,B,_,_), C, D, E) :-
        '~Scripts.SymbolInScript.P4'(C, B, A, 'ProgDefs.PredicateDecl.F3'(_,D,E)).
'Scripts.OpenStatementPredicate.P2'('MetaDefs.<-''.F2'(A,_), B) :-
        'Scripts.OpenHeadPred.P2'(A, B).
'~Scripts.OpenStatementPredicate.P2'('MetaDefs.<-''.F2'(A,_), B) :-
        '~Scripts.OpenHeadPred.P2'(A, B).
'Scripts.SymbolInScript.P4'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G) :-
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(H,'ProgDefs.Categories.F2'(_,I),_)),
        'AVLTrees':'AVLTrees.AVLSearch.P3'(F, A, 'ProgDefs.ModDef.F4'(J,_,_,_)),
        user:goedel_freeze(ground([J]), (J='ProgDefs.ClosedKind.C0',true->H='ProgDefs.Exported.C0';H='ProgDefs.Hidden.C0')),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(I, B, K),
        'SharedPrograms':'SharedPrograms.FindDescriptor.P5'(K, H, C, D, G).
'~Scripts.SymbolInScript.P4'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G) :-
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(H,'ProgDefs.Categories.F2'(_,I),_)),
        'AVLTrees':'~AVLTrees.AVLSearch.P3'(F, A, 'ProgDefs.ModDef.F4'(J,_,_,_)),
        user:goedel_freeze(ground([J]), (J='ProgDefs.ClosedKind.C0',true->H='ProgDefs.Exported.C0';H='ProgDefs.Hidden.C0')),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(I, B, K),
        'SharedPrograms':'~SharedPrograms.FindDescriptor.P5'(K, H, C, D, G).
'Scripts.SymbolFreeInModuleCode.P2'(A, 'ProgDefs.Code.F2'(_,B)) :-
        user:goedel_freeze(ground([A,B]), user:goedel_not((('AVLTrees':'~AVLTrees.AVLMember.P3'(B,_,C),'Lists':'~Lists.Member.P2'('ProgDefs.PredDef.F4'(_,D,E,F),C)),('Lists':'~Lists.Member.P2'(G,D),'Scripts':'~Scripts.SymbolInFormula.P2'(G,A);'Lists':'~Lists.Member.P2'(H,E),'Scripts':'~Scripts.SymbolInDelay.P2'(H,A);'Lists':'~Lists.Member.P2'(I,F),'Scripts':'~Scripts.SymbolInDelay.P2'(I,A))))).
'~Scripts.SymbolFreeInModuleCode.P2'(A, 'ProgDefs.Code.F2'(_,B)) :-
        user:goedel_freeze(ground([A,B]), user:goedel_not((('AVLTrees':'~AVLTrees.AVLMember.P3'(B,_,C),'Lists':'~Lists.Member.P2'('ProgDefs.PredDef.F4'(_,D,E,F),C)),('Lists':'~Lists.Member.P2'(G,D),'Scripts':'~Scripts.SymbolInFormula.P2'(G,A);'Lists':'~Lists.Member.P2'(H,E),'Scripts':'~Scripts.SymbolInDelay.P2'(H,A);'Lists':'~Lists.Member.P2'(I,F),'Scripts':'~Scripts.SymbolInDelay.P2'(I,A))))).
'Scripts.StatementInScript.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.StatementInScript.P2.0'(A,B)).
'~Scripts.StatementInScript.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.StatementInScript.P2.0'(A,B)).
'Scripts.StatementInScript.P2.0'(A, B) :-
        'Scripts.OpenStatementPredicate.P2'(B, C),
        'Scripts.DefinitionInScript.P3'(A, C, D),
        'Lists':'Lists.Member.P2'(B, D).
'~Scripts.StatementInScript.P2.0'(A, B) :-
        '~Scripts.OpenStatementPredicate.P2'(B, C),
        '~Scripts.DefinitionInScript.P3'(A, C, D),
        'Lists':'~Lists.Member.P2'(B, D).
'Scripts.PropositionInScript.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Scripts':'Scripts.PropositionInScript.P2.0'(A,B)).
'~Scripts.PropositionInScript.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Scripts':'~Scripts.PropositionInScript.P2.0'(A,B)).
'Scripts.PropositionInScript.P2.0'('Scripts.Script.F5'(_,A,B,_,_), C) :-
        'Scripts.SymbolInScript.P4'(C, B, A, 'ProgDefs.PropositionDecl.C0').
'~Scripts.PropositionInScript.P2.0'('Scripts.Script.F5'(_,A,B,_,_), C) :-
        '~Scripts.SymbolInScript.P4'(C, B, A, 'ProgDefs.PropositionDecl.C0').
'Scripts.ReformatDelays.P3'([], [], []).
'~Scripts.ReformatDelays.P3'([], [], []).
'Scripts.ReformatDelays.P3'(['ProgDefs.Delay.F2'(B,C)|A], [B|D], [C|E]) :-
        'Scripts.ReformatDelays.P3'(A, D, E).
'~Scripts.ReformatDelays.P3'(['ProgDefs.Delay.F2'(B,C)|A], [B|D], [C|E]) :-
        '~Scripts.ReformatDelays.P3'(A, D, E).
'Scripts.StatementMatchAtom.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Scripts':'Scripts.StatementMatchAtom.P3.0'(A,B,C)).
'~Scripts.StatementMatchAtom.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Scripts':'~Scripts.StatementMatchAtom.P3.0'(A,B,C)).
'Scripts.StatementMatchAtom.P3.0'('Scripts.Script.F5'(_,A,B,_,C), D, E) :-
        'Scripts.OpenHeadPred.P2'(D, 'MetaDefs.Name.F4'(F,G,_,H)),
        'SharedPrograms':'SharedPrograms.FormulaInLanguage.P4'(D, B, 'MetaDefs.VarTyping.F1'([]), _),
        user:goedel_freeze(ground([F,A]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(F,A))),
        'AVLTrees':'AVLTrees.AVLSearch.P3'(C, F, 'ProgDefs.Code.F2'(_,I)),
        'AVLTrees':'AVLTrees.AVLSearch.P3'(I, G, J),
        'Lists':'Lists.MemberCheck.P2'('ProgDefs.PredDef.F4'(H,K,_,_), J),
        'Lists':'Lists.Member.P2'(E, K).
'~Scripts.StatementMatchAtom.P3.0'('Scripts.Script.F5'(_,A,B,_,C), D, E) :-
        '~Scripts.OpenHeadPred.P2'(D, 'MetaDefs.Name.F4'(F,G,_,H)),
        'SharedPrograms':'~SharedPrograms.FormulaInLanguage.P4'(D, B, 'MetaDefs.VarTyping.F1'([]), _),
        user:goedel_freeze(ground([F,A]), user:goedel_not('Scripts':'~Scripts.ClosedModule.P2'(F,A))),
        'AVLTrees':'~AVLTrees.AVLSearch.P3'(C, F, 'ProgDefs.Code.F2'(_,I)),
        'AVLTrees':'~AVLTrees.AVLSearch.P3'(I, G, J),
        'Lists':'~Lists.MemberCheck.P2'('ProgDefs.PredDef.F4'(H,K,_,_), J),
        'Lists':'~Lists.Member.P2'(E, K).
'Scripts.SymbolInFormula.P2'('MetaDefs.PAtom.F1'(A), A).
'~Scripts.SymbolInFormula.P2'('MetaDefs.PAtom.F1'(A), A).
'Scripts.SymbolInFormula.P2'('MetaDefs.Atom.F2'(A,B), C) :-
        (   C=A
        ;   'Scripts.SymbolInTermList.P2'(B, C)
        ).
'~Scripts.SymbolInFormula.P2'('MetaDefs.Atom.F2'(A,B), C) :-
        (   C=A
        ;   '~Scripts.SymbolInTermList.P2'(B, C)
        ).
'Scripts.SymbolInFormula.P2'('MetaDefs.~''.F1'(A), B) :-
        'Scripts.SymbolInFormula.P2'(A, B).
'~Scripts.SymbolInFormula.P2'('MetaDefs.~''.F1'(A), B) :-
        '~Scripts.SymbolInFormula.P2'(A, B).
'Scripts.SymbolInFormula.P2'('MetaDefs.&''.F2'(A,B), C) :-
        (   'Scripts.SymbolInFormula.P2'(A, C)
        ;   'Scripts.SymbolInFormula.P2'(B, C)
        ).
'~Scripts.SymbolInFormula.P2'('MetaDefs.&''.F2'(A,B), C) :-
        (   '~Scripts.SymbolInFormula.P2'(A, C)
        ;   '~Scripts.SymbolInFormula.P2'(B, C)
        ).
'Scripts.SymbolInFormula.P2'('MetaDefs.\/''.F2'(A,B), C) :-
        (   'Scripts.SymbolInFormula.P2'(A, C)
        ;   'Scripts.SymbolInFormula.P2'(B, C)
        ).
'~Scripts.SymbolInFormula.P2'('MetaDefs.\/''.F2'(A,B), C) :-
        (   '~Scripts.SymbolInFormula.P2'(A, C)
        ;   '~Scripts.SymbolInFormula.P2'(B, C)
        ).
'Scripts.SymbolInFormula.P2'('MetaDefs.->''.F2'(A,B), C) :-
        (   'Scripts.SymbolInFormula.P2'(A, C)
        ;   'Scripts.SymbolInFormula.P2'(B, C)
        ).
'~Scripts.SymbolInFormula.P2'('MetaDefs.->''.F2'(A,B), C) :-
        (   '~Scripts.SymbolInFormula.P2'(A, C)
        ;   '~Scripts.SymbolInFormula.P2'(B, C)
        ).
'Scripts.SymbolInFormula.P2'('MetaDefs.<-''.F2'(A,B), C) :-
        (   'Scripts.SymbolInFormula.P2'(A, C)
        ;   'Scripts.SymbolInFormula.P2'(B, C)
        ).
'~Scripts.SymbolInFormula.P2'('MetaDefs.<-''.F2'(A,B), C) :-
        (   '~Scripts.SymbolInFormula.P2'(A, C)
        ;   '~Scripts.SymbolInFormula.P2'(B, C)
        ).
'Scripts.SymbolInFormula.P2'('MetaDefs.<->''.F2'(A,B), C) :-
        (   'Scripts.SymbolInFormula.P2'(A, C)
        ;   'Scripts.SymbolInFormula.P2'(B, C)
        ).
'~Scripts.SymbolInFormula.P2'('MetaDefs.<->''.F2'(A,B), C) :-
        (   '~Scripts.SymbolInFormula.P2'(A, C)
        ;   '~Scripts.SymbolInFormula.P2'(B, C)
        ).
'Scripts.SymbolInFormula.P2'('MetaDefs.All.F2'(_,A), B) :-
        'Scripts.SymbolInFormula.P2'(A, B).
'~Scripts.SymbolInFormula.P2'('MetaDefs.All.F2'(_,A), B) :-
        '~Scripts.SymbolInFormula.P2'(A, B).
'Scripts.SymbolInFormula.P2'('MetaDefs.Some.F2'(_,A), B) :-
        'Scripts.SymbolInFormula.P2'(A, B).
'~Scripts.SymbolInFormula.P2'('MetaDefs.Some.F2'(_,A), B) :-
        '~Scripts.SymbolInFormula.P2'(A, B).
'Scripts.SymbolInFormula.P2'('MetaDefs.ISTE.F4'(_,A,B,C), D) :-
        (   'Scripts.SymbolInFormula.P2'(A, D)
        ;   'Scripts.SymbolInFormula.P2'(B, D)
        ;   'Scripts.SymbolInFormula.P2'(C, D)
        ).
'~Scripts.SymbolInFormula.P2'('MetaDefs.ISTE.F4'(_,A,B,C), D) :-
        (   '~Scripts.SymbolInFormula.P2'(A, D)
        ;   '~Scripts.SymbolInFormula.P2'(B, D)
        ;   '~Scripts.SymbolInFormula.P2'(C, D)
        ).
'Scripts.SymbolInFormula.P2'('MetaDefs.ITE.F3'(A,B,C), D) :-
        (   'Scripts.SymbolInFormula.P2'(A, D)
        ;   'Scripts.SymbolInFormula.P2'(B, D)
        ;   'Scripts.SymbolInFormula.P2'(C, D)
        ).
'~Scripts.SymbolInFormula.P2'('MetaDefs.ITE.F3'(A,B,C), D) :-
        (   '~Scripts.SymbolInFormula.P2'(A, D)
        ;   '~Scripts.SymbolInFormula.P2'(B, D)
        ;   '~Scripts.SymbolInFormula.P2'(C, D)
        ).
'Scripts.SymbolInFormula.P2'('MetaDefs.IST.F3'(_,A,B), C) :-
        (   'Scripts.SymbolInFormula.P2'(A, C)
        ;   'Scripts.SymbolInFormula.P2'(B, C)
        ).
'~Scripts.SymbolInFormula.P2'('MetaDefs.IST.F3'(_,A,B), C) :-
        (   '~Scripts.SymbolInFormula.P2'(A, C)
        ;   '~Scripts.SymbolInFormula.P2'(B, C)
        ).
'Scripts.SymbolInFormula.P2'('MetaDefs.IT.F2'(A,B), C) :-
        (   'Scripts.SymbolInFormula.P2'(A, C)
        ;   'Scripts.SymbolInFormula.P2'(B, C)
        ).
'~Scripts.SymbolInFormula.P2'('MetaDefs.IT.F2'(A,B), C) :-
        (   '~Scripts.SymbolInFormula.P2'(A, C)
        ;   '~Scripts.SymbolInFormula.P2'(B, C)
        ).
'Scripts.SymbolInDelay.P2'('ProgDefs.Delay.F2'(A,_), B) :-
        'Scripts.SymbolInFormula.P2'(A, B).
'~Scripts.SymbolInDelay.P2'('ProgDefs.Delay.F2'(A,_), B) :-
        '~Scripts.SymbolInFormula.P2'(A, B).
'Scripts.TermInScript.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C]), 'Scripts':'Scripts.TermInScript.P5.0'(A,B,C,D,E)).
'~Scripts.TermInScript.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C]), 'Scripts':'~Scripts.TermInScript.P5.0'(A,B,C,D,E)).
'Scripts.TermInScript.P5.0'('Scripts.Script.F5'(_,_,A,_,_), B, C, D, E) :-
        'SharedPrograms':'SharedPrograms.TermInLanguage.P5'(C, A, B, E, D).
'~Scripts.TermInScript.P5.0'('Scripts.Script.F5'(_,_,A,_,_), B, C, D, E) :-
        'SharedPrograms':'~SharedPrograms.TermInLanguage.P5'(C, A, B, E, D).
'Scripts.SymbolInTermList.P2'([A|B], C) :-
        (   'Scripts.SymbolInTerm.P2'(A, C)
        ;   'Scripts.SymbolInTermList.P2'(B, C)
        ).
'~Scripts.SymbolInTermList.P2'([A|B], C) :-
        (   '~Scripts.SymbolInTerm.P2'(A, C)
        ;   '~Scripts.SymbolInTermList.P2'(B, C)
        ).
'Scripts.SymbolInTerm.P2'('MetaDefs.CTerm.F1'(A), A).
'~Scripts.SymbolInTerm.P2'('MetaDefs.CTerm.F1'(A), A).
'Scripts.SymbolInTerm.P2'('MetaDefs.Term.F2'(A,B), C) :-
        (   A=C
        ;   'Scripts.SymbolInTermList.P2'(B, C)
        ).
'~Scripts.SymbolInTerm.P2'('MetaDefs.Term.F2'(A,B), C) :-
        (   A=C
        ;   '~Scripts.SymbolInTermList.P2'(B, C)
        ).
'Scripts.SymbolInTerm.P2'('MetaDefs.SuchThat.F2'(A,B), C) :-
        (   'Scripts.SymbolInTerm.P2'(A, C)
        ;   'Scripts.SymbolInFormula.P2'(B, C)
        ).
'~Scripts.SymbolInTerm.P2'('MetaDefs.SuchThat.F2'(A,B), C) :-
        (   '~Scripts.SymbolInTerm.P2'(A, C)
        ;   '~Scripts.SymbolInFormula.P2'(B, C)
        ).
'Scripts.TypeSymbolInScriptLanguage.P4'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G) :-
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(H,_),_)),
        'AVLTrees':'AVLTrees.AVLSearch.P3'(F, A, 'ProgDefs.ModDef.F4'(I,_,_,_)),
        user:goedel_freeze(ground([I]), (I='ProgDefs.ClosedKind.C0',true->J='ProgDefs.Exported.C0';J='ProgDefs.Hidden.C0')),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(H, B, K),
        'SharedPrograms':'SharedPrograms.FindDescriptor.P5'(K, J, C, D, G).
'~Scripts.TypeSymbolInScriptLanguage.P4'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G) :-
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(H,_),_)),
        'AVLTrees':'~AVLTrees.AVLSearch.P3'(F, A, 'ProgDefs.ModDef.F4'(I,_,_,_)),
        user:goedel_freeze(ground([I]), (I='ProgDefs.ClosedKind.C0',true->J='ProgDefs.Exported.C0';J='ProgDefs.Hidden.C0')),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(H, B, K),
        'SharedPrograms':'~SharedPrograms.FindDescriptor.P5'(K, J, C, D, G).
'Scripts.TypeInScript.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Scripts':'Scripts.TypeInScript.P2.0'(A,B)).
'~Scripts.TypeInScript.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Scripts':'~Scripts.TypeInScript.P2.0'(A,B)).
'Scripts.TypeInScript.P2.0'('Scripts.Script.F5'(_,_,A,_,_), B) :-
        'SharedPrograms':'SharedPrograms.TypeInLanguage.P2'(B, A).
'~Scripts.TypeInScript.P2.0'('Scripts.Script.F5'(_,_,A,_,_), B) :-
        'SharedPrograms':'~SharedPrograms.TypeInLanguage.P2'(B, A).
'Scripts.ValidDeclaration.P2'('ProgDefs.PropositionDecl.C0', _).
'~Scripts.ValidDeclaration.P2'('ProgDefs.PropositionDecl.C0', _).
'Scripts.ValidDeclaration.P2'('ProgDefs.PredicateDecl.F3'(_,_,A), B) :-
        'SharedPrograms':'SharedPrograms.ListTypesInLanguage.P2'(A, B).
'~Scripts.ValidDeclaration.P2'('ProgDefs.PredicateDecl.F3'(_,_,A), B) :-
        'SharedPrograms':'~SharedPrograms.ListTypesInLanguage.P2'(A, B).
