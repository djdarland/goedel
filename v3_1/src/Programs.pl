:- module('Programs', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

'Programs.MainModuleInProgram.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.MainModuleInProgram.P2.0'(A,B)).
'~Programs.MainModuleInProgram.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.MainModuleInProgram.P2.0'(A,B)).
'Programs.MainModuleInProgram.P2.0'(A, B) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, C),
        'SharedPrograms':'SharedPrograms.SMainModuleInProgram.P2'(C, B).
'~Programs.MainModuleInProgram.P2.0'(A, B) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, C),
        'SharedPrograms':'~SharedPrograms.SMainModuleInProgram.P2'(C, B).
'Programs.FormulaInModule.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C,D,E]), 'Programs':'Programs.FormulaInModule.P6.0'(A,B,C,D,E,F)).
'~Programs.FormulaInModule.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C,D,E]), 'Programs':'~Programs.FormulaInModule.P6.0'(A,B,C,D,E,F)).
'Programs.FormulaInModule.P6.0'(A, B, C, D, E, F) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, G),
        'Programs.CModulePart.P2'(C, H),
        'Programs.ModulePartLanguage.P4'(H, B, G, I),
        'SharedPrograms':'SharedPrograms.FormulaInLanguage.P4'(E, I, D, F).
'~Programs.FormulaInModule.P6.0'(A, B, C, D, E, F) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, G),
        '~Programs.CModulePart.P2'(C, H),
        '~Programs.ModulePartLanguage.P4'(H, B, G, I),
        'SharedPrograms':'~SharedPrograms.FormulaInLanguage.P4'(E, I, D, F).
'Programs.DeclaredInOpenModule.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,C]), 'Programs':'Programs.DeclaredInOpenModule.P3.0'(A,B,C)).
'~Programs.DeclaredInOpenModule.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,C]), 'Programs':'~Programs.DeclaredInOpenModule.P3.0'(A,B,C)).
'Programs.DeclaredInOpenModule.P3.0'('ProgDefs.CachedProgram.F1'(A), B, C) :-
        'ProgramCache':'ProgramCache.GetCachedProgram.P2'(A, D),
        'Programs.DeclaredInOpenModule.P3'(D, B, C).
'~Programs.DeclaredInOpenModule.P3.0'('ProgDefs.CachedProgram.F1'(A), B, C) :-
        'ProgramCache':'~ProgramCache.GetCachedProgram.P2'(A, D),
        '~Programs.DeclaredInOpenModule.P3'(D, B, C).
'Programs.DeclaredInOpenModule.P3.0'('ProgDefs.Program.F4'(_,A,B,_), C, D) :-
        'Syntax':'Syntax.Atom.P1'(D),
        'Syntax':'Syntax.EmptyVarTyping.P1'(E),
        'SharedPrograms':'SharedPrograms.FormulaInLanguage.P4'(D, B, E, _),
        'Programs.TopLevelName.P2'(D, 'MetaDefs.Name.F4'(C,_,_,_)),
        'SharedPrograms':'SharedPrograms.OpenModuleAux.P2'(A, C).
'~Programs.DeclaredInOpenModule.P3.0'('ProgDefs.Program.F4'(_,A,B,_), C, D) :-
        'Syntax':'~Syntax.Atom.P1'(D),
        'Syntax':'~Syntax.EmptyVarTyping.P1'(E),
        'SharedPrograms':'~SharedPrograms.FormulaInLanguage.P4'(D, B, E, _),
        '~Programs.TopLevelName.P2'(D, 'MetaDefs.Name.F4'(C,_,_,_)),
        'SharedPrograms':'~SharedPrograms.OpenModuleAux.P2'(A, C).
'Programs.Compute.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C,E]), 'Programs':'Programs.Compute.P7.0'(A,B,C,D,E,F,G)).
'~Programs.Compute.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C,E]), 'Programs':'~Programs.Compute.P7.0'(A,B,C,D,E,F,G)).
'Programs.Compute.P7.0'(A, B, C, D, E, F, G) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, H),
        'SharedPrograms':'SharedPrograms.ProgramLanguage.P2'(H, I),
        'Syntax':'Syntax.ApplySubstToFormula.P3'(B, E, J),
        'SharedPrograms':'SharedPrograms.MeltedBodyTyping.P8'(J, I, [], _, _, [], [], []),
        'Programs.CompileObjectProgram.P1'(H),
        'Programs.RunGoal.P6'(J, C, E, K, D, L),
        'Programs.ConstraintsToBindings.P4'(L, G, K, F).
'~Programs.Compute.P7.0'(A, B, C, D, E, F, G) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, H),
        'SharedPrograms':'~SharedPrograms.ProgramLanguage.P2'(H, I),
        'Syntax':'~Syntax.ApplySubstToFormula.P3'(B, E, J),
        'SharedPrograms':'~SharedPrograms.MeltedBodyTyping.P8'(J, I, [], _, _, [], [], []),
        '~Programs.CompileObjectProgram.P1'(H),
        '~Programs.RunGoal.P6'(J, C, E, K, D, L),
        '~Programs.ConstraintsToBindings.P4'(L, G, K, F).
'Programs.BaseInModule.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.BaseInModule.P5.0'(A,B,C,D,E)).
'~Programs.BaseInModule.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.BaseInModule.P5.0'(A,B,C,D,E)).
'Programs.BaseInModule.P5.0'(A, B, C, D, E) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, F),
        'Programs.CModulePart.P2'(C, G),
        'SharedPrograms':'SharedPrograms.ModulePartInProgram.P3'(F, B, G),
        'Programs.ModulePartLanguage.P4'(G, B, F, H),
        'SharedPrograms':'SharedPrograms.BaseInLanguage.P2'(D, H),
        D='MetaDefs.Name.F4'(E,_,_,_).
'~Programs.BaseInModule.P5.0'(A, B, C, D, E) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, F),
        '~Programs.CModulePart.P2'(C, G),
        'SharedPrograms':'~SharedPrograms.ModulePartInProgram.P3'(F, B, G),
        '~Programs.ModulePartLanguage.P4'(G, B, F, H),
        'SharedPrograms':'~SharedPrograms.BaseInLanguage.P2'(D, H),
        D='MetaDefs.Name.F4'(E,_,_,_).
'Programs.AndCondition.P3'(A, B, 'ProgDefs.And.F2'(A,B)).
'~Programs.AndCondition.P3'(A, B, 'ProgDefs.And.F2'(A,B)).
'Programs.ApplicableDelay.P5'([A|B], [C|D], E, F, G) :-
        'Syntax':'Syntax.EmptyTermSubst.P1'(H),
        user:goedel_freeze(ground([E,H,A]), ('Syntax':'~Syntax.UnifyAtoms.P4'(A,E,H,_),true->F=A,G=C;'Programs':'Programs.ApplicableDelay.P5'(B,D,E,F,G))).
'~Programs.ApplicableDelay.P5'([A|B], [C|D], E, F, G) :-
        'Syntax':'~Syntax.EmptyTermSubst.P1'(H),
        user:goedel_freeze(ground([E,H,A]), ('Syntax':'~Syntax.UnifyAtoms.P4'(A,E,H,_),true->F=A,G=C;'Programs':'~Programs.ApplicableDelay.P5'(B,D,E,F,G))).
'Programs.CompileObjectCode.P1'('AVLTrees.Empty.C0').
'~Programs.CompileObjectCode.P1'('AVLTrees.Empty.C0').
'Programs.CompileObjectCode.P1'('AVLTrees.Tree.F4'(A,B,'ProgDefs.Code.F2'(C,D),E)) :-
        user:goedel_freeze(ground([B]), ('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B),true->'Programs':'Programs.EnsureLoaded.P1'(B);'Programs':'Programs.EnsureCompiled.P3'(B,C,D))),
        'Programs.CompileObjectCode.P1'(A),
        'Programs.CompileObjectCode.P1'(E).
'~Programs.CompileObjectCode.P1'('AVLTrees.Tree.F4'(A,B,'ProgDefs.Code.F2'(C,D),E)) :-
        user:goedel_freeze(ground([B]), ('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B),true->'Programs':'~Programs.EnsureLoaded.P1'(B);'Programs':'~Programs.EnsureCompiled.P3'(B,C,D))),
        '~Programs.CompileObjectCode.P1'(A),
        '~Programs.CompileObjectCode.P1'(E).
'Programs.CModulePart.P2'(A, B) :-
        user:goedel_freeze(nonvar(A)or nonvar(B), 'Programs':'Programs.CModulePart.P2.0'(A,B)).
'~Programs.CModulePart.P2'(A, B) :-
        user:goedel_freeze(nonvar(A)or nonvar(B), 'Programs':'~Programs.CModulePart.P2.0'(A,B)).
'Programs.CModulePart.P2.0'('Programs.Export.C0', 'ProgDefs.Export.C0') :- !.
'~Programs.CModulePart.P2.0'('Programs.Export.C0', 'ProgDefs.Export.C0').
'Programs.CModulePart.P2.0'('Programs.Local.C0', 'ProgDefs.Local.C0') :- !.
'~Programs.CModulePart.P2.0'('Programs.Local.C0', 'ProgDefs.Local.C0').
'Programs.CModulePart.P2.0'('Programs.Module.C0', 'ProgDefs.Module.C0') :- !.
'~Programs.CModulePart.P2.0'('Programs.Module.C0', 'ProgDefs.Module.C0').
'Programs.CModulePart.P2.0'('Programs.Closed.C0', 'ProgDefs.Closed.C0') :- !.
'~Programs.CModulePart.P2.0'('Programs.Closed.C0', 'ProgDefs.Closed.C0').
'Programs.CompileObjectProgram.P1'('ProgDefs.Program.F4'(_,_,_,A)) :-
        'AVLTrees':'AVLTrees.AVLToBinary.P2'(A, B),
        'Programs.CompileObjectCode.P1'(B).
'~Programs.CompileObjectProgram.P1'('ProgDefs.Program.F4'(_,_,_,A)) :-
        'AVLTrees':'~AVLTrees.AVLToBinary.P2'(A, B),
        '~Programs.CompileObjectCode.P1'(B).
'Programs.ComposeAll.P3'([], [], []).
'~Programs.ComposeAll.P3'([], [], []).
'Programs.ComposeAll.P3'([A|B], [C|D], [E|F]) :-
        'Syntax':'Syntax.ComposeTermSubsts.P3'(A, C, E),
        'Programs.ComposeAll.P3'(B, D, F).
'~Programs.ComposeAll.P3'([A|B], [C|D], [E|F]) :-
        'Syntax':'~Syntax.ComposeTermSubsts.P3'(A, C, E),
        '~Programs.ComposeAll.P3'(B, D, F).
'Programs.ConstructorInModule.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.ConstructorInModule.P6.0'(A,B,C,D,E,F)).
'~Programs.ConstructorInModule.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.ConstructorInModule.P6.0'(A,B,C,D,E,F)).
'Programs.ConstructorInModule.P6.0'(A, B, C, D, E, F) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, G),
        'Programs.CModulePart.P2'(C, H),
        'SharedPrograms':'SharedPrograms.ModulePartInProgram.P3'(G, B, H),
        'Programs.ModulePartLanguage.P4'(H, B, G, I),
        'SharedPrograms':'SharedPrograms.ConstructorInLanguage.P3'(D, I, E),
        D='MetaDefs.Name.F4'(F,_,_,_).
'~Programs.ConstructorInModule.P6.0'(A, B, C, D, E, F) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, G),
        '~Programs.CModulePart.P2'(C, H),
        'SharedPrograms':'~SharedPrograms.ModulePartInProgram.P3'(G, B, H),
        '~Programs.ModulePartLanguage.P4'(H, B, G, I),
        'SharedPrograms':'~SharedPrograms.ConstructorInLanguage.P3'(D, I, E),
        D='MetaDefs.Name.F4'(F,_,_,_).
'Programs.ConditionToString.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.ConditionToString.P2.0'(A,B)).
'~Programs.ConditionToString.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.ConditionToString.P2.0'(A,B)).
'Programs.ConditionToString.P2.0'(A, B) :-
        'SharedPrograms':'SharedPrograms.SConditionToString.P2'(A, B).
'~Programs.ConditionToString.P2.0'(A, B) :-
        'SharedPrograms':'~SharedPrograms.SConditionToString.P2'(A, B).
'Programs.ComputeAll.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C,E]), 'Programs':'Programs.ComputeAll.P7.0'(A,B,C,D,E,F,G)).
'~Programs.ComputeAll.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C,E]), 'Programs':'~Programs.ComputeAll.P7.0'(A,B,C,D,E,F,G)).
'Programs.ComputeAll.P7.0'(A, B, C, D, E, F, G) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, H),
        'SharedPrograms':'SharedPrograms.ProgramLanguage.P2'(H, I),
        'Syntax':'Syntax.ApplySubstToFormula.P3'(B, E, J),
        'SharedPrograms':'SharedPrograms.MeltedBodyTyping.P8'(J, I, [], _, _, [], [], []),
        'Programs.CompileObjectProgram.P1'(H),
        'Programs.RunAllGoal.P6'(J, C, E, K, D, L),
        'Programs.ConstraintsToBindingsAll.P4'(L, G, K, F).
'~Programs.ComputeAll.P7.0'(A, B, C, D, E, F, G) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, H),
        'SharedPrograms':'~SharedPrograms.ProgramLanguage.P2'(H, I),
        'Syntax':'~Syntax.ApplySubstToFormula.P3'(B, E, J),
        'SharedPrograms':'~SharedPrograms.MeltedBodyTyping.P8'(J, I, [], _, _, [], [], []),
        '~Programs.CompileObjectProgram.P1'(H),
        '~Programs.RunAllGoal.P6'(J, C, E, K, D, L),
        '~Programs.ConstraintsToBindingsAll.P4'(L, G, K, F).
'Programs.ConditionSatisfied.P2'('ProgDefs.Nonvar.F1'(A), B) :-
        'Syntax':'Syntax.BindingInTermSubst.P3'(B, A, C),
        'Syntax':'Syntax.NonVariable.P1'(C).
'~Programs.ConditionSatisfied.P2'('ProgDefs.Nonvar.F1'(A), B) :-
        'Syntax':'~Syntax.BindingInTermSubst.P3'(B, A, C),
        'Syntax':'~Syntax.NonVariable.P1'(C).
'Programs.ConditionSatisfied.P2'('ProgDefs.And.F2'(A,B), C) :-
        'Programs.ConditionSatisfied.P2'(A, C),
        'Programs.ConditionSatisfied.P2'(B, C).
'~Programs.ConditionSatisfied.P2'('ProgDefs.And.F2'(A,B), C) :-
        '~Programs.ConditionSatisfied.P2'(A, C),
        '~Programs.ConditionSatisfied.P2'(B, C).
'Programs.ConditionSatisfied.P2'('ProgDefs.Ground.F1'(A), B) :-
        'Syntax':'Syntax.BindingInTermSubst.P3'(B, A, C),
        'Syntax':'Syntax.GroundTerm.P1'(C).
'~Programs.ConditionSatisfied.P2'('ProgDefs.Ground.F1'(A), B) :-
        'Syntax':'~Syntax.BindingInTermSubst.P3'(B, A, C),
        'Syntax':'~Syntax.GroundTerm.P1'(C).
'Programs.ConditionSatisfied.P2'('ProgDefs.TrueCond.C0', _).
'~Programs.ConditionSatisfied.P2'('ProgDefs.TrueCond.C0', _).
'Programs.ConditionSatisfied.P2'('ProgDefs.Or.F2'(A,B), C) :-
        user:one_solution(('Programs':'Programs.ConditionSatisfied.P2'(A,C);'Programs':'Programs.ConditionSatisfied.P2'(B,C))).
'~Programs.ConditionSatisfied.P2'('ProgDefs.Or.F2'(A,B), C) :-
        (   '~Programs.ConditionSatisfied.P2'(A, C)
        ;   '~Programs.ConditionSatisfied.P2'(B, C)
        ).
'Programs.ConstraintsToBindings.P4'('MetaDefs.Empty.C0', 'MetaDefs.Empty.C0', A, A).
'~Programs.ConstraintsToBindings.P4'('MetaDefs.Empty.C0', 'MetaDefs.Empty.C0', A, A).
'Programs.ConstraintsToBindings.P4'('MetaDefs.&''.F2'(A,B), C, D, E) :-
        'Programs.ConstraintsToBindings.P4'(A, F, D, G),
        'Programs.ConstraintsToBindings.P4'(B, H, G, E),
        'Syntax':'Syntax.AndWithEmpty.P3'(F, H, C).
'~Programs.ConstraintsToBindings.P4'('MetaDefs.&''.F2'(A,B), C, D, E) :-
        '~Programs.ConstraintsToBindings.P4'(A, F, D, G),
        '~Programs.ConstraintsToBindings.P4'(B, H, G, E),
        'Syntax':'~Syntax.AndWithEmpty.P3'(F, H, C).
'Programs.ConstraintsToBindings.P4'('MetaDefs.Atom.F2'(A,B), C, D, E) :-
        user:one_solution(user:goedel_freeze(ground([B,A]),if(((A='MetaDefs.Name.F4'('"','"=',_,2),B=[F,G],'Syntax':'~Syntax.Variable.P1'(F)),true),('Substs':'Substs.BindVariable.P4'(F,G,D,E),C='MetaDefs.Empty.C0'),(E=D,C='MetaDefs.Atom.F2'(A,B))))).
'~Programs.ConstraintsToBindings.P4'('MetaDefs.Atom.F2'(A,B), C, D, E) :-
        user:goedel_freeze(ground([B,A]), if(((A='MetaDefs.Name.F4'('"','"=',_,2),B=[F,G],'Syntax':'~Syntax.Variable.P1'(F)),true),('Substs':'~Substs.BindVariable.P4'(F,G,D,E),C='MetaDefs.Empty.C0'),(E=D,C='MetaDefs.Atom.F2'(A,B)))).
'Programs.ConstantInModule.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.ConstantInModule.P6.0'(A,B,C,D,E,F)).
'~Programs.ConstantInModule.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.ConstantInModule.P6.0'(A,B,C,D,E,F)).
'Programs.ConstantInModule.P6.0'(A, B, C, D, E, F) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, G),
        'Programs.CModulePart.P2'(C, H),
        'SharedPrograms':'SharedPrograms.ModulePartInProgram.P3'(G, B, H),
        'Programs.ModulePartLanguage.P4'(H, B, G, I),
        'SharedPrograms':'SharedPrograms.ConstantInLanguage.P3'(D, I, E),
        D='MetaDefs.Name.F4'(F,_,_,_).
'~Programs.ConstantInModule.P6.0'(A, B, C, D, E, F) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, G),
        '~Programs.CModulePart.P2'(C, H),
        'SharedPrograms':'~SharedPrograms.ModulePartInProgram.P3'(G, B, H),
        '~Programs.ModulePartLanguage.P4'(H, B, G, I),
        'SharedPrograms':'~SharedPrograms.ConstantInLanguage.P3'(D, I, E),
        D='MetaDefs.Name.F4'(F,_,_,_).
'Programs.ConstraintsToBindingsAll.P4'([], [], [], []).
'~Programs.ConstraintsToBindingsAll.P4'([], [], [], []).
'Programs.ConstraintsToBindingsAll.P4'([A|B], [C|D], [E|F], [G|H]) :-
        'Programs.ConstraintsToBindings.P4'(A, C, E, G),
        'Programs.ConstraintsToBindingsAll.P4'(B, D, F, H).
'~Programs.ConstraintsToBindingsAll.P4'([A|B], [C|D], [E|F], [G|H]) :-
        '~Programs.ConstraintsToBindings.P4'(A, C, E, G),
        '~Programs.ConstraintsToBindingsAll.P4'(B, D, F, H).
'Programs.ControlInProgram.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.ControlInProgram.P5.0'(A,B,C,D,E)).
'~Programs.ControlInProgram.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.ControlInProgram.P5.0'(A,B,C,D,E)).
'Programs.ControlInProgram.P5.0'(A, B, C, D, E) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, F),
        'SharedPrograms':'SharedPrograms.SControlInProgram.P5'(F, B, C, D, E).
'~Programs.ControlInProgram.P5.0'(A, B, C, D, E) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, F),
        'SharedPrograms':'~SharedPrograms.SControlInProgram.P5'(F, B, C, D, E).
'Programs.DeclaredInClosedModule.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,C]), 'Programs':'Programs.DeclaredInClosedModule.P3.0'(A,B,C)).
'~Programs.DeclaredInClosedModule.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,C]), 'Programs':'~Programs.DeclaredInClosedModule.P3.0'(A,B,C)).
'Programs.DeclaredInClosedModule.P3.0'('ProgDefs.CachedProgram.F1'(A), B, C) :-
        'ProgramCache':'ProgramCache.GetCachedProgram.P2'(A, D),
        'Programs.DeclaredInClosedModule.P3'(D, B, C).
'~Programs.DeclaredInClosedModule.P3.0'('ProgDefs.CachedProgram.F1'(A), B, C) :-
        'ProgramCache':'~ProgramCache.GetCachedProgram.P2'(A, D),
        '~Programs.DeclaredInClosedModule.P3'(D, B, C).
'Programs.DeclaredInClosedModule.P3.0'('ProgDefs.Program.F4'(_,A,B,_), C, D) :-
        'Syntax':'Syntax.Atom.P1'(D),
        'Syntax':'Syntax.EmptyVarTyping.P1'(E),
        'SharedPrograms':'SharedPrograms.FormulaInLanguage.P4'(D, B, E, _),
        'Programs.TopLevelName.P2'(D, 'MetaDefs.Name.F4'(C,_,_,_)),
        user:goedel_freeze(ground([C]), (user:not_equal([],[C],C,'"'),true->'AVLTrees':'AVLTrees.AVLSearch.P3'(A,C,'ProgDefs.ModDef.F4'('ProgDefs.ClosedKind.C0',_,_,_));true)).
'~Programs.DeclaredInClosedModule.P3.0'('ProgDefs.Program.F4'(_,A,B,_), C, D) :-
        'Syntax':'~Syntax.Atom.P1'(D),
        'Syntax':'~Syntax.EmptyVarTyping.P1'(E),
        'SharedPrograms':'~SharedPrograms.FormulaInLanguage.P4'(D, B, E, _),
        '~Programs.TopLevelName.P2'(D, 'MetaDefs.Name.F4'(C,_,_,_)),
        user:goedel_freeze(ground([C]), (user:not_equal([],[C],C,'"'),true->'AVLTrees':'~AVLTrees.AVLSearch.P3'(A,C,'ProgDefs.ModDef.F4'('ProgDefs.ClosedKind.C0',_,_,_));true)).
'Programs.DeleteProgramConstant.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.DeleteProgramConstant.P6.0'(A,B,C,D,E,F)).
'~Programs.DeleteProgramConstant.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.DeleteProgramConstant.P6.0'(A,B,C,D,E,F)).
'Programs.DeleteProgramConstant.P6.0'(A, B, C, D, E, F) :-
        'Programs.DelDeclaration.P6'(F, B, C, D, 'ProgDefs.ConstantDecl.F1'(E), A).
'~Programs.DeleteProgramConstant.P6.0'(A, B, C, D, E, F) :-
        '~Programs.DelDeclaration.P6'(F, B, C, D, 'ProgDefs.ConstantDecl.F1'(E), A).
'Programs.DelayInModule.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.DelayInModule.P5.0'(A,B,C,D,E)).
'~Programs.DelayInModule.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.DelayInModule.P5.0'(A,B,C,D,E)).
'Programs.DelayInModule.P5.0'('ProgDefs.CachedProgram.F1'(A), B, C, D, E) :-
        'ProgramCache':'ProgramCache.GetCachedProgram.P2'(A, F),
        'Programs.DelayInModule.P5'(F, B, C, D, E).
'~Programs.DelayInModule.P5.0'('ProgDefs.CachedProgram.F1'(A), B, C, D, E) :-
        'ProgramCache':'~ProgramCache.GetCachedProgram.P2'(A, F),
        '~Programs.DelayInModule.P5'(F, B, C, D, E).
'Programs.DelayInModule.P5.0'('ProgDefs.Program.F4'(_,A,_,B), C, D, E, F) :-
        'Programs.TopLevelName.P2'(E, 'MetaDefs.Name.F4'(C,G,_,H)),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(B, C, 'ProgDefs.Code.F2'(_,I)),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(I, G, J),
        'SharedPrograms':'SharedPrograms.FindPredDef.P3'(H, J, 'ProgDefs.PredDef.F4'(H,_,K,L)),
        'AVLTrees':'AVLTrees.AVLSearch.P3'(A, C, 'ProgDefs.ModDef.F4'(M,_,_,_)),
        'Programs.DelayInKind.P5'(M, D, K, L, 'ProgDefs.Delay.F2'(E,F)).
'~Programs.DelayInModule.P5.0'('ProgDefs.Program.F4'(_,A,_,B), C, D, E, F) :-
        '~Programs.TopLevelName.P2'(E, 'MetaDefs.Name.F4'(C,G,_,H)),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(B, C, 'ProgDefs.Code.F2'(_,I)),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(I, G, J),
        'SharedPrograms':'~SharedPrograms.FindPredDef.P3'(H, J, 'ProgDefs.PredDef.F4'(H,_,K,L)),
        'AVLTrees':'~AVLTrees.AVLSearch.P3'(A, C, 'ProgDefs.ModDef.F4'(M,_,_,_)),
        '~Programs.DelayInKind.P5'(M, D, K, L, 'ProgDefs.Delay.F2'(E,F)).
'Programs.DelTypeDeclaration.P6'(A, B, C, D, E, F) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(F, G),
        'Programs.CModulePart.P2'(C, H),
        'SharedPrograms':'SharedPrograms.ModulePartInProgram.P3'(G, B, H),
        user:goedel_freeze(ground([B]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B))),
        D='MetaDefs.Name.F4'(B,_,_,_),
        'Programs.PartAccessibility.P2'(C, I),
        G='ProgDefs.Program.F4'(J,K,L,M),
        'Programs.DeleteTypeSymbol.P5'(D, L, I, E, N),
        'Programs.FreeTypeSymbol.P4'(B, C, D, G),
        A='ProgDefs.Program.F4'(J,K,N,M).
'~Programs.DelTypeDeclaration.P6'(A, B, C, D, E, F) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(F, G),
        '~Programs.CModulePart.P2'(C, H),
        'SharedPrograms':'~SharedPrograms.ModulePartInProgram.P3'(G, B, H),
        user:goedel_freeze(ground([B]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B))),
        D='MetaDefs.Name.F4'(B,_,_,_),
        '~Programs.PartAccessibility.P2'(C, I),
        G='ProgDefs.Program.F4'(J,K,L,M),
        '~Programs.DeleteTypeSymbol.P5'(D, L, I, E, N),
        '~Programs.FreeTypeSymbol.P4'(B, C, D, G),
        A='ProgDefs.Program.F4'(J,K,N,M).
'Programs.DefinitionInProgram.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.DefinitionInProgram.P4.0'(A,B,C,D)).
'~Programs.DefinitionInProgram.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.DefinitionInProgram.P4.0'(A,B,C,D)).
'Programs.DefinitionInProgram.P4.0'('ProgDefs.CachedProgram.F1'(A), B, C, D) :-
        'ProgramCache':'ProgramCache.GetCachedProgram.P2'(A, E),
        'Programs.DefinitionInProgram.P4'(E, B, C, D).
'~Programs.DefinitionInProgram.P4.0'('ProgDefs.CachedProgram.F1'(A), B, C, D) :-
        'ProgramCache':'~ProgramCache.GetCachedProgram.P2'(A, E),
        '~Programs.DefinitionInProgram.P4'(E, B, C, D).
'Programs.DefinitionInProgram.P4.0'('ProgDefs.Program.F4'(_,A,B,C), D, E, F) :-
        E='MetaDefs.Name.F4'(D,G,H,I),
        'SharedPrograms':'SharedPrograms.OpenModuleAux.P2'(A, D),
        'Programs.PredOrPropInLanguage.P3'(H, E, B),
        'AVLTrees':'AVLTrees.AVLSearch.P3'(C, D, 'ProgDefs.Code.F2'(_,J)),
        user:goedel_freeze(ground([I,J,G]), if((('AVLTrees':'~AVLTrees.AVLSearch.P3'(J,G,K),'SharedPrograms':'~SharedPrograms.FindPredDef.P3'(I,K,'ProgDefs.PredDef.F4'(I,L,_,_))),true),F=L,F=[])).
'~Programs.DefinitionInProgram.P4.0'('ProgDefs.Program.F4'(_,A,B,C), D, E, F) :-
        E='MetaDefs.Name.F4'(D,G,H,I),
        'SharedPrograms':'~SharedPrograms.OpenModuleAux.P2'(A, D),
        '~Programs.PredOrPropInLanguage.P3'(H, E, B),
        'AVLTrees':'~AVLTrees.AVLSearch.P3'(C, D, 'ProgDefs.Code.F2'(_,J)),
        user:goedel_freeze(ground([I,J,G]), if((('AVLTrees':'~AVLTrees.AVLSearch.P3'(J,G,K),'SharedPrograms':'~SharedPrograms.FindPredDef.P3'(I,K,'ProgDefs.PredDef.F4'(I,L,_,_))),true),F=L,F=[])).
'Programs.DelDeclaration.P6'(A, B, C, D, E, F) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(F, G),
        'Programs.CModulePart.P2'(C, H),
        'SharedPrograms':'SharedPrograms.ModulePartInProgram.P3'(G, B, H),
        user:goedel_freeze(ground([B]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B))),
        D='MetaDefs.Name.F4'(B,_,_,_),
        'Programs.PartAccessibility.P2'(C, I),
        G='ProgDefs.Program.F4'(J,K,L,M),
        'Programs.DeleteSymbol.P5'(D, L, I, E, N),
        'Programs.FreeSymbol.P4'(B, C, D, G),
        A='ProgDefs.Program.F4'(J,K,N,M).
'~Programs.DelDeclaration.P6'(A, B, C, D, E, F) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(F, G),
        '~Programs.CModulePart.P2'(C, H),
        'SharedPrograms':'~SharedPrograms.ModulePartInProgram.P3'(G, B, H),
        user:goedel_freeze(ground([B]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B))),
        D='MetaDefs.Name.F4'(B,_,_,_),
        '~Programs.PartAccessibility.P2'(C, I),
        G='ProgDefs.Program.F4'(J,K,L,M),
        '~Programs.DeleteSymbol.P5'(D, L, I, E, N),
        '~Programs.FreeSymbol.P4'(B, C, D, G),
        A='ProgDefs.Program.F4'(J,K,N,M).
'Programs.DelayInKind.P5'('ProgDefs.NormalKind.C0', A, B, C, D) :-
        'Programs.DelayInPart.P4'(A, B, C, D).
'~Programs.DelayInKind.P5'('ProgDefs.NormalKind.C0', A, B, C, D) :-
        '~Programs.DelayInPart.P4'(A, B, C, D).
'Programs.DelayInKind.P5'('ProgDefs.ClosedKind.C0', 'Programs.Closed.C0', A, _, B) :-
        'Lists':'Lists.Member.P2'(B, A).
'~Programs.DelayInKind.P5'('ProgDefs.ClosedKind.C0', 'Programs.Closed.C0', A, _, B) :-
        'Lists':'~Lists.Member.P2'(B, A).
'Programs.DelayInKind.P5'('ProgDefs.ModuleKind.C0', 'Programs.Module.C0', _, A, B) :-
        'Lists':'Lists.Member.P2'(B, A).
'~Programs.DelayInKind.P5'('ProgDefs.ModuleKind.C0', 'Programs.Module.C0', _, A, B) :-
        'Lists':'~Lists.Member.P2'(B, A).
'Programs.DeleteDelay.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.DeleteDelay.P6.0'(A,B,C,D,E,F)).
'~Programs.DeleteDelay.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.DeleteDelay.P6.0'(A,B,C,D,E,F)).
'Programs.DeleteDelay.P6.0'(A, B, C, D, E, 'ProgDefs.Program.F4'(F,G,H,I)) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, J),
        'Programs.CModulePart.P2'(C, K),
        'SharedPrograms':'SharedPrograms.ModulePartInProgram.P3'(J, B, K),
        user:goedel_freeze(ground([B]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B))),
        'SharedPrograms':'SharedPrograms.NextModuleVersion.P2'(B, L),
        J='ProgDefs.Program.F4'(F,G,H,M),
        'AVLTrees':'AVLTrees.AVLUpdate.P5'(M, B, 'ProgDefs.Code.F2'(L,N), I, 'ProgDefs.Code.F2'(_,O)),
        D='MetaDefs.Atom.F2'('MetaDefs.Name.F4'(B,P,'MetaDefs.Predicate.C0',Q),_),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(O, P, R),
        'SharedPrograms':'SharedPrograms.PickPredDef.P4'(Q, R, 'ProgDefs.PredDef.F4'(Q,S,T,U), V),
        'Programs.DeleteDelayFromPart.P6'(C, 'ProgDefs.Delay.F2'(D,E), T, U, W, X),
        user:goedel_freeze(ground([W,X,S]), ((S=[],W=[],X=[]),true->Y=V;Y=['ProgDefs.PredDef.F4'(Q,S,W,X)|V])),
        user:goedel_freeze(ground([Y]), (Y=[],true->'AVLTrees':'AVLTrees.AVLDelete.P3'(O,P,N);'AVLTrees':'AVLTrees.AVLUpdate.P5'(O,P,Y,N,_))).
'~Programs.DeleteDelay.P6.0'(A, B, C, D, E, 'ProgDefs.Program.F4'(F,G,H,I)) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, J),
        '~Programs.CModulePart.P2'(C, K),
        'SharedPrograms':'~SharedPrograms.ModulePartInProgram.P3'(J, B, K),
        user:goedel_freeze(ground([B]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B))),
        'SharedPrograms':'~SharedPrograms.NextModuleVersion.P2'(B, L),
        J='ProgDefs.Program.F4'(F,G,H,M),
        'AVLTrees':'~AVLTrees.AVLUpdate.P5'(M, B, 'ProgDefs.Code.F2'(L,N), I, 'ProgDefs.Code.F2'(_,O)),
        D='MetaDefs.Atom.F2'('MetaDefs.Name.F4'(B,P,'MetaDefs.Predicate.C0',Q),_),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(O, P, R),
        'SharedPrograms':'~SharedPrograms.PickPredDef.P4'(Q, R, 'ProgDefs.PredDef.F4'(Q,S,T,U), V),
        '~Programs.DeleteDelayFromPart.P6'(C, 'ProgDefs.Delay.F2'(D,E), T, U, W, X),
        user:goedel_freeze(ground([W,X,S]), ((S=[],W=[],X=[]),true->Y=V;Y=['ProgDefs.PredDef.F4'(Q,S,W,X)|V])),
        user:goedel_freeze(ground([Y]), (Y=[],true->'AVLTrees':'~AVLTrees.AVLDelete.P3'(O,P,N);'AVLTrees':'~AVLTrees.AVLUpdate.P5'(O,P,Y,N,_))).
'Programs.DelayInPart.P4'('Programs.Export.C0', A, _, B) :-
        'Lists':'Lists.Member.P2'(B, A).
'~Programs.DelayInPart.P4'('Programs.Export.C0', A, _, B) :-
        'Lists':'~Lists.Member.P2'(B, A).
'Programs.DelayInPart.P4'('Programs.Local.C0', _, A, B) :-
        'Lists':'Lists.Member.P2'(B, A).
'~Programs.DelayInPart.P4'('Programs.Local.C0', _, A, B) :-
        'Lists':'~Lists.Member.P2'(B, A).
'Programs.DeleteImportFromPart.P6'('Programs.Export.C0', A, B, C, D, C) :-
        'SharedPrograms':'SharedPrograms.PickOne.P3'(A, B, D).
'~Programs.DeleteImportFromPart.P6'('Programs.Export.C0', A, B, C, D, C) :-
        'SharedPrograms':'~SharedPrograms.PickOne.P3'(A, B, D).
'Programs.DeleteImportFromPart.P6'('Programs.Local.C0', A, B, C, B, D) :-
        'SharedPrograms':'SharedPrograms.PickOne.P3'(A, C, D).
'~Programs.DeleteImportFromPart.P6'('Programs.Local.C0', A, B, C, B, D) :-
        'SharedPrograms':'~SharedPrograms.PickOne.P3'(A, C, D).
'Programs.DeleteImportFromPart.P6'('Programs.Module.C0', A, B, C, B, D) :-
        'SharedPrograms':'SharedPrograms.PickOne.P3'(A, C, D).
'~Programs.DeleteImportFromPart.P6'('Programs.Module.C0', A, B, C, B, D) :-
        'SharedPrograms':'~SharedPrograms.PickOne.P3'(A, C, D).
'Programs.DeleteDelayFromPart.P6'('Programs.Export.C0', A, B, C, D, C) :-
        'SharedPrograms':'SharedPrograms.PickOne.P3'(A, B, D).
'~Programs.DeleteDelayFromPart.P6'('Programs.Export.C0', A, B, C, D, C) :-
        'SharedPrograms':'~SharedPrograms.PickOne.P3'(A, B, D).
'Programs.DeleteDelayFromPart.P6'('Programs.Local.C0', A, B, C, B, D) :-
        'SharedPrograms':'SharedPrograms.PickOne.P3'(A, C, D).
'~Programs.DeleteDelayFromPart.P6'('Programs.Local.C0', A, B, C, B, D) :-
        'SharedPrograms':'~SharedPrograms.PickOne.P3'(A, C, D).
'Programs.DeleteDelayFromPart.P6'('Programs.Module.C0', A, B, C, B, D) :-
        'SharedPrograms':'SharedPrograms.PickOne.P3'(A, C, D).
'~Programs.DeleteDelayFromPart.P6'('Programs.Module.C0', A, B, C, B, D) :-
        'SharedPrograms':'~SharedPrograms.PickOne.P3'(A, C, D).
'Programs.DeleteProgramBase.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.DeleteProgramBase.P5.0'(A,B,C,D,E)).
'~Programs.DeleteProgramBase.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.DeleteProgramBase.P5.0'(A,B,C,D,E)).
'Programs.DeleteProgramBase.P5.0'(A, B, C, D, E) :-
        'Programs.DelTypeDeclaration.P6'(E, B, C, D, 'ProgDefs.BaseDecl.C0', A).
'~Programs.DeleteProgramBase.P5.0'(A, B, C, D, E) :-
        '~Programs.DelTypeDeclaration.P6'(E, B, C, D, 'ProgDefs.BaseDecl.C0', A).
'Programs.DeleteProgramPredicate.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.DeleteProgramPredicate.P7.0'(A,B,C,D,E,F,G)).
'~Programs.DeleteProgramPredicate.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.DeleteProgramPredicate.P7.0'(A,B,C,D,E,F,G)).
'Programs.DeleteProgramPredicate.P7.0'(A, B, C, D, E, F, G) :-
        'Programs.DelDeclaration.P6'(G, B, C, D, 'ProgDefs.PredicateDecl.F3'(_,E,F), A).
'~Programs.DeleteProgramPredicate.P7.0'(A, B, C, D, E, F, G) :-
        '~Programs.DelDeclaration.P6'(G, B, C, D, 'ProgDefs.PredicateDecl.F3'(_,E,F), A).
'Programs.DeleteProgramFunction.P8'(A, B, C, D, E, F, G, H) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.DeleteProgramFunction.P8.0'(A,B,C,D,E,F,G,H)).
'~Programs.DeleteProgramFunction.P8'(A, B, C, D, E, F, G, H) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.DeleteProgramFunction.P8.0'(A,B,C,D,E,F,G,H)).
'Programs.DeleteProgramFunction.P8.0'(A, B, C, D, E, F, G, H) :-
        'Programs.DelDeclaration.P6'(H, B, C, D, 'ProgDefs.FunctionDecl.F4'(_,E,F,G), A).
'~Programs.DeleteProgramFunction.P8.0'(A, B, C, D, E, F, G, H) :-
        '~Programs.DelDeclaration.P6'(H, B, C, D, 'ProgDefs.FunctionDecl.F4'(_,E,F,G), A).
'Programs.DeleteProgramConstructor.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.DeleteProgramConstructor.P6.0'(A,B,C,D,E,F)).
'~Programs.DeleteProgramConstructor.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.DeleteProgramConstructor.P6.0'(A,B,C,D,E,F)).
'Programs.DeleteProgramConstructor.P6.0'(A, B, C, D, E, F) :-
        'Programs.DelTypeDeclaration.P6'(F, B, C, D, 'ProgDefs.ConstructorDecl.F1'(E), A).
'~Programs.DeleteProgramConstructor.P6.0'(A, B, C, D, E, F) :-
        '~Programs.DelTypeDeclaration.P6'(F, B, C, D, 'ProgDefs.ConstructorDecl.F1'(E), A).
'Programs.DeleteProgramImport.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.DeleteProgramImport.P5.0'(A,B,C,D,E)).
'~Programs.DeleteProgramImport.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.DeleteProgramImport.P5.0'(A,B,C,D,E)).
'Programs.DeleteProgramImport.P5.0'('ProgDefs.CachedProgram.F1'(A), B, C, D, E) :-
        'ProgramCache':'ProgramCache.GetCachedProgram.P2'(A, E),
        'Programs.DeleteProgramImport.P5'(E, B, C, D, E).
'~Programs.DeleteProgramImport.P5.0'('ProgDefs.CachedProgram.F1'(A), B, C, D, E) :-
        'ProgramCache':'~ProgramCache.GetCachedProgram.P2'(A, E),
        '~Programs.DeleteProgramImport.P5'(E, B, C, D, E).
'Programs.DeleteProgramImport.P5.0'('ProgDefs.Program.F4'(A,B,'ProgDefs.Language.F1'(C),D), E, F, G, 'ProgDefs.Program.F4'(A,H,'ProgDefs.Language.F1'(I),J)) :-
        'Programs.CModulePart.P2'(F, K),
        'AVLTrees':'AVLTrees.AVLUpdate.P5'(B, E, 'ProgDefs.ModDef.F4'(L,M,N,O), P, 'ProgDefs.ModDef.F4'(L,Q,R,O)),
        'SharedPrograms':'SharedPrograms.OpenKind.P1'(L),
        'SharedPrograms':'SharedPrograms.KindHasPart.P2'(L, K),
        'Programs.DeleteImportFromPart.P6'(F, G, Q, R, M, N),
        user:goedel_freeze(ground([G,B]), ('Programs':'~Programs.UnreferencedModule.P2'(G,B),true->'AVLTrees':'AVLTrees.AVLDelete.P3'(P,G,H),'AVLTrees':'AVLTrees.AVLDelete.P3'(C,G,I),'AVLTrees':'AVLTrees.AVLDelete.P3'(D,G,J);H=P,I=C,J=D)).
'~Programs.DeleteProgramImport.P5.0'('ProgDefs.Program.F4'(A,B,'ProgDefs.Language.F1'(C),D), E, F, G, 'ProgDefs.Program.F4'(A,H,'ProgDefs.Language.F1'(I),J)) :-
        '~Programs.CModulePart.P2'(F, K),
        'AVLTrees':'~AVLTrees.AVLUpdate.P5'(B, E, 'ProgDefs.ModDef.F4'(L,M,N,O), P, 'ProgDefs.ModDef.F4'(L,Q,R,O)),
        'SharedPrograms':'~SharedPrograms.OpenKind.P1'(L),
        'SharedPrograms':'~SharedPrograms.KindHasPart.P2'(L, K),
        '~Programs.DeleteImportFromPart.P6'(F, G, Q, R, M, N),
        user:goedel_freeze(ground([G,B]), ('Programs':'~Programs.UnreferencedModule.P2'(G,B),true->'AVLTrees':'~AVLTrees.AVLDelete.P3'(P,G,H),'AVLTrees':'~AVLTrees.AVLDelete.P3'(C,G,I),'AVLTrees':'~AVLTrees.AVLDelete.P3'(D,G,J);H=P,I=C,J=D)).
'Programs.DeleteStatement.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), 'Programs':'Programs.DeleteStatement.P4.0'(A,B,C,D)).
'~Programs.DeleteStatement.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), 'Programs':'~Programs.DeleteStatement.P4.0'(A,B,C,D)).
'Programs.DeleteStatement.P4.0'('ProgDefs.CachedProgram.F1'(A), B, C, D) :-
        'ProgramCache':'ProgramCache.GetCachedProgram.P2'(A, E),
        'Programs.DeleteStatement.P4'(E, B, C, D).
'~Programs.DeleteStatement.P4.0'('ProgDefs.CachedProgram.F1'(A), B, C, D) :-
        'ProgramCache':'~ProgramCache.GetCachedProgram.P2'(A, E),
        '~Programs.DeleteStatement.P4'(E, B, C, D).
'Programs.DeleteStatement.P4.0'('ProgDefs.Program.F4'(A,B,C,D), E, F, 'ProgDefs.Program.F4'(A,B,C,G)) :-
        user:goedel_freeze(ground([E]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(E))),
        'SharedPrograms':'SharedPrograms.NextModuleVersion.P2'(E, H),
        'AVLTrees':'AVLTrees.AVLUpdate.P5'(D, E, 'ProgDefs.Code.F2'(H,I), G, 'ProgDefs.Code.F2'(_,J)),
        'Programs.OpenStatementPredicate.P2'(F, 'MetaDefs.Name.F4'(_,K,_,L)),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(J, K, M),
        'SharedPrograms':'SharedPrograms.PickPredDef.P4'(L, M, 'ProgDefs.PredDef.F4'(L,N,O,P), Q),
        'Lists':'Lists.Delete.P3'(F, N, R),
        user:goedel_freeze(ground([O,P,R]), ((R=[],O=[],P=[]),true->S=Q;S=['ProgDefs.PredDef.F4'(L,R,O,P)|Q])),
        user:goedel_freeze(ground([S]), (S=[],true->'AVLTrees':'AVLTrees.AVLDelete.P3'(J,K,I);'AVLTrees':'AVLTrees.AVLUpdate.P5'(J,K,S,I,_))).
'~Programs.DeleteStatement.P4.0'('ProgDefs.Program.F4'(A,B,C,D), E, F, 'ProgDefs.Program.F4'(A,B,C,G)) :-
        user:goedel_freeze(ground([E]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(E))),
        'SharedPrograms':'~SharedPrograms.NextModuleVersion.P2'(E, H),
        'AVLTrees':'~AVLTrees.AVLUpdate.P5'(D, E, 'ProgDefs.Code.F2'(H,I), G, 'ProgDefs.Code.F2'(_,J)),
        '~Programs.OpenStatementPredicate.P2'(F, 'MetaDefs.Name.F4'(_,K,_,L)),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(J, K, M),
        'SharedPrograms':'~SharedPrograms.PickPredDef.P4'(L, M, 'ProgDefs.PredDef.F4'(L,N,O,P), Q),
        'Lists':'~Lists.Delete.P3'(F, N, R),
        user:goedel_freeze(ground([O,P,R]), ((R=[],O=[],P=[]),true->S=Q;S=['ProgDefs.PredDef.F4'(L,R,O,P)|Q])),
        user:goedel_freeze(ground([S]), (S=[],true->'AVLTrees':'~AVLTrees.AVLDelete.P3'(J,K,I);'AVLTrees':'~AVLTrees.AVLUpdate.P5'(J,K,S,I,_))).
'Programs.DeleteProgramProposition.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.DeleteProgramProposition.P5.0'(A,B,C,D,E)).
'~Programs.DeleteProgramProposition.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.DeleteProgramProposition.P5.0'(A,B,C,D,E)).
'Programs.DeleteProgramProposition.P5.0'(A, B, C, D, E) :-
        'Programs.DelDeclaration.P6'(E, B, C, D, 'ProgDefs.PropositionDecl.C0', A).
'~Programs.DeleteProgramProposition.P5.0'(A, B, C, D, E) :-
        '~Programs.DelDeclaration.P6'(E, B, C, D, 'ProgDefs.PropositionDecl.C0', A).
'Programs.DeleteTypeSymbol.P5'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G, 'ProgDefs.Language.F1'(H)) :-
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(I,'ProgDefs.Categories.F2'(J,K),L)),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(J, B, M),
        'SharedPrograms':'SharedPrograms.FindDescriptor.P5'(M, I, C, D, G),
        'Lists':'Lists.DeleteFirst.P3'('ProgDefs.Symbol.F2'(F,G), M, N),
        user:goedel_freeze(ground([N]), (N=[],true->'AVLTrees':'AVLTrees.AVLDelete.P3'(J,B,O);'AVLTrees':'AVLTrees.AVLUpdate.P5'(J,B,N,O,_))),
        'AVLTrees':'AVLTrees.AVLUpdate.P5'(E, A, 'ProgDefs.Module.F3'(I,'ProgDefs.Categories.F2'(O,K),L), H, _).
'~Programs.DeleteTypeSymbol.P5'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G, 'ProgDefs.Language.F1'(H)) :-
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(I,'ProgDefs.Categories.F2'(J,K),L)),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(J, B, M),
        'SharedPrograms':'~SharedPrograms.FindDescriptor.P5'(M, I, C, D, G),
        'Lists':'~Lists.DeleteFirst.P3'('ProgDefs.Symbol.F2'(F,G), M, N),
        user:goedel_freeze(ground([N]), (N=[],true->'AVLTrees':'~AVLTrees.AVLDelete.P3'(J,B,O);'AVLTrees':'~AVLTrees.AVLUpdate.P5'(J,B,N,O,_))),
        'AVLTrees':'~AVLTrees.AVLUpdate.P5'(E, A, 'ProgDefs.Module.F3'(I,'ProgDefs.Categories.F2'(O,K),L), H, _).
'Programs.DeleteSymbol.P5'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G, 'ProgDefs.Language.F1'(H)) :-
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(I,'ProgDefs.Categories.F2'(J,K),L)),
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(K, B, M),
        'SharedPrograms':'SharedPrograms.FindDescriptor.P5'(M, I, C, D, G),
        'Lists':'Lists.DeleteFirst.P3'('ProgDefs.Symbol.F2'(F,G), M, N),
        user:goedel_freeze(ground([N]), (N=[],true->'AVLTrees':'AVLTrees.AVLDelete.P3'(K,B,O);'AVLTrees':'AVLTrees.AVLUpdate.P5'(K,B,N,O,_))),
        'AVLTrees':'AVLTrees.AVLUpdate.P5'(E, A, 'ProgDefs.Module.F3'(I,'ProgDefs.Categories.F2'(J,O),L), H, _).
'~Programs.DeleteSymbol.P5'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G, 'ProgDefs.Language.F1'(H)) :-
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(I,'ProgDefs.Categories.F2'(J,K),L)),
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(K, B, M),
        'SharedPrograms':'~SharedPrograms.FindDescriptor.P5'(M, I, C, D, G),
        'Lists':'~Lists.DeleteFirst.P3'('ProgDefs.Symbol.F2'(F,G), M, N),
        user:goedel_freeze(ground([N]), (N=[],true->'AVLTrees':'~AVLTrees.AVLDelete.P3'(K,B,O);'AVLTrees':'~AVLTrees.AVLUpdate.P5'(K,B,N,O,_))),
        'AVLTrees':'~AVLTrees.AVLUpdate.P5'(E, A, 'ProgDefs.Module.F3'(I,'ProgDefs.Categories.F2'(J,O),L), H, _).
'Programs.Fail.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Programs':'Programs.Fail.P2.0'(A,B)).
'~Programs.Fail.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Programs':'~Programs.Fail.P2.0'(A,B)).
'Programs.Fail.P2.0'(A, B) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, C),
        'SharedPrograms':'SharedPrograms.ProgramLanguage.P2'(C, D),
        'SharedPrograms':'SharedPrograms.MeltedBodyTyping.P8'(B, D, [], _, _, [], [], []),
        'Programs.CompileObjectProgram.P1'(C),
        'Programs.FailedGoal.P1'(B).
'~Programs.Fail.P2.0'(A, B) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, C),
        'SharedPrograms':'~SharedPrograms.ProgramLanguage.P2'(C, D),
        'SharedPrograms':'~SharedPrograms.MeltedBodyTyping.P8'(B, D, [], _, _, [], [], []),
        '~Programs.CompileObjectProgram.P1'(C),
        '~Programs.FailedGoal.P1'(B).
'Programs.InsertProgramBase.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C,D]), 'Programs':'Programs.InsertProgramBase.P5.0'(A,B,C,D,E)).
'~Programs.InsertProgramBase.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C,D]), 'Programs':'~Programs.InsertProgramBase.P5.0'(A,B,C,D,E)).
'Programs.InsertProgramBase.P5.0'(A, B, C, D, E) :-
        'Programs.InsTypeDeclaration.P6'(A, B, C, D, 'ProgDefs.BaseDecl.C0', E).
'~Programs.InsertProgramBase.P5.0'(A, B, C, D, E) :-
        '~Programs.InsTypeDeclaration.P6'(A, B, C, D, 'ProgDefs.BaseDecl.C0', E).
'Programs.GroundCondition.P2'('MetaDefs.Var.F2'(A,B), 'ProgDefs.Ground.F1'('MetaDefs.Var.F2'(A,B))).
'~Programs.GroundCondition.P2'('MetaDefs.Var.F2'(A,B), 'ProgDefs.Ground.F1'('MetaDefs.Var.F2'(A,B))).
'Programs.GroundCondition.P2'('MetaDefs.Var.F1'(A), 'ProgDefs.Ground.F1'('MetaDefs.Var.F1'(A))).
'~Programs.GroundCondition.P2'('MetaDefs.Var.F1'(A), 'ProgDefs.Ground.F1'('MetaDefs.Var.F1'(A))).
'Programs.FreeTypeSymbol.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([B]), (B='Programs.Export.C0',true->'Programs':'Programs.TypeSymbolFreeInModule.P3'(A,C,D),D='ProgDefs.Program.F4'(_,E,_,_),user:goedel_freeze(ground([A,E,C,D]),user:goedel_not((('AVLTrees':'~AVLTrees.AVLMember.P3'(E,F,G),user:not_equal([],[F,A],F,A),'Programs':'~Programs.ModuleImported.P3'(A,G,E)),user:goedel_freeze(ground([F,C,D]),user:goedel_not('Programs':'~Programs.TypeSymbolFreeInModule.P3'(F,C,D))))));'Programs':'Programs.TypeSymbolFreeInModule.P3'(A,C,D))).
'~Programs.FreeTypeSymbol.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([B]), (B='Programs.Export.C0',true->'Programs':'~Programs.TypeSymbolFreeInModule.P3'(A,C,D),D='ProgDefs.Program.F4'(_,E,_,_),user:goedel_freeze(ground([A,E,C,D]),user:goedel_not((('AVLTrees':'~AVLTrees.AVLMember.P3'(E,F,G),user:not_equal([],[F,A],F,A),'Programs':'~Programs.ModuleImported.P3'(A,G,E)),user:goedel_freeze(ground([F,C,D]),user:goedel_not('Programs':'~Programs.TypeSymbolFreeInModule.P3'(F,C,D))))));'Programs':'~Programs.TypeSymbolFreeInModule.P3'(A,C,D))).
'Programs.FormulaInProgram.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.FormulaInProgram.P4.0'(A,B,C,D)).
'~Programs.FormulaInProgram.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.FormulaInProgram.P4.0'(A,B,C,D)).
'Programs.FormulaInProgram.P4.0'('ProgDefs.CachedProgram.F1'(A), B, C, D) :-
        'ProgramCache':'ProgramCache.GetCachedProgram.P2'(A, E),
        'Programs.FormulaInProgram.P4'(E, B, C, D).
'~Programs.FormulaInProgram.P4.0'('ProgDefs.CachedProgram.F1'(A), B, C, D) :-
        'ProgramCache':'~ProgramCache.GetCachedProgram.P2'(A, E),
        '~Programs.FormulaInProgram.P4'(E, B, C, D).
'Programs.FormulaInProgram.P4.0'('ProgDefs.Program.F4'(_,_,A,_), B, C, D) :-
        'SharedPrograms':'SharedPrograms.FormulaInLanguage.P4'(C, A, B, D).
'~Programs.FormulaInProgram.P4.0'('ProgDefs.Program.F4'(_,_,A,_), B, C, D) :-
        'SharedPrograms':'~SharedPrograms.FormulaInLanguage.P4'(C, A, B, D).
'Programs.FreeSymbol.P4'(A, B, C, D) :-
        'Programs.SymbolFreeInModule.P3'(A, C, D),
        user:goedel_freeze(ground([B]), (B='Programs.Export.C0',true->D='ProgDefs.Program.F4'(_,E,_,_),user:goedel_freeze(ground([A,E,C,D]),user:goedel_not((('AVLTrees':'~AVLTrees.AVLMember.P3'(E,F,G),user:not_equal([],[F,A],F,A),'Programs':'~Programs.ModuleImported.P3'(A,G,E)),user:goedel_freeze(ground([F,C,D]),user:goedel_not('Programs':'~Programs.SymbolFreeInModule.P3'(F,C,D))))));true)).
'~Programs.FreeSymbol.P4'(A, B, C, D) :-
        '~Programs.SymbolFreeInModule.P3'(A, C, D),
        user:goedel_freeze(ground([B]), (B='Programs.Export.C0',true->D='ProgDefs.Program.F4'(_,E,_,_),user:goedel_freeze(ground([A,E,C,D]),user:goedel_not((('AVLTrees':'~AVLTrees.AVLMember.P3'(E,F,G),user:not_equal([],[F,A],F,A),'Programs':'~Programs.ModuleImported.P3'(A,G,E)),user:goedel_freeze(ground([F,C,D]),user:goedel_not('Programs':'~Programs.SymbolFreeInModule.P3'(F,C,D))))));true)).
'Programs.FunctionInModule.P8'(A, B, C, D, E, F, G, H) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.FunctionInModule.P8.0'(A,B,C,D,E,F,G,H)).
'~Programs.FunctionInModule.P8'(A, B, C, D, E, F, G, H) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.FunctionInModule.P8.0'(A,B,C,D,E,F,G,H)).
'Programs.FunctionInModule.P8.0'(A, B, C, D, E, F, G, H) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, I),
        'Programs.CModulePart.P2'(C, J),
        'SharedPrograms':'SharedPrograms.ModulePartInProgram.P3'(I, B, J),
        'Programs.ModulePartLanguage.P4'(J, B, I, K),
        'SharedPrograms':'SharedPrograms.FunctionInLanguage.P5'(D, K, E, F, G),
        D='MetaDefs.Name.F4'(H,_,_,_).
'~Programs.FunctionInModule.P8.0'(A, B, C, D, E, F, G, H) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, I),
        '~Programs.CModulePart.P2'(C, J),
        'SharedPrograms':'~SharedPrograms.ModulePartInProgram.P3'(I, B, J),
        '~Programs.ModulePartLanguage.P4'(J, B, I, K),
        'SharedPrograms':'~SharedPrograms.FunctionInLanguage.P5'(D, K, E, F, G),
        D='MetaDefs.Name.F4'(H,_,_,_).
'Programs.ImportInModule.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.ImportInModule.P4.0'(A,B,C,D)).
'~Programs.ImportInModule.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.ImportInModule.P4.0'(A,B,C,D)).
'Programs.ImportInModule.P4.0'('ProgDefs.CachedProgram.F1'(A), B, C, D) :-
        'ProgramCache':'ProgramCache.GetCachedProgram.P2'(A, E),
        'Programs.ImportInModule.P4'(E, B, C, D).
'~Programs.ImportInModule.P4.0'('ProgDefs.CachedProgram.F1'(A), B, C, D) :-
        'ProgramCache':'~ProgramCache.GetCachedProgram.P2'(A, E),
        '~Programs.ImportInModule.P4'(E, B, C, D).
'Programs.ImportInModule.P4.0'('ProgDefs.Program.F4'(_,A,_,_), B, C, D) :-
        'SharedPrograms':'SharedPrograms.AVLFind.P3'(A, B, 'ProgDefs.ModDef.F4'(E,F,G,_)),
        'Programs.ImportInPart.P5'(C, E, F, G, D).
'~Programs.ImportInModule.P4.0'('ProgDefs.Program.F4'(_,A,_,_), B, C, D) :-
        'SharedPrograms':'~SharedPrograms.AVLFind.P3'(A, B, 'ProgDefs.ModDef.F4'(E,F,G,_)),
        '~Programs.ImportInPart.P5'(C, E, F, G, D).
'Programs.HasIntensionalSet.P1'('MetaDefs.Term.F2'(_,A)) :-
        'Programs.HasIntensionalSet1.P1'(A).
'~Programs.HasIntensionalSet.P1'('MetaDefs.Term.F2'(_,A)) :-
        '~Programs.HasIntensionalSet1.P1'(A).
'Programs.HasIntensionalSet.P1'('MetaDefs.SuchThat.F2'(_,_)).
'~Programs.HasIntensionalSet.P1'('MetaDefs.SuchThat.F2'(_,_)).
'Programs.HasIntensionalSet1.P1'([A|B]) :-
        (   'Programs.HasIntensionalSet.P1'(A)
        ;   'Programs.HasIntensionalSet1.P1'(B)
        ).
'~Programs.HasIntensionalSet1.P1'([A|B]) :-
        (   '~Programs.HasIntensionalSet.P1'(A)
        ;   '~Programs.HasIntensionalSet1.P1'(B)
        ).
'Programs.InsTypeDeclaration.P6'(A, B, C, D, E, F) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, G),
        'Programs.CModulePart.P2'(C, H),
        'SharedPrograms':'SharedPrograms.ModulePartInProgram.P3'(G, B, H),
        user:goedel_freeze(ground([B]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B))),
        D='MetaDefs.Name.F4'(B,_,_,_),
        'Programs.ValidDeclaration.P4'(E, G, B, C),
        'Programs.PartAccessibility.P2'(C, I),
        G='ProgDefs.Program.F4'(J,K,L,M),
        'SharedPrograms':'SharedPrograms.InsertTypeSymbol.P5'(D, L, I, E, N),
        F='ProgDefs.Program.F4'(J,K,N,M).
'~Programs.InsTypeDeclaration.P6'(A, B, C, D, E, F) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, G),
        '~Programs.CModulePart.P2'(C, H),
        'SharedPrograms':'~SharedPrograms.ModulePartInProgram.P3'(G, B, H),
        user:goedel_freeze(ground([B]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B))),
        D='MetaDefs.Name.F4'(B,_,_,_),
        '~Programs.ValidDeclaration.P4'(E, G, B, C),
        '~Programs.PartAccessibility.P2'(C, I),
        G='ProgDefs.Program.F4'(J,K,L,M),
        'SharedPrograms':'~SharedPrograms.InsertTypeSymbol.P5'(D, L, I, E, N),
        F='ProgDefs.Program.F4'(J,K,N,M).
'Programs.InsDeclaration.P6'(A, B, C, D, E, F) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, G),
        'Programs.CModulePart.P2'(C, H),
        'SharedPrograms':'SharedPrograms.ModulePartInProgram.P3'(G, B, H),
        user:goedel_freeze(ground([B]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B))),
        D='MetaDefs.Name.F4'(B,_,_,_),
        'Programs.ValidDeclaration.P4'(E, G, B, C),
        'Programs.PartAccessibility.P2'(C, I),
        G='ProgDefs.Program.F4'(J,K,L,M),
        'SharedPrograms':'SharedPrograms.InsertSymbol.P5'(D, L, I, E, N),
        F='ProgDefs.Program.F4'(J,K,N,M).
'~Programs.InsDeclaration.P6'(A, B, C, D, E, F) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, G),
        '~Programs.CModulePart.P2'(C, H),
        'SharedPrograms':'~SharedPrograms.ModulePartInProgram.P3'(G, B, H),
        user:goedel_freeze(ground([B]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B))),
        D='MetaDefs.Name.F4'(B,_,_,_),
        '~Programs.ValidDeclaration.P4'(E, G, B, C),
        '~Programs.PartAccessibility.P2'(C, I),
        G='ProgDefs.Program.F4'(J,K,L,M),
        'SharedPrograms':'~SharedPrograms.InsertSymbol.P5'(D, L, I, E, N),
        F='ProgDefs.Program.F4'(J,K,N,M).
'Programs.ImportInPart.P5'('Programs.Closed.C0', _, A, _, B) :-
        'SharedPrograms':'SharedPrograms.FindMember.P2'(B, A).
'~Programs.ImportInPart.P5'('Programs.Closed.C0', _, A, _, B) :-
        'SharedPrograms':'~SharedPrograms.FindMember.P2'(B, A).
'Programs.ImportInPart.P5'('Programs.Export.C0', _, A, _, B) :-
        'SharedPrograms':'SharedPrograms.FindMember.P2'(B, A).
'~Programs.ImportInPart.P5'('Programs.Export.C0', _, A, _, B) :-
        'SharedPrograms':'~SharedPrograms.FindMember.P2'(B, A).
'Programs.ImportInPart.P5'('Programs.Module.C0', _, _, A, B) :-
        'SharedPrograms':'SharedPrograms.FindMember.P2'(B, A).
'~Programs.ImportInPart.P5'('Programs.Module.C0', _, _, A, B) :-
        'SharedPrograms':'~SharedPrograms.FindMember.P2'(B, A).
'Programs.ImportInPart.P5'('Programs.Local.C0', A, _, B, C) :-
        user:not_equal([], [A], A, 'ProgDefs.ClosedKind.C0'),
        'SharedPrograms':'SharedPrograms.FindMember.P2'(C, B).
'~Programs.ImportInPart.P5'('Programs.Local.C0', A, _, B, C) :-
        user:not_equal([], [A], A, 'ProgDefs.ClosedKind.C0'),
        'SharedPrograms':'~SharedPrograms.FindMember.P2'(C, B).
'Programs.InsertDelay.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C,D,E]), 'Programs':'Programs.InsertDelay.P6.0'(A,B,C,D,E,F)).
'~Programs.InsertDelay.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C,D,E]), 'Programs':'~Programs.InsertDelay.P6.0'(A,B,C,D,E,F)).
'Programs.InsertDelay.P6.0'(A, B, C, D, E, 'ProgDefs.Program.F4'(F,G,H,I)) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, J),
        'Programs.CModulePart.P2'(C, K),
        'SharedPrograms':'SharedPrograms.ModulePartInProgram.P3'(J, B, K),
        user:not_equal([], [K], K, 'ProgDefs.Closed.C0'),
        'SharedPrograms':'SharedPrograms.ModuleLanguage.P3'(J, B, H),
        'Syntax':'Syntax.EmptyVarTyping.P1'(L),
        'SharedPrograms':'SharedPrograms.FormulaInLanguage.P4'(D, H, L, _),
        'Syntax':'Syntax.PredicateAtom.P3'(D, 'MetaDefs.Name.F4'(B,M,_,N), _),
        'Programs.IntensionalSetFree.P1'(D),
        'SharedPrograms':'SharedPrograms.NextModuleVersion.P2'(B, O),
        J='ProgDefs.Program.F4'(F,G,H,P),
        'AVLTrees':'AVLTrees.AVLUpdate.P5'(P, B, 'ProgDefs.Code.F2'(O,Q), I, 'ProgDefs.Code.F2'(_,R)),
        'AVLTrees':'AVLTrees.AVLAmend.P6'(R, M, S, [], Q, T),
        user:goedel_freeze(ground([N,T]), if(('Lists':'~Lists.DeleteFirst.P3'('ProgDefs.PredDef.F4'(N,U,V,W),T,X),true),('SharedPrograms':'SharedPrograms.UpdateDelays.P6'(K,'ProgDefs.Delay.F2'(D,E),V,W,Y,Z),S=['ProgDefs.PredDef.F4'(N,U,Y,Z)|X]),('SharedPrograms':'SharedPrograms.UpdateDelays.P6'(K,'ProgDefs.Delay.F2'(D,E),[],[],Y,Z),S=['ProgDefs.PredDef.F4'(N,[],Y,Z)|T]))).
'~Programs.InsertDelay.P6.0'(A, B, C, D, E, 'ProgDefs.Program.F4'(F,G,H,I)) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, J),
        '~Programs.CModulePart.P2'(C, K),
        'SharedPrograms':'~SharedPrograms.ModulePartInProgram.P3'(J, B, K),
        user:not_equal([], [K], K, 'ProgDefs.Closed.C0'),
        'SharedPrograms':'~SharedPrograms.ModuleLanguage.P3'(J, B, H),
        'Syntax':'~Syntax.EmptyVarTyping.P1'(L),
        'SharedPrograms':'~SharedPrograms.FormulaInLanguage.P4'(D, H, L, _),
        'Syntax':'~Syntax.PredicateAtom.P3'(D, 'MetaDefs.Name.F4'(B,M,_,N), _),
        '~Programs.IntensionalSetFree.P1'(D),
        'SharedPrograms':'~SharedPrograms.NextModuleVersion.P2'(B, O),
        J='ProgDefs.Program.F4'(F,G,H,P),
        'AVLTrees':'~AVLTrees.AVLUpdate.P5'(P, B, 'ProgDefs.Code.F2'(O,Q), I, 'ProgDefs.Code.F2'(_,R)),
        'AVLTrees':'~AVLTrees.AVLAmend.P6'(R, M, S, [], Q, T),
        user:goedel_freeze(ground([N,T]), if(('Lists':'~Lists.DeleteFirst.P3'('ProgDefs.PredDef.F4'(N,U,V,W),T,X),true),('SharedPrograms':'~SharedPrograms.UpdateDelays.P6'(K,'ProgDefs.Delay.F2'(D,E),V,W,Y,Z),S=['ProgDefs.PredDef.F4'(N,U,Y,Z)|X]),('SharedPrograms':'~SharedPrograms.UpdateDelays.P6'(K,'ProgDefs.Delay.F2'(D,E),[],[],Y,Z),S=['ProgDefs.PredDef.F4'(N,[],Y,Z)|T]))).
'Programs.InsertProgramProposition.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C,D]), 'Programs':'Programs.InsertProgramProposition.P5.0'(A,B,C,D,E)).
'~Programs.InsertProgramProposition.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C,D]), 'Programs':'~Programs.InsertProgramProposition.P5.0'(A,B,C,D,E)).
'Programs.InsertProgramProposition.P5.0'(A, B, C, D, E) :-
        'Programs.InsDeclaration.P6'(A, B, C, D, 'ProgDefs.PropositionDecl.C0', E).
'~Programs.InsertProgramProposition.P5.0'(A, B, C, D, E) :-
        '~Programs.InsDeclaration.P6'(A, B, C, D, 'ProgDefs.PropositionDecl.C0', E).
'Programs.InsertProgramFunction.P8'(A, B, C, D, E, F, G, H) :-
        user:goedel_freeze(ground([A,B,C,D,E,F,G]), 'Programs':'Programs.InsertProgramFunction.P8.0'(A,B,C,D,E,F,G,H)).
'~Programs.InsertProgramFunction.P8'(A, B, C, D, E, F, G, H) :-
        user:goedel_freeze(ground([A,B,C,D,E,F,G]), 'Programs':'~Programs.InsertProgramFunction.P8.0'(A,B,C,D,E,F,G,H)).
'Programs.InsertProgramFunction.P8.0'(A, B, C, D, E, F, G, H) :-
        'Programs.InsDeclaration.P6'(A, B, C, D, 'ProgDefs.FunctionDecl.F4'(_,E,F,G), H).
'~Programs.InsertProgramFunction.P8.0'(A, B, C, D, E, F, G, H) :-
        '~Programs.InsDeclaration.P6'(A, B, C, D, 'ProgDefs.FunctionDecl.F4'(_,E,F,G), H).
'Programs.InsertProgramConstructor.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C,D,E]), 'Programs':'Programs.InsertProgramConstructor.P6.0'(A,B,C,D,E,F)).
'~Programs.InsertProgramConstructor.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C,D,E]), 'Programs':'~Programs.InsertProgramConstructor.P6.0'(A,B,C,D,E,F)).
'Programs.InsertProgramConstructor.P6.0'(A, B, C, D, E, F) :-
        'Programs.InsTypeDeclaration.P6'(A, B, C, D, 'ProgDefs.ConstructorDecl.F1'(E), F).
'~Programs.InsertProgramConstructor.P6.0'(A, B, C, D, E, F) :-
        '~Programs.InsTypeDeclaration.P6'(A, B, C, D, 'ProgDefs.ConstructorDecl.F1'(E), F).
'Programs.InsertProgramConstant.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C,D,E]), 'Programs':'Programs.InsertProgramConstant.P6.0'(A,B,C,D,E,F)).
'~Programs.InsertProgramConstant.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(ground([A,B,C,D,E]), 'Programs':'~Programs.InsertProgramConstant.P6.0'(A,B,C,D,E,F)).
'Programs.InsertProgramConstant.P6.0'(A, B, C, D, E, F) :-
        'Programs.InsDeclaration.P6'(A, B, C, D, 'ProgDefs.ConstantDecl.F1'(E), F).
'~Programs.InsertProgramConstant.P6.0'(A, B, C, D, E, F) :-
        '~Programs.InsDeclaration.P6'(A, B, C, D, 'ProgDefs.ConstantDecl.F1'(E), F).
'Programs.InsertProgramImport.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C,D]), 'Programs':'Programs.InsertProgramImport.P5.0'(A,B,C,D,E)).
'~Programs.InsertProgramImport.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C,D]), 'Programs':'~Programs.InsertProgramImport.P5.0'(A,B,C,D,E)).
'Programs.InsertProgramImport.P5.0'('ProgDefs.CachedProgram.F1'(A), B, C, D, E) :-
        'ProgramCache':'ProgramCache.GetCachedProgram.P2'(A, E),
        'Programs.InsertProgramImport.P5'(E, B, C, D, E).
'~Programs.InsertProgramImport.P5.0'('ProgDefs.CachedProgram.F1'(A), B, C, D, E) :-
        'ProgramCache':'~ProgramCache.GetCachedProgram.P2'(A, E),
        '~Programs.InsertProgramImport.P5'(E, B, C, D, E).
'Programs.InsertProgramImport.P5.0'('ProgDefs.Program.F4'(A,B,'ProgDefs.Language.F1'(C),D), E, F, G, 'ProgDefs.Program.F4'(A,H,'ProgDefs.Language.F1'(I),J)) :-
        'Programs.CModulePart.P2'(F, K),
        'AVLTrees':'AVLTrees.AVLUpdate.P5'(B, E, 'ProgDefs.ModDef.F4'(L,M,N,O), P, 'ProgDefs.ModDef.F4'(L,Q,R,O)),
        user:goedel_freeze(ground([E]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(E))),
        'SharedPrograms':'SharedPrograms.KindHasPart.P2'(L, K),
        'SharedPrograms':'SharedPrograms.AddImportToPart.P6'(K, G, Q, R, M, N),
        user:goedel_freeze(ground([G,P]), ('AVLTrees':'~AVLTrees.AVLSearch.P3'(P,G,_),true->H=P,I=C,J=D;'SharedPrograms':'SharedPrograms.AddModule.P7'(G,P,C,D,H,I,J))).
'~Programs.InsertProgramImport.P5.0'('ProgDefs.Program.F4'(A,B,'ProgDefs.Language.F1'(C),D), E, F, G, 'ProgDefs.Program.F4'(A,H,'ProgDefs.Language.F1'(I),J)) :-
        '~Programs.CModulePart.P2'(F, K),
        'AVLTrees':'~AVLTrees.AVLUpdate.P5'(B, E, 'ProgDefs.ModDef.F4'(L,M,N,O), P, 'ProgDefs.ModDef.F4'(L,Q,R,O)),
        user:goedel_freeze(ground([E]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(E))),
        'SharedPrograms':'~SharedPrograms.KindHasPart.P2'(L, K),
        'SharedPrograms':'~SharedPrograms.AddImportToPart.P6'(K, G, Q, R, M, N),
        user:goedel_freeze(ground([G,P]), ('AVLTrees':'~AVLTrees.AVLSearch.P3'(P,G,_),true->H=P,I=C,J=D;'SharedPrograms':'~SharedPrograms.AddModule.P7'(G,P,C,D,H,I,J))).
'Programs.InsertProgramPredicate.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C,D,E,F]), 'Programs':'Programs.InsertProgramPredicate.P7.0'(A,B,C,D,E,F,G)).
'~Programs.InsertProgramPredicate.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C,D,E,F]), 'Programs':'~Programs.InsertProgramPredicate.P7.0'(A,B,C,D,E,F,G)).
'Programs.InsertProgramPredicate.P7.0'(A, B, C, D, E, F, G) :-
        'Programs.InsDeclaration.P6'(A, B, C, D, 'ProgDefs.PredicateDecl.F3'(_,E,F), G).
'~Programs.InsertProgramPredicate.P7.0'(A, B, C, D, E, F, G) :-
        '~Programs.InsDeclaration.P6'(A, B, C, D, 'ProgDefs.PredicateDecl.F3'(_,E,F), G).
'Programs.IntensionalSetFree.P1'('MetaDefs.Atom.F2'(_,A)) :-
        user:goedel_freeze(ground([A]), user:goedel_not('Programs':'~Programs.HasIntensionalSet1.P1'(A))).
'~Programs.IntensionalSetFree.P1'('MetaDefs.Atom.F2'(_,A)) :-
        user:goedel_freeze(ground([A]), user:goedel_not('Programs':'~Programs.HasIntensionalSet1.P1'(A))).
'Programs.InstanceOfHead.P3'('MetaDefs.Atom.F2'(A,B), 'MetaDefs.Atom.F2'(A,C), D) :-
        'Syntax':'Syntax.EmptyTermSubst.P1'(E),
        'Programs.InstanceOfHead2.P4'(B, C, E, D).
'~Programs.InstanceOfHead.P3'('MetaDefs.Atom.F2'(A,B), 'MetaDefs.Atom.F2'(A,C), D) :-
        'Syntax':'~Syntax.EmptyTermSubst.P1'(E),
        '~Programs.InstanceOfHead2.P4'(B, C, E, D).
'Programs.InsertStatement.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.InsertStatement.P4.0'(A,B,C,D)).
'~Programs.InsertStatement.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.InsertStatement.P4.0'(A,B,C,D)).
'Programs.InsertStatement.P4.0'(A, B, C, 'ProgDefs.Program.F4'(D,E,F,G)) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, H),
        user:goedel_freeze(ground([B]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B))),
        'Programs.StatementInModuleLanguage.P3'(H, B, C),
        'Programs.OpenStatementPredicate.P2'(C, 'MetaDefs.Name.F4'(B,I,_,J)),
        'SharedPrograms':'SharedPrograms.NextModuleVersion.P2'(B, K),
        H='ProgDefs.Program.F4'(D,E,F,L),
        'AVLTrees':'AVLTrees.AVLUpdate.P5'(L, B, 'ProgDefs.Code.F2'(K,M), G, 'ProgDefs.Code.F2'(_,N)),
        'AVLTrees':'AVLTrees.AVLAmend.P6'(N, I, O, [], M, P),
        user:goedel_freeze(ground([J,P]), if(('Lists':'~Lists.DeleteFirst.P3'('ProgDefs.PredDef.F4'(J,Q,R,S),P,T),true),O=['ProgDefs.PredDef.F4'(J,[C|Q],R,S)|T],O=['ProgDefs.PredDef.F4'(J,[C],[],[])|P])).
'~Programs.InsertStatement.P4.0'(A, B, C, 'ProgDefs.Program.F4'(D,E,F,G)) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, H),
        user:goedel_freeze(ground([B]), user:goedel_not('SharedPrograms':'~SharedPrograms.SystemModule.P1'(B))),
        '~Programs.StatementInModuleLanguage.P3'(H, B, C),
        '~Programs.OpenStatementPredicate.P2'(C, 'MetaDefs.Name.F4'(B,I,_,J)),
        'SharedPrograms':'~SharedPrograms.NextModuleVersion.P2'(B, K),
        H='ProgDefs.Program.F4'(D,E,F,L),
        'AVLTrees':'~AVLTrees.AVLUpdate.P5'(L, B, 'ProgDefs.Code.F2'(K,M), G, 'ProgDefs.Code.F2'(_,N)),
        'AVLTrees':'~AVLTrees.AVLAmend.P6'(N, I, O, [], M, P),
        user:goedel_freeze(ground([J,P]), if(('Lists':'~Lists.DeleteFirst.P3'('ProgDefs.PredDef.F4'(J,Q,R,S),P,T),true),O=['ProgDefs.PredDef.F4'(J,[C|Q],R,S)|T],O=['ProgDefs.PredDef.F4'(J,[C],[],[])|P])).
'Programs.InstanceOfHead1.P4'('MetaDefs.Var.F2'(A,B), C, D, E) :-
        'Syntax':'Syntax.BindingToTermSubst.P3'('MetaDefs.Var.F2'(A,B), C, F),
        'Syntax':'Syntax.ComposeTermSubsts.P3'(D, F, E).
'~Programs.InstanceOfHead1.P4'('MetaDefs.Var.F2'(A,B), C, D, E) :-
        'Syntax':'~Syntax.BindingToTermSubst.P3'('MetaDefs.Var.F2'(A,B), C, F),
        'Syntax':'~Syntax.ComposeTermSubsts.P3'(D, F, E).
'Programs.InstanceOfHead1.P4'('MetaDefs.Var.F1'(A), B, C, D) :-
        'Syntax':'Syntax.BindingToTermSubst.P3'('MetaDefs.Var.F1'(A), B, E),
        'Syntax':'Syntax.ComposeTermSubsts.P3'(C, E, D).
'~Programs.InstanceOfHead1.P4'('MetaDefs.Var.F1'(A), B, C, D) :-
        'Syntax':'~Syntax.BindingToTermSubst.P3'('MetaDefs.Var.F1'(A), B, E),
        'Syntax':'~Syntax.ComposeTermSubsts.P3'(C, E, D).
'Programs.InstanceOfHead1.P4'('MetaDefs.Term.F2'(A,B), 'MetaDefs.Term.F2'(A,C), D, E) :-
        'Programs.InstanceOfHead2.P4'(B, C, D, E).
'~Programs.InstanceOfHead1.P4'('MetaDefs.Term.F2'(A,B), 'MetaDefs.Term.F2'(A,C), D, E) :-
        '~Programs.InstanceOfHead2.P4'(B, C, D, E).
'Programs.InstanceOfHead1.P4'('MetaDefs.CTerm.F1'(A), 'MetaDefs.CTerm.F1'(A), B, B).
'~Programs.InstanceOfHead1.P4'('MetaDefs.CTerm.F1'(A), 'MetaDefs.CTerm.F1'(A), B, B).
'Programs.InstanceOfHead1.P4'('MetaDefs.Int.F1'(A), 'MetaDefs.Int.F1'(A), B, B).
'~Programs.InstanceOfHead1.P4'('MetaDefs.Int.F1'(A), 'MetaDefs.Int.F1'(A), B, B).
'Programs.InstanceOfHead1.P4'('MetaDefs.Str.F1'(A), 'MetaDefs.Str.F1'(A), B, B).
'~Programs.InstanceOfHead1.P4'('MetaDefs.Str.F1'(A), 'MetaDefs.Str.F1'(A), B, B).
'Programs.InstanceOfHead1.P4'('MetaDefs.Prm.F1'(A), 'MetaDefs.Prm.F1'(A), B, B).
'~Programs.InstanceOfHead1.P4'('MetaDefs.Prm.F1'(A), 'MetaDefs.Prm.F1'(A), B, B).
'Programs.InstanceOfHead2.P4'([], [], A, A).
'~Programs.InstanceOfHead2.P4'([], [], A, A).
'Programs.InstanceOfHead2.P4'([A|B], [C|D], E, F) :-
        'Programs.InstanceOfHead1.P4'(A, C, E, G),
        'Programs.InstanceOfHead2.P4'(B, D, G, F).
'~Programs.InstanceOfHead2.P4'([A|B], [C|D], E, F) :-
        '~Programs.InstanceOfHead1.P4'(A, C, E, G),
        '~Programs.InstanceOfHead2.P4'(B, D, G, F).
'Programs.ListTypesInModule.P4'(A, B, C, D) :-
        'Programs.CModulePart.P2'(C, E),
        'Programs.ModulePartLanguage.P4'(E, B, A, F),
        'SharedPrograms':'SharedPrograms.ListTypesInLanguage.P2'(D, F).
'~Programs.ListTypesInModule.P4'(A, B, C, D) :-
        '~Programs.CModulePart.P2'(C, E),
        '~Programs.ModulePartLanguage.P4'(E, B, A, F),
        'SharedPrograms':'~SharedPrograms.ListTypesInLanguage.P2'(D, F).
'Programs.ProgramTermToString.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.ProgramTermToString.P4.0'(A,B,C,D)).
'~Programs.ProgramTermToString.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.ProgramTermToString.P4.0'(A,B,C,D)).
'Programs.ProgramTermToString.P4.0'(A, B, C, D) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, E),
        'SharedPrograms':'SharedPrograms.SProgramTermToString.P4'(E, B, C, D).
'~Programs.ProgramTermToString.P4.0'(A, B, C, D) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, E),
        'SharedPrograms':'~SharedPrograms.SProgramTermToString.P4'(E, B, C, D).
'Programs.ProgramConstructorName.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A])and(ground([B])and(ground([C])and ground([D])))or ground([E]), 'Programs':'Programs.ProgramConstructorName.P5.0'(A,B,C,D,E)).
'~Programs.ProgramConstructorName.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A])and(ground([B])and(ground([C])and ground([D])))or ground([E]), 'Programs':'~Programs.ProgramConstructorName.P5.0'(A,B,C,D,E)).
'Programs.ProgramConstructorName.P5.0'(A, B, C, D, 'MetaDefs.Name.F4'(B,C,'MetaDefs.Constructor.C0',D)) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, E),
        'Programs.ModuleInProgram.P2'(E, B).
'~Programs.ProgramConstructorName.P5.0'(A, B, C, D, 'MetaDefs.Name.F4'(B,C,'MetaDefs.Constructor.C0',D)) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, E),
        '~Programs.ModuleInProgram.P2'(E, B).
'Programs.OpenModule.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.OpenModule.P2.0'(A,B)).
'~Programs.OpenModule.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.OpenModule.P2.0'(A,B)).
'Programs.OpenModule.P2.0'(A, B) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, C),
        'SharedPrograms':'SharedPrograms.SOpenModule.P2'(C, B).
'~Programs.OpenModule.P2.0'(A, B) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, C),
        'SharedPrograms':'~SharedPrograms.SOpenModule.P2'(C, B).
'Programs.ModulePartLanguage.P4'('ProgDefs.Export.C0', A, B, C) :-
        'SharedPrograms':'SharedPrograms.ExportLanguage.P3'(B, A, C).
'~Programs.ModulePartLanguage.P4'('ProgDefs.Export.C0', A, B, C) :-
        'SharedPrograms':'~SharedPrograms.ExportLanguage.P3'(B, A, C).
'Programs.ModulePartLanguage.P4'('ProgDefs.Closed.C0', A, B, C) :-
        'SharedPrograms':'SharedPrograms.ExportLanguage.P3'(B, A, C).
'~Programs.ModulePartLanguage.P4'('ProgDefs.Closed.C0', A, B, C) :-
        'SharedPrograms':'~SharedPrograms.ExportLanguage.P3'(B, A, C).
'Programs.ModulePartLanguage.P4'('ProgDefs.Local.C0', A, B, C) :-
        'SharedPrograms':'SharedPrograms.ModuleLanguage.P3'(B, A, C).
'~Programs.ModulePartLanguage.P4'('ProgDefs.Local.C0', A, B, C) :-
        'SharedPrograms':'~SharedPrograms.ModuleLanguage.P3'(B, A, C).
'Programs.ModulePartLanguage.P4'('ProgDefs.Module.C0', A, B, C) :-
        'SharedPrograms':'SharedPrograms.ModuleLanguage.P3'(B, A, C).
'~Programs.ModulePartLanguage.P4'('ProgDefs.Module.C0', A, B, C) :-
        'SharedPrograms':'~SharedPrograms.ModuleLanguage.P3'(B, A, C).
'Programs.ModuleInProgram.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.ModuleInProgram.P2.0'(A,B)).
'~Programs.ModuleInProgram.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.ModuleInProgram.P2.0'(A,B)).
'Programs.ModuleInProgram.P2.0'('ProgDefs.CachedProgram.F1'(A), B) :-
        'ProgramCache':'ProgramCache.GetCachedProgram.P2'(A, C),
        'Programs.ModuleInProgram.P2'(C, B).
'~Programs.ModuleInProgram.P2.0'('ProgDefs.CachedProgram.F1'(A), B) :-
        'ProgramCache':'~ProgramCache.GetCachedProgram.P2'(A, C),
        '~Programs.ModuleInProgram.P2'(C, B).
'Programs.ModuleInProgram.P2.0'('ProgDefs.Program.F4'(_,A,_,_), B) :-
        'Programs.ModuleInProgramAux.P2'(A, B).
'~Programs.ModuleInProgram.P2.0'('ProgDefs.Program.F4'(_,A,_,_), B) :-
        '~Programs.ModuleInProgramAux.P2'(A, B).
'Programs.ModuleImported.P3'(A, 'ProgDefs.ModDef.F4'(_,B,C,D), E) :-
        'Lists':'Lists.Append.P3'(B, C, F),
        'Lists':'Lists.Append.P3'(D, F, G),
        'Programs.ReachableModules.P4'(G, E, [], H),
        'Lists':'Lists.MemberCheck.P2'(A, H).
'~Programs.ModuleImported.P3'(A, 'ProgDefs.ModDef.F4'(_,B,C,D), E) :-
        'Lists':'~Lists.Append.P3'(B, C, F),
        'Lists':'~Lists.Append.P3'(D, F, G),
        '~Programs.ReachableModules.P4'(G, E, [], H),
        'Lists':'~Lists.MemberCheck.P2'(A, H).
'Programs.NewProgram.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.NewProgram.P2.0'(A,B)).
'~Programs.NewProgram.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.NewProgram.P2.0'(A,B)).
'Programs.NewProgram.P2.0'(A, B) :-
        'Programs.NewProgramAux.P2'(B, A).
'~Programs.NewProgram.P2.0'(A, B) :-
        '~Programs.NewProgramAux.P2'(B, A).
'Programs.NonVarCondition.P2'('MetaDefs.Var.F2'(A,B), 'ProgDefs.Nonvar.F1'('MetaDefs.Var.F2'(A,B))).
'~Programs.NonVarCondition.P2'('MetaDefs.Var.F2'(A,B), 'ProgDefs.Nonvar.F1'('MetaDefs.Var.F2'(A,B))).
'Programs.NonVarCondition.P2'('MetaDefs.Var.F1'(A), 'ProgDefs.Nonvar.F1'('MetaDefs.Var.F1'(A))).
'~Programs.NonVarCondition.P2'('MetaDefs.Var.F1'(A), 'ProgDefs.Nonvar.F1'('MetaDefs.Var.F1'(A))).
'Programs.PredOrPropInLanguage.P3'('MetaDefs.Predicate.C0', A, B) :-
        'SharedPrograms':'SharedPrograms.PredicateInLanguage.P4'(A, B, _, _).
'~Programs.PredOrPropInLanguage.P3'('MetaDefs.Predicate.C0', A, B) :-
        'SharedPrograms':'~SharedPrograms.PredicateInLanguage.P4'(A, B, _, _).
'Programs.PredOrPropInLanguage.P3'('MetaDefs.Proposition.C0', A, B) :-
        'SharedPrograms':'SharedPrograms.PropositionInLanguage.P2'(A, B).
'~Programs.PredOrPropInLanguage.P3'('MetaDefs.Proposition.C0', A, B) :-
        'SharedPrograms':'~SharedPrograms.PropositionInLanguage.P2'(A, B).
'Programs.OrCondition.P3'(A, B, 'ProgDefs.Or.F2'(A,B)).
'~Programs.OrCondition.P3'(A, B, 'ProgDefs.Or.F2'(A,B)).
'Programs.OpenStatementPredicate.P2'(B, A) :-
        user:goedel_freeze(nonvar(B), 'Programs':'Programs.OpenStatementPredicate.P2.1'(B,A)).
'~Programs.OpenStatementPredicate.P2'(B, A) :-
        user:goedel_freeze(nonvar(B), 'Programs':'~Programs.OpenStatementPredicate.P2.1'(B,A)).
'Programs.OpenStatementPredicate.P2.1'('MetaDefs.<-''.F2'(A,B), C) :- !,
        user:goedel_freeze(nonvar(A), 'Programs':'Programs.OpenStatementPredicate.P2.0'('MetaDefs.<-''.F2'(A,B),C)).
'~Programs.OpenStatementPredicate.P2.1'('MetaDefs.<-''.F2'(A,B), C) :- !,
        user:goedel_freeze(nonvar(A), 'Programs':'~Programs.OpenStatementPredicate.P2.0'('MetaDefs.<-''.F2'(A,B),C)).
'Programs.OpenStatementPredicate.P2.1'(A, B) :-
        'Programs.OpenStatementPredicate.P2.0'(A, B).
'~Programs.OpenStatementPredicate.P2.1'(A, B) :-
        '~Programs.OpenStatementPredicate.P2.0'(A, B).
'Programs.OpenStatementPredicate.P2.0'('MetaDefs.<-''.F2'('MetaDefs.PAtom.F1'(A),_), A).
'~Programs.OpenStatementPredicate.P2.0'('MetaDefs.<-''.F2'('MetaDefs.PAtom.F1'(A),_), A).
'Programs.OpenStatementPredicate.P2.0'('MetaDefs.<-''.F2'('MetaDefs.Atom.F2'(A,_),_), A).
'~Programs.OpenStatementPredicate.P2.0'('MetaDefs.<-''.F2'('MetaDefs.Atom.F2'(A,_),_), A).
'Programs.PartAccessibility.P2'('Programs.Export.C0', 'ProgDefs.Exported.C0').
'~Programs.PartAccessibility.P2'('Programs.Export.C0', 'ProgDefs.Exported.C0').
'Programs.PartAccessibility.P2'('Programs.Closed.C0', 'ProgDefs.Exported.C0').
'~Programs.PartAccessibility.P2'('Programs.Closed.C0', 'ProgDefs.Exported.C0').
'Programs.PartAccessibility.P2'('Programs.Local.C0', 'ProgDefs.Hidden.C0').
'~Programs.PartAccessibility.P2'('Programs.Local.C0', 'ProgDefs.Hidden.C0').
'Programs.PartAccessibility.P2'('Programs.Module.C0', 'ProgDefs.Hidden.C0').
'~Programs.PartAccessibility.P2'('Programs.Module.C0', 'ProgDefs.Hidden.C0').
'Programs.ProgramBaseName.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A])and(ground([B])and ground([C]))or ground([D]), 'Programs':'Programs.ProgramBaseName.P4.0'(A,B,C,D)).
'~Programs.ProgramBaseName.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A])and(ground([B])and ground([C]))or ground([D]), 'Programs':'~Programs.ProgramBaseName.P4.0'(A,B,C,D)).
'Programs.ProgramBaseName.P4.0'(A, B, C, 'MetaDefs.Name.F4'(B,C,'MetaDefs.Base.C0',0)) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, D),
        'Programs.ModuleInProgram.P2'(D, B).
'~Programs.ProgramBaseName.P4.0'(A, B, C, 'MetaDefs.Name.F4'(B,C,'MetaDefs.Base.C0',0)) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, D),
        '~Programs.ModuleInProgram.P2'(D, B).
'Programs.PredicateInModule.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.PredicateInModule.P7.0'(A,B,C,D,E,F,G)).
'~Programs.PredicateInModule.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.PredicateInModule.P7.0'(A,B,C,D,E,F,G)).
'Programs.PredicateInModule.P7.0'(A, B, C, D, E, F, G) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, H),
        'Programs.CModulePart.P2'(C, I),
        'SharedPrograms':'SharedPrograms.ModulePartInProgram.P3'(H, B, I),
        'Programs.ModulePartLanguage.P4'(I, B, H, J),
        'SharedPrograms':'SharedPrograms.PredicateInLanguage.P4'(D, J, E, F),
        D='MetaDefs.Name.F4'(G,_,_,_).
'~Programs.PredicateInModule.P7.0'(A, B, C, D, E, F, G) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, H),
        '~Programs.CModulePart.P2'(C, I),
        'SharedPrograms':'~SharedPrograms.ModulePartInProgram.P3'(H, B, I),
        '~Programs.ModulePartLanguage.P4'(I, B, H, J),
        'SharedPrograms':'~SharedPrograms.PredicateInLanguage.P4'(D, J, E, F),
        D='MetaDefs.Name.F4'(G,_,_,_).
'Programs.ProgramConstantName.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A])and(ground([B])and ground([C]))or ground([D]), 'Programs':'Programs.ProgramConstantName.P4.0'(A,B,C,D)).
'~Programs.ProgramConstantName.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A])and(ground([B])and ground([C]))or ground([D]), 'Programs':'~Programs.ProgramConstantName.P4.0'(A,B,C,D)).
'Programs.ProgramConstantName.P4.0'(A, B, C, 'MetaDefs.Name.F4'(B,C,'MetaDefs.Constant.C0',0)) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, D),
        'Programs.ModuleInProgram.P2'(D, B).
'~Programs.ProgramConstantName.P4.0'(A, B, C, 'MetaDefs.Name.F4'(B,C,'MetaDefs.Constant.C0',0)) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, D),
        '~Programs.ModuleInProgram.P2'(D, B).
'Programs.ProgramFunctionName.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A])and(ground([B])and(ground([C])and ground([D])))or ground([E]), 'Programs':'Programs.ProgramFunctionName.P5.0'(A,B,C,D,E)).
'~Programs.ProgramFunctionName.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A])and(ground([B])and(ground([C])and ground([D])))or ground([E]), 'Programs':'~Programs.ProgramFunctionName.P5.0'(A,B,C,D,E)).
'Programs.ProgramFunctionName.P5.0'(A, B, C, D, 'MetaDefs.Name.F4'(B,C,'MetaDefs.Function.C0',D)) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, E),
        'Programs.ModuleInProgram.P2'(E, B).
'~Programs.ProgramFunctionName.P5.0'(A, B, C, D, 'MetaDefs.Name.F4'(B,C,'MetaDefs.Function.C0',D)) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, E),
        '~Programs.ModuleInProgram.P2'(E, B).
'Programs.ProgramFormulaToString.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.ProgramFormulaToString.P4.0'(A,B,C,D)).
'~Programs.ProgramFormulaToString.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.ProgramFormulaToString.P4.0'(A,B,C,D)).
'Programs.ProgramFormulaToString.P4.0'(A, B, C, D) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, E),
        'SharedPrograms':'SharedPrograms.SProgramFormulaToString.P4'(E, B, C, D).
'~Programs.ProgramFormulaToString.P4.0'(A, B, C, D) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, E),
        'SharedPrograms':'~SharedPrograms.SProgramFormulaToString.P4'(E, B, C, D).
'Programs.ProgramPropositionName.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A])and(ground([B])and ground([C]))or ground([D]), 'Programs':'Programs.ProgramPropositionName.P4.0'(A,B,C,D)).
'~Programs.ProgramPropositionName.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A])and(ground([B])and ground([C]))or ground([D]), 'Programs':'~Programs.ProgramPropositionName.P4.0'(A,B,C,D)).
'Programs.ProgramPropositionName.P4.0'(A, B, C, 'MetaDefs.Name.F4'(B,C,'MetaDefs.Proposition.C0',0)) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, D),
        'Programs.ModuleInProgram.P2'(D, B).
'~Programs.ProgramPropositionName.P4.0'(A, B, C, 'MetaDefs.Name.F4'(B,C,'MetaDefs.Proposition.C0',0)) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, D),
        '~Programs.ModuleInProgram.P2'(D, B).
'Programs.ProgramPredicateName.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A])and(ground([B])and(ground([C])and ground([D])))or ground([E]), 'Programs':'Programs.ProgramPredicateName.P5.0'(A,B,C,D,E)).
'~Programs.ProgramPredicateName.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A])and(ground([B])and(ground([C])and ground([D])))or ground([E]), 'Programs':'~Programs.ProgramPredicateName.P5.0'(A,B,C,D,E)).
'Programs.ProgramPredicateName.P5.0'(A, B, C, D, 'MetaDefs.Name.F4'(B,C,'MetaDefs.Predicate.C0',D)) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, E),
        'Programs.ModuleInProgram.P2'(E, B).
'~Programs.ProgramPredicateName.P5.0'(A, B, C, D, 'MetaDefs.Name.F4'(B,C,'MetaDefs.Predicate.C0',D)) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, E),
        '~Programs.ModuleInProgram.P2'(E, B).
'Programs.StringToProgramTerm.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.StringToProgramTerm.P4.0'(A,B,C,D)).
'~Programs.StringToProgramTerm.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.StringToProgramTerm.P4.0'(A,B,C,D)).
'Programs.StringToProgramTerm.P4.0'(A, B, C, D) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, E),
        'Programs.StringToProgramTermAux.P4'(E, B, C, D).
'~Programs.StringToProgramTerm.P4.0'(A, B, C, D) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, E),
        '~Programs.StringToProgramTermAux.P4'(E, B, C, D).
'Programs.StatementInModule.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.StatementInModule.P3.0'(A,B,C)).
'~Programs.StatementInModule.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.StatementInModule.P3.0'(A,B,C)).
'Programs.StatementInModule.P3.0'(A, B, C) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, D),
        'Programs.OpenStatementPredicate.P2'(C, E),
        'Programs.DefinitionInProgram.P4'(D, B, E, F),
        'Lists':'Lists.Member.P2'(C, F).
'~Programs.StatementInModule.P3.0'(A, B, C) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, D),
        '~Programs.OpenStatementPredicate.P2'(C, E),
        '~Programs.DefinitionInProgram.P4'(D, B, E, F),
        'Lists':'~Lists.Member.P2'(C, F).
'Programs.PropositionInModule.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.PropositionInModule.P5.0'(A,B,C,D,E)).
'~Programs.PropositionInModule.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.PropositionInModule.P5.0'(A,B,C,D,E)).
'Programs.PropositionInModule.P5.0'(A, B, C, D, E) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, F),
        'Programs.CModulePart.P2'(C, G),
        'SharedPrograms':'SharedPrograms.ModulePartInProgram.P3'(F, B, G),
        'Programs.ModulePartLanguage.P4'(G, B, F, H),
        'SharedPrograms':'SharedPrograms.PropositionInLanguage.P2'(D, H),
        D='MetaDefs.Name.F4'(E,_,_,_).
'~Programs.PropositionInModule.P5.0'(A, B, C, D, E) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, F),
        '~Programs.CModulePart.P2'(C, G),
        'SharedPrograms':'~SharedPrograms.ModulePartInProgram.P3'(F, B, G),
        '~Programs.ModulePartLanguage.P4'(G, B, F, H),
        'SharedPrograms':'~SharedPrograms.PropositionInLanguage.P2'(D, H),
        D='MetaDefs.Name.F4'(E,_,_,_).
'Programs.ProgramTypeToString.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.ProgramTypeToString.P4.0'(A,B,C,D)).
'~Programs.ProgramTypeToString.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.ProgramTypeToString.P4.0'(A,B,C,D)).
'Programs.ProgramTypeToString.P4.0'(A, B, C, D) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, E),
        'SharedPrograms':'SharedPrograms.SProgramTypeToString.P4'(E, B, C, D).
'~Programs.ProgramTypeToString.P4.0'(A, B, C, D) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, E),
        'SharedPrograms':'~SharedPrograms.SProgramTypeToString.P4'(E, B, C, D).
'Programs.RunnableAtom.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Programs':'Programs.RunnableAtom.P2.0'(A,B)).
'~Programs.RunnableAtom.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Programs':'~Programs.RunnableAtom.P2.0'(A,B)).
'Programs.RunnableAtom.P2.0'(A, B) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, C),
        'Programs.DeclaredInOpenModule.P3'(C, D, B),
        'Syntax':'Syntax.PredicateAtom.P3'(B, E, _),
        'Programs.ControlInProgram.P5'(C, D, E, F, G),
        user:goedel_freeze(ground([B,G,F]), if(('Programs':'~Programs.ApplicableDelay.P5'(F,G,B,H,I),true),('Syntax':'Syntax.RenameFormulas.P3'([H],[B],[J]),'Programs':'Programs.InstanceOfHead.P3'(H,J,K),'Programs':'Programs.ConditionSatisfied.P2'(I,K)),true)).
'~Programs.RunnableAtom.P2.0'(A, B) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, C),
        '~Programs.DeclaredInOpenModule.P3'(C, D, B),
        'Syntax':'~Syntax.PredicateAtom.P3'(B, E, _),
        '~Programs.ControlInProgram.P5'(C, D, E, F, G),
        user:goedel_freeze(ground([B,G,F]), if(('Programs':'~Programs.ApplicableDelay.P5'(F,G,B,H,I),true),('Syntax':'~Syntax.RenameFormulas.P3'([H],[B],[J]),'Programs':'~Programs.InstanceOfHead.P3'(H,J,K),'Programs':'~Programs.ConditionSatisfied.P2'(I,K)),true)).
'Programs.RightHandType.P4'('MetaDefs.BType.F1'(A), B, C, D) :-
        'Programs.BaseInModule.P5'(B, C, D, A, C).
'~Programs.RightHandType.P4'('MetaDefs.BType.F1'(A), B, C, D) :-
        '~Programs.BaseInModule.P5'(B, C, D, A, C).
'Programs.RightHandType.P4'('MetaDefs.Type.F2'(A,_), B, C, D) :-
        'Programs.ConstructorInModule.P6'(B, C, D, A, _, C).
'~Programs.RightHandType.P4'('MetaDefs.Type.F2'(A,_), B, C, D) :-
        '~Programs.ConstructorInModule.P6'(B, C, D, A, _, C).
'Programs.StringToCondition.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Programs':'Programs.StringToCondition.P2.0'(A,B)).
'~Programs.StringToCondition.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Programs':'~Programs.StringToCondition.P2.0'(A,B)).
'Programs.StringToCondition.P2.0'(A, B) :-
        'SharedPrograms':'SharedPrograms.SStringToCondition.P2'(A, B).
'~Programs.StringToCondition.P2.0'(A, B) :-
        'SharedPrograms':'~SharedPrograms.SStringToCondition.P2'(A, B).
'Programs.StatementMatchAtom.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,C]), 'Programs':'Programs.StatementMatchAtom.P4.0'(A,B,C,D)).
'~Programs.StatementMatchAtom.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,C]), 'Programs':'~Programs.StatementMatchAtom.P4.0'(A,B,C,D)).
'Programs.StatementMatchAtom.P4.0'('ProgDefs.CachedProgram.F1'(A), B, C, D) :-
        'ProgramCache':'ProgramCache.GetCachedProgram.P2'(A, E),
        'Programs.StatementMatchAtom.P4'(E, B, C, D).
'~Programs.StatementMatchAtom.P4.0'('ProgDefs.CachedProgram.F1'(A), B, C, D) :-
        'ProgramCache':'~ProgramCache.GetCachedProgram.P2'(A, E),
        '~Programs.StatementMatchAtom.P4'(E, B, C, D).
'Programs.StatementMatchAtom.P4.0'('ProgDefs.Program.F4'(_,A,B,C), D, E, F) :-
        'Syntax':'Syntax.Atom.P1'(E),
        'Syntax':'Syntax.EmptyVarTyping.P1'(G),
        'SharedPrograms':'SharedPrograms.FormulaInLanguage.P4'(E, B, G, _),
        'Programs.TopLevelName.P2'(E, 'MetaDefs.Name.F4'(D,H,_,I)),
        'SharedPrograms':'SharedPrograms.OpenModuleAux.P2'(A, D),
        'AVLTrees':'AVLTrees.AVLSearch.P3'(C, D, 'ProgDefs.Code.F2'(_,J)),
        'AVLTrees':'AVLTrees.AVLSearch.P3'(J, H, K),
        'Lists':'Lists.MemberCheck.P2'('ProgDefs.PredDef.F4'(I,L,_,_), K),
        'Lists':'Lists.Member.P2'(F, L).
'~Programs.StatementMatchAtom.P4.0'('ProgDefs.Program.F4'(_,A,B,C), D, E, F) :-
        'Syntax':'~Syntax.Atom.P1'(E),
        'Syntax':'~Syntax.EmptyVarTyping.P1'(G),
        'SharedPrograms':'~SharedPrograms.FormulaInLanguage.P4'(E, B, G, _),
        '~Programs.TopLevelName.P2'(E, 'MetaDefs.Name.F4'(D,H,_,I)),
        'SharedPrograms':'~SharedPrograms.OpenModuleAux.P2'(A, D),
        'AVLTrees':'~AVLTrees.AVLSearch.P3'(C, D, 'ProgDefs.Code.F2'(_,J)),
        'AVLTrees':'~AVLTrees.AVLSearch.P3'(J, H, K),
        'Lists':'~Lists.MemberCheck.P2'('ProgDefs.PredDef.F4'(I,L,_,_), K),
        'Lists':'~Lists.Member.P2'(F, L).
'Programs.StatementInModuleLanguage.P3'(A, B, C) :-
        'SharedPrograms':'SharedPrograms.ModuleLanguage.P3'(A, B, D),
        'SharedPrograms':'SharedPrograms.StatementInLanguage.P5'(C, D, _, _, []).
'~Programs.StatementInModuleLanguage.P3'(A, B, C) :-
        'SharedPrograms':'~SharedPrograms.ModuleLanguage.P3'(A, B, D),
        'SharedPrograms':'~SharedPrograms.StatementInLanguage.P5'(C, D, _, _, []).
'Programs.StringToProgramFormula.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.StringToProgramFormula.P4.0'(A,B,C,D)).
'~Programs.StringToProgramFormula.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.StringToProgramFormula.P4.0'(A,B,C,D)).
'Programs.StringToProgramFormula.P4.0'(A, B, C, D) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, E),
        'Programs.StringToProgramFormulaAux.P4'(E, B, C, D).
'~Programs.StringToProgramFormula.P4.0'(A, B, C, D) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, E),
        '~Programs.StringToProgramFormulaAux.P4'(E, B, C, D).
'Programs.TermInProgram.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.TermInProgram.P5.0'(A,B,C,D,E)).
'~Programs.TermInProgram.P5'(A, B, C, D, E) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.TermInProgram.P5.0'(A,B,C,D,E)).
'Programs.TermInProgram.P5.0'('ProgDefs.CachedProgram.F1'(A), B, C, D, E) :-
        'ProgramCache':'ProgramCache.GetCachedProgram.P2'(A, F),
        'Programs.TermInProgram.P5'(F, B, C, D, E).
'~Programs.TermInProgram.P5.0'('ProgDefs.CachedProgram.F1'(A), B, C, D, E) :-
        'ProgramCache':'~ProgramCache.GetCachedProgram.P2'(A, F),
        '~Programs.TermInProgram.P5'(F, B, C, D, E).
'Programs.TermInProgram.P5.0'('ProgDefs.Program.F4'(_,_,A,_), B, C, D, E) :-
        'SharedPrograms':'SharedPrograms.TermInLanguage.P5'(C, A, B, E, D).
'~Programs.TermInProgram.P5.0'('ProgDefs.Program.F4'(_,_,A,_), B, C, D, E) :-
        'SharedPrograms':'~SharedPrograms.TermInLanguage.P5'(C, A, B, E, D).
'Programs.SymbolFreeInModule.P3'(A, B, 'ProgDefs.Program.F4'(_,_,_,C)) :-
        'AVLTrees':'AVLTrees.AVLSearch.P3'(C, A, 'ProgDefs.Code.F2'(_,D)),
        user:goedel_freeze(ground([B,D]), user:goedel_not((('AVLTrees':'~AVLTrees.AVLMember.P3'(D,_,E),'Lists':'~Lists.Member.P2'('ProgDefs.PredDef.F4'(_,F,G,H),E)),('Lists':'~Lists.Member.P2'(I,F),'Programs':'~Programs.SymbolInFormula.P2'(I,B);'Lists':'~Lists.Member.P2'(J,G),'Programs':'~Programs.SymbolInDelay.P2'(J,B);'Lists':'~Lists.Member.P2'(K,H),'Programs':'~Programs.SymbolInDelay.P2'(K,B))))).
'~Programs.SymbolFreeInModule.P3'(A, B, 'ProgDefs.Program.F4'(_,_,_,C)) :-
        'AVLTrees':'~AVLTrees.AVLSearch.P3'(C, A, 'ProgDefs.Code.F2'(_,D)),
        user:goedel_freeze(ground([B,D]), user:goedel_not((('AVLTrees':'~AVLTrees.AVLMember.P3'(D,_,E),'Lists':'~Lists.Member.P2'('ProgDefs.PredDef.F4'(_,F,G,H),E)),('Lists':'~Lists.Member.P2'(I,F),'Programs':'~Programs.SymbolInFormula.P2'(I,B);'Lists':'~Lists.Member.P2'(J,G),'Programs':'~Programs.SymbolInDelay.P2'(J,B);'Lists':'~Lists.Member.P2'(K,H),'Programs':'~Programs.SymbolInDelay.P2'(K,B))))).
'Programs.Succeed.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Programs':'Programs.Succeed.P3.0'(A,B,C)).
'~Programs.Succeed.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Programs':'~Programs.Succeed.P3.0'(A,B,C)).
'Programs.Succeed.P3.0'(A, B, C) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, D),
        'SharedPrograms':'SharedPrograms.ProgramLanguage.P2'(D, E),
        'SharedPrograms':'SharedPrograms.MeltedBodyTyping.P8'(B, E, [], _, _, [], [], []),
        'Programs.CompileObjectProgram.P1'(D),
        'Syntax':'Syntax.FormulaMaxVarIndex.P2'([B], F),
        'Syntax':'Syntax.EmptyTermSubst.P1'(G),
        'Programs.RunGoal.P6'(B, F, G, C, _, _).
'~Programs.Succeed.P3.0'(A, B, C) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, D),
        'SharedPrograms':'~SharedPrograms.ProgramLanguage.P2'(D, E),
        'SharedPrograms':'~SharedPrograms.MeltedBodyTyping.P8'(B, E, [], _, _, [], [], []),
        '~Programs.CompileObjectProgram.P1'(D),
        'Syntax':'~Syntax.FormulaMaxVarIndex.P2'([B], F),
        'Syntax':'~Syntax.EmptyTermSubst.P1'(G),
        '~Programs.RunGoal.P6'(B, F, G, C, _, _).
'Programs.StringToProgramType.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'Programs.StringToProgramType.P4.0'(A,B,C,D)).
'~Programs.StringToProgramType.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Programs':'~Programs.StringToProgramType.P4.0'(A,B,C,D)).
'Programs.StringToProgramType.P4.0'(A, B, C, D) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, E),
        'Programs.StringToProgramTypeAux.P4'(E, B, C, D).
'~Programs.StringToProgramType.P4.0'(A, B, C, D) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, E),
        '~Programs.StringToProgramTypeAux.P4'(E, B, C, D).
'Programs.SucceedAll.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Programs':'Programs.SucceedAll.P3.0'(A,B,C)).
'~Programs.SucceedAll.P3'(A, B, C) :-
        user:goedel_freeze(ground([A,B]), 'Programs':'~Programs.SucceedAll.P3.0'(A,B,C)).
'Programs.SucceedAll.P3.0'(A, B, C) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, D),
        'SharedPrograms':'SharedPrograms.ProgramLanguage.P2'(D, E),
        'SharedPrograms':'SharedPrograms.MeltedBodyTyping.P8'(B, E, [], _, _, [], [], []),
        'Programs.CompileObjectProgram.P1'(D),
        'Syntax':'Syntax.FormulaMaxVarIndex.P2'([B], F),
        'Syntax':'Syntax.EmptyTermSubst.P1'(G),
        'Programs.RunAllGoal.P6'(B, F, G, C, _, _).
'~Programs.SucceedAll.P3.0'(A, B, C) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, D),
        'SharedPrograms':'~SharedPrograms.ProgramLanguage.P2'(D, E),
        'SharedPrograms':'~SharedPrograms.MeltedBodyTyping.P8'(B, E, [], _, _, [], [], []),
        '~Programs.CompileObjectProgram.P1'(D),
        'Syntax':'~Syntax.FormulaMaxVarIndex.P2'([B], F),
        'Syntax':'~Syntax.EmptyTermSubst.P1'(G),
        '~Programs.RunAllGoal.P6'(B, F, G, C, _, _).
'Programs.SymbolInTermList.P2'([A|B], C) :-
        (   'Programs.SymbolInTerm.P2'(A, C)
        ;   'Programs.SymbolInTermList.P2'(B, C)
        ).
'~Programs.SymbolInTermList.P2'([A|B], C) :-
        (   '~Programs.SymbolInTerm.P2'(A, C)
        ;   '~Programs.SymbolInTermList.P2'(B, C)
        ).
'Programs.SymbolInFormula.P2'('MetaDefs.PAtom.F1'(A), A).
'~Programs.SymbolInFormula.P2'('MetaDefs.PAtom.F1'(A), A).
'Programs.SymbolInFormula.P2'('MetaDefs.Atom.F2'(A,B), C) :-
        (   C=A
        ;   'Programs.SymbolInTermList.P2'(B, C)
        ).
'~Programs.SymbolInFormula.P2'('MetaDefs.Atom.F2'(A,B), C) :-
        (   C=A
        ;   '~Programs.SymbolInTermList.P2'(B, C)
        ).
'Programs.SymbolInFormula.P2'('MetaDefs.~''.F1'(A), B) :-
        'Programs.SymbolInFormula.P2'(A, B).
'~Programs.SymbolInFormula.P2'('MetaDefs.~''.F1'(A), B) :-
        '~Programs.SymbolInFormula.P2'(A, B).
'Programs.SymbolInFormula.P2'('MetaDefs.&''.F2'(A,B), C) :-
        (   'Programs.SymbolInFormula.P2'(A, C)
        ;   'Programs.SymbolInFormula.P2'(B, C)
        ).
'~Programs.SymbolInFormula.P2'('MetaDefs.&''.F2'(A,B), C) :-
        (   '~Programs.SymbolInFormula.P2'(A, C)
        ;   '~Programs.SymbolInFormula.P2'(B, C)
        ).
'Programs.SymbolInFormula.P2'('MetaDefs.\/''.F2'(A,B), C) :-
        (   'Programs.SymbolInFormula.P2'(A, C)
        ;   'Programs.SymbolInFormula.P2'(B, C)
        ).
'~Programs.SymbolInFormula.P2'('MetaDefs.\/''.F2'(A,B), C) :-
        (   '~Programs.SymbolInFormula.P2'(A, C)
        ;   '~Programs.SymbolInFormula.P2'(B, C)
        ).
'Programs.SymbolInFormula.P2'('MetaDefs.->''.F2'(A,B), C) :-
        (   'Programs.SymbolInFormula.P2'(A, C)
        ;   'Programs.SymbolInFormula.P2'(B, C)
        ).
'~Programs.SymbolInFormula.P2'('MetaDefs.->''.F2'(A,B), C) :-
        (   '~Programs.SymbolInFormula.P2'(A, C)
        ;   '~Programs.SymbolInFormula.P2'(B, C)
        ).
'Programs.SymbolInFormula.P2'('MetaDefs.<-''.F2'(A,B), C) :-
        (   'Programs.SymbolInFormula.P2'(A, C)
        ;   'Programs.SymbolInFormula.P2'(B, C)
        ).
'~Programs.SymbolInFormula.P2'('MetaDefs.<-''.F2'(A,B), C) :-
        (   '~Programs.SymbolInFormula.P2'(A, C)
        ;   '~Programs.SymbolInFormula.P2'(B, C)
        ).
'Programs.SymbolInFormula.P2'('MetaDefs.<->''.F2'(A,B), C) :-
        (   'Programs.SymbolInFormula.P2'(A, C)
        ;   'Programs.SymbolInFormula.P2'(B, C)
        ).
'~Programs.SymbolInFormula.P2'('MetaDefs.<->''.F2'(A,B), C) :-
        (   '~Programs.SymbolInFormula.P2'(A, C)
        ;   '~Programs.SymbolInFormula.P2'(B, C)
        ).
'Programs.SymbolInFormula.P2'('MetaDefs.All.F2'(_,A), B) :-
        'Programs.SymbolInFormula.P2'(A, B).
'~Programs.SymbolInFormula.P2'('MetaDefs.All.F2'(_,A), B) :-
        '~Programs.SymbolInFormula.P2'(A, B).
'Programs.SymbolInFormula.P2'('MetaDefs.Some.F2'(_,A), B) :-
        'Programs.SymbolInFormula.P2'(A, B).
'~Programs.SymbolInFormula.P2'('MetaDefs.Some.F2'(_,A), B) :-
        '~Programs.SymbolInFormula.P2'(A, B).
'Programs.SymbolInFormula.P2'('MetaDefs.ISTE.F4'(_,A,B,C), D) :-
        (   'Programs.SymbolInFormula.P2'(A, D)
        ;   'Programs.SymbolInFormula.P2'(B, D)
        ;   'Programs.SymbolInFormula.P2'(C, D)
        ).
'~Programs.SymbolInFormula.P2'('MetaDefs.ISTE.F4'(_,A,B,C), D) :-
        (   '~Programs.SymbolInFormula.P2'(A, D)
        ;   '~Programs.SymbolInFormula.P2'(B, D)
        ;   '~Programs.SymbolInFormula.P2'(C, D)
        ).
'Programs.SymbolInFormula.P2'('MetaDefs.ITE.F3'(A,B,C), D) :-
        (   'Programs.SymbolInFormula.P2'(A, D)
        ;   'Programs.SymbolInFormula.P2'(B, D)
        ;   'Programs.SymbolInFormula.P2'(C, D)
        ).
'~Programs.SymbolInFormula.P2'('MetaDefs.ITE.F3'(A,B,C), D) :-
        (   '~Programs.SymbolInFormula.P2'(A, D)
        ;   '~Programs.SymbolInFormula.P2'(B, D)
        ;   '~Programs.SymbolInFormula.P2'(C, D)
        ).
'Programs.SymbolInFormula.P2'('MetaDefs.IST.F3'(_,A,B), C) :-
        (   'Programs.SymbolInFormula.P2'(A, C)
        ;   'Programs.SymbolInFormula.P2'(B, C)
        ).
'~Programs.SymbolInFormula.P2'('MetaDefs.IST.F3'(_,A,B), C) :-
        (   '~Programs.SymbolInFormula.P2'(A, C)
        ;   '~Programs.SymbolInFormula.P2'(B, C)
        ).
'Programs.SymbolInFormula.P2'('MetaDefs.IT.F2'(A,B), C) :-
        (   'Programs.SymbolInFormula.P2'(A, C)
        ;   'Programs.SymbolInFormula.P2'(B, C)
        ).
'~Programs.SymbolInFormula.P2'('MetaDefs.IT.F2'(A,B), C) :-
        (   '~Programs.SymbolInFormula.P2'(A, C)
        ;   '~Programs.SymbolInFormula.P2'(B, C)
        ).
'Programs.SymbolInDelay.P2'('ProgDefs.Delay.F2'(A,_), B) :-
        'Programs.SymbolInFormula.P2'(A, B).
'~Programs.SymbolInDelay.P2'('ProgDefs.Delay.F2'(A,_), B) :-
        '~Programs.SymbolInFormula.P2'(A, B).
'Programs.SymbolInTerm.P2'('MetaDefs.CTerm.F1'(A), A).
'~Programs.SymbolInTerm.P2'('MetaDefs.CTerm.F1'(A), A).
'Programs.SymbolInTerm.P2'('MetaDefs.Term.F2'(A,B), C) :-
        (   A=C
        ;   'Programs.SymbolInTermList.P2'(B, C)
        ).
'~Programs.SymbolInTerm.P2'('MetaDefs.Term.F2'(A,B), C) :-
        (   A=C
        ;   '~Programs.SymbolInTermList.P2'(B, C)
        ).
'Programs.SymbolInTerm.P2'('MetaDefs.SuchThat.F2'(A,B), C) :-
        (   'Programs.SymbolInTerm.P2'(A, C)
        ;   'Programs.SymbolInFormula.P2'(B, C)
        ).
'~Programs.SymbolInTerm.P2'('MetaDefs.SuchThat.F2'(A,B), C) :-
        (   '~Programs.SymbolInTerm.P2'(A, C)
        ;   '~Programs.SymbolInFormula.P2'(B, C)
        ).
'Programs.TermInModule.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C,D,E]), 'Programs':'Programs.TermInModule.P7.0'(A,B,C,D,E,F,G)).
'~Programs.TermInModule.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A,B,C,D,E]), 'Programs':'~Programs.TermInModule.P7.0'(A,B,C,D,E,F,G)).
'Programs.TermInModule.P7.0'(A, B, C, D, E, F, G) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, H),
        'Programs.CModulePart.P2'(C, I),
        'Programs.ModulePartLanguage.P4'(I, B, H, J),
        'SharedPrograms':'SharedPrograms.TermInLanguage.P5'(E, J, D, G, F).
'~Programs.TermInModule.P7.0'(A, B, C, D, E, F, G) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, H),
        '~Programs.CModulePart.P2'(C, I),
        '~Programs.ModulePartLanguage.P4'(I, B, H, J),
        'SharedPrograms':'~SharedPrograms.TermInLanguage.P5'(E, J, D, G, F).
'Programs.TypeInModule.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C,D]), 'Programs':'Programs.TypeInModule.P4.0'(A,B,C,D)).
'~Programs.TypeInModule.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C,D]), 'Programs':'~Programs.TypeInModule.P4.0'(A,B,C,D)).
'Programs.TypeInModule.P4.0'(A, B, C, D) :-
        'ProgramCache':'ProgramCache.UnCacheProgram.P2'(A, E),
        'Programs.CModulePart.P2'(C, F),
        'Programs.ModulePartLanguage.P4'(F, B, E, G),
        'SharedPrograms':'SharedPrograms.TypeInLanguage.P2'(D, G).
'~Programs.TypeInModule.P4.0'(A, B, C, D) :-
        'ProgramCache':'~ProgramCache.UnCacheProgram.P2'(A, E),
        '~Programs.CModulePart.P2'(C, F),
        '~Programs.ModulePartLanguage.P4'(F, B, E, G),
        'SharedPrograms':'~SharedPrograms.TypeInLanguage.P2'(D, G).
'Programs.TopLevelName.P2'(A, B) :-
        user:goedel_freeze(nonvar(A), 'Programs':'Programs.TopLevelName.P2.0'(A,B)).
'~Programs.TopLevelName.P2'(A, B) :-
        user:goedel_freeze(nonvar(A), 'Programs':'~Programs.TopLevelName.P2.0'(A,B)).
'Programs.TopLevelName.P2.0'('MetaDefs.PAtom.F1'(A), A).
'~Programs.TopLevelName.P2.0'('MetaDefs.PAtom.F1'(A), A).
'Programs.TopLevelName.P2.0'('MetaDefs.Atom.F2'(A,_), A).
'~Programs.TopLevelName.P2.0'('MetaDefs.Atom.F2'(A,_), A).
'Programs.TopLevelName.P2.0'('MetaDefs.XPAtom.F1'(A), A).
'~Programs.TopLevelName.P2.0'('MetaDefs.XPAtom.F1'(A), A).
'Programs.TopLevelName.P2.0'('MetaDefs.XAtom.F2'(A,_), A).
'~Programs.TopLevelName.P2.0'('MetaDefs.XAtom.F2'(A,_), A).
'Programs.TrueCondition.P1'('ProgDefs.TrueCond.C0').
'~Programs.TrueCondition.P1'('ProgDefs.TrueCond.C0').
'Programs.TypeSymbolInType.P2'('MetaDefs.BType.F1'(A), A).
'~Programs.TypeSymbolInType.P2'('MetaDefs.BType.F1'(A), A).
'Programs.TypeSymbolInType.P2'('MetaDefs.Type.F2'(A,B), C) :-
        (   A=C
        ;   'Programs.TypeSymbolInTypeList.P2'(B, C)
        ).
'~Programs.TypeSymbolInType.P2'('MetaDefs.Type.F2'(A,B), C) :-
        (   A=C
        ;   '~Programs.TypeSymbolInTypeList.P2'(B, C)
        ).
'Programs.TypeSymbolFreeInModule.P3'(A, B, 'ProgDefs.Program.F4'(_,_,'ProgDefs.Language.F1'(C),_)) :-
        'AVLTrees':'AVLTrees.AVLSearch.P3'(C, A, 'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(_,D),_)),
        user:goedel_freeze(ground([B,D]), user:goedel_not(user:goedel_freeze(ground([F,B,D]),user:goedel_not(((user:goedel_freeze(ground([E,D]),user:goedel_not('AVLTrees':'~AVLTrees.AVLMember.P3'(D,_,E)));user:goedel_freeze(ground([F,E]),user:goedel_not('Lists':'~Lists.Member.P2'('ProgDefs.Symbol.F2'(_,F),E))));user:goedel_freeze(ground([F,B]),user:goedel_not('Programs':'~Programs.TypeSymbolInDeclaration.P2'(F,B)))))))).
'~Programs.TypeSymbolFreeInModule.P3'(A, B, 'ProgDefs.Program.F4'(_,_,'ProgDefs.Language.F1'(C),_)) :-
        'AVLTrees':'~AVLTrees.AVLSearch.P3'(C, A, 'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(_,D),_)),
        user:goedel_freeze(ground([B,D]), user:goedel_not(user:goedel_freeze(ground([F,B,D]),user:goedel_not(((user:goedel_freeze(ground([E,D]),user:goedel_not('AVLTrees':'~AVLTrees.AVLMember.P3'(D,_,E)));user:goedel_freeze(ground([F,E]),user:goedel_not('Lists':'~Lists.Member.P2'('ProgDefs.Symbol.F2'(_,F),E))));user:goedel_freeze(ground([F,B]),user:goedel_not('Programs':'~Programs.TypeSymbolInDeclaration.P2'(F,B)))))))).
'Programs.TypeInProgram.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Programs':'Programs.TypeInProgram.P2.0'(A,B)).
'~Programs.TypeInProgram.P2'(A, B) :-
        user:goedel_freeze(ground([A,B]), 'Programs':'~Programs.TypeInProgram.P2.0'(A,B)).
'Programs.TypeInProgram.P2.0'('ProgDefs.CachedProgram.F1'(A), B) :-
        'ProgramCache':'ProgramCache.GetCachedProgram.P2'(A, C),
        'Programs.TypeInProgram.P2'(C, B).
'~Programs.TypeInProgram.P2.0'('ProgDefs.CachedProgram.F1'(A), B) :-
        'ProgramCache':'~ProgramCache.GetCachedProgram.P2'(A, C),
        '~Programs.TypeInProgram.P2'(C, B).
'Programs.TypeInProgram.P2.0'('ProgDefs.Program.F4'(_,_,A,_), B) :-
        'SharedPrograms':'SharedPrograms.TypeInLanguage.P2'(B, A).
'~Programs.TypeInProgram.P2.0'('ProgDefs.Program.F4'(_,_,A,_), B) :-
        'SharedPrograms':'~SharedPrograms.TypeInLanguage.P2'(B, A).
'Programs.TypeSymbolInDeclaration.P2'('ProgDefs.ConstantDecl.F1'(A), B) :-
        'Programs.TypeSymbolInType.P2'(A, B).
'~Programs.TypeSymbolInDeclaration.P2'('ProgDefs.ConstantDecl.F1'(A), B) :-
        '~Programs.TypeSymbolInType.P2'(A, B).
'Programs.TypeSymbolInDeclaration.P2'('ProgDefs.FunctionDecl.F4'(_,_,A,B), C) :-
        (   'Programs.TypeSymbolInTypeList.P2'(A, C)
        ;   'Programs.TypeSymbolInType.P2'(B, C)
        ).
'~Programs.TypeSymbolInDeclaration.P2'('ProgDefs.FunctionDecl.F4'(_,_,A,B), C) :-
        (   '~Programs.TypeSymbolInTypeList.P2'(A, C)
        ;   '~Programs.TypeSymbolInType.P2'(B, C)
        ).
'Programs.TypeSymbolInDeclaration.P2'('ProgDefs.PredicateDecl.F3'(_,_,A), B) :-
        'Programs.TypeSymbolInTypeList.P2'(A, B).
'~Programs.TypeSymbolInDeclaration.P2'('ProgDefs.PredicateDecl.F3'(_,_,A), B) :-
        '~Programs.TypeSymbolInTypeList.P2'(A, B).
'Programs.UnreferencedModule.P2'(A, B) :-
        user:goedel_freeze(ground([E,A,B]), user:goedel_not(('AVLTrees':'~AVLTrees.AVLMember.P3'(B,_,'ProgDefs.ModDef.F4'(_,C,D,E)),('Lists':'~Lists.Member.P2'(A,D);'Lists':'~Lists.Member.P2'(A,C);'Lists':'~Lists.Member.P2'(A,E))))).
'~Programs.UnreferencedModule.P2'(A, B) :-
        user:goedel_freeze(ground([E,A,B]), user:goedel_not(('AVLTrees':'~AVLTrees.AVLMember.P3'(B,_,'ProgDefs.ModDef.F4'(_,C,D,E)),('Lists':'~Lists.Member.P2'(A,D);'Lists':'~Lists.Member.P2'(A,C);'Lists':'~Lists.Member.P2'(A,E))))).
'Programs.TypeSymbolInTypeList.P2'([A|B], C) :-
        (   'Programs.TypeSymbolInType.P2'(A, C)
        ;   'Programs.TypeSymbolInTypeList.P2'(B, C)
        ).
'~Programs.TypeSymbolInTypeList.P2'([A|B], C) :-
        (   '~Programs.TypeSymbolInType.P2'(A, C)
        ;   '~Programs.TypeSymbolInTypeList.P2'(B, C)
        ).
'Programs.ValidDeclaration.P4'('ProgDefs.BaseDecl.C0', _, _, _).
'~Programs.ValidDeclaration.P4'('ProgDefs.BaseDecl.C0', _, _, _).
'Programs.ValidDeclaration.P4'('ProgDefs.ConstructorDecl.F1'(_), _, _, _).
'~Programs.ValidDeclaration.P4'('ProgDefs.ConstructorDecl.F1'(_), _, _, _).
'Programs.ValidDeclaration.P4'('ProgDefs.ConstantDecl.F1'(A), B, C, D) :-
        'Programs.TypeInModule.P4'(B, C, D, A),
        'Programs.RightHandType.P4'(A, B, C, D).
'~Programs.ValidDeclaration.P4'('ProgDefs.ConstantDecl.F1'(A), B, C, D) :-
        '~Programs.TypeInModule.P4'(B, C, D, A),
        '~Programs.RightHandType.P4'(A, B, C, D).
'Programs.ValidDeclaration.P4'('ProgDefs.FunctionDecl.F4'(_,_,A,B), C, D, E) :-
        'Programs.ListTypesInModule.P4'(C, D, E, [B|A]),
        'Programs.RightHandType.P4'(B, C, D, E),
        'Programs.Transparent.P2'(A, B).
'~Programs.ValidDeclaration.P4'('ProgDefs.FunctionDecl.F4'(_,_,A,B), C, D, E) :-
        '~Programs.ListTypesInModule.P4'(C, D, E, [B|A]),
        '~Programs.RightHandType.P4'(B, C, D, E),
        '~Programs.Transparent.P2'(A, B).
'Programs.ValidDeclaration.P4'('ProgDefs.PropositionDecl.C0', _, _, _).
'~Programs.ValidDeclaration.P4'('ProgDefs.PropositionDecl.C0', _, _, _).
'Programs.ValidDeclaration.P4'('ProgDefs.PredicateDecl.F3'(_,_,A), B, C, D) :-
        'Programs.ListTypesInModule.P4'(B, C, D, A).
'~Programs.ValidDeclaration.P4'('ProgDefs.PredicateDecl.F3'(_,_,A), B, C, D) :-
        '~Programs.ListTypesInModule.P4'(B, C, D, A).
%------------------------------------------------------------------------------
% Supplementary Prolog routines for system module Program
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% String to Ground Representation conversion
%------------------------------------------------------------------------------

'Programs.StringToProgramTypeAux.P4'(Program, Module, String, [Type]) :-
   name(String, [0'"|Chars]),
   user:append(Chars, [0'.], Chars1),
   user:get_one_item_2nd_aux(Chars1, Remains, Tokens),
   Remains = [],
   user:'ParserPrograms.LocalSymbolTable.P3'(Program, Module, SymbolTable),
   user:declared_type(Tokens, RestTokens, [], Type, _ModuleName, 0, 0,
			SymbolTable, donot_print),
   Type \== null,
   RestTokens = [].

'~Programs.StringToProgramTypeAux.P4'(Program, Module, String, Types) :-
   'Programs.StringToProgramTypeAux.P4'(Program, Module, String, Types).

%------------------------------------------------------------------------------
 
'Programs.StringToProgramTermAux.P4'(Program, Module, String, Terms) :-
   name(String, [0'"|Chars]),
   user:append(Chars, [0'.], Chars1),
   user:get_one_item_2nd_aux(Chars1, Remains, Tokens),
   Remains = [],
   user:'ParserPrograms.LocalSymbolTable.P3'(Program, Module, SymbolTable),
   user:term(Tokens, Return, SymbolTable),
   ( user:select_return(Return, null, Terms2, Remains, _ErrorReturn)
     -> Remains = []
     ;  format(user_error, 'Internal error No 29.~n', [])
   ),
   Terms2 = [_|_],
   user:'SharedPrograms.ProgramLanguage.P2'(Program, Language),
   user:'ParserPrograms.CheckTermAux.P6'(Terms2, Language, 0, [], Terms, []).

'~Programs.StringToProgramTermAux.P4'(Program, Module, String, Terms) :-
   'Programs.StringToProgramTermAux.P4'(Program, Module, String, Terms).

%------------------------------------------------------------------------------
 
% OFormula ::= Statement | Body | Goal
% NB: the colon notation for Goal is not accepted

'Programs.StringToProgramFormulaAux.P4'(Program, Module, String, Formulae) :-
   name(String, [0'"|Chars]),
   user:append(Chars, [0'.], Chars1),
   user:get_one_item_2nd_aux(Chars1, Rest, Tokens),
   Rest= [],
   user:'ParserPrograms.LocalSymbolTable.P3'(Program, Module, SymbolTable),
   ( Tokens = [graphic_name('<-')|Tokens2]
     -> ( Tokens2 = []
	  -> CFormulae = ['MetaDefs.<-''.F2'('MetaDefs.Empty.C0','MetaDefs.Empty.C0')]
	  ;  user:body(Tokens2, CFormulae2, _ErrorReturn1, SymbolTable),
             CFormulae2 = [_|_],
	     user:create_formula('MetaDefs.<-''.F2',
			[['MetaDefs.Empty.C0'], CFormulae2], CFormulae)
	)
     ;  user:headatom(Tokens, Remains, Module, HeadAtoms, _ErrorReturn2,
			SymbolTable),
	( ( HeadAtoms = [];
	    \+ Remains = [graphic_name('<-')|_] % this covers the case of atom
	  )
	  -> user:body(Tokens, CFormulae, _ErrorReturn3, SymbolTable),
             CFormulae = [_|_]
	  ;  Remains = [graphic_name('<-')|Remains2],
	     ( Remains2 = []
	       -> user:create_formula('MetaDefs.<-''.F2',
				[HeadAtoms, ['MetaDefs.Empty.C0']], CFormulae)
	       ;  user:body(Remains2, BodyFormulae, _ErrorReturn4, SymbolTable),
                  BodyFormulae = [_|_],
		  user:create_formula('MetaDefs.<-''.F2',
				[HeadAtoms, BodyFormulae], CFormulae)
	     )
	)
   ),
   user:'SharedPrograms.ProgramLanguage.P2'(Program, Language),
   user:'ParserPrograms.CheckFormulaAux.P6'(CFormulae, Language, 0, []
      , Formulae, _),
   Formulae = [_|_]. % Fail if no parses are well-typed

'~Programs.StringToProgramFormulaAux.P4'(Program, Module, String, Formulae) :-
   'Programs.StringToProgramFormulaAux.P4'(Program, Module, String, Formulae).

%------------------------------------------------------------------------------
% New Program
%------------------------------------------------------------------------------

% problem: AVLTrees is taken as a system module and loaded automatically

'Programs.NewProgramAux.P2'(Program, MainModule) :-
   ( user:system_module_name(MainModule) -> 
     user:gstring2string(MainModule, Str),
     user:load_system_module(Str, Program)
   ; user:'SharedPrograms.NextModuleVersion.P2'(MainModule, Version),
     user:'SharedPrograms.ProgramSkeleton.P4'(MainModule
        , 'ProgDefs.NormalKind.C0', Version, Program)
   ).

'~Programs.NewProgramAux.P2'(Program, MainModule) :-
   'Programs.NewProgramAux.P2'(Program, MainModule).

%------------------------------------------------------------------------------
% Transparency check
%------------------------------------------------------------------------------

'Programs.Transparent.P2'(DomainType, RangeType) :-
   user:transparency_check(DomainType, RangeType).

'~Programs.Transparent.P2'(DomainType, RangeType) :-
   user:transparency_check(DomainType, RangeType).

%------------------------------------------------------------------------------
% Module manipulation
%------------------------------------------------------------------------------

'Programs.ModuleInProgramAux.P2'(Modules, Module) :-
   ( var(Module) ->
     (user:'AVLTrees.AVLMember.P3'(Modules, Module, _); Module = '"')
   ; ( Module == '"' ->
       true
     ; user:'AVLTrees.AVLSearch.P3'(Modules, Module, _)
     )
   ).

'~Programs.ModuleInProgramAux.P2'(Modules, Module) :-
   'Programs.ModuleInProgramAux.P2'(Modules, Module).

%------------------------------------------------------------------------------
% Support for Succeed (primitive version)
%------------------------------------------------------------------------------

:- dynamic compiled_module_version/2.

'Programs.EnsureCompiled.P3'(Module, Version, Code) :-
   ( compiled_module_version(Module, Version) ->
     true
   ; user:compile_object_module(Module, Code),
     retractall(compiled_module_version(Module, _)),
     assert(compiled_module_version(Module, Version))
   ).   

'~Programs.EnsureCompiled.P3'(Module, Version, Code) :-
   'Programs.EnsureCompiled.P3'(Module, Version, Code).


% 'Programs.EnsureLoaded.P1'(GModule) :-
%    ( user:loaded(GModule) ->
%      true
%    ; user:gstring2string(GModule, Module),
%      user:file_finder(Module, _, PrologFile, _),
%      nofileerrors,
%      ( load(PrologFile) ->
%        assert(user:loaded(GModule))
%      ; format(user_error, 'Succeed: can''t find file ~a~n', [PrologFile])
%      ),
%      fileerrors
%    ).
% 
% '~Programs.EnsureLoaded.P1'(GModule) :-
%    'Programs.EnsureLoaded.P1'(GModule).

'Programs.EnsureLoaded.P1'(M) :-
   user:system_module_name(M).

'~Programs.EnsureLoaded.P1'(M) :-
   'Programs.EnsureLoaded.P1'(M).


'Programs.RunAllGoal.P6'(Goal, FirstFree, InputSubst, Answers, LastFree
      , Residues
      ) :-
   user:compile_object_goal(Goal, ReflectedGoal, FreeVars, VarDict, []),
   findall( (VarDict, Suspended), 
            execute_goal(ReflectedGoal, Suspended),
            Results
          ),
   make_results(Results, FreeVars, FirstFree, InputSubst, FirstFree, Answers
      , Residues, LastFree).

'~Programs.RunAllGoal.P6'(Goal, FirstFree, InputSubst, Answers, LastFree
      , Residues
      ) :-
   'Programs.RunAllGoal.P6'(Goal, FirstFree, InputSubst, Answers, LastFree
      , Residues).


make_results([], _, _, _, MaxFree, [], [], MaxFree).

make_results([(VarDict, Unsolved)|Results], FreeVars, FirstFree, InputSubst
      , MaxFreeSoFar, [Answer|Answers], [Residue|Residues], MaxFree
      ) :-
   make_answer_subst(FreeVars, VarDict, FirstFree, InputSubst, Answer
      , Free1),
   make_residue(Unsolved, Free1, Residue, NextFree),
   ( NextFree > MaxFreeSoFar ->
     NewMaxSoFar = NextFree
   ; NewMaxSoFar = MaxFreeSoFar
   ),
   make_results(Results, FreeVars, FirstFree, InputSubst, NewMaxSoFar
      , Answers, Residues, MaxFree).


'Programs.RunGoal.P6'(Goal, FirstFree, InputSubst, Answer, LastFree
      , Residue
      ) :-
   user:compile_object_goal(Goal, ReflectedGoal, FreeVars, VarDict, []),
   execute_goal(ReflectedGoal, Unsolved),
   make_answer_subst(FreeVars, VarDict, FirstFree, InputSubst, Answer
      , Free1),
   make_residue(Unsolved, Free1, Residue, LastFree).

'~Programs.RunGoal.P6'(Goal, FirstFree, InputSubst, Answer, LastFree
      , Residue
      ) :-
   'Programs.RunGoal.P6'(Goal, FirstFree, InputSubst, Answer, LastFree
      , Residue).


'Programs.FailedGoal.P1'(Goal) :-
   user:compile_object_goal(Goal, ReflectedGoal, _, _, [0'~]),
   execute_goal(\+ ReflectedGoal, []).

'~Programs.FailedGoal.P1'(Goal) :-
   'Programs.FailedGoal.P1'(Goal).


execute_goal(ReflectedGoal, Suspended) :-
   on_exception(catch_in_query, 
      call_residue(ReflectedGoal, Suspended),
      fail).                               % fail if an exception occurs


make_answer_subst([], _, Free, Answer, Answer, Free).

make_answer_subst([Var|Vars], VarDict, VarNumber, SubstSoFar, Answer
      , NewVarNumber
      ) :-
   user:member(Var/Value, VarDict), !,
   user:pobj_to_ground(Value, VarNumber, VarNumber1, GroundTerm),
   'Substs':'Substs.BindVariable.P4'(Var, GroundTerm, SubstSoFar, SubstSoFar1),
   make_answer_subst(Vars, VarDict, VarNumber1, SubstSoFar1, Answer
      , NewVarNumber).


make_residue(Unsolved, FirstFree, Residue, LastFree) :-
   clean_unsolved_goals(Unsolved, CleanUnsolved),
   make_residue1(CleanUnsolved, FirstFree, Residue, LastFree).


make_residue1([], LastFree, 'MetaDefs.Empty.C0', LastFree).

make_residue1([Goal|Goals], FirstFree, Residue, LastFree) :-
   patom_to_ground(Goal, FirstFree, Ground, Free1),
   make_residue2(Goals, Free1, Ground, Residue, LastFree).


make_residue2([], LastFree, Residue, Residue, LastFree).

make_residue2([Goal|Goals], FirstFree, SoFar, Residue, LastFree) :-
   patom_to_ground(Goal, FirstFree, Ground, Free1),
   make_residue2(Goals, Free1, 'MetaDefs.&''.F2'(SoFar, Ground), Residue
      , LastFree).


patom_to_ground(Atom, VarNo, Ground, NewVarNo) :-
   ( Atom = not_equal(_, Term1, Term2) ->
     user:list_pobj_to_ground([Term1, Term2], VarNo, NewVarNo, GArgs),
     Ground = 'MetaDefs.Atom.F2'('MetaDefs.Name.F4'('"', '"~='
        , 'MetaDefs.Predicate.C0', 2), GArgs)
   ; Atom = Module:RealAtom ->   % deals with constraints
     RealAtom =.. [Predicate|ExtendedArgs],
     remove_last(ExtendedArgs, Args, Last),
     constraint_goal_conversion(Module, Predicate, GSymbol, Arity),
     user:string2Gstring(Module, GModule),
     Name = 'MetaDefs.Name.F4'(GModule, GSymbol, 'MetaDefs.Function.C0', Arity),
     Ground = 'MetaDefs.Atom.F2'('MetaDefs.Name.F4'('"','"=',
                                         'MetaDefs.Predicate.C0',2),
                          [Left, 'MetaDefs.Term.F2'(Name, GArgs)]),
     user:pobj_to_ground(Last, VarNo, VarNo2, Left),
     user:list_pobj_to_ground(Args, VarNo2, NewVarNo, GArgs)      
   ; Atom =.. [Predicate|Args],
     user:string2Gstring(Predicate, PredString),
     user:symbol_name(Name, PredString),
     ( Args = [] ->
       Ground = 'MetaDefs.PAtom.F1'(Name),
       NewVarNo = VarNo
     ; Ground = 'MetaDefs.Atom.F2'(Name, GArgs),
       user:list_pobj_to_ground(Args, VarNo, NewVarNo, GArgs)
     )
   ).

remove_last([X], [], X) :- !.
remove_last([X|Xs], [X|Ys], Z) :-
   remove_last(Xs, Ys, Z).

clean_unsolved_goals([], []).

clean_unsolved_goals([_-Goal|Goals], NewGoals) :-
   clean_unsolved_goals_aux(Goal, NewGoal),
   ( NewGoal = null
     -> NewGoals = NewGoals2
     ;  NewGoals = [NewGoal|NewGoals2]
   ),
   clean_unsolved_goals(Goals, NewGoals2).


clean_unsolved_goals_aux(freeze_on_vlist(_, _, _, _), null) :- !.
clean_unsolved_goals_aux(_:freeze_on_vlist(_, _, _, _), null) :- !.

clean_unsolved_goals_aux(freeze_nonvar_and(_, _, _), null) :- !.
clean_unsolved_goals_aux(_:freeze_nonvar_and(_, _, _), null) :- !.

clean_unsolved_goals_aux(evaluate_delay_aux(_, _), null) :- !.
clean_unsolved_goals_aux(_:evaluate_delay_aux(_, _), null) :- !.

clean_unsolved_goals_aux(my_freeze(_, _:Goal, _), NewGoal) :- !,
   clean_unsolved_goals_aux(Goal, NewGoal).
clean_unsolved_goals_aux(_:my_freeze(_, _:Goal, _), NewGoal) :- !,
   clean_unsolved_goals_aux(Goal, NewGoal).

clean_unsolved_goals_aux(call(Goal), NewGoal) :- !,
   clean_unsolved_goals_aux(Goal, NewGoal).
clean_unsolved_goals_aux(_:call(Goal), NewGoal) :- !,
   clean_unsolved_goals_aux(Goal, NewGoal).

clean_unsolved_goals_aux(Module:Goal, NewGoal) :- !,
   ( constraint_goal(Module, Goal) ->
     NewGoal = Module:Goal
   ; clean_unsolved_goals_aux(Goal, NewGoal)
   ).

clean_unsolved_goals_aux(Goal, Goal) :- !.


constraint_goal(Module, Goal) :-
   module_with_constraints(Module),
   Goal =.. [Predicate|_],
   constraint_goal_conversion(Module, Predicate, _, _).


module_with_constraints('Integers').
module_with_constraints('Rationals').
module_with_constraints('Strings').
module_with_constraints('Sets').

constraint_goal_conversion('Integers', negative, '"-', 1):- !.
constraint_goal_conversion('Integers', minus, '"-', 2):- !.
constraint_goal_conversion('Integers', plus, '"+', 2):- !.
constraint_goal_conversion('Integers', times, '"*', 2):- !.
constraint_goal_conversion('Integers', divides, '"Div', 2):- !.
constraint_goal_conversion('Integers', power, '"^', 2):- !.
constraint_goal_conversion('Integers', mod, '"Mod', 2):- !.
constraint_goal_conversion('Integers', maximum, '"Max', 2):- !.
constraint_goal_conversion('Integers', minimum, '"Min', 2):- !.
constraint_goal_conversion('Integers', absolute, '"Abs', 1):- !.

constraint_goal_conversion('Rationals', negative, '"-', 1):- !.
constraint_goal_conversion('Rationals', minus, '"-', 2):- !.
constraint_goal_conversion('Rationals', plus, '"+', 2):- !.
constraint_goal_conversion('Rationals', times, '"*', 2):- !.
constraint_goal_conversion('Rationals', divides, '"/', 2):- !.
constraint_goal_conversion('Rationals', power, '"^', 2):- !.
constraint_goal_conversion('Rationals', mod, '"Mod', 2):- !.
constraint_goal_conversion('Rationals', maximum, '"Max', 2):- !.
constraint_goal_conversion('Rationals', minimum, '"Min', 2):- !.
constraint_goal_conversion('Rationals', absolute, '"Abs', 1):- !.
constraint_goal_conversion('Rationals', rational, '"//', 2):- !.

constraint_goal_conversion('Sets', normalise, '"Inc', 2):- !. % Wrong!
constraint_goal_conversion('Sets', diff, '"\', 2):- !.          
constraint_goal_conversion('Sets', set_of, '"SuchThat', 3):- !.

constraint_goal_conversion('Strings', concat, '"++', 2):- !.
