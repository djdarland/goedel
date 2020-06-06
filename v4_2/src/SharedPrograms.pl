:- module('SharedPrograms', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

'SharedPrograms.ModulePartInProgram.P3'('ProgDefs.Program.F4'(_,A,_,_), B, C) :-
        'SharedPrograms.AVLFind.P3'(A, B, 'ProgDefs.ModDef.F4'(D,_,_,_)),
        'SharedPrograms.KindHasPart.P2'(D, C).
'~SharedPrograms.ModulePartInProgram.P3'('ProgDefs.Program.F4'(_,A,_,_), B, C) :-
        '~SharedPrograms.AVLFind.P3'(A, B, 'ProgDefs.ModDef.F4'(D,_,_,_)),
        '~SharedPrograms.KindHasPart.P2'(D, C).
'SharedPrograms.InitialiseLanguage.P2'(A, B) :-
        'SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'('"','"True','MetaDefs.Proposition.C0',0), A, 'ProgDefs.Exported.C0', 'ProgDefs.PropositionDecl.C0', C),
        'SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'('"','"False','MetaDefs.Proposition.C0',0), C, 'ProgDefs.Exported.C0', 'ProgDefs.PropositionDecl.C0', D),
        'SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'('"','"=','MetaDefs.Predicate.C0',2), D, 'ProgDefs.Exported.C0', 'ProgDefs.PredicateDecl.F3'(2,'Syntax.ZPZ.C0',['MetaDefs.Par.F1'(0),'MetaDefs.Par.F1'(0)]), E),
        'SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'('"','"~=','MetaDefs.Predicate.C0',2), E, 'ProgDefs.Exported.C0', 'ProgDefs.PredicateDecl.F3'(2,'Syntax.ZPZ.C0',['MetaDefs.Par.F1'(0),'MetaDefs.Par.F1'(0)]), B).
'~SharedPrograms.InitialiseLanguage.P2'(A, B) :-
        '~SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'('"','"True','MetaDefs.Proposition.C0',0), A, 'ProgDefs.Exported.C0', 'ProgDefs.PropositionDecl.C0', C),
        '~SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'('"','"False','MetaDefs.Proposition.C0',0), C, 'ProgDefs.Exported.C0', 'ProgDefs.PropositionDecl.C0', D),
        '~SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'('"','"=','MetaDefs.Predicate.C0',2), D, 'ProgDefs.Exported.C0', 'ProgDefs.PredicateDecl.F3'(2,'Syntax.ZPZ.C0',['MetaDefs.Par.F1'(0),'MetaDefs.Par.F1'(0)]), E),
        '~SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'('"','"~=','MetaDefs.Predicate.C0',2), E, 'ProgDefs.Exported.C0', 'ProgDefs.PredicateDecl.F3'(2,'Syntax.ZPZ.C0',['MetaDefs.Par.F1'(0),'MetaDefs.Par.F1'(0)]), B).
'SharedPrograms.ChainExprToIntDL.P6'('MetaDefs.CTerm.F1'(_), _, _, _, A, A).
'~SharedPrograms.ChainExprToIntDL.P6'('MetaDefs.CTerm.F1'(_), _, _, _, A, A).
'SharedPrograms.ChainExprToIntDL.P6'('MetaDefs.Var.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('"|', F, H),
        'SharedPrograms.TermToIntDL.P9'('MetaDefs.Var.F2'(A,B), C, D, E, _, _, _, H, G).
'~SharedPrograms.ChainExprToIntDL.P6'('MetaDefs.Var.F2'(A,B), C, D, E, F, G) :-
        '~SharedPrograms.CharDL.P3'('"|', F, H),
        '~SharedPrograms.TermToIntDL.P9'('MetaDefs.Var.F2'(A,B), C, D, E, _, _, _, H, G).
'SharedPrograms.ChainExprToIntDL.P6'('MetaDefs.Var.F1'(A), B, C, D, E, F) :-
        'SharedPrograms.CharDL.P3'('"|', E, G),
        'SharedPrograms.TermToIntDL.P9'('MetaDefs.Var.F1'(A), B, C, D, _, _, _, G, F).
'~SharedPrograms.ChainExprToIntDL.P6'('MetaDefs.Var.F1'(A), B, C, D, E, F) :-
        '~SharedPrograms.CharDL.P3'('"|', E, G),
        '~SharedPrograms.TermToIntDL.P9'('MetaDefs.Var.F1'(A), B, C, D, _, _, _, G, F).
'SharedPrograms.ChainExprToIntDL.P6'('MetaDefs.Term.F2'(_,[A,B]), C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('",', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'SharedPrograms.ChainExprToIntDL.P6'(B, C, D, E, I, G).
'~SharedPrograms.ChainExprToIntDL.P6'('MetaDefs.Term.F2'(_,[A,B]), C, D, E, F, G) :-
        '~SharedPrograms.CharDL.P3'('",', F, H),
        '~SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        '~SharedPrograms.ChainExprToIntDL.P6'(B, C, D, E, I, G).
'SharedPrograms.AddOtherModulesAux.P7'([], A, B, C, A, B, C).
'~SharedPrograms.AddOtherModulesAux.P7'([], A, B, C, A, B, C).
'SharedPrograms.AddOtherModulesAux.P7'([A|B], C, D, E, F, G, H) :-
        'SharedPrograms.AddModule.P7'(A, C, D, E, I, J, K),
        'SharedPrograms.AddOtherModulesAux.P7'(B, I, J, K, F, G, H).
'~SharedPrograms.AddOtherModulesAux.P7'([A|B], C, D, E, F, G, H) :-
        '~SharedPrograms.AddModule.P7'(A, C, D, E, I, J, K),
        '~SharedPrograms.AddOtherModulesAux.P7'(B, I, J, K, F, G, H).
'SharedPrograms.AddModule.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A]), ('SharedPrograms':'~SharedPrograms.SystemModule.P1'(A),true->'SharedPrograms':'SharedPrograms.SystemModuleRep.P4'(A,H,I,J),'AVLTrees':'AVLTrees.AVLInsert.P4'(B,A,H,K),'AVLTrees':'AVLTrees.AVLInsert.P4'(C,A,I,L),'AVLTrees':'AVLTrees.AVLInsert.P4'(D,A,'ProgDefs.Code.F2'(0,J),M),'SharedPrograms':'SharedPrograms.AddOtherModules.P7'(H,K,L,M,E,F,G);'AVLTrees':'AVLTrees.AVLInsert.P4'(B,A,'ProgDefs.ModDef.F4'('ProgDefs.NormalKind.C0',[],[],[]),E),F=C,'AVLTrees':'AVLTrees.AVLIsEmpty.P1'(N),'SharedPrograms':'SharedPrograms.NextModuleVersion.P2'(A,O),'AVLTrees':'AVLTrees.AVLInsert.P4'(D,A,'ProgDefs.Code.F2'(O,N),G))).
'~SharedPrograms.AddModule.P7'(A, B, C, D, E, F, G) :-
        user:goedel_freeze(ground([A]), ('SharedPrograms':'~SharedPrograms.SystemModule.P1'(A),true->'SharedPrograms':'~SharedPrograms.SystemModuleRep.P4'(A,H,I,J),'AVLTrees':'~AVLTrees.AVLInsert.P4'(B,A,H,K),'AVLTrees':'~AVLTrees.AVLInsert.P4'(C,A,I,L),'AVLTrees':'~AVLTrees.AVLInsert.P4'(D,A,'ProgDefs.Code.F2'(0,J),M),'SharedPrograms':'~SharedPrograms.AddOtherModules.P7'(H,K,L,M,E,F,G);'AVLTrees':'~AVLTrees.AVLInsert.P4'(B,A,'ProgDefs.ModDef.F4'('ProgDefs.NormalKind.C0',[],[],[]),E),F=C,'AVLTrees':'~AVLTrees.AVLIsEmpty.P1'(N),'SharedPrograms':'~SharedPrograms.NextModuleVersion.P2'(A,O),'AVLTrees':'~AVLTrees.AVLInsert.P4'(D,A,'ProgDefs.Code.F2'(O,N),G))).
'SharedPrograms.AddImportToPart.P6'('ProgDefs.Export.C0', A, B, C, [A|B], C).
'~SharedPrograms.AddImportToPart.P6'('ProgDefs.Export.C0', A, B, C, [A|B], C).
'SharedPrograms.AddImportToPart.P6'('ProgDefs.Closed.C0', A, B, C, [A|B], C).
'~SharedPrograms.AddImportToPart.P6'('ProgDefs.Closed.C0', A, B, C, [A|B], C).
'SharedPrograms.AddImportToPart.P6'('ProgDefs.Local.C0', A, B, C, B, [A|C]).
'~SharedPrograms.AddImportToPart.P6'('ProgDefs.Local.C0', A, B, C, B, [A|C]).
'SharedPrograms.AddImportToPart.P6'('ProgDefs.Module.C0', A, B, C, B, [A|C]).
'~SharedPrograms.AddImportToPart.P6'('ProgDefs.Module.C0', A, B, C, B, [A|C]).
'SharedPrograms.Accessible.P2'('ProgDefs.Exported.C0', 'ProgDefs.Exported.C0').
'~SharedPrograms.Accessible.P2'('ProgDefs.Exported.C0', 'ProgDefs.Exported.C0').
'SharedPrograms.Accessible.P2'('ProgDefs.Hidden.C0', _).
'~SharedPrograms.Accessible.P2'('ProgDefs.Hidden.C0', _).
'SharedPrograms.AddLiftedLanguage.P4'([], _, A, A).
'~SharedPrograms.AddLiftedLanguage.P4'([], _, A, A).
'SharedPrograms.AddLiftedLanguage.P4'(['ProgDefs.Lift.F2'(B,C)|A], D, E, F) :-
        user:goedel_freeze(ground([D,B]), ('Lists':'~Lists.Member.P2'(B,D),true->G=E;'AVLTrees':'AVLTrees.AVLIsEmpty.P1'(H),user:goedel_freeze(ground([H,B,E]),if(('AVLTrees':'~AVLTrees.AVLUpdate.P5'(E,B,'ProgDefs.Module.F3'('ProgDefs.Exported.C0','ProgDefs.Categories.F2'(I,H),[]),J,'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(K,_),_)),true),('AVLTrees':'AVLTrees.AVLJoin.P3'(C,K,I),G=J),'AVLTrees':'AVLTrees.AVLInsert.P4'(E,B,'ProgDefs.Module.F3'('ProgDefs.Exported.C0','ProgDefs.Categories.F2'(C,H),[]),G))))),
        'SharedPrograms.AddLiftedLanguage.P4'(A, D, G, F).
'~SharedPrograms.AddLiftedLanguage.P4'(['ProgDefs.Lift.F2'(B,C)|A], D, E, F) :-
        user:goedel_freeze(ground([D,B]), ('Lists':'~Lists.Member.P2'(B,D),true->G=E;'AVLTrees':'~AVLTrees.AVLIsEmpty.P1'(H),user:goedel_freeze(ground([H,B,E]),if(('AVLTrees':'~AVLTrees.AVLUpdate.P5'(E,B,'ProgDefs.Module.F3'('ProgDefs.Exported.C0','ProgDefs.Categories.F2'(I,H),[]),J,'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(K,_),_)),true),('AVLTrees':'~AVLTrees.AVLJoin.P3'(C,K,I),G=J),'AVLTrees':'~AVLTrees.AVLInsert.P4'(E,B,'ProgDefs.Module.F3'('ProgDefs.Exported.C0','ProgDefs.Categories.F2'(C,H),[]),G))))),
        '~SharedPrograms.AddLiftedLanguage.P4'(A, D, G, F).
'SharedPrograms.AddOtherModules.P7'('ProgDefs.ModDef.F4'(_,A,B,C), D, E, F, G, H, I) :-
        'Lists':'Lists.Append.P3'(C, A, J),
        'Lists':'Lists.Append.P3'(B, J, K),
        'SharedPrograms.AddOtherModulesAux.P7'(K, D, E, F, G, H, I).
'~SharedPrograms.AddOtherModules.P7'('ProgDefs.ModDef.F4'(_,A,B,C), D, E, F, G, H, I) :-
        'Lists':'~Lists.Append.P3'(C, A, J),
        'Lists':'~Lists.Append.P3'(B, J, K),
        '~SharedPrograms.AddOtherModulesAux.P7'(K, D, E, F, G, H, I).
'SharedPrograms.BinaryConnective.P3'('MetaDefs.&''.F2'(A,B), A, B).
'~SharedPrograms.BinaryConnective.P3'('MetaDefs.&''.F2'(A,B), A, B).
'SharedPrograms.BinaryConnective.P3'('MetaDefs.\/''.F2'(A,B), A, B).
'~SharedPrograms.BinaryConnective.P3'('MetaDefs.\/''.F2'(A,B), A, B).
'SharedPrograms.BinaryConnective.P3'('MetaDefs.->''.F2'(A,B), A, B).
'~SharedPrograms.BinaryConnective.P3'('MetaDefs.->''.F2'(A,B), A, B).
'SharedPrograms.BinaryConnective.P3'('MetaDefs.<-''.F2'(A,B), A, B).
'~SharedPrograms.BinaryConnective.P3'('MetaDefs.<-''.F2'(A,B), A, B).
'SharedPrograms.BinaryConnective.P3'('MetaDefs.<->''.F2'(A,B), A, B).
'~SharedPrograms.BinaryConnective.P3'('MetaDefs.<->''.F2'(A,B), A, B).
'SharedPrograms.BaseInLanguage.P2'(A, B) :-
        'SharedPrograms.TypeSymbolInLanguage.P3'(A, B, 'ProgDefs.BaseDecl.C0').
'~SharedPrograms.BaseInLanguage.P2'(A, B) :-
        '~SharedPrograms.TypeSymbolInLanguage.P3'(A, B, 'ProgDefs.BaseDecl.C0').
'SharedPrograms.AtomToIntDL.P8'('Syntax.NoPredInd.C0', 'MetaDefs.Name.F4'(A,B,C,D), E, F, G, H, I, J) :-
        user:goedel_freeze(ground([D,C,A,B]), ((A='"Integers',B='"Interval',C='MetaDefs.Predicate.C0',D=3),true->E=[K,L,M],'SharedPrograms':'SharedPrograms.ClassifyToken.P2'('"=<',N),'Strings':'Strings.StringInts.P2'('"=<',O),'SharedPrograms':'SharedPrograms.TermToIntDL.P9'(K,F,G,H,_,_,P,I,Q),'SharedPrograms':'SharedPrograms.SpaceIfNeeded.P4'(P,N,Q,R),'Lists':'Lists.Append.P3'(O,S,R),'SharedPrograms':'SharedPrograms.TermToIntDL.P9'(L,F,G,H,_,T,U,V,W),'SharedPrograms':'SharedPrograms.SpaceIfNeeded.P4'(N,T,S,V),'SharedPrograms':'SharedPrograms.SpaceIfNeeded.P4'(U,N,W,X),'Lists':'Lists.Append.P3'(O,Y,X),'SharedPrograms':'SharedPrograms.TermToIntDL.P9'(M,F,G,H,_,Z,_,A1,J),'SharedPrograms':'SharedPrograms.SpaceIfNeeded.P4'(N,Z,Y,A1);'Strings':'Strings.StringInts.P2'(B,B1),'Lists':'Lists.Append.P3'(B1,Q,I),'SharedPrograms':'SharedPrograms.CharDL.P3'('"(',Q,R),'SharedPrograms':'SharedPrograms.TermListToIntDL.P6'(E,F,G,H,R,S),'SharedPrograms':'SharedPrograms.CharDL.P3'('")',S,J))).
'~SharedPrograms.AtomToIntDL.P8'('Syntax.NoPredInd.C0', 'MetaDefs.Name.F4'(A,B,C,D), E, F, G, H, I, J) :-
        user:goedel_freeze(ground([D,C,A,B]), ((A='"Integers',B='"Interval',C='MetaDefs.Predicate.C0',D=3),true->E=[K,L,M],'SharedPrograms':'~SharedPrograms.ClassifyToken.P2'('"=<',N),'Strings':'~Strings.StringInts.P2'('"=<',O),'SharedPrograms':'~SharedPrograms.TermToIntDL.P9'(K,F,G,H,_,_,P,I,Q),'SharedPrograms':'~SharedPrograms.SpaceIfNeeded.P4'(P,N,Q,R),'Lists':'~Lists.Append.P3'(O,S,R),'SharedPrograms':'~SharedPrograms.TermToIntDL.P9'(L,F,G,H,_,T,U,V,W),'SharedPrograms':'~SharedPrograms.SpaceIfNeeded.P4'(N,T,S,V),'SharedPrograms':'~SharedPrograms.SpaceIfNeeded.P4'(U,N,W,X),'Lists':'~Lists.Append.P3'(O,Y,X),'SharedPrograms':'~SharedPrograms.TermToIntDL.P9'(M,F,G,H,_,Z,_,A1,J),'SharedPrograms':'~SharedPrograms.SpaceIfNeeded.P4'(N,Z,Y,A1);'Strings':'~Strings.StringInts.P2'(B,B1),'Lists':'~Lists.Append.P3'(B1,Q,I),'SharedPrograms':'~SharedPrograms.CharDL.P3'('"(',Q,R),'SharedPrograms':'~SharedPrograms.TermListToIntDL.P6'(E,F,G,H,R,S),'SharedPrograms':'~SharedPrograms.CharDL.P3'('")',S,J))).
'SharedPrograms.AtomToIntDL.P8'('Syntax.ZPZ.C0', 'MetaDefs.Name.F4'(_,A,_,_), [B,C], D, E, F, G, H) :-
        'SharedPrograms.TermToIntDL.P9'(B, D, E, F, _, _, I, G, J),
        'SharedPrograms.ClassifyToken.P2'(A, K),
        'SharedPrograms.SpaceIfNeeded.P4'(I, K, J, L),
        'Strings':'Strings.StringInts.P2'(A, M),
        'Lists':'Lists.Append.P3'(M, N, L),
        'SharedPrograms.TermToIntDL.P9'(C, D, E, F, _, O, _, P, H),
        'SharedPrograms.SpaceIfNeeded.P4'(O, K, N, P).
'~SharedPrograms.AtomToIntDL.P8'('Syntax.ZPZ.C0', 'MetaDefs.Name.F4'(_,A,_,_), [B,C], D, E, F, G, H) :-
        '~SharedPrograms.TermToIntDL.P9'(B, D, E, F, _, _, I, G, J),
        '~SharedPrograms.ClassifyToken.P2'(A, K),
        '~SharedPrograms.SpaceIfNeeded.P4'(I, K, J, L),
        'Strings':'~Strings.StringInts.P2'(A, M),
        'Lists':'~Lists.Append.P3'(M, N, L),
        '~SharedPrograms.TermToIntDL.P9'(C, D, E, F, _, O, _, P, H),
        '~SharedPrograms.SpaceIfNeeded.P4'(O, K, N, P).
'SharedPrograms.AtomToIntDL.P8'('Syntax.ZP.C0', 'MetaDefs.Name.F4'(_,A,_,_), [B], C, D, E, F, G) :-
        'SharedPrograms.TermToIntDL.P9'(B, C, D, E, _, _, H, F, I),
        'SharedPrograms.ClassifyToken.P2'(A, J),
        'SharedPrograms.SpaceIfNeeded.P4'(H, J, I, K),
        'Strings':'Strings.StringInts.P2'(A, L),
        'Lists':'Lists.Append.P3'(L, G, K).
'~SharedPrograms.AtomToIntDL.P8'('Syntax.ZP.C0', 'MetaDefs.Name.F4'(_,A,_,_), [B], C, D, E, F, G) :-
        '~SharedPrograms.TermToIntDL.P9'(B, C, D, E, _, _, H, F, I),
        '~SharedPrograms.ClassifyToken.P2'(A, J),
        '~SharedPrograms.SpaceIfNeeded.P4'(H, J, I, K),
        'Strings':'~Strings.StringInts.P2'(A, L),
        'Lists':'~Lists.Append.P3'(L, G, K).
'SharedPrograms.AtomToIntDL.P8'('Syntax.PZ.C0', 'MetaDefs.Name.F4'(_,A,_,_), [B], C, D, E, F, G) :-
        'SharedPrograms.ClassifyToken.P2'(A, H),
        'Strings':'Strings.StringInts.P2'(A, I),
        'Lists':'Lists.Append.P3'(I, J, F),
        'SharedPrograms.TermToIntDL.P9'(B, C, D, E, _, K, _, L, G),
        'SharedPrograms.SpaceIfNeeded.P4'(K, H, J, L).
'~SharedPrograms.AtomToIntDL.P8'('Syntax.PZ.C0', 'MetaDefs.Name.F4'(_,A,_,_), [B], C, D, E, F, G) :-
        '~SharedPrograms.ClassifyToken.P2'(A, H),
        'Strings':'~Strings.StringInts.P2'(A, I),
        'Lists':'~Lists.Append.P3'(I, J, F),
        '~SharedPrograms.TermToIntDL.P9'(B, C, D, E, _, K, _, L, G),
        '~SharedPrograms.SpaceIfNeeded.P4'(K, H, J, L).
'SharedPrograms.AndSeqToIntDL.P3'('ProgDefs.And.F2'(A,B), C, D) :-
        'SharedPrograms.SimpleCondToIntDL.P3'(A, C, E),
        'Strings':'Strings.StringInts.P2'('" & ', F),
        'Lists':'Lists.Append.P3'(F, G, E),
        user:goedel_freeze(ground([B]), (B='ProgDefs.And.F2'(_,_),true->'SharedPrograms':'SharedPrograms.AndSeqToIntDL.P3'(B,G,D);'SharedPrograms':'SharedPrograms.SimpleCondToIntDL.P3'(B,G,D))).
'~SharedPrograms.AndSeqToIntDL.P3'('ProgDefs.And.F2'(A,B), C, D) :-
        '~SharedPrograms.SimpleCondToIntDL.P3'(A, C, E),
        'Strings':'~Strings.StringInts.P2'('" & ', F),
        'Lists':'~Lists.Append.P3'(F, G, E),
        user:goedel_freeze(ground([B]), (B='ProgDefs.And.F2'(_,_),true->'SharedPrograms':'~SharedPrograms.AndSeqToIntDL.P3'(B,G,D);'SharedPrograms':'~SharedPrograms.SimpleCondToIntDL.P3'(B,G,D))).
'SharedPrograms.BinConnectiveChars.P3'('MetaDefs.&''.F2'(_,_), A, B) :-
        'Strings':'Strings.StringInts.P2'('" & ', C),
        'Lists':'Lists.Append.P3'(C, B, A).
'~SharedPrograms.BinConnectiveChars.P3'('MetaDefs.&''.F2'(_,_), A, B) :-
        'Strings':'~Strings.StringInts.P2'('" & ', C),
        'Lists':'~Lists.Append.P3'(C, B, A).
'SharedPrograms.BinConnectiveChars.P3'('MetaDefs.\/''.F2'(_,_), A, B) :-
        'Strings':'Strings.StringInts.P2'('" \/ ', C),
        'Lists':'Lists.Append.P3'(C, B, A).
'~SharedPrograms.BinConnectiveChars.P3'('MetaDefs.\/''.F2'(_,_), A, B) :-
        'Strings':'~Strings.StringInts.P2'('" \/ ', C),
        'Lists':'~Lists.Append.P3'(C, B, A).
'SharedPrograms.BinConnectiveChars.P3'('MetaDefs.->''.F2'(_,_), A, B) :-
        'Strings':'Strings.StringInts.P2'('" -> ', C),
        'Lists':'Lists.Append.P3'(C, B, A).
'~SharedPrograms.BinConnectiveChars.P3'('MetaDefs.->''.F2'(_,_), A, B) :-
        'Strings':'~Strings.StringInts.P2'('" -> ', C),
        'Lists':'~Lists.Append.P3'(C, B, A).
'SharedPrograms.BinConnectiveChars.P3'('MetaDefs.<-''.F2'(_,_), A, B) :-
        'Strings':'Strings.StringInts.P2'('" <- ', C),
        'Lists':'Lists.Append.P3'(C, B, A).
'~SharedPrograms.BinConnectiveChars.P3'('MetaDefs.<-''.F2'(_,_), A, B) :-
        'Strings':'~Strings.StringInts.P2'('" <- ', C),
        'Lists':'~Lists.Append.P3'(C, B, A).
'SharedPrograms.BinConnectiveChars.P3'('MetaDefs.<->''.F2'(_,_), A, B) :-
        'Strings':'Strings.StringInts.P2'('" <-> ', C),
        'Lists':'Lists.Append.P3'(C, B, A).
'~SharedPrograms.BinConnectiveChars.P3'('MetaDefs.<->''.F2'(_,_), A, B) :-
        'Strings':'~Strings.StringInts.P2'('" <-> ', C),
        'Lists':'~Lists.Append.P3'(C, B, A).
'SharedPrograms.BinaryInfixToIntDL.P12'(A, 'MetaDefs.Name.F4'(B,C,_,_), D, E, F, G, H, I, J, K, L, M) :-
        user:goedel_freeze(ground([D,B,E,C]), if(((B='"Rationals',C='"//',D='MetaDefs.Int.F1'(N),E='MetaDefs.Int.F1'(O)),true),'SharedPrograms':'SharedPrograms.RationalToIntDL.P9'(N,O,F,G,I,J,K,L,M),(I=A,'SharedPrograms':'SharedPrograms.TermToIntDL.P9'(D,F,G,H,P,Q,R,S,T),'SharedPrograms':'SharedPrograms.TermToIntDL.P9'(E,F,G,H,U,V,W,X,Y),'SharedPrograms':'SharedPrograms.ClassifyToken.P2'(C,Z),user:goedel_freeze(ground([A,P]),(('SharedPrograms':'~SharedPrograms.RPrec.P2'(P,A1),'SharedPrograms':'~SharedPrograms.LPrec.P2'(A,B1),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(A1,B1)),true->J=Q,L=S,'SharedPrograms':'SharedPrograms.SpaceIfNeeded.P4'(R,Z,T,C1);J='SharedPrograms.Bounded.C0','SharedPrograms':'SharedPrograms.CharDL.P3'('"(',L,S),'SharedPrograms':'SharedPrograms.CharDL.P3'('")',T,C1))),'Strings':'Strings.StringInts.P2'(C,D1),'Lists':'Lists.Append.P3'(D1,E1,C1),user:goedel_freeze(ground([A,U]),(('SharedPrograms':'~SharedPrograms.LPrec.P2'(U,F1),'SharedPrograms':'~SharedPrograms.RPrec.P2'(A,G1),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(F1,G1)),true->K=W,'SharedPrograms':'SharedPrograms.SpaceIfNeeded.P4'(Z,V,E1,X),M=Y;K='SharedPrograms.Bounded.C0','SharedPrograms':'SharedPrograms.CharDL.P3'('"(',E1,X),'SharedPrograms':'SharedPrograms.CharDL.P3'('")',Y,M)))))).
'~SharedPrograms.BinaryInfixToIntDL.P12'(A, 'MetaDefs.Name.F4'(B,C,_,_), D, E, F, G, H, I, J, K, L, M) :-
        user:goedel_freeze(ground([D,B,E,C]), if(((B='"Rationals',C='"//',D='MetaDefs.Int.F1'(N),E='MetaDefs.Int.F1'(O)),true),'SharedPrograms':'~SharedPrograms.RationalToIntDL.P9'(N,O,F,G,I,J,K,L,M),(I=A,'SharedPrograms':'~SharedPrograms.TermToIntDL.P9'(D,F,G,H,P,Q,R,S,T),'SharedPrograms':'~SharedPrograms.TermToIntDL.P9'(E,F,G,H,U,V,W,X,Y),'SharedPrograms':'~SharedPrograms.ClassifyToken.P2'(C,Z),user:goedel_freeze(ground([A,P]),(('SharedPrograms':'~SharedPrograms.RPrec.P2'(P,A1),'SharedPrograms':'~SharedPrograms.LPrec.P2'(A,B1),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(A1,B1)),true->J=Q,L=S,'SharedPrograms':'~SharedPrograms.SpaceIfNeeded.P4'(R,Z,T,C1);J='SharedPrograms.Bounded.C0','SharedPrograms':'~SharedPrograms.CharDL.P3'('"(',L,S),'SharedPrograms':'~SharedPrograms.CharDL.P3'('")',T,C1))),'Strings':'~Strings.StringInts.P2'(C,D1),'Lists':'~Lists.Append.P3'(D1,E1,C1),user:goedel_freeze(ground([A,U]),(('SharedPrograms':'~SharedPrograms.LPrec.P2'(U,F1),'SharedPrograms':'~SharedPrograms.RPrec.P2'(A,G1),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(F1,G1)),true->K=W,'SharedPrograms':'~SharedPrograms.SpaceIfNeeded.P4'(Z,V,E1,X),M=Y;K='SharedPrograms.Bounded.C0','SharedPrograms':'~SharedPrograms.CharDL.P3'('"(',E1,X),'SharedPrograms':'~SharedPrograms.CharDL.P3'('")',Y,M)))))).
'SharedPrograms.BinaryFormulaToIntDL.P6'(A, B, C, D, E, F) :-
        'SharedPrograms.BinaryConnective.P3'(A, G, H),
        'SharedPrograms.FormulaToIntDL.P6'(G, B, C, D, I, J),
        'SharedPrograms.FormulaToIntDL.P6'(H, B, C, D, K, L),
        user:goedel_freeze(ground([A,G]), (('SharedPrograms':'~SharedPrograms.CRPrec.P2'(G,M),'SharedPrograms':'~SharedPrograms.CLPrec.P2'(A,N),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(M,N)),true->E=I,O=J;'SharedPrograms':'SharedPrograms.CharDL.P3'('"(',E,I),'SharedPrograms':'SharedPrograms.CharDL.P3'('")',J,O))),
        'SharedPrograms.BinConnectiveChars.P3'(A, O, P),
        user:goedel_freeze(ground([A,H]), (('SharedPrograms':'~SharedPrograms.CLPrec.P2'(H,Q),'SharedPrograms':'~SharedPrograms.CRPrec.P2'(A,R),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(Q,R)),true->P=K,F=L;'SharedPrograms':'SharedPrograms.CharDL.P3'('"(',P,K),'SharedPrograms':'SharedPrograms.CharDL.P3'('")',L,F))).
'~SharedPrograms.BinaryFormulaToIntDL.P6'(A, B, C, D, E, F) :-
        '~SharedPrograms.BinaryConnective.P3'(A, G, H),
        '~SharedPrograms.FormulaToIntDL.P6'(G, B, C, D, I, J),
        '~SharedPrograms.FormulaToIntDL.P6'(H, B, C, D, K, L),
        user:goedel_freeze(ground([A,G]), (('SharedPrograms':'~SharedPrograms.CRPrec.P2'(G,M),'SharedPrograms':'~SharedPrograms.CLPrec.P2'(A,N),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(M,N)),true->E=I,O=J;'SharedPrograms':'~SharedPrograms.CharDL.P3'('"(',E,I),'SharedPrograms':'~SharedPrograms.CharDL.P3'('")',J,O))),
        '~SharedPrograms.BinConnectiveChars.P3'(A, O, P),
        user:goedel_freeze(ground([A,H]), (('SharedPrograms':'~SharedPrograms.CLPrec.P2'(H,Q),'SharedPrograms':'~SharedPrograms.CRPrec.P2'(A,R),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(Q,R)),true->P=K,F=L;'SharedPrograms':'~SharedPrograms.CharDL.P3'('"(',P,K),'SharedPrograms':'~SharedPrograms.CharDL.P3'('")',L,F))).
'SharedPrograms.CLPrec.P2'('MetaDefs.Empty.C0', 'SharedPrograms.Infinity.C0').
'~SharedPrograms.CLPrec.P2'('MetaDefs.Empty.C0', 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.PAtom.F1'(_), 'SharedPrograms.Infinity.C0').
'~SharedPrograms.CLPrec.P2'('MetaDefs.PAtom.F1'(_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.Atom.F2'(_,_), 'SharedPrograms.Infinity.C0').
'~SharedPrograms.CLPrec.P2'('MetaDefs.Atom.F2'(_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.~''.F1'(_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.X.C0')).
'~SharedPrograms.CLPrec.P2'('MetaDefs.~''.F1'(_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.&''.F2'(_,_), 'SharedPrograms.Prec.F2'(100,'SharedPrograms.X.C0')).
'~SharedPrograms.CLPrec.P2'('MetaDefs.&''.F2'(_,_), 'SharedPrograms.Prec.F2'(100,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.\/''.F2'(_,_), 'SharedPrograms.Prec.F2'(90,'SharedPrograms.X.C0')).
'~SharedPrograms.CLPrec.P2'('MetaDefs.\/''.F2'(_,_), 'SharedPrograms.Prec.F2'(90,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.->''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.X.C0')).
'~SharedPrograms.CLPrec.P2'('MetaDefs.->''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.<-''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.X.C0')).
'~SharedPrograms.CLPrec.P2'('MetaDefs.<-''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.<->''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.X.C0')).
'~SharedPrograms.CLPrec.P2'('MetaDefs.<->''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.Some.F2'(_,_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.X.C0')).
'~SharedPrograms.CLPrec.P2'('MetaDefs.Some.F2'(_,_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.All.F2'(_,_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.X.C0')).
'~SharedPrograms.CLPrec.P2'('MetaDefs.All.F2'(_,_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.X.C0')).
'SharedPrograms.CLPrec.P2'('MetaDefs.Commit.F2'(_,_), 'SharedPrograms.Infinity.C0').
'~SharedPrograms.CLPrec.P2'('MetaDefs.Commit.F2'(_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.ISTE.F4'(_,_,_,_), 'SharedPrograms.Infinity.C0').
'~SharedPrograms.CLPrec.P2'('MetaDefs.ISTE.F4'(_,_,_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.IST.F3'(_,_,_), 'SharedPrograms.Infinity.C0').
'~SharedPrograms.CLPrec.P2'('MetaDefs.IST.F3'(_,_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.ITE.F3'(_,_,_), 'SharedPrograms.Infinity.C0').
'~SharedPrograms.CLPrec.P2'('MetaDefs.ITE.F3'(_,_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CLPrec.P2'('MetaDefs.IT.F2'(_,_), 'SharedPrograms.Infinity.C0').
'~SharedPrograms.CLPrec.P2'('MetaDefs.IT.F2'(_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.BindsTighter.P2'('SharedPrograms.Infinity.C0', _).
'~SharedPrograms.BindsTighter.P2'('SharedPrograms.Infinity.C0', _).
'SharedPrograms.BindsTighter.P2'('SharedPrograms.Prec.F2'(A,B), 'SharedPrograms.Prec.F2'(C,D)) :-
        call_residue(A=C, E),
        (   E=[] ->
            !
        ;   user:release_suspended(E)
        ),
        B='SharedPrograms.X.C0',
        D='SharedPrograms.Y.C0'.
'~SharedPrograms.BindsTighter.P2'('SharedPrograms.Prec.F2'(A,B), 'SharedPrograms.Prec.F2'(C,D)) :-
        A=C,
        B='SharedPrograms.X.C0',
        D='SharedPrograms.Y.C0'.
'SharedPrograms.BindsTighter.P2'('SharedPrograms.Prec.F2'(A,_), 'SharedPrograms.Prec.F2'(B,_)) :-
        call_residue('Integers':'Integers.>.P2'(A,B), C),
        (   C=[] ->
            !
        ;   user:release_suspended(C)
        ).
'~SharedPrograms.BindsTighter.P2'('SharedPrograms.Prec.F2'(A,_), 'SharedPrograms.Prec.F2'(B,_)) :-
        'Integers':'~Integers.>.P2'(A, B).
'SharedPrograms.CRPrec.P2'('MetaDefs.Empty.C0', 'SharedPrograms.Infinity.C0').
'~SharedPrograms.CRPrec.P2'('MetaDefs.Empty.C0', 'SharedPrograms.Infinity.C0').
'SharedPrograms.CRPrec.P2'('MetaDefs.PAtom.F1'(_), 'SharedPrograms.Infinity.C0').
'~SharedPrograms.CRPrec.P2'('MetaDefs.PAtom.F1'(_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CRPrec.P2'('MetaDefs.Atom.F2'(_,_), 'SharedPrograms.Infinity.C0').
'~SharedPrograms.CRPrec.P2'('MetaDefs.Atom.F2'(_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CRPrec.P2'('MetaDefs.~''.F1'(_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.Y.C0')).
'~SharedPrograms.CRPrec.P2'('MetaDefs.~''.F1'(_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.&''.F2'(_,_), 'SharedPrograms.Prec.F2'(100,'SharedPrograms.Y.C0')).
'~SharedPrograms.CRPrec.P2'('MetaDefs.&''.F2'(_,_), 'SharedPrograms.Prec.F2'(100,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.\/''.F2'(_,_), 'SharedPrograms.Prec.F2'(90,'SharedPrograms.Y.C0')).
'~SharedPrograms.CRPrec.P2'('MetaDefs.\/''.F2'(_,_), 'SharedPrograms.Prec.F2'(90,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.->''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.Y.C0')).
'~SharedPrograms.CRPrec.P2'('MetaDefs.->''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.<-''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.Y.C0')).
'~SharedPrograms.CRPrec.P2'('MetaDefs.<-''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.<->''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.X.C0')).
'~SharedPrograms.CRPrec.P2'('MetaDefs.<->''.F2'(_,_), 'SharedPrograms.Prec.F2'(80,'SharedPrograms.X.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.Some.F2'(_,_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.Y.C0')).
'~SharedPrograms.CRPrec.P2'('MetaDefs.Some.F2'(_,_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.All.F2'(_,_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.Y.C0')).
'~SharedPrograms.CRPrec.P2'('MetaDefs.All.F2'(_,_), 'SharedPrograms.Prec.F2'(120,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.Commit.F2'(_,_), 'SharedPrograms.Infinity.C0').
'~SharedPrograms.CRPrec.P2'('MetaDefs.Commit.F2'(_,_), 'SharedPrograms.Infinity.C0').
'SharedPrograms.CRPrec.P2'('MetaDefs.ISTE.F4'(_,_,_,_), 'SharedPrograms.Prec.F2'(70,'SharedPrograms.Y.C0')).
'~SharedPrograms.CRPrec.P2'('MetaDefs.ISTE.F4'(_,_,_,_), 'SharedPrograms.Prec.F2'(70,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.IST.F3'(_,_,_), 'SharedPrograms.Prec.F2'(70,'SharedPrograms.Y.C0')).
'~SharedPrograms.CRPrec.P2'('MetaDefs.IST.F3'(_,_,_), 'SharedPrograms.Prec.F2'(70,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.ITE.F3'(_,_,_), 'SharedPrograms.Prec.F2'(70,'SharedPrograms.Y.C0')).
'~SharedPrograms.CRPrec.P2'('MetaDefs.ITE.F3'(_,_,_), 'SharedPrograms.Prec.F2'(70,'SharedPrograms.Y.C0')).
'SharedPrograms.CRPrec.P2'('MetaDefs.IT.F2'(_,_), 'SharedPrograms.Prec.F2'(70,'SharedPrograms.Y.C0')).
'~SharedPrograms.CRPrec.P2'('MetaDefs.IT.F2'(_,_), 'SharedPrograms.Prec.F2'(70,'SharedPrograms.Y.C0')).
'SharedPrograms.EmptyCategoryTable.P1'('ProgDefs.Categories.F2'(A,A)) :-
        'AVLTrees':'AVLTrees.AVLIsEmpty.P1'(A).
'~SharedPrograms.EmptyCategoryTable.P1'('ProgDefs.Categories.F2'(A,A)) :-
        'AVLTrees':'~AVLTrees.AVLIsEmpty.P1'(A).
'SharedPrograms.ConstantInLanguage.P3'(A, B, C) :-
        'SharedPrograms.SymbolInLanguage.P3'(A, B, 'ProgDefs.ConstantDecl.F1'(C)).
'~SharedPrograms.ConstantInLanguage.P3'(A, B, C) :-
        '~SharedPrograms.SymbolInLanguage.P3'(A, B, 'ProgDefs.ConstantDecl.F1'(C)).
'SharedPrograms.CheckHeadArgs1.P4'([], _, _, []).
'~SharedPrograms.CheckHeadArgs1.P4'([], _, _, []).
'SharedPrograms.CheckHeadArgs1.P4'([A|B], [C|D], [E|F], G) :-
        'SharedPrograms.VariantTypes.P4'(A, C, E, H),
        'Lists':'Lists.Append.P3'(H, I, G),
        'SharedPrograms.CheckHeadArgs1.P4'(B, D, F, I).
'~SharedPrograms.CheckHeadArgs1.P4'([A|B], [C|D], [E|F], G) :-
        '~SharedPrograms.VariantTypes.P4'(A, C, E, H),
        'Lists':'~Lists.Append.P3'(H, I, G),
        '~SharedPrograms.CheckHeadArgs1.P4'(B, D, F, I).
'SharedPrograms.CheckHeadArgs.P4'('MetaDefs.PAtom.F1'(_), _, _, []).
'~SharedPrograms.CheckHeadArgs.P4'('MetaDefs.PAtom.F1'(_), _, _, []).
'SharedPrograms.CheckHeadArgs.P4'('MetaDefs.Atom.F2'(A,B), C, D, E) :-
        'SharedPrograms.PredicateInLanguage.P4'(A, D, _, F),
        'SharedPrograms.MeltTypeList.P4'(F, [], _, G),
        'SharedPrograms.CheckHeadArgs1.P4'(G, C, B, E).
'~SharedPrograms.CheckHeadArgs.P4'('MetaDefs.Atom.F2'(A,B), C, D, E) :-
        '~SharedPrograms.PredicateInLanguage.P4'(A, D, _, F),
        '~SharedPrograms.MeltTypeList.P4'(F, [], _, G),
        '~SharedPrograms.CheckHeadArgs1.P4'(G, C, B, E).
'SharedPrograms.ConditionToIntDL.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), (A='ProgDefs.And.F2'(_,_),true->'SharedPrograms':'SharedPrograms.AndSeqToIntDL.P3'(A,B,C);user:goedel_freeze(ground([A]),(A='ProgDefs.Or.F2'(_,_),true->'SharedPrograms':'SharedPrograms.OrSeqToIntDL.P3'(A,B,C);'SharedPrograms':'SharedPrograms.SimpleCondToIntDL.P3'(A,B,C))))).
'~SharedPrograms.ConditionToIntDL.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), (A='ProgDefs.And.F2'(_,_),true->'SharedPrograms':'~SharedPrograms.AndSeqToIntDL.P3'(A,B,C);user:goedel_freeze(ground([A]),(A='ProgDefs.Or.F2'(_,_),true->'SharedPrograms':'~SharedPrograms.OrSeqToIntDL.P3'(A,B,C);'SharedPrograms':'~SharedPrograms.SimpleCondToIntDL.P3'(A,B,C))))).
'SharedPrograms.CookRationalTerm.P2'(A, B) :-
        user:goedel_freeze(ground([A]), ('Integers':'~Integers.<.P2'(A,0),true->('Integers':negative(A,D),C=D),B='MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Rationals','"-','MetaDefs.Function.C0',1),['MetaDefs.Int.F1'(C)]);B='MetaDefs.Int.F1'(A))).
'~SharedPrograms.CookRationalTerm.P2'(A, B) :-
        user:goedel_freeze(ground([A]), ('Integers':'~Integers.<.P2'(A,0),true->('Integers':negative(A,D),C=D),B='MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Rationals','"-','MetaDefs.Function.C0',1),['MetaDefs.Int.F1'(C)]);B='MetaDefs.Int.F1'(A))).
'SharedPrograms.ConstructorInLanguage.P3'(A, B, C) :-
        'SharedPrograms.TypeSymbolInLanguage.P3'(A, B, 'ProgDefs.ConstructorDecl.F1'(C)).
'~SharedPrograms.ConstructorInLanguage.P3'(A, B, C) :-
        '~SharedPrograms.TypeSymbolInLanguage.P3'(A, B, 'ProgDefs.ConstructorDecl.F1'(C)).
'SharedPrograms.ConstantToIntDL.P4'('MetaDefs.Name.F4'(A,B,C,_), D, E, F) :-
        user:goedel_freeze(ground([C,A,B]), ((A='"Lists',B='"Nil',C='MetaDefs.Constant.C0'),true->D='SharedPrograms.Bounded.C0','SharedPrograms':'SharedPrograms.CharDL.P3'('"[',E,G),'SharedPrograms':'SharedPrograms.CharDL.P3'('"]',G,F);user:goedel_freeze(ground([C,A,B]),((A='"Sets',B='"Null',C='MetaDefs.Constant.C0'),true->D='SharedPrograms.Bounded.C0','SharedPrograms':'SharedPrograms.CharDL.P3'('"{',E,G),'SharedPrograms':'SharedPrograms.CharDL.P3'('"}',G,F);'SharedPrograms':'SharedPrograms.ClassifyToken.P2'(B,D),'Strings':'Strings.StringInts.P2'(B,H),'Lists':'Lists.Append.P3'(H,F,E))))).
'~SharedPrograms.ConstantToIntDL.P4'('MetaDefs.Name.F4'(A,B,C,_), D, E, F) :-
        user:goedel_freeze(ground([C,A,B]), ((A='"Lists',B='"Nil',C='MetaDefs.Constant.C0'),true->D='SharedPrograms.Bounded.C0','SharedPrograms':'~SharedPrograms.CharDL.P3'('"[',E,G),'SharedPrograms':'~SharedPrograms.CharDL.P3'('"]',G,F);user:goedel_freeze(ground([C,A,B]),((A='"Sets',B='"Null',C='MetaDefs.Constant.C0'),true->D='SharedPrograms.Bounded.C0','SharedPrograms':'~SharedPrograms.CharDL.P3'('"{',E,G),'SharedPrograms':'~SharedPrograms.CharDL.P3'('"}',G,F);'SharedPrograms':'~SharedPrograms.ClassifyToken.P2'(B,D),'Strings':'~Strings.StringInts.P2'(B,H),'Lists':'~Lists.Append.P3'(H,F,E))))).
'SharedPrograms.ContainsConditional.P1'('MetaDefs.ISTE.F4'(_,_,_,_)).
'~SharedPrograms.ContainsConditional.P1'('MetaDefs.ISTE.F4'(_,_,_,_)).
'SharedPrograms.ContainsConditional.P1'('MetaDefs.IST.F3'(_,_,_)).
'~SharedPrograms.ContainsConditional.P1'('MetaDefs.IST.F3'(_,_,_)).
'SharedPrograms.ContainsConditional.P1'('MetaDefs.ITE.F3'(_,_,_)).
'~SharedPrograms.ContainsConditional.P1'('MetaDefs.ITE.F3'(_,_,_)).
'SharedPrograms.ContainsConditional.P1'('MetaDefs.IT.F2'(_,_)).
'~SharedPrograms.ContainsConditional.P1'('MetaDefs.IT.F2'(_,_)).
'SharedPrograms.ContainsConditional.P1'('MetaDefs.&''.F2'(A,B)) :-
        user:one_solution(('SharedPrograms':'SharedPrograms.ContainsConditional.P1'(A);'SharedPrograms':'SharedPrograms.ContainsConditional.P1'(B))).
'~SharedPrograms.ContainsConditional.P1'('MetaDefs.&''.F2'(A,B)) :-
        (   '~SharedPrograms.ContainsConditional.P1'(A)
        ;   '~SharedPrograms.ContainsConditional.P1'(B)
        ).
'SharedPrograms.DivideBindings.P4'([], A, A, []).
'~SharedPrograms.DivideBindings.P4'([], A, A, []).
'SharedPrograms.DivideBindings.P4'([A|B], C, D, E) :-
        'SharedPrograms.IsVariable.P1'(A),
        'SharedPrograms.DivideBindingsAux.P5'(C, A, F, E, G),
        'SharedPrograms.DivideBindings.P4'(B, F, D, G).
'~SharedPrograms.DivideBindings.P4'([A|B], C, D, E) :-
        '~SharedPrograms.IsVariable.P1'(A),
        '~SharedPrograms.DivideBindingsAux.P5'(C, A, F, E, G),
        '~SharedPrograms.DivideBindings.P4'(B, F, D, G).
'SharedPrograms.DeleteUnderscores.P2'([], []).
'~SharedPrograms.DeleteUnderscores.P2'([], []).
'SharedPrograms.DeleteUnderscores.P2'([A|B], C) :-
        user:goedel_freeze(ground([A]), (('Strings':concat('"_',_,D),A='MetaDefs.Var.F2'(D,_)),true->C=E;C=[A|E])),
        'SharedPrograms.DeleteUnderscores.P2'(B, E).
'~SharedPrograms.DeleteUnderscores.P2'([A|B], C) :-
        user:goedel_freeze(ground([A]), (('Strings':concat('"_',_,D),A='MetaDefs.Var.F2'(D,_)),true->C=E;C=[A|E])),
        '~SharedPrograms.DeleteUnderscores.P2'(B, E).
'SharedPrograms.DivideBindingsAux.P5'([], _, [], A, A).
'~SharedPrograms.DivideBindingsAux.P5'([], _, [], A, A).
'SharedPrograms.DivideBindingsAux.P5'(['MetaDefs.@.F2'(B,C)|A], D, E, F, G) :-
        user:goedel_freeze(ground([B,D]), (D=B,true->F=['MetaDefs.@.F2'(B,C)|G],E=A;E=['MetaDefs.@.F2'(B,C)|H],'SharedPrograms':'SharedPrograms.DivideBindingsAux.P5'(A,D,H,F,G))).
'~SharedPrograms.DivideBindingsAux.P5'(['MetaDefs.@.F2'(B,C)|A], D, E, F, G) :-
        user:goedel_freeze(ground([B,D]), (D=B,true->F=['MetaDefs.@.F2'(B,C)|G],E=A;E=['MetaDefs.@.F2'(B,C)|H],'SharedPrograms':'~SharedPrograms.DivideBindingsAux.P5'(A,D,H,F,G))).
'SharedPrograms.FormulaInLanguage.P4'(A, B, 'MetaDefs.VarTyping.F1'(C), 'MetaDefs.VarTyping.F1'(D)) :-
        'SharedPrograms.MeltVariableTyping.P4'(C, B, [], E),
        'SharedPrograms.MeltedFormulaTyping.P8'(A, B, E, D, _, [], [], []),
        'SharedPrograms.FixVariableTyping.P2'(D, 0).
'~SharedPrograms.FormulaInLanguage.P4'(A, B, 'MetaDefs.VarTyping.F1'(C), 'MetaDefs.VarTyping.F1'(D)) :-
        '~SharedPrograms.MeltVariableTyping.P4'(C, B, [], E),
        '~SharedPrograms.MeltedFormulaTyping.P8'(A, B, E, D, _, [], [], []),
        '~SharedPrograms.FixVariableTyping.P2'(D, 0).
'SharedPrograms.ExportLanguage.P3'('ProgDefs.Program.F4'(_,A,'ProgDefs.Language.F1'(B),_), C, 'ProgDefs.Language.F1'(D)) :-
        'SharedPrograms.EmptyLanguage.P1'('ProgDefs.Language.F1'(E)),
        'SharedPrograms.ImportedLanguage.P6'([C], A, B, [], E, D).
'~SharedPrograms.ExportLanguage.P3'('ProgDefs.Program.F4'(_,A,'ProgDefs.Language.F1'(B),_), C, 'ProgDefs.Language.F1'(D)) :-
        '~SharedPrograms.EmptyLanguage.P1'('ProgDefs.Language.F1'(E)),
        '~SharedPrograms.ImportedLanguage.P6'([C], A, B, [], E, D).
'SharedPrograms.ExpandString.P3'(A, B, C) :-
        'SharedPrograms.CharDL.P3'('""', B, D),
        'SharedPrograms.ExpandStringAux.P3'(A, D, C).
'~SharedPrograms.ExpandString.P3'(A, B, C) :-
        '~SharedPrograms.CharDL.P3'('""', B, D),
        '~SharedPrograms.ExpandStringAux.P3'(A, D, C).
'SharedPrograms.EmptyLanguage.P1'(A) :-
        'AVLTrees':'AVLTrees.AVLIsEmpty.P1'(B),
        'SharedPrograms.InitialiseLanguage.P2'('ProgDefs.Language.F1'(B), A).
'~SharedPrograms.EmptyLanguage.P1'(A) :-
        'AVLTrees':'~AVLTrees.AVLIsEmpty.P1'(B),
        '~SharedPrograms.InitialiseLanguage.P2'('ProgDefs.Language.F1'(B), A).
'SharedPrograms.ExpandStringAux.P3'([], A, B) :-
        'SharedPrograms.CharDL.P3'('""', A, B).
'~SharedPrograms.ExpandStringAux.P3'([], A, B) :-
        '~SharedPrograms.CharDL.P3'('""', A, B).
'SharedPrograms.ExpandStringAux.P3'([A|B], C, D) :-
        user:goedel_freeze(ground([A]), (A=34,true->'SharedPrograms':'SharedPrograms.CharDL.P3'('"\',C,E),'SharedPrograms':'SharedPrograms.CharDL.P3'('""',E,F);user:goedel_freeze(ground([A]),(A=92,true->'SharedPrograms':'SharedPrograms.CharDL.P3'('"\',C,E),'SharedPrograms':'SharedPrograms.CharDL.P3'('"\',E,F);C=[A|F])))),
        'SharedPrograms.ExpandStringAux.P3'(B, F, D).
'~SharedPrograms.ExpandStringAux.P3'([A|B], C, D) :-
        user:goedel_freeze(ground([A]), (A=34,true->'SharedPrograms':'~SharedPrograms.CharDL.P3'('"\',C,E),'SharedPrograms':'~SharedPrograms.CharDL.P3'('""',E,F);user:goedel_freeze(ground([A]),(A=92,true->'SharedPrograms':'~SharedPrograms.CharDL.P3'('"\',C,E),'SharedPrograms':'~SharedPrograms.CharDL.P3'('"\',E,F);C=[A|F])))),
        '~SharedPrograms.ExpandStringAux.P3'(B, F, D).
'SharedPrograms.FixVariableTyping.P2'([], _).
'~SharedPrograms.FixVariableTyping.P2'([], _).
'SharedPrograms.FixVariableTyping.P2'(['MetaDefs.@.F2'(_,B)|A], C) :-
        'SharedPrograms.FixType.P3'(B, C, D),
        'SharedPrograms.FixVariableTyping.P2'(A, D).
'~SharedPrograms.FixVariableTyping.P2'(['MetaDefs.@.F2'(_,B)|A], C) :-
        '~SharedPrograms.FixType.P3'(B, C, D),
        '~SharedPrograms.FixVariableTyping.P2'(A, D).
'SharedPrograms.FunctionInLanguage.P5'(A, B, C, D, E) :-
        'SharedPrograms.SymbolInLanguage.P3'(A, B, 'ProgDefs.FunctionDecl.F4'(_,C,D,E)).
'~SharedPrograms.FunctionInLanguage.P5'(A, B, C, D, E) :-
        '~SharedPrograms.SymbolInLanguage.P3'(A, B, 'ProgDefs.FunctionDecl.F4'(_,C,D,E)).
'SharedPrograms.FormulaToIntList.P4'(A, B, C, D) :-
        'SharedPrograms.ProgramLanguage.P2'(A, E),
        'SharedPrograms.ModuleLanguage.P3'(A, B, F),
        'SharedPrograms.FormulaInLanguage.P4'(C, E, 'MetaDefs.VarTyping.F1'([]), 'MetaDefs.VarTyping.F1'(G)),
        'SharedPrograms.FormulaToIntDL.P6'(C, F, E, G, D, []).
'~SharedPrograms.FormulaToIntList.P4'(A, B, C, D) :-
        '~SharedPrograms.ProgramLanguage.P2'(A, E),
        '~SharedPrograms.ModuleLanguage.P3'(A, B, F),
        '~SharedPrograms.FormulaInLanguage.P4'(C, E, 'MetaDefs.VarTyping.F1'([]), 'MetaDefs.VarTyping.F1'(G)),
        '~SharedPrograms.FormulaToIntDL.P6'(C, F, E, G, D, []).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Empty.C0', _, _, _, A, A).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Empty.C0', _, _, _, A, A).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.PAtom.F1'(A), B, _, _, C, D) :-
        'SharedPrograms.PropositionInLanguage.P2'(A, B),
        A='MetaDefs.Name.F4'(_,E,_,_),
        'Strings':'Strings.StringInts.P2'(E, F),
        'Lists':'Lists.Append.P3'(F, D, C).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.PAtom.F1'(A), B, _, _, C, D) :-
        '~SharedPrograms.PropositionInLanguage.P2'(A, B),
        A='MetaDefs.Name.F4'(_,E,_,_),
        'Strings':'~Strings.StringInts.P2'(E, F),
        'Lists':'~Lists.Append.P3'(F, D, C).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Atom.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.PredicateInLanguage.P4'(A, C, H, _),
        'SharedPrograms.AtomToIntDL.P8'(H, A, B, C, D, E, F, G).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Atom.F2'(A,B), C, D, E, F, G) :-
        '~SharedPrograms.PredicateInLanguage.P4'(A, C, H, _),
        '~SharedPrograms.AtomToIntDL.P8'(H, A, B, C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.~''.F1'(A), B, C, D, E, F) :-
        'SharedPrograms.CharDL.P3'('"~', E, G),
        'SharedPrograms.FormulaToIntDL.P6'(A, B, C, D, H, I),
        user:goedel_freeze(ground([A]), (('SharedPrograms':'~SharedPrograms.CLPrec.P2'(A,J),'SharedPrograms':'~SharedPrograms.CRPrec.P2'('MetaDefs.~''.F1'(_),K),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(J,K)),true->G=H,F=I;'SharedPrograms':'SharedPrograms.CharDL.P3'('"(',G,H),'SharedPrograms':'SharedPrograms.CharDL.P3'('")',I,F))).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.~''.F1'(A), B, C, D, E, F) :-
        '~SharedPrograms.CharDL.P3'('"~', E, G),
        '~SharedPrograms.FormulaToIntDL.P6'(A, B, C, D, H, I),
        user:goedel_freeze(ground([A]), (('SharedPrograms':'~SharedPrograms.CLPrec.P2'(A,J),'SharedPrograms':'~SharedPrograms.CRPrec.P2'('MetaDefs.~''.F1'(_),K),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(J,K)),true->G=H,F=I;'SharedPrograms':'~SharedPrograms.CharDL.P3'('"(',G,H),'SharedPrograms':'~SharedPrograms.CharDL.P3'('")',I,F))).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.&''.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.&''.F2'(A,B), C, D, E, F, G).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.&''.F2'(A,B), C, D, E, F, G) :-
        '~SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.&''.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.\/''.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.\/''.F2'(A,B), C, D, E, F, G).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.\/''.F2'(A,B), C, D, E, F, G) :-
        '~SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.\/''.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.->''.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.->''.F2'(A,B), C, D, E, F, G).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.->''.F2'(A,B), C, D, E, F, G) :-
        '~SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.->''.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.<-''.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.<-''.F2'(A,B), C, D, E, F, G).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.<-''.F2'(A,B), C, D, E, F, G) :-
        '~SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.<-''.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.<->''.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.<->''.F2'(A,B), C, D, E, F, G).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.<->''.F2'(A,B), C, D, E, F, G) :-
        '~SharedPrograms.BinaryFormulaToIntDL.P6'('MetaDefs.<->''.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.All.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.QuantifiedFormulaToIntDL.P6'('MetaDefs.All.F2'(A,B), C, D, E, F, G).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.All.F2'(A,B), C, D, E, F, G) :-
        '~SharedPrograms.QuantifiedFormulaToIntDL.P6'('MetaDefs.All.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Some.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.QuantifiedFormulaToIntDL.P6'('MetaDefs.Some.F2'(A,B), C, D, E, F, G).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Some.F2'(A,B), C, D, E, F, G) :-
        '~SharedPrograms.QuantifiedFormulaToIntDL.P6'('MetaDefs.Some.F2'(A,B), C, D, E, F, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.ISTE.F4'(A,B,C,D), E, F, G, H, I) :-
        'Strings':'Strings.StringInts.P2'('"IF ', J),
        'Lists':'Lists.Append.P3'(J, K, H),
        'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Some.F2'(A,B), E, F, G, K, L),
        'Strings':'Strings.StringInts.P2'('" THEN ', M),
        'Lists':'Lists.Append.P3'(M, N, L),
        'SharedPrograms.ThenPartToIntDL.P6'(C, E, F, G, N, O),
        'Strings':'Strings.StringInts.P2'('" ELSE ', P),
        'Lists':'Lists.Append.P3'(P, Q, O),
        'SharedPrograms.FormulaToIntDL.P6'(D, E, F, G, Q, I).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.ISTE.F4'(A,B,C,D), E, F, G, H, I) :-
        'Strings':'~Strings.StringInts.P2'('"IF ', J),
        'Lists':'~Lists.Append.P3'(J, K, H),
        '~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Some.F2'(A,B), E, F, G, K, L),
        'Strings':'~Strings.StringInts.P2'('" THEN ', M),
        'Lists':'~Lists.Append.P3'(M, N, L),
        '~SharedPrograms.ThenPartToIntDL.P6'(C, E, F, G, N, O),
        'Strings':'~Strings.StringInts.P2'('" ELSE ', P),
        'Lists':'~Lists.Append.P3'(P, Q, O),
        '~SharedPrograms.FormulaToIntDL.P6'(D, E, F, G, Q, I).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.ITE.F3'(A,B,C), D, E, F, G, H) :-
        'Strings':'Strings.StringInts.P2'('"IF ', I),
        'Lists':'Lists.Append.P3'(I, J, G),
        'SharedPrograms.FormulaToIntDL.P6'(A, D, E, F, K, L),
        user:goedel_freeze(ground([A]), (A='MetaDefs.Some.F2'(_,_),true->'SharedPrograms':'SharedPrograms.CharDL.P3'('"(',J,K),'SharedPrograms':'SharedPrograms.CharDL.P3'('")',L,M);J=K,M=L)),
        'Strings':'Strings.StringInts.P2'('" THEN ', N),
        'Lists':'Lists.Append.P3'(N, O, M),
        'SharedPrograms.ThenPartToIntDL.P6'(B, D, E, F, O, P),
        'Strings':'Strings.StringInts.P2'('" ELSE ', Q),
        'Lists':'Lists.Append.P3'(Q, R, P),
        'SharedPrograms.FormulaToIntDL.P6'(C, D, E, F, R, H).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.ITE.F3'(A,B,C), D, E, F, G, H) :-
        'Strings':'~Strings.StringInts.P2'('"IF ', I),
        'Lists':'~Lists.Append.P3'(I, J, G),
        '~SharedPrograms.FormulaToIntDL.P6'(A, D, E, F, K, L),
        user:goedel_freeze(ground([A]), (A='MetaDefs.Some.F2'(_,_),true->'SharedPrograms':'~SharedPrograms.CharDL.P3'('"(',J,K),'SharedPrograms':'~SharedPrograms.CharDL.P3'('")',L,M);J=K,M=L)),
        'Strings':'~Strings.StringInts.P2'('" THEN ', N),
        'Lists':'~Lists.Append.P3'(N, O, M),
        '~SharedPrograms.ThenPartToIntDL.P6'(B, D, E, F, O, P),
        'Strings':'~Strings.StringInts.P2'('" ELSE ', Q),
        'Lists':'~Lists.Append.P3'(Q, R, P),
        '~SharedPrograms.FormulaToIntDL.P6'(C, D, E, F, R, H).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.IST.F3'(A,B,C), D, E, F, G, H) :-
        'Strings':'Strings.StringInts.P2'('"IF ', I),
        'Lists':'Lists.Append.P3'(I, J, G),
        'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Some.F2'(A,B), D, E, F, J, K),
        'Strings':'Strings.StringInts.P2'('" THEN ', L),
        'Lists':'Lists.Append.P3'(L, M, K),
        'SharedPrograms.ThenPartToIntDL.P6'(C, D, E, F, M, H).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.IST.F3'(A,B,C), D, E, F, G, H) :-
        'Strings':'~Strings.StringInts.P2'('"IF ', I),
        'Lists':'~Lists.Append.P3'(I, J, G),
        '~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Some.F2'(A,B), D, E, F, J, K),
        'Strings':'~Strings.StringInts.P2'('" THEN ', L),
        'Lists':'~Lists.Append.P3'(L, M, K),
        '~SharedPrograms.ThenPartToIntDL.P6'(C, D, E, F, M, H).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.IT.F2'(A,B), C, D, E, F, G) :-
        'Strings':'Strings.StringInts.P2'('"IF ', H),
        'Lists':'Lists.Append.P3'(H, I, F),
        'SharedPrograms.FormulaToIntDL.P6'(A, C, D, E, J, K),
        user:goedel_freeze(ground([A]), (A='MetaDefs.Some.F2'(_,_),true->'SharedPrograms':'SharedPrograms.CharDL.P3'('"(',I,J),'SharedPrograms':'SharedPrograms.CharDL.P3'('")',K,L);I=J,L=K)),
        'Strings':'Strings.StringInts.P2'('" THEN ', M),
        'Lists':'Lists.Append.P3'(M, N, L),
        'SharedPrograms.ThenPartToIntDL.P6'(B, C, D, E, N, G).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.IT.F2'(A,B), C, D, E, F, G) :-
        'Strings':'~Strings.StringInts.P2'('"IF ', H),
        'Lists':'~Lists.Append.P3'(H, I, F),
        '~SharedPrograms.FormulaToIntDL.P6'(A, C, D, E, J, K),
        user:goedel_freeze(ground([A]), (A='MetaDefs.Some.F2'(_,_),true->'SharedPrograms':'~SharedPrograms.CharDL.P3'('"(',I,J),'SharedPrograms':'~SharedPrograms.CharDL.P3'('")',K,L);I=J,L=K)),
        'Strings':'~Strings.StringInts.P2'('" THEN ', M),
        'Lists':'~Lists.Append.P3'(M, N, L),
        '~SharedPrograms.ThenPartToIntDL.P6'(B, C, D, E, N, G).
'SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Commit.F2'(A,B), C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('"{', F, H),
        'SharedPrograms.FormulaToIntDL.P6'(B, C, D, E, H, I),
        'SharedPrograms.CharDL.P3'('"}', I, J),
        'SharedPrograms.CharDL.P3'('"_', J, K),
        'SharedSyntax':'SharedSyntax.IntegerToCharDL.P3'(A, K, G).
'~SharedPrograms.FormulaToIntDL.P6'('MetaDefs.Commit.F2'(A,B), C, D, E, F, G) :-
        '~SharedPrograms.CharDL.P3'('"{', F, H),
        '~SharedPrograms.FormulaToIntDL.P6'(B, C, D, E, H, I),
        '~SharedPrograms.CharDL.P3'('"}', I, J),
        '~SharedPrograms.CharDL.P3'('"_', J, K),
        'SharedSyntax':'~SharedSyntax.IntegerToCharDL.P3'(A, K, G).
'SharedPrograms.ImportedLanguage.P6'([], _, _, _, A, A).
'~SharedPrograms.ImportedLanguage.P6'([], _, _, _, A, A).
'SharedPrograms.ImportedLanguage.P6'([A|B], C, D, E, F, G) :-
        user:goedel_freeze(ground([A,E]), ('Lists':'~Lists.Member.P2'(A,E),true->H=F,I=E,J=B;I=[A|E],user:goedel_freeze(ground([D,A]),if(('AVLTrees':'~AVLTrees.AVLSearch.P3'(D,A,'ProgDefs.Module.F3'(_,K,L)),true),('AVLTrees':'AVLTrees.AVLAmend.P6'(F,A,'ProgDefs.Module.F3'('ProgDefs.Exported.C0',K,[]),_,M,_),'SharedPrograms':'SharedPrograms.AddLiftedLanguage.P4'(L,I,M,H)),('SharedPrograms':'SharedPrograms.EmptyCategoryTable.P1'(N),'AVLTrees':'AVLTrees.AVLAmend.P6'(F,A,'ProgDefs.Module.F3'('ProgDefs.Exported.C0',N,[]),_,H,_)))),'AVLTrees':'AVLTrees.AVLSearch.P3'(C,A,'ProgDefs.ModDef.F4'(_,O,_,_)),'Lists':'Lists.Append.P3'(O,B,J))),
        'SharedPrograms.ImportedLanguage.P6'(J, C, D, I, H, G).
'~SharedPrograms.ImportedLanguage.P6'([A|B], C, D, E, F, G) :-
        user:goedel_freeze(ground([A,E]), ('Lists':'~Lists.Member.P2'(A,E),true->H=F,I=E,J=B;I=[A|E],user:goedel_freeze(ground([D,A]),if(('AVLTrees':'~AVLTrees.AVLSearch.P3'(D,A,'ProgDefs.Module.F3'(_,K,L)),true),('AVLTrees':'~AVLTrees.AVLAmend.P6'(F,A,'ProgDefs.Module.F3'('ProgDefs.Exported.C0',K,[]),_,M,_),'SharedPrograms':'~SharedPrograms.AddLiftedLanguage.P4'(L,I,M,H)),('SharedPrograms':'~SharedPrograms.EmptyCategoryTable.P1'(N),'AVLTrees':'~AVLTrees.AVLAmend.P6'(F,A,'ProgDefs.Module.F3'('ProgDefs.Exported.C0',N,[]),_,H,_)))),'AVLTrees':'~AVLTrees.AVLSearch.P3'(C,A,'ProgDefs.ModDef.F4'(_,O,_,_)),'Lists':'~Lists.Append.P3'(O,B,J))),
        '~SharedPrograms.ImportedLanguage.P6'(J, C, D, I, H, G).
'SharedPrograms.HeadAtom.P9'('MetaDefs.PAtom.F1'(A), B, C, C, 'MetaDefs.PAtom.F1'(A), D, D, [], []) :-
        'SharedPrograms.PropositionInLanguage.P2'(A, B).
'~SharedPrograms.HeadAtom.P9'('MetaDefs.PAtom.F1'(A), B, C, C, 'MetaDefs.PAtom.F1'(A), D, D, [], []) :-
        '~SharedPrograms.PropositionInLanguage.P2'(A, B).
'SharedPrograms.HeadAtom.P9'('MetaDefs.Atom.F2'(A,B), C, D, E, 'MetaDefs.Atom.F2'(A,F), G, H, I, J) :-
        'SharedPrograms.PredicateInLanguage.P4'(A, C, _, K),
        'SharedPrograms.MeltTypeList.P4'(K, [], _, I),
        'SharedPrograms.MeltedArgumentTyping.P9'(I, B, C, D, E, F, G, H, J).
'~SharedPrograms.HeadAtom.P9'('MetaDefs.Atom.F2'(A,B), C, D, E, 'MetaDefs.Atom.F2'(A,F), G, H, I, J) :-
        '~SharedPrograms.PredicateInLanguage.P4'(A, C, _, K),
        '~SharedPrograms.MeltTypeList.P4'(K, [], _, I),
        '~SharedPrograms.MeltedArgumentTyping.P9'(I, B, C, D, E, F, G, H, J).
'SharedPrograms.MatchingDescriptor.P5'('ProgDefs.Symbol.F2'(A,B), C, D, E, B) :-
        'SharedPrograms.Accessible.P2'(C, A),
        'SharedPrograms.MatchingName.P3'(B, D, E).
'~SharedPrograms.MatchingDescriptor.P5'('ProgDefs.Symbol.F2'(A,B), C, D, E, B) :-
        '~SharedPrograms.Accessible.P2'(C, A),
        '~SharedPrograms.MatchingName.P3'(B, D, E).
'SharedPrograms.KindHasPart.P2'('ProgDefs.ModuleKind.C0', 'ProgDefs.Module.C0').
'~SharedPrograms.KindHasPart.P2'('ProgDefs.ModuleKind.C0', 'ProgDefs.Module.C0').
'SharedPrograms.KindHasPart.P2'('ProgDefs.NormalKind.C0', A) :-
        'SharedPrograms.FindMember.P2'(A, ['ProgDefs.Export.C0','ProgDefs.Local.C0']).
'~SharedPrograms.KindHasPart.P2'('ProgDefs.NormalKind.C0', A) :-
        '~SharedPrograms.FindMember.P2'(A, ['ProgDefs.Export.C0','ProgDefs.Local.C0']).
'SharedPrograms.KindHasPart.P2'('ProgDefs.ClosedKind.C0', 'ProgDefs.Closed.C0').
'~SharedPrograms.KindHasPart.P2'('ProgDefs.ClosedKind.C0', 'ProgDefs.Closed.C0').
'SharedPrograms.InsertTypeSymbol.P5'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G, 'ProgDefs.Language.F1'(H)) :-
        'AVLTrees':'AVLTrees.AVLIsEmpty.P1'(I),
        'AVLTrees':'AVLTrees.AVLIsEmpty.P1'(J),
        'AVLTrees':'AVLTrees.AVLAmend.P6'(E, A, 'ProgDefs.Module.F3'(K,'ProgDefs.Categories.F2'(L,M),N), 'ProgDefs.Module.F3'('ProgDefs.Hidden.C0','ProgDefs.Categories.F2'(I,J),[]), H, 'ProgDefs.Module.F3'(K,'ProgDefs.Categories.F2'(O,M),N)),
        'AVLTrees':'AVLTrees.AVLAmend.P6'(O, B, P, [], L, Q),
        'SharedPrograms.MatchingName.P3'(G, C, D),
        'SharedPrograms.AddToDescriptorList.P6'(Q, K, C, D, 'ProgDefs.Symbol.F2'(F,G), P).
'~SharedPrograms.InsertTypeSymbol.P5'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G, 'ProgDefs.Language.F1'(H)) :-
        'AVLTrees':'~AVLTrees.AVLIsEmpty.P1'(I),
        'AVLTrees':'~AVLTrees.AVLIsEmpty.P1'(J),
        'AVLTrees':'~AVLTrees.AVLAmend.P6'(E, A, 'ProgDefs.Module.F3'(K,'ProgDefs.Categories.F2'(L,M),N), 'ProgDefs.Module.F3'('ProgDefs.Hidden.C0','ProgDefs.Categories.F2'(I,J),[]), H, 'ProgDefs.Module.F3'(K,'ProgDefs.Categories.F2'(O,M),N)),
        'AVLTrees':'~AVLTrees.AVLAmend.P6'(O, B, P, [], L, Q),
        '~SharedPrograms.MatchingName.P3'(G, C, D),
        '~SharedPrograms.AddToDescriptorList.P6'(Q, K, C, D, 'ProgDefs.Symbol.F2'(F,G), P).
'SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G, 'ProgDefs.Language.F1'(H)) :-
        'AVLTrees':'AVLTrees.AVLIsEmpty.P1'(I),
        'AVLTrees':'AVLTrees.AVLIsEmpty.P1'(J),
        'AVLTrees':'AVLTrees.AVLAmend.P6'(E, A, 'ProgDefs.Module.F3'(K,'ProgDefs.Categories.F2'(L,M),N), 'ProgDefs.Module.F3'('ProgDefs.Hidden.C0','ProgDefs.Categories.F2'(I,J),[]), H, 'ProgDefs.Module.F3'(K,'ProgDefs.Categories.F2'(L,O),N)),
        'AVLTrees':'AVLTrees.AVLAmend.P6'(O, B, P, [], M, Q),
        'SharedPrograms.MatchingName.P3'(G, C, D),
        'SharedPrograms.AddToDescriptorList.P6'(Q, K, C, D, 'ProgDefs.Symbol.F2'(F,G), P).
'~SharedPrograms.InsertSymbol.P5'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F, G, 'ProgDefs.Language.F1'(H)) :-
        'AVLTrees':'~AVLTrees.AVLIsEmpty.P1'(I),
        'AVLTrees':'~AVLTrees.AVLIsEmpty.P1'(J),
        'AVLTrees':'~AVLTrees.AVLAmend.P6'(E, A, 'ProgDefs.Module.F3'(K,'ProgDefs.Categories.F2'(L,M),N), 'ProgDefs.Module.F3'('ProgDefs.Hidden.C0','ProgDefs.Categories.F2'(I,J),[]), H, 'ProgDefs.Module.F3'(K,'ProgDefs.Categories.F2'(L,O),N)),
        'AVLTrees':'~AVLTrees.AVLAmend.P6'(O, B, P, [], M, Q),
        '~SharedPrograms.MatchingName.P3'(G, C, D),
        '~SharedPrograms.AddToDescriptorList.P6'(Q, K, C, D, 'ProgDefs.Symbol.F2'(F,G), P).
'SharedPrograms.IsVariable.P1'('MetaDefs.Var.F2'(_,_)).
'~SharedPrograms.IsVariable.P1'('MetaDefs.Var.F2'(_,_)).
'SharedPrograms.IsVariable.P1'('MetaDefs.Var.F1'(_)).
'~SharedPrograms.IsVariable.P1'('MetaDefs.Var.F1'(_)).
'SharedPrograms.LookupParDict.P4'(A, B, C, D) :-
        'SharedPrograms.LookupParDictAux.P5'(A, B, A, C, D).
'~SharedPrograms.LookupParDict.P4'(A, B, C, D) :-
        '~SharedPrograms.LookupParDictAux.P5'(A, B, A, C, D).
'SharedPrograms.ListExprToIntDL.P6'([A,B], C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('"[', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'SharedPrograms.ChainExprToIntDL.P6'(B, C, D, E, I, J),
        'SharedPrograms.CharDL.P3'('"]', J, G).
'~SharedPrograms.ListExprToIntDL.P6'([A,B], C, D, E, F, G) :-
        '~SharedPrograms.CharDL.P3'('"[', F, H),
        '~SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        '~SharedPrograms.ChainExprToIntDL.P6'(B, C, D, E, I, J),
        '~SharedPrograms.CharDL.P3'('"]', J, G).
'SharedPrograms.LPrec.P2'('Syntax.NoFunctInd.C0', 'SharedPrograms.Infinity.C0').
'~SharedPrograms.LPrec.P2'('Syntax.NoFunctInd.C0', 'SharedPrograms.Infinity.C0').
'SharedPrograms.LPrec.P2'('Syntax.XFX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'~SharedPrograms.LPrec.P2'('Syntax.XFX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.LPrec.P2'('Syntax.XFY.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'~SharedPrograms.LPrec.P2'('Syntax.XFY.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.LPrec.P2'('Syntax.YFX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.Y.C0')).
'~SharedPrograms.LPrec.P2'('Syntax.YFX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.Y.C0')).
'SharedPrograms.LPrec.P2'('Syntax.FX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'~SharedPrograms.LPrec.P2'('Syntax.FX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.LPrec.P2'('Syntax.FY.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'~SharedPrograms.LPrec.P2'('Syntax.FY.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.LPrec.P2'('Syntax.XF.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'~SharedPrograms.LPrec.P2'('Syntax.XF.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.LPrec.P2'('Syntax.YF.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.Y.C0')).
'~SharedPrograms.LPrec.P2'('Syntax.YF.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.Y.C0')).
'SharedPrograms.ListTypesInLanguage.P2'([], _).
'~SharedPrograms.ListTypesInLanguage.P2'([], _).
'SharedPrograms.ListTypesInLanguage.P2'([A|B], C) :-
        'SharedPrograms.TypeInLanguage.P2'(A, C),
        'SharedPrograms.ListTypesInLanguage.P2'(B, C).
'~SharedPrograms.ListTypesInLanguage.P2'([A|B], C) :-
        '~SharedPrograms.TypeInLanguage.P2'(A, C),
        '~SharedPrograms.ListTypesInLanguage.P2'(B, C).
'SharedPrograms.LookupVarTyping.P4'(A, B, C, D) :-
        'SharedPrograms.LookupVarTypingAux.P5'(A, B, A, C, D).
'~SharedPrograms.LookupVarTyping.P4'(A, B, C, D) :-
        '~SharedPrograms.LookupVarTypingAux.P5'(A, B, A, C, D).
'SharedPrograms.LookupParDictAux.P5'([], A, B, ['MetaDefs.!.F2'(A,C)|B], C).
'~SharedPrograms.LookupParDictAux.P5'([], A, B, ['MetaDefs.!.F2'(A,C)|B], C).
'SharedPrograms.LookupParDictAux.P5'(['MetaDefs.!.F2'(B,C)|A], D, E, F, G) :-
        user:goedel_freeze(ground([B,D]), (D=B,true->G=C,F=E;'SharedPrograms':'SharedPrograms.LookupParDictAux.P5'(A,D,E,F,G))).
'~SharedPrograms.LookupParDictAux.P5'(['MetaDefs.!.F2'(B,C)|A], D, E, F, G) :-
        user:goedel_freeze(ground([B,D]), (D=B,true->G=C,F=E;'SharedPrograms':'~SharedPrograms.LookupParDictAux.P5'(A,D,E,F,G))).
'SharedPrograms.LookupVarTypingAux.P5'([], A, B, ['MetaDefs.@.F2'(A,C)|B], C).
'~SharedPrograms.LookupVarTypingAux.P5'([], A, B, ['MetaDefs.@.F2'(A,C)|B], C).
'SharedPrograms.LookupVarTypingAux.P5'(['MetaDefs.@.F2'(B,C)|A], D, E, F, G) :-
        user:goedel_freeze(ground([B,D]), (D=B,true->G=C,F=E;'SharedPrograms':'SharedPrograms.LookupVarTypingAux.P5'(A,D,E,F,G))).
'~SharedPrograms.LookupVarTypingAux.P5'(['MetaDefs.@.F2'(B,C)|A], D, E, F, G) :-
        user:goedel_freeze(ground([B,D]), (D=B,true->G=C,F=E;'SharedPrograms':'~SharedPrograms.LookupVarTypingAux.P5'(A,D,E,F,G))).
'SharedPrograms.MeltedFormulaTyping.P8'(A, B, C, D, E, F, G, H) :-
        user:goedel_freeze(ground([A]), if((A='MetaDefs.<-''.F2'(I,J),true),('SharedPrograms':'SharedPrograms.MeltedBodyTyping.P8'(I,B,C,K,L,F,M,N),user:goedel_freeze(ground([N]),(N=[],true->'SharedPrograms':'SharedPrograms.MeltedBodyTyping.P8'(J,B,K,D,O,M,G,H),E='MetaDefs.<-''.F2'(L,O);H=N))),'SharedPrograms':'SharedPrograms.MeltedBodyTyping.P8'(A,B,C,D,E,F,G,H))).
'~SharedPrograms.MeltedFormulaTyping.P8'(A, B, C, D, E, F, G, H) :-
        user:goedel_freeze(ground([A]), if((A='MetaDefs.<-''.F2'(I,J),true),('SharedPrograms':'~SharedPrograms.MeltedBodyTyping.P8'(I,B,C,K,L,F,M,N),user:goedel_freeze(ground([N]),(N=[],true->'SharedPrograms':'~SharedPrograms.MeltedBodyTyping.P8'(J,B,K,D,O,M,G,H),E='MetaDefs.<-''.F2'(L,O);H=N))),'SharedPrograms':'~SharedPrograms.MeltedBodyTyping.P8'(A,B,C,D,E,F,G,H))).
'SharedPrograms.MeltVariableTyping.P4'([], _, _, []).
'~SharedPrograms.MeltVariableTyping.P4'([], _, _, []).
'SharedPrograms.MeltVariableTyping.P4'(['MetaDefs.@.F2'(B,C)|A], D, E, ['MetaDefs.@.F2'(B,G)|F]) :-
        'SharedPrograms.IsVariable.P1'(B),
        'SharedPrograms.TypeInLanguage.P2'(C, D),
        'SharedPrograms.MeltType.P4'(C, E, H, G),
        'SharedPrograms.MeltVariableTyping.P4'(A, D, H, F).
'~SharedPrograms.MeltVariableTyping.P4'(['MetaDefs.@.F2'(B,C)|A], D, E, ['MetaDefs.@.F2'(B,G)|F]) :-
        '~SharedPrograms.IsVariable.P1'(B),
        '~SharedPrograms.TypeInLanguage.P2'(C, D),
        '~SharedPrograms.MeltType.P4'(C, E, H, G),
        '~SharedPrograms.MeltVariableTyping.P4'(A, D, H, F).
'SharedPrograms.MeltType.P4'('MetaDefs.Par.F2'(A,B), C, D, E) :-
        'SharedPrograms.LookupParDict.P4'(C, 'MetaDefs.Par.F2'(A,B), D, E).
'~SharedPrograms.MeltType.P4'('MetaDefs.Par.F2'(A,B), C, D, E) :-
        '~SharedPrograms.LookupParDict.P4'(C, 'MetaDefs.Par.F2'(A,B), D, E).
'SharedPrograms.MeltType.P4'('MetaDefs.Par.F1'(A), B, C, D) :-
        'SharedPrograms.LookupParDict.P4'(B, 'MetaDefs.Par.F1'(A), C, D).
'~SharedPrograms.MeltType.P4'('MetaDefs.Par.F1'(A), B, C, D) :-
        '~SharedPrograms.LookupParDict.P4'(B, 'MetaDefs.Par.F1'(A), C, D).
'SharedPrograms.MeltType.P4'('MetaDefs.BType.F1'(A), B, B, 'MetaDefs.BType.F1'(A)).
'~SharedPrograms.MeltType.P4'('MetaDefs.BType.F1'(A), B, B, 'MetaDefs.BType.F1'(A)).
'SharedPrograms.MeltType.P4'('MetaDefs.XBType.F1'(A), B, B, 'MetaDefs.XBType.F1'(A)).
'~SharedPrograms.MeltType.P4'('MetaDefs.XBType.F1'(A), B, B, 'MetaDefs.XBType.F1'(A)).
'SharedPrograms.MeltType.P4'('MetaDefs.Type.F2'(A,B), C, D, 'MetaDefs.Type.F2'(A,E)) :-
        'SharedPrograms.MeltTypeList.P4'(B, C, D, E).
'~SharedPrograms.MeltType.P4'('MetaDefs.Type.F2'(A,B), C, D, 'MetaDefs.Type.F2'(A,E)) :-
        '~SharedPrograms.MeltTypeList.P4'(B, C, D, E).
'SharedPrograms.MeltType.P4'('MetaDefs.XType.F2'(A,B), C, D, 'MetaDefs.XType.F2'(A,E)) :-
        'SharedPrograms.MeltTypeList.P4'(B, C, D, E).
'~SharedPrograms.MeltType.P4'('MetaDefs.XType.F2'(A,B), C, D, 'MetaDefs.XType.F2'(A,E)) :-
        '~SharedPrograms.MeltTypeList.P4'(B, C, D, E).
'SharedPrograms.MatchingName.P3'('ProgDefs.BaseDecl.C0', 'MetaDefs.Base.C0', 0).
'~SharedPrograms.MatchingName.P3'('ProgDefs.BaseDecl.C0', 'MetaDefs.Base.C0', 0).
'SharedPrograms.MatchingName.P3'('ProgDefs.ConstructorDecl.F1'(A), 'MetaDefs.Constructor.C0', A).
'~SharedPrograms.MatchingName.P3'('ProgDefs.ConstructorDecl.F1'(A), 'MetaDefs.Constructor.C0', A).
'SharedPrograms.MatchingName.P3'('ProgDefs.ConstantDecl.F1'(_), 'MetaDefs.Constant.C0', 0).
'~SharedPrograms.MatchingName.P3'('ProgDefs.ConstantDecl.F1'(_), 'MetaDefs.Constant.C0', 0).
'SharedPrograms.MatchingName.P3'('ProgDefs.FunctionDecl.F4'(A,_,_,_), 'MetaDefs.Function.C0', A).
'~SharedPrograms.MatchingName.P3'('ProgDefs.FunctionDecl.F4'(A,_,_,_), 'MetaDefs.Function.C0', A).
'SharedPrograms.MatchingName.P3'('ProgDefs.PropositionDecl.C0', 'MetaDefs.Proposition.C0', 0).
'~SharedPrograms.MatchingName.P3'('ProgDefs.PropositionDecl.C0', 'MetaDefs.Proposition.C0', 0).
'SharedPrograms.MatchingName.P3'('ProgDefs.PredicateDecl.F3'(A,_,_), 'MetaDefs.Predicate.C0', A).
'~SharedPrograms.MatchingName.P3'('ProgDefs.PredicateDecl.F3'(A,_,_), 'MetaDefs.Predicate.C0', A).
'SharedPrograms.MeltTypeList.P4'([], A, A, []).
'~SharedPrograms.MeltTypeList.P4'([], A, A, []).
'SharedPrograms.MeltTypeList.P4'([A|B], C, D, [E|F]) :-
        'SharedPrograms.MeltType.P4'(A, C, G, E),
        'SharedPrograms.MeltTypeList.P4'(B, G, D, F).
'~SharedPrograms.MeltTypeList.P4'([A|B], C, D, [E|F]) :-
        '~SharedPrograms.MeltType.P4'(A, C, G, E),
        '~SharedPrograms.MeltTypeList.P4'(B, G, D, F).
'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.PAtom.F1'(A), B, C, C, 'MetaDefs.PAtom.F1'(A), D, D, []) :-
        'SharedPrograms.PropositionInLanguage.P2'(A, B).
'~SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.PAtom.F1'(A), B, C, C, 'MetaDefs.PAtom.F1'(A), D, D, []) :-
        '~SharedPrograms.PropositionInLanguage.P2'(A, B).
'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.XPAtom.F1'(A), B, C, C, 'MetaDefs.XPAtom.F1'(A), D, D, []) :-
        'SharedPrograms.PropositionInLanguage.P2'(A, B).
'~SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.XPAtom.F1'(A), B, C, C, 'MetaDefs.XPAtom.F1'(A), D, D, []) :-
        '~SharedPrograms.PropositionInLanguage.P2'(A, B).
'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.Atom.F2'(A,B), C, D, E, 'MetaDefs.Atom.F2'(A,F), G, H, I) :-
        'SharedPrograms.PredicateInLanguage.P4'(A, C, _, J),
        'SharedPrograms.MeltTypeList.P4'(J, [], _, K),
        'SharedPrograms.MeltedArgumentTyping.P9'(K, B, C, D, E, F, G, H, I).
'~SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.Atom.F2'(A,B), C, D, E, 'MetaDefs.Atom.F2'(A,F), G, H, I) :-
        '~SharedPrograms.PredicateInLanguage.P4'(A, C, _, J),
        '~SharedPrograms.MeltTypeList.P4'(J, [], _, K),
        '~SharedPrograms.MeltedArgumentTyping.P9'(K, B, C, D, E, F, G, H, I).
'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.XAtom.F2'(A,B), C, D, E, 'MetaDefs.XAtom.F2'(A,F), G, H, I) :-
        'SharedPrograms.PredicateInLanguage.P4'(A, C, _, J),
        'SharedPrograms.MeltTypeList.P4'(J, [], _, K),
        'SharedPrograms.MeltedArgumentTyping.P9'(K, B, C, D, E, F, G, H, I).
'~SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.XAtom.F2'(A,B), C, D, E, 'MetaDefs.XAtom.F2'(A,F), G, H, I) :-
        '~SharedPrograms.PredicateInLanguage.P4'(A, C, _, J),
        '~SharedPrograms.MeltTypeList.P4'(J, [], _, K),
        '~SharedPrograms.MeltedArgumentTyping.P9'(K, B, C, D, E, F, G, H, I).
'SharedPrograms.MeltedArgumentTyping.P9'([], [], _, A, A, [], B, B, []).
'~SharedPrograms.MeltedArgumentTyping.P9'([], [], _, A, A, [], B, B, []).
'SharedPrograms.MeltedArgumentTyping.P9'([A|B], [C|D], E, F, G, [H|I], J, K, L) :-
        'SharedPrograms.MeltedTermType.P9'(C, A, E, F, M, H, J, N, O),
        user:goedel_freeze(ground([O]), (O=[],true->'SharedPrograms':'SharedPrograms.MeltedArgumentTyping.P9'(B,D,E,M,G,I,N,K,L);L=O)).
'~SharedPrograms.MeltedArgumentTyping.P9'([A|B], [C|D], E, F, G, [H|I], J, K, L) :-
        '~SharedPrograms.MeltedTermType.P9'(C, A, E, F, M, H, J, N, O),
        user:goedel_freeze(ground([O]), (O=[],true->'SharedPrograms':'~SharedPrograms.MeltedArgumentTyping.P9'(B,D,E,M,G,I,N,K,L);L=O)).
'SharedPrograms.MeltedBodyTyping.P8'(A, B, C, D, E, F, G, H) :-
        user:goedel_freeze(ground([A]), (A='MetaDefs.Empty.C0',true->D=C,F=G,H=[],E='MetaDefs.Empty.C0';user:goedel_freeze(ground([A]),if((A='MetaDefs.&''.F2'(I,J),true),('SharedPrograms':'SharedPrograms.MeltedBodyTyping.P8'(I,B,C,K,L,F,M,N),user:goedel_freeze(ground([N]),(N=[],true->'SharedPrograms':'SharedPrograms.MeltedBodyTyping.P8'(J,B,K,D,O,M,G,H),E='MetaDefs.&''.F2'(L,O);H=N))),user:goedel_freeze(ground([A]),if((A='MetaDefs.Commit.F2'(P,Q),true),('SharedPrograms':'SharedPrograms.MeltedBodyTyping.P8'(Q,B,C,D,L,F,G,H),E='MetaDefs.Commit.F2'(P,L)),'SharedPrograms':'SharedPrograms.MeltedStandardTyping.P8'(A,B,C,D,E,F,G,H))))))).
'~SharedPrograms.MeltedBodyTyping.P8'(A, B, C, D, E, F, G, H) :-
        user:goedel_freeze(ground([A]), (A='MetaDefs.Empty.C0',true->D=C,F=G,H=[],E='MetaDefs.Empty.C0';user:goedel_freeze(ground([A]),if((A='MetaDefs.&''.F2'(I,J),true),('SharedPrograms':'~SharedPrograms.MeltedBodyTyping.P8'(I,B,C,K,L,F,M,N),user:goedel_freeze(ground([N]),(N=[],true->'SharedPrograms':'~SharedPrograms.MeltedBodyTyping.P8'(J,B,K,D,O,M,G,H),E='MetaDefs.&''.F2'(L,O);H=N))),user:goedel_freeze(ground([A]),if((A='MetaDefs.Commit.F2'(P,Q),true),('SharedPrograms':'~SharedPrograms.MeltedBodyTyping.P8'(Q,B,C,D,L,F,G,H),E='MetaDefs.Commit.F2'(P,L)),'SharedPrograms':'~SharedPrograms.MeltedStandardTyping.P8'(A,B,C,D,E,F,G,H))))))).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Var.F2'(A,B), C, _, D, E, 'MetaDefs.Var.F2'(A,B), F, F, G) :-
        'SharedPrograms.LookupVarTyping.P4'(D, 'MetaDefs.Var.F2'(A,B), E, H),
        'SharedPrograms.UnifyMeltedTypes.P4'(C, H, 'MetaDefs.Var.F2'(A,B), G).
'~SharedPrograms.MeltedTermType.P9'('MetaDefs.Var.F2'(A,B), C, _, D, E, 'MetaDefs.Var.F2'(A,B), F, F, G) :-
        '~SharedPrograms.LookupVarTyping.P4'(D, 'MetaDefs.Var.F2'(A,B), E, H),
        '~SharedPrograms.UnifyMeltedTypes.P4'(C, H, 'MetaDefs.Var.F2'(A,B), G).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Var.F1'(A), B, _, C, D, 'MetaDefs.Var.F1'(A), E, E, F) :-
        'SharedPrograms.LookupVarTyping.P4'(C, 'MetaDefs.Var.F1'(A), D, G),
        'SharedPrograms.UnifyMeltedTypes.P4'(B, G, 'MetaDefs.Var.F1'(A), F).
'~SharedPrograms.MeltedTermType.P9'('MetaDefs.Var.F1'(A), B, _, C, D, 'MetaDefs.Var.F1'(A), E, E, F) :-
        '~SharedPrograms.LookupVarTyping.P4'(C, 'MetaDefs.Var.F1'(A), D, G),
        '~SharedPrograms.UnifyMeltedTypes.P4'(B, G, 'MetaDefs.Var.F1'(A), F).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.CTerm.F1'(A), B, C, D, D, 'MetaDefs.CTerm.F1'(A), E, E, F) :-
        'SharedPrograms.ConstantInLanguage.P3'(A, C, G),
        'SharedPrograms.MeltType.P4'(G, [], _, H),
        'SharedPrograms.UnifyMeltedTypes.P4'(B, H, 'MetaDefs.CTerm.F1'(A), F).
'~SharedPrograms.MeltedTermType.P9'('MetaDefs.CTerm.F1'(A), B, C, D, D, 'MetaDefs.CTerm.F1'(A), E, E, F) :-
        '~SharedPrograms.ConstantInLanguage.P3'(A, C, G),
        '~SharedPrograms.MeltType.P4'(G, [], _, H),
        '~SharedPrograms.UnifyMeltedTypes.P4'(B, H, 'MetaDefs.CTerm.F1'(A), F).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.XCTerm.F2'(A,_), B, C, D, D, 'MetaDefs.XCTerm.F2'(A,E), F, F, G) :-
        'SharedPrograms.ConstantInLanguage.P3'(A, C, H),
        'SharedPrograms.MeltType.P4'(H, [], _, E),
        'SharedPrograms.UnifyMeltedTypes.P4'(B, E, 'MetaDefs.CTerm.F1'(A), G).
'~SharedPrograms.MeltedTermType.P9'('MetaDefs.XCTerm.F2'(A,_), B, C, D, D, 'MetaDefs.XCTerm.F2'(A,E), F, F, G) :-
        '~SharedPrograms.ConstantInLanguage.P3'(A, C, H),
        '~SharedPrograms.MeltType.P4'(H, [], _, E),
        '~SharedPrograms.UnifyMeltedTypes.P4'(B, E, 'MetaDefs.CTerm.F1'(A), G).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Term.F2'(A,B), C, D, E, F, 'MetaDefs.Term.F2'(A,G), H, I, J) :-
        'SharedPrograms.FunctionInLanguage.P5'(A, D, _, K, L),
        'SharedPrograms.MeltType.P4'(L, [], M, N),
        'SharedPrograms.MeltTypeList.P4'(K, M, _, O),
        'SharedPrograms.UnifyMeltedTypes.P4'(C, N, 'MetaDefs.Term.F2'(A,B), P),
        user:goedel_freeze(ground([P]), (P=[],true->'SharedPrograms':'SharedPrograms.MeltedArgumentTyping.P9'(O,B,D,E,F,G,H,I,J);J=P)).
'~SharedPrograms.MeltedTermType.P9'('MetaDefs.Term.F2'(A,B), C, D, E, F, 'MetaDefs.Term.F2'(A,G), H, I, J) :-
        '~SharedPrograms.FunctionInLanguage.P5'(A, D, _, K, L),
        '~SharedPrograms.MeltType.P4'(L, [], M, N),
        '~SharedPrograms.MeltTypeList.P4'(K, M, _, O),
        '~SharedPrograms.UnifyMeltedTypes.P4'(C, N, 'MetaDefs.Term.F2'(A,B), P),
        user:goedel_freeze(ground([P]), (P=[],true->'SharedPrograms':'~SharedPrograms.MeltedArgumentTyping.P9'(O,B,D,E,F,G,H,I,J);J=P)).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.XTerm.F3'(A,B,_), C, D, E, F, 'MetaDefs.XTerm.F3'(A,G,H), I, J, K) :-
        'SharedPrograms.FunctionInLanguage.P5'(A, D, _, L, M),
        'SharedPrograms.MeltType.P4'(M, [], N, H),
        'SharedPrograms.MeltTypeList.P4'(L, N, _, O),
        'SharedPrograms.UnifyMeltedTypes.P4'(C, H, 'MetaDefs.Term.F2'(A,B), P),
        user:goedel_freeze(ground([P]), (P=[],true->'SharedPrograms':'SharedPrograms.MeltedArgumentTyping.P9'(O,B,D,E,F,G,I,J,K);K=P)).
'~SharedPrograms.MeltedTermType.P9'('MetaDefs.XTerm.F3'(A,B,_), C, D, E, F, 'MetaDefs.XTerm.F3'(A,G,H), I, J, K) :-
        '~SharedPrograms.FunctionInLanguage.P5'(A, D, _, L, M),
        '~SharedPrograms.MeltType.P4'(M, [], N, H),
        '~SharedPrograms.MeltTypeList.P4'(L, N, _, O),
        '~SharedPrograms.UnifyMeltedTypes.P4'(C, H, 'MetaDefs.Term.F2'(A,B), P),
        user:goedel_freeze(ground([P]), (P=[],true->'SharedPrograms':'~SharedPrograms.MeltedArgumentTyping.P9'(O,B,D,E,F,G,I,J,K);K=P)).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Int.F1'(A), B, C, D, D, 'MetaDefs.Int.F1'(A), E, E, F) :-
        G='MetaDefs.Name.F4'('"Integers','"Integer','MetaDefs.Base.C0',0),
        'SharedPrograms.BaseInLanguage.P2'(G, C),
        'SharedPrograms.UnifyMeltedTypes.P4'(B, 'MetaDefs.BType.F1'(G), 'MetaDefs.Int.F1'(A), F).
'~SharedPrograms.MeltedTermType.P9'('MetaDefs.Int.F1'(A), B, C, D, D, 'MetaDefs.Int.F1'(A), E, E, F) :-
        G='MetaDefs.Name.F4'('"Integers','"Integer','MetaDefs.Base.C0',0),
        '~SharedPrograms.BaseInLanguage.P2'(G, C),
        '~SharedPrograms.UnifyMeltedTypes.P4'(B, 'MetaDefs.BType.F1'(G), 'MetaDefs.Int.F1'(A), F).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Str.F1'(A), B, C, D, D, 'MetaDefs.Str.F1'(A), E, E, F) :-
        G='MetaDefs.Name.F4'('"Strings','"String','MetaDefs.Base.C0',0),
        'SharedPrograms.BaseInLanguage.P2'(G, C),
        'SharedPrograms.UnifyMeltedTypes.P4'(B, 'MetaDefs.BType.F1'(G), 'MetaDefs.Str.F1'(A), F).
'~SharedPrograms.MeltedTermType.P9'('MetaDefs.Str.F1'(A), B, C, D, D, 'MetaDefs.Str.F1'(A), E, E, F) :-
        G='MetaDefs.Name.F4'('"Strings','"String','MetaDefs.Base.C0',0),
        '~SharedPrograms.BaseInLanguage.P2'(G, C),
        '~SharedPrograms.UnifyMeltedTypes.P4'(B, 'MetaDefs.BType.F1'(G), 'MetaDefs.Str.F1'(A), F).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Prm.F1'(A), B, C, D, D, 'MetaDefs.Prm.F1'(A), E, E, F) :-
        G='MetaDefs.Name.F4'('"ProgDefs','"Program','MetaDefs.Base.C0',0),
        'SharedPrograms.BaseInLanguage.P2'(G, C),
        'SharedPrograms.UnifyMeltedTypes.P4'(B, 'MetaDefs.BType.F1'(G), 'MetaDefs.Prm.F1'(A), F).
'~SharedPrograms.MeltedTermType.P9'('MetaDefs.Prm.F1'(A), B, C, D, D, 'MetaDefs.Prm.F1'(A), E, E, F) :-
        G='MetaDefs.Name.F4'('"ProgDefs','"Program','MetaDefs.Base.C0',0),
        '~SharedPrograms.BaseInLanguage.P2'(G, C),
        '~SharedPrograms.UnifyMeltedTypes.P4'(B, 'MetaDefs.BType.F1'(G), 'MetaDefs.Prm.F1'(A), F).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.SuchThat.F2'(A,B), C, D, E, F, 'MetaDefs.SuchThat.F2'(G,H), I, J, K) :-
        L='MetaDefs.Name.F4'('"Sets','"Set','MetaDefs.Constructor.C0',1),
        'SharedPrograms.ConstructorInLanguage.P3'(L, D, _),
        'SharedPrograms.UnifyMeltedTypes.P4'(C, 'MetaDefs.Type.F2'(L,[M]), 'MetaDefs.SuchThat.F2'(A,B), N),
        user:goedel_freeze(ground([N]), (N=[],true->'SharedSyntax':'SharedSyntax.STermFreeVars.P2'(A,O),'SharedPrograms':'SharedPrograms.DivideBindings.P4'(O,E,P,Q),'SharedPrograms':'SharedPrograms.MeltedStandardTyping.P8'(B,D,P,R,H,I,S,T),user:goedel_freeze(ground([T]),(T=[],true->'SharedPrograms':'SharedPrograms.MeltedTermType.P9'(A,M,D,R,U,G,S,J,K),user:goedel_freeze(ground([K]),(K=[],true->'SharedPrograms':'SharedPrograms.DivideBindings.P4'(O,U,V,_),'Lists':'Lists.Append.P3'(Q,V,F);true));K=T));K=N)).
'~SharedPrograms.MeltedTermType.P9'('MetaDefs.SuchThat.F2'(A,B), C, D, E, F, 'MetaDefs.SuchThat.F2'(G,H), I, J, K) :-
        L='MetaDefs.Name.F4'('"Sets','"Set','MetaDefs.Constructor.C0',1),
        '~SharedPrograms.ConstructorInLanguage.P3'(L, D, _),
        '~SharedPrograms.UnifyMeltedTypes.P4'(C, 'MetaDefs.Type.F2'(L,[M]), 'MetaDefs.SuchThat.F2'(A,B), N),
        user:goedel_freeze(ground([N]), (N=[],true->'SharedSyntax':'~SharedSyntax.STermFreeVars.P2'(A,O),'SharedPrograms':'~SharedPrograms.DivideBindings.P4'(O,E,P,Q),'SharedPrograms':'~SharedPrograms.MeltedStandardTyping.P8'(B,D,P,R,H,I,S,T),user:goedel_freeze(ground([T]),(T=[],true->'SharedPrograms':'~SharedPrograms.MeltedTermType.P9'(A,M,D,R,U,G,S,J,K),user:goedel_freeze(ground([K]),(K=[],true->'SharedPrograms':'~SharedPrograms.DivideBindings.P4'(O,U,V,_),'Lists':'~Lists.Append.P3'(Q,V,F);true));K=T));K=N)).
'SharedPrograms.MeltedTermType.P9'('MetaDefs.Num.F1'(A), B, C, D, D, E, F, G, H) :-
        user:goedel_freeze(ground([C]), ('SharedPrograms':'~SharedPrograms.BaseInLanguage.P2'('MetaDefs.Name.F4'('"Rationals','"Rational','MetaDefs.Base.C0',0),C),true->F=['SharedPrograms.NumType.F3'(A,'MetaDefs.BType.F1'(I),E)|G],'SharedPrograms':'SharedPrograms.UnifyMeltedTypes.P4'(B,'MetaDefs.BType.F1'(I),'MetaDefs.Num.F1'(A),H);'SharedPrograms':'SharedPrograms.MeltedTermType.P9'('MetaDefs.Int.F1'(A),B,C,D,_,E,F,G,H))).
'~SharedPrograms.MeltedTermType.P9'('MetaDefs.Num.F1'(A), B, C, D, D, E, F, G, H) :-
        user:goedel_freeze(ground([C]), ('SharedPrograms':'~SharedPrograms.BaseInLanguage.P2'('MetaDefs.Name.F4'('"Rationals','"Rational','MetaDefs.Base.C0',0),C),true->F=['SharedPrograms.NumType.F3'(A,'MetaDefs.BType.F1'(I),E)|G],'SharedPrograms':'~SharedPrograms.UnifyMeltedTypes.P4'(B,'MetaDefs.BType.F1'(I),'MetaDefs.Num.F1'(A),H);'SharedPrograms':'~SharedPrograms.MeltedTermType.P9'('MetaDefs.Int.F1'(A),B,C,D,_,E,F,G,H))).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.ISTE.F4'(A,B,C,D), E, F, G, 'MetaDefs.ISTE.F4'(A,H,I,J), K, L, M) :-
        'SharedPrograms.DivideBindings.P4'(A, F, N, O),
        'SharedPrograms.MeltedStandardTyping.P8'(B, E, N, P, H, K, Q, R),
        user:goedel_freeze(ground([R]), (R=[],true->'SharedPrograms':'SharedPrograms.MeltedStandardTyping.P8'(C,E,P,S,I,Q,T,U),user:goedel_freeze(ground([U]),(U=[],true->'SharedPrograms':'SharedPrograms.DivideBindings.P4'(A,S,V,_),'Lists':'Lists.Append.P3'(O,V,W),'SharedPrograms':'SharedPrograms.MeltedStandardTyping.P8'(D,E,W,G,J,T,L,M);M=U));M=R)).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.ISTE.F4'(A,B,C,D), E, F, G, 'MetaDefs.ISTE.F4'(A,H,I,J), K, L, M) :-
        '~SharedPrograms.DivideBindings.P4'(A, F, N, O),
        '~SharedPrograms.MeltedStandardTyping.P8'(B, E, N, P, H, K, Q, R),
        user:goedel_freeze(ground([R]), (R=[],true->'SharedPrograms':'~SharedPrograms.MeltedStandardTyping.P8'(C,E,P,S,I,Q,T,U),user:goedel_freeze(ground([U]),(U=[],true->'SharedPrograms':'~SharedPrograms.DivideBindings.P4'(A,S,V,_),'Lists':'~Lists.Append.P3'(O,V,W),'SharedPrograms':'~SharedPrograms.MeltedStandardTyping.P8'(D,E,W,G,J,T,L,M);M=U));M=R)).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.IST.F3'(A,B,C), D, E, F, 'MetaDefs.IST.F3'(A,G,H), I, J, K) :-
        'SharedPrograms.DivideBindings.P4'(A, E, L, M),
        'SharedPrograms.MeltedStandardTyping.P8'(B, D, L, N, G, I, O, P),
        user:goedel_freeze(ground([P]), (P=[],true->'SharedPrograms':'SharedPrograms.MeltedStandardTyping.P8'(C,D,N,Q,H,O,J,K),user:goedel_freeze(ground([K]),(K=[],true->'SharedPrograms':'SharedPrograms.DivideBindings.P4'(A,Q,R,_),'Lists':'Lists.Append.P3'(M,R,F);true));K=P)).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.IST.F3'(A,B,C), D, E, F, 'MetaDefs.IST.F3'(A,G,H), I, J, K) :-
        '~SharedPrograms.DivideBindings.P4'(A, E, L, M),
        '~SharedPrograms.MeltedStandardTyping.P8'(B, D, L, N, G, I, O, P),
        user:goedel_freeze(ground([P]), (P=[],true->'SharedPrograms':'~SharedPrograms.MeltedStandardTyping.P8'(C,D,N,Q,H,O,J,K),user:goedel_freeze(ground([K]),(K=[],true->'SharedPrograms':'~SharedPrograms.DivideBindings.P4'(A,Q,R,_),'Lists':'~Lists.Append.P3'(M,R,F);true));K=P)).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.ITE.F3'(A,B,C), D, E, F, 'MetaDefs.ITE.F3'(G,H,I), J, K, L) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, D, E, M, G, J, N, O),
        user:goedel_freeze(ground([O]), (O=[],true->'SharedPrograms':'SharedPrograms.MeltedStandardTyping.P8'(B,D,M,P,H,N,Q,R),user:goedel_freeze(ground([R]),(R=[],true->'SharedPrograms':'SharedPrograms.MeltedStandardTyping.P8'(C,D,P,F,I,Q,K,L);L=R));L=O)).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.ITE.F3'(A,B,C), D, E, F, 'MetaDefs.ITE.F3'(G,H,I), J, K, L) :-
        '~SharedPrograms.MeltedStandardTyping.P8'(A, D, E, M, G, J, N, O),
        user:goedel_freeze(ground([O]), (O=[],true->'SharedPrograms':'~SharedPrograms.MeltedStandardTyping.P8'(B,D,M,P,H,N,Q,R),user:goedel_freeze(ground([R]),(R=[],true->'SharedPrograms':'~SharedPrograms.MeltedStandardTyping.P8'(C,D,P,F,I,Q,K,L);L=R));L=O)).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.IT.F2'(A,B), C, D, E, 'MetaDefs.IT.F2'(F,G), H, I, J) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        user:goedel_freeze(ground([M]), (M=[],true->'SharedPrograms':'SharedPrograms.MeltedStandardTyping.P8'(B,C,K,E,G,L,I,J);J=M)).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.IT.F2'(A,B), C, D, E, 'MetaDefs.IT.F2'(F,G), H, I, J) :-
        '~SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        user:goedel_freeze(ground([M]), (M=[],true->'SharedPrograms':'~SharedPrograms.MeltedStandardTyping.P8'(B,C,K,E,G,L,I,J);J=M)).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.&''.F2'(A,B), C, D, E, 'MetaDefs.&''.F2'(F,G), H, I, J) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        user:goedel_freeze(ground([M]), (M=[],true->'SharedPrograms':'SharedPrograms.MeltedStandardTyping.P8'(B,C,K,E,G,L,I,J);J=M)).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.&''.F2'(A,B), C, D, E, 'MetaDefs.&''.F2'(F,G), H, I, J) :-
        '~SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        user:goedel_freeze(ground([M]), (M=[],true->'SharedPrograms':'~SharedPrograms.MeltedStandardTyping.P8'(B,C,K,E,G,L,I,J);J=M)).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.\/''.F2'(A,B), C, D, E, 'MetaDefs.\/''.F2'(F,G), H, I, J) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        user:goedel_freeze(ground([M]), (M=[],true->'SharedPrograms':'SharedPrograms.MeltedStandardTyping.P8'(B,C,K,E,G,L,I,J);J=M)).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.\/''.F2'(A,B), C, D, E, 'MetaDefs.\/''.F2'(F,G), H, I, J) :-
        '~SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        user:goedel_freeze(ground([M]), (M=[],true->'SharedPrograms':'~SharedPrograms.MeltedStandardTyping.P8'(B,C,K,E,G,L,I,J);J=M)).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.->''.F2'(A,B), C, D, E, 'MetaDefs.->''.F2'(F,G), H, I, J) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        user:goedel_freeze(ground([M]), (M=[],true->'SharedPrograms':'SharedPrograms.MeltedStandardTyping.P8'(B,C,K,E,G,L,I,J);J=M)).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.->''.F2'(A,B), C, D, E, 'MetaDefs.->''.F2'(F,G), H, I, J) :-
        '~SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        user:goedel_freeze(ground([M]), (M=[],true->'SharedPrograms':'~SharedPrograms.MeltedStandardTyping.P8'(B,C,K,E,G,L,I,J);J=M)).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.<-''.F2'(A,B), C, D, E, 'MetaDefs.<-''.F2'(F,G), H, I, J) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        user:goedel_freeze(ground([M]), (M=[],true->'SharedPrograms':'SharedPrograms.MeltedStandardTyping.P8'(B,C,K,E,G,L,I,J);J=M)).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.<-''.F2'(A,B), C, D, E, 'MetaDefs.<-''.F2'(F,G), H, I, J) :-
        '~SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        user:goedel_freeze(ground([M]), (M=[],true->'SharedPrograms':'~SharedPrograms.MeltedStandardTyping.P8'(B,C,K,E,G,L,I,J);J=M)).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.<->''.F2'(A,B), C, D, E, 'MetaDefs.<->''.F2'(F,G), H, I, J) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        user:goedel_freeze(ground([M]), (M=[],true->'SharedPrograms':'SharedPrograms.MeltedStandardTyping.P8'(B,C,K,E,G,L,I,J);J=M)).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.<->''.F2'(A,B), C, D, E, 'MetaDefs.<->''.F2'(F,G), H, I, J) :-
        '~SharedPrograms.MeltedStandardTyping.P8'(A, C, D, K, F, H, L, M),
        user:goedel_freeze(ground([M]), (M=[],true->'SharedPrograms':'~SharedPrograms.MeltedStandardTyping.P8'(B,C,K,E,G,L,I,J);J=M)).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.~''.F1'(A), B, C, D, 'MetaDefs.~''.F1'(E), F, G, H) :-
        'SharedPrograms.MeltedStandardTyping.P8'(A, B, C, D, E, F, G, H).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.~''.F1'(A), B, C, D, 'MetaDefs.~''.F1'(E), F, G, H) :-
        '~SharedPrograms.MeltedStandardTyping.P8'(A, B, C, D, E, F, G, H).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.All.F2'(A,B), C, D, E, 'MetaDefs.All.F2'(A,F), G, H, I) :-
        'SharedPrograms.DivideBindings.P4'(A, D, J, K),
        'SharedPrograms.MeltedStandardTyping.P8'(B, C, J, L, F, G, H, I),
        'SharedPrograms.DivideBindings.P4'(A, L, M, _),
        'Lists':'Lists.Append.P3'(K, M, E).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.All.F2'(A,B), C, D, E, 'MetaDefs.All.F2'(A,F), G, H, I) :-
        '~SharedPrograms.DivideBindings.P4'(A, D, J, K),
        '~SharedPrograms.MeltedStandardTyping.P8'(B, C, J, L, F, G, H, I),
        '~SharedPrograms.DivideBindings.P4'(A, L, M, _),
        'Lists':'~Lists.Append.P3'(K, M, E).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.Some.F2'(A,B), C, D, E, 'MetaDefs.Some.F2'(A,F), G, H, I) :-
        'SharedPrograms.DivideBindings.P4'(A, D, J, K),
        'SharedPrograms.MeltedStandardTyping.P8'(B, C, J, L, F, G, H, I),
        'SharedPrograms.DivideBindings.P4'(A, L, M, _),
        'Lists':'Lists.Append.P3'(K, M, E).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.Some.F2'(A,B), C, D, E, 'MetaDefs.Some.F2'(A,F), G, H, I) :-
        '~SharedPrograms.DivideBindings.P4'(A, D, J, K),
        '~SharedPrograms.MeltedStandardTyping.P8'(B, C, J, L, F, G, H, I),
        '~SharedPrograms.DivideBindings.P4'(A, L, M, _),
        'Lists':'~Lists.Append.P3'(K, M, E).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.PAtom.F1'(A), B, C, D, E, F, G, H) :-
        'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.PAtom.F1'(A), B, C, D, E, F, G, H).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.PAtom.F1'(A), B, C, D, E, F, G, H) :-
        '~SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.PAtom.F1'(A), B, C, D, E, F, G, H).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.Atom.F2'(A,B), C, D, E, F, G, H, I) :-
        'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.Atom.F2'(A,B), C, D, E, F, G, H, I).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.Atom.F2'(A,B), C, D, E, F, G, H, I) :-
        '~SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.Atom.F2'(A,B), C, D, E, F, G, H, I).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.XPAtom.F1'(A), B, C, D, E, F, G, H) :-
        'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.XPAtom.F1'(A), B, C, D, E, F, G, H).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.XPAtom.F1'(A), B, C, D, E, F, G, H) :-
        '~SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.XPAtom.F1'(A), B, C, D, E, F, G, H).
'SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.XAtom.F2'(A,B), C, D, E, F, G, H, I) :-
        'SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.XAtom.F2'(A,B), C, D, E, F, G, H, I).
'~SharedPrograms.MeltedStandardTyping.P8'('MetaDefs.XAtom.F2'(A,B), C, D, E, F, G, H, I) :-
        '~SharedPrograms.MeltedAtomTyping.P8'('MetaDefs.XAtom.F2'(A,B), C, D, E, F, G, H, I).
'SharedPrograms.ModuleLanguage.P3'('ProgDefs.Program.F4'(_,A,'ProgDefs.Language.F1'(B),_), C, 'ProgDefs.Language.F1'(D)) :-
        'SharedPrograms.EmptyLanguage.P1'('ProgDefs.Language.F1'(E)),
        'AVLTrees':'AVLTrees.AVLSearch.P3'(A, C, 'ProgDefs.ModDef.F4'(F,G,H,I)),
        user:goedel_freeze(ground([F]), (F='ProgDefs.ClosedKind.C0',true->'SharedPrograms':'SharedPrograms.ImportedLanguage.P6'([C],A,B,[],E,D);user:goedel_freeze(ground([B,C]),if(('AVLTrees':'~AVLTrees.AVLSearch.P3'(B,C,J),true),'AVLTrees':'AVLTrees.AVLInsert.P4'(E,C,J,K),K=E)),'Lists':'Lists.Append.P3'(G,H,L),'Lists':'Lists.Append.P3'(I,L,M),'SharedPrograms':'SharedPrograms.ImportedLanguage.P6'(M,A,B,[C],K,D))).
'~SharedPrograms.ModuleLanguage.P3'('ProgDefs.Program.F4'(_,A,'ProgDefs.Language.F1'(B),_), C, 'ProgDefs.Language.F1'(D)) :-
        '~SharedPrograms.EmptyLanguage.P1'('ProgDefs.Language.F1'(E)),
        'AVLTrees':'~AVLTrees.AVLSearch.P3'(A, C, 'ProgDefs.ModDef.F4'(F,G,H,I)),
        user:goedel_freeze(ground([F]), (F='ProgDefs.ClosedKind.C0',true->'SharedPrograms':'~SharedPrograms.ImportedLanguage.P6'([C],A,B,[],E,D);user:goedel_freeze(ground([B,C]),if(('AVLTrees':'~AVLTrees.AVLSearch.P3'(B,C,J),true),'AVLTrees':'~AVLTrees.AVLInsert.P4'(E,C,J,K),K=E)),'Lists':'~Lists.Append.P3'(G,H,L),'Lists':'~Lists.Append.P3'(I,L,M),'SharedPrograms':'~SharedPrograms.ImportedLanguage.P6'(M,A,B,[C],K,D))).
'SharedPrograms.SymbolInLanguage.P3'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F) :-
        'SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(G,'ProgDefs.Categories.F2'(_,H),_)),
        'SharedPrograms.AVLFind.P3'(H, B, I),
        'SharedPrograms.FindDescriptor.P5'(I, G, C, D, F).
'~SharedPrograms.SymbolInLanguage.P3'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F) :-
        '~SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(G,'ProgDefs.Categories.F2'(_,H),_)),
        '~SharedPrograms.AVLFind.P3'(H, B, I),
        '~SharedPrograms.FindDescriptor.P5'(I, G, C, D, F).
'SharedPrograms.SControlInProgram.P5'('ProgDefs.Program.F4'(_,A,'ProgDefs.Language.F1'(B),C), D, E, F, G) :-
        E='MetaDefs.Name.F4'(D,H,_,I),
        'SharedPrograms.AVLFind.P3'(B, D, 'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(_,J),_)),
        'SharedPrograms.AVLFind.P3'(J, H, K),
        user:goedel_freeze(ground([D]), (D='"',true->L='ProgDefs.ClosedKind.C0',M='ProgDefs.Exported.C0';'AVLTrees':'AVLTrees.AVLSearch.P3'(A,D,'ProgDefs.ModDef.F4'(L,_,_,_)),user:goedel_freeze(ground([L]),(L='ProgDefs.ClosedKind.C0',true->M='ProgDefs.Exported.C0';M='ProgDefs.Hidden.C0')))),
        'SharedPrograms.FindDescriptor.P5'(K, M, _, I, 'ProgDefs.PredicateDecl.F3'(_,_,_)),
        user:goedel_freeze(ground([I,C,D,H]), if((('AVLTrees':'~AVLTrees.AVLSearch.P3'(C,D,'ProgDefs.Code.F2'(_,N)),'AVLTrees':'~AVLTrees.AVLSearch.P3'(N,H,O),'SharedPrograms':'~SharedPrograms.FindPredDef.P3'(I,O,'ProgDefs.PredDef.F4'(I,_,P,Q))),true),(user:goedel_freeze(ground([L]),(L='ProgDefs.ClosedKind.C0',true->R=P;'Lists':'Lists.Append.P3'(P,Q,R))),'SharedPrograms':'SharedPrograms.ReformatDelays.P3'(R,F,G)),(F=[],G=[]))).
'~SharedPrograms.SControlInProgram.P5'('ProgDefs.Program.F4'(_,A,'ProgDefs.Language.F1'(B),C), D, E, F, G) :-
        E='MetaDefs.Name.F4'(D,H,_,I),
        '~SharedPrograms.AVLFind.P3'(B, D, 'ProgDefs.Module.F3'(_,'ProgDefs.Categories.F2'(_,J),_)),
        '~SharedPrograms.AVLFind.P3'(J, H, K),
        user:goedel_freeze(ground([D]), (D='"',true->L='ProgDefs.ClosedKind.C0',M='ProgDefs.Exported.C0';'AVLTrees':'~AVLTrees.AVLSearch.P3'(A,D,'ProgDefs.ModDef.F4'(L,_,_,_)),user:goedel_freeze(ground([L]),(L='ProgDefs.ClosedKind.C0',true->M='ProgDefs.Exported.C0';M='ProgDefs.Hidden.C0')))),
        '~SharedPrograms.FindDescriptor.P5'(K, M, _, I, 'ProgDefs.PredicateDecl.F3'(_,_,_)),
        user:goedel_freeze(ground([I,C,D,H]), if((('AVLTrees':'~AVLTrees.AVLSearch.P3'(C,D,'ProgDefs.Code.F2'(_,N)),'AVLTrees':'~AVLTrees.AVLSearch.P3'(N,H,O),'SharedPrograms':'~SharedPrograms.FindPredDef.P3'(I,O,'ProgDefs.PredDef.F4'(I,_,P,Q))),true),(user:goedel_freeze(ground([L]),(L='ProgDefs.ClosedKind.C0',true->R=P;'Lists':'~Lists.Append.P3'(P,Q,R))),'SharedPrograms':'~SharedPrograms.ReformatDelays.P3'(R,F,G)),(F=[],G=[]))).
'SharedPrograms.ProgramSkeleton.P4'(A, B, C, 'ProgDefs.Program.F4'(A,D,E,F)) :-
        'AVLTrees':'AVLTrees.AVLIsEmpty.P1'(G),
        'AVLTrees':'AVLTrees.AVLIsEmpty.P1'(H),
        'AVLTrees':'AVLTrees.AVLIsEmpty.P1'(I),
        'AVLTrees':'AVLTrees.AVLInsert.P4'(G, A, 'ProgDefs.ModDef.F4'(B,[],[],[]), D),
        'AVLTrees':'AVLTrees.AVLInsert.P4'(H, A, 'ProgDefs.Code.F2'(C,I), F),
        'SharedPrograms.EmptyLanguage.P1'(E).
'~SharedPrograms.ProgramSkeleton.P4'(A, B, C, 'ProgDefs.Program.F4'(A,D,E,F)) :-
        'AVLTrees':'~AVLTrees.AVLIsEmpty.P1'(G),
        'AVLTrees':'~AVLTrees.AVLIsEmpty.P1'(H),
        'AVLTrees':'~AVLTrees.AVLIsEmpty.P1'(I),
        'AVLTrees':'~AVLTrees.AVLInsert.P4'(G, A, 'ProgDefs.ModDef.F4'(B,[],[],[]), D),
        'AVLTrees':'~AVLTrees.AVLInsert.P4'(H, A, 'ProgDefs.Code.F2'(C,I), F),
        '~SharedPrograms.EmptyLanguage.P1'(E).
'SharedPrograms.OpenModuleAux.P2'(A, B) :-
        'SharedPrograms.AVLFind.P3'(A, B, 'ProgDefs.ModDef.F4'(C,_,_,_)),
        'SharedPrograms.OpenKind.P1'(C).
'~SharedPrograms.OpenModuleAux.P2'(A, B) :-
        '~SharedPrograms.AVLFind.P3'(A, B, 'ProgDefs.ModDef.F4'(C,_,_,_)),
        '~SharedPrograms.OpenKind.P1'(C).
'SharedPrograms.OpenKind.P1'('ProgDefs.NormalKind.C0').
'~SharedPrograms.OpenKind.P1'('ProgDefs.NormalKind.C0').
'SharedPrograms.OpenKind.P1'('ProgDefs.ModuleKind.C0').
'~SharedPrograms.OpenKind.P1'('ProgDefs.ModuleKind.C0').
'SharedPrograms.MustBracketThen.P1'('MetaDefs.ITE.F3'(_,_,_)).
'~SharedPrograms.MustBracketThen.P1'('MetaDefs.ITE.F3'(_,_,_)).
'SharedPrograms.MustBracketThen.P1'('MetaDefs.ISTE.F4'(_,_,_,_)).
'~SharedPrograms.MustBracketThen.P1'('MetaDefs.ISTE.F4'(_,_,_,_)).
'SharedPrograms.MustBracketThen.P1'('MetaDefs.&''.F2'(A,B)) :-
        'SharedPrograms.ContainsConditional.P1'('MetaDefs.&''.F2'(A,B)).
'~SharedPrograms.MustBracketThen.P1'('MetaDefs.&''.F2'(A,B)) :-
        '~SharedPrograms.ContainsConditional.P1'('MetaDefs.&''.F2'(A,B)).
'SharedPrograms.PredicateInLanguage.P4'(A, B, C, D) :-
        'SharedPrograms.SymbolInLanguage.P3'(A, B, 'ProgDefs.PredicateDecl.F3'(_,C,D)).
'~SharedPrograms.PredicateInLanguage.P4'(A, B, C, D) :-
        '~SharedPrograms.SymbolInLanguage.P3'(A, B, 'ProgDefs.PredicateDecl.F3'(_,C,D)).
'SharedPrograms.OrSeqToIntDL.P3'('ProgDefs.Or.F2'(A,B), C, D) :-
        'SharedPrograms.SimpleCondToIntDL.P3'(A, C, E),
        'Strings':'Strings.StringInts.P2'('" \/ ', F),
        'Lists':'Lists.Append.P3'(F, G, E),
        user:goedel_freeze(ground([B]), (B='ProgDefs.Or.F2'(_,_),true->'SharedPrograms':'SharedPrograms.OrSeqToIntDL.P3'(B,G,D);'SharedPrograms':'SharedPrograms.SimpleCondToIntDL.P3'(B,G,D))).
'~SharedPrograms.OrSeqToIntDL.P3'('ProgDefs.Or.F2'(A,B), C, D) :-
        '~SharedPrograms.SimpleCondToIntDL.P3'(A, C, E),
        'Strings':'~Strings.StringInts.P2'('" \/ ', F),
        'Lists':'~Lists.Append.P3'(F, G, E),
        user:goedel_freeze(ground([B]), (B='ProgDefs.Or.F2'(_,_),true->'SharedPrograms':'~SharedPrograms.OrSeqToIntDL.P3'(B,G,D);'SharedPrograms':'~SharedPrograms.SimpleCondToIntDL.P3'(B,G,D))).
'SharedPrograms.ProgramLanguage.P2'('ProgDefs.Program.F4'(_,_,A,_), A).
'~SharedPrograms.ProgramLanguage.P2'('ProgDefs.Program.F4'(_,_,A,_), A).
'SharedPrograms.RationalTerm.P2'(A, 'MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Rationals','"//','MetaDefs.Function.C0',2),['MetaDefs.Int.F1'(A),'MetaDefs.Int.F1'(1)])).
'~SharedPrograms.RationalTerm.P2'(A, 'MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Rationals','"//','MetaDefs.Function.C0',2),['MetaDefs.Int.F1'(A),'MetaDefs.Int.F1'(1)])).
'SharedPrograms.QuantifiedFormulaToIntDL.P6'(A, B, C, D, E, F) :-
        'SharedPrograms.QuantifiedFormula.P3'(A, G, H),
        'SharedPrograms.DeleteUnderscores.P2'(G, I),
        user:goedel_freeze(ground([I]), (I=[],true->'SharedPrograms':'SharedPrograms.FormulaToIntDL.P6'(H,B,C,D,E,F);'SharedPrograms':'SharedPrograms.QuantifierChars.P3'(A,E,J),'SharedPrograms':'SharedPrograms.VarListToIntDL.P6'(G,B,C,D,J,K),'SharedPrograms':'SharedPrograms.CharDL.P3'('" ',K,L),'SharedPrograms':'SharedPrograms.FormulaToIntDL.P6'(H,B,C,D,M,N),user:goedel_freeze(ground([A,H]),(('SharedPrograms':'~SharedPrograms.CLPrec.P2'(H,O),'SharedPrograms':'~SharedPrograms.CRPrec.P2'(A,P),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(O,P)),true->L=M,F=N;'SharedPrograms':'SharedPrograms.CharDL.P3'('"(',L,M),'SharedPrograms':'SharedPrograms.CharDL.P3'('")',N,F))))).
'~SharedPrograms.QuantifiedFormulaToIntDL.P6'(A, B, C, D, E, F) :-
        '~SharedPrograms.QuantifiedFormula.P3'(A, G, H),
        '~SharedPrograms.DeleteUnderscores.P2'(G, I),
        user:goedel_freeze(ground([I]), (I=[],true->'SharedPrograms':'~SharedPrograms.FormulaToIntDL.P6'(H,B,C,D,E,F);'SharedPrograms':'~SharedPrograms.QuantifierChars.P3'(A,E,J),'SharedPrograms':'~SharedPrograms.VarListToIntDL.P6'(G,B,C,D,J,K),'SharedPrograms':'~SharedPrograms.CharDL.P3'('" ',K,L),'SharedPrograms':'~SharedPrograms.FormulaToIntDL.P6'(H,B,C,D,M,N),user:goedel_freeze(ground([A,H]),(('SharedPrograms':'~SharedPrograms.CLPrec.P2'(H,O),'SharedPrograms':'~SharedPrograms.CRPrec.P2'(A,P),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(O,P)),true->L=M,F=N;'SharedPrograms':'~SharedPrograms.CharDL.P3'('"(',L,M),'SharedPrograms':'~SharedPrograms.CharDL.P3'('")',N,F))))).
'SharedPrograms.PropositionInLanguage.P2'(A, B) :-
        'SharedPrograms.SymbolInLanguage.P3'(A, B, 'ProgDefs.PropositionDecl.C0').
'~SharedPrograms.PropositionInLanguage.P2'(A, B) :-
        '~SharedPrograms.SymbolInLanguage.P3'(A, B, 'ProgDefs.PropositionDecl.C0').
'SharedPrograms.QuantifiedFormula.P3'('MetaDefs.Some.F2'(A,B), A, B).
'~SharedPrograms.QuantifiedFormula.P3'('MetaDefs.Some.F2'(A,B), A, B).
'SharedPrograms.QuantifiedFormula.P3'('MetaDefs.All.F2'(A,B), A, B).
'~SharedPrograms.QuantifiedFormula.P3'('MetaDefs.All.F2'(A,B), A, B).
'SharedPrograms.RPrec.P2'('Syntax.NoFunctInd.C0', 'SharedPrograms.Infinity.C0').
'~SharedPrograms.RPrec.P2'('Syntax.NoFunctInd.C0', 'SharedPrograms.Infinity.C0').
'SharedPrograms.RPrec.P2'('Syntax.XFX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'~SharedPrograms.RPrec.P2'('Syntax.XFX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.RPrec.P2'('Syntax.XFY.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.Y.C0')).
'~SharedPrograms.RPrec.P2'('Syntax.XFY.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.Y.C0')).
'SharedPrograms.RPrec.P2'('Syntax.YFX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'~SharedPrograms.RPrec.P2'('Syntax.YFX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.RPrec.P2'('Syntax.FX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'~SharedPrograms.RPrec.P2'('Syntax.FX.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.RPrec.P2'('Syntax.FY.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.Y.C0')).
'~SharedPrograms.RPrec.P2'('Syntax.FY.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.Y.C0')).
'SharedPrograms.RPrec.P2'('Syntax.XF.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'~SharedPrograms.RPrec.P2'('Syntax.XF.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.RPrec.P2'('Syntax.YF.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'~SharedPrograms.RPrec.P2'('Syntax.YF.F1'(A), 'SharedPrograms.Prec.F2'(A,'SharedPrograms.X.C0')).
'SharedPrograms.QuantifierChars.P3'('MetaDefs.Some.F2'(_,_), A, B) :-
        'Strings':'Strings.StringInts.P2'('"SOME ', C),
        'Lists':'Lists.Append.P3'(C, B, A).
'~SharedPrograms.QuantifierChars.P3'('MetaDefs.Some.F2'(_,_), A, B) :-
        'Strings':'~Strings.StringInts.P2'('"SOME ', C),
        'Lists':'~Lists.Append.P3'(C, B, A).
'SharedPrograms.QuantifierChars.P3'('MetaDefs.All.F2'(_,_), A, B) :-
        'Strings':'Strings.StringInts.P2'('"ALL ', C),
        'Lists':'Lists.Append.P3'(C, B, A).
'~SharedPrograms.QuantifierChars.P3'('MetaDefs.All.F2'(_,_), A, B) :-
        'Strings':'~Strings.StringInts.P2'('"ALL ', C),
        'Lists':'~Lists.Append.P3'(C, B, A).
'SharedPrograms.ReformatDelays.P3'([], [], []).
'~SharedPrograms.ReformatDelays.P3'([], [], []).
'SharedPrograms.ReformatDelays.P3'(['ProgDefs.Delay.F2'(B,C)|A], [B|D], [C|E]) :-
        'SharedPrograms.ReformatDelays.P3'(A, D, E).
'~SharedPrograms.ReformatDelays.P3'(['ProgDefs.Delay.F2'(B,C)|A], [B|D], [C|E]) :-
        '~SharedPrograms.ReformatDelays.P3'(A, D, E).
'SharedPrograms.RationalToIntDL.P9'(A, B, C, D, E, F, G, H, I) :-
        'SharedPrograms.CookRationalTerm.P2'(A, J),
        user:goedel_freeze(ground([B]), (B=1,true->'SharedPrograms':'SharedPrograms.TermToIntDL.P9'(J,C,D,[],E,F,G,H,I);'SharedPrograms':'SharedPrograms.CookRationalTerm.P2'(B,K),'SharedPrograms':'SharedPrograms.TermToIntDL.P9'('MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Rationals','"/','MetaDefs.Function.C0',2),[J,K]),C,D,[],E,F,G,H,I))).
'~SharedPrograms.RationalToIntDL.P9'(A, B, C, D, E, F, G, H, I) :-
        '~SharedPrograms.CookRationalTerm.P2'(A, J),
        user:goedel_freeze(ground([B]), (B=1,true->'SharedPrograms':'~SharedPrograms.TermToIntDL.P9'(J,C,D,[],E,F,G,H,I);'SharedPrograms':'~SharedPrograms.CookRationalTerm.P2'(B,K),'SharedPrograms':'~SharedPrograms.TermToIntDL.P9'('MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Rationals','"/','MetaDefs.Function.C0',2),[J,K]),C,D,[],E,F,G,H,I))).
'SharedPrograms.RestVarListToIntDL.P6'([], _, _, _, A, A).
'~SharedPrograms.RestVarListToIntDL.P6'([], _, _, _, A, A).
'SharedPrograms.RestVarListToIntDL.P6'([A|B], C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('",', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'SharedPrograms.RestVarListToIntDL.P6'(B, C, D, E, I, G).
'~SharedPrograms.RestVarListToIntDL.P6'([A|B], C, D, E, F, G) :-
        '~SharedPrograms.CharDL.P3'('",', F, H),
        '~SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        '~SharedPrograms.RestVarListToIntDL.P6'(B, C, D, E, I, G).
'SharedPrograms.SConditionToString.P2'(A, B) :-
        'SharedPrograms.ConditionToIntDL.P3'(A, C, []),
        'Strings':'Strings.StringInts.P2'(B, C).
'~SharedPrograms.SConditionToString.P2'(A, B) :-
        '~SharedPrograms.ConditionToIntDL.P3'(A, C, []),
        'Strings':'~Strings.StringInts.P2'(B, C).
'SharedPrograms.SProgramTypeToString.P4'(A, B, C, D) :-
        'SharedPrograms.ModuleLanguage.P3'(A, B, E),
        'SharedPrograms.TypeToIntDL.P4'(C, E, F, []),
        'Strings':'Strings.StringInts.P2'(D, F).
'~SharedPrograms.SProgramTypeToString.P4'(A, B, C, D) :-
        '~SharedPrograms.ModuleLanguage.P3'(A, B, E),
        '~SharedPrograms.TypeToIntDL.P4'(C, E, F, []),
        'Strings':'~Strings.StringInts.P2'(D, F).
'SharedPrograms.SOpenModule.P2'('ProgDefs.Program.F4'(_,A,_,_), B) :-
        'SharedPrograms.OpenModuleAux.P2'(A, B).
'~SharedPrograms.SOpenModule.P2'('ProgDefs.Program.F4'(_,A,_,_), B) :-
        '~SharedPrograms.OpenModuleAux.P2'(A, B).
'SharedPrograms.SMainModuleInProgram.P2'('ProgDefs.Program.F4'(A,_,_,_), A).
'~SharedPrograms.SMainModuleInProgram.P2'('ProgDefs.Program.F4'(A,_,_,_), A).
'SharedPrograms.SProgramTermToString.P4'(A, B, C, D) :-
        'SharedPrograms.TermToIntList.P4'(A, B, C, E),
        'Strings':'Strings.StringInts.P2'(D, E).
'~SharedPrograms.SProgramTermToString.P4'(A, B, C, D) :-
        '~SharedPrograms.TermToIntList.P4'(A, B, C, E),
        'Strings':'~Strings.StringInts.P2'(D, E).
'SharedPrograms.SProgramFormulaToString.P4'(A, B, C, D) :-
        'SharedPrograms.FormulaToIntList.P4'(A, B, C, E),
        'Strings':'Strings.StringInts.P2'(D, E).
'~SharedPrograms.SProgramFormulaToString.P4'(A, B, C, D) :-
        '~SharedPrograms.FormulaToIntList.P4'(A, B, C, E),
        'Strings':'~Strings.StringInts.P2'(D, E).
'SharedPrograms.SpaceIfNeeded.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), ((A=B,user:not_equal([],[A],A,'SharedPrograms.Bounded.C0')),true->'SharedPrograms':'SharedPrograms.CharDL.P3'('" ',C,D);C=D)).
'~SharedPrograms.SpaceIfNeeded.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), ((A=B,user:not_equal([],[A],A,'SharedPrograms.Bounded.C0')),true->'SharedPrograms':'~SharedPrograms.CharDL.P3'('" ',C,D);C=D)).
'SharedPrograms.SetExprToIntDL.P6'([A,B], C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('"{', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'SharedPrograms.ChainExprToIntDL.P6'(B, C, D, E, I, J),
        'SharedPrograms.CharDL.P3'('"}', J, G).
'~SharedPrograms.SetExprToIntDL.P6'([A,B], C, D, E, F, G) :-
        '~SharedPrograms.CharDL.P3'('"{', F, H),
        '~SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        '~SharedPrograms.ChainExprToIntDL.P6'(B, C, D, E, I, J),
        '~SharedPrograms.CharDL.P3'('"}', J, G).
'SharedPrograms.SimpleCondToIntDL.P3'('ProgDefs.Nonvar.F1'(A), B, C) :-
        'Strings':'Strings.StringInts.P2'('"NONVAR(', D),
        'Lists':'Lists.Append.P3'(D, E, B),
        'SharedPrograms.TermToIntDL.P9'(A, _, _, _, _, _, _, E, F),
        'SharedPrograms.CharDL.P3'('")', F, C).
'~SharedPrograms.SimpleCondToIntDL.P3'('ProgDefs.Nonvar.F1'(A), B, C) :-
        'Strings':'~Strings.StringInts.P2'('"NONVAR(', D),
        'Lists':'~Lists.Append.P3'(D, E, B),
        '~SharedPrograms.TermToIntDL.P9'(A, _, _, _, _, _, _, E, F),
        '~SharedPrograms.CharDL.P3'('")', F, C).
'SharedPrograms.SimpleCondToIntDL.P3'('ProgDefs.Ground.F1'(A), B, C) :-
        'Strings':'Strings.StringInts.P2'('"GROUND(', D),
        'Lists':'Lists.Append.P3'(D, E, B),
        'SharedPrograms.TermToIntDL.P9'(A, _, _, _, _, _, _, E, F),
        'SharedPrograms.CharDL.P3'('")', F, C).
'~SharedPrograms.SimpleCondToIntDL.P3'('ProgDefs.Ground.F1'(A), B, C) :-
        'Strings':'~Strings.StringInts.P2'('"GROUND(', D),
        'Lists':'~Lists.Append.P3'(D, E, B),
        '~SharedPrograms.TermToIntDL.P9'(A, _, _, _, _, _, _, E, F),
        '~SharedPrograms.CharDL.P3'('")', F, C).
'SharedPrograms.SimpleCondToIntDL.P3'('ProgDefs.And.F2'(A,B), C, D) :-
        'SharedPrograms.CharDL.P3'('"(', C, E),
        'SharedPrograms.AndSeqToIntDL.P3'('ProgDefs.And.F2'(A,B), E, F),
        'SharedPrograms.CharDL.P3'('")', F, D).
'~SharedPrograms.SimpleCondToIntDL.P3'('ProgDefs.And.F2'(A,B), C, D) :-
        '~SharedPrograms.CharDL.P3'('"(', C, E),
        '~SharedPrograms.AndSeqToIntDL.P3'('ProgDefs.And.F2'(A,B), E, F),
        '~SharedPrograms.CharDL.P3'('")', F, D).
'SharedPrograms.SimpleCondToIntDL.P3'('ProgDefs.Or.F2'(A,B), C, D) :-
        'SharedPrograms.CharDL.P3'('"(', C, E),
        'SharedPrograms.OrSeqToIntDL.P3'('ProgDefs.Or.F2'(A,B), E, F),
        'SharedPrograms.CharDL.P3'('")', F, D).
'~SharedPrograms.SimpleCondToIntDL.P3'('ProgDefs.Or.F2'(A,B), C, D) :-
        '~SharedPrograms.CharDL.P3'('"(', C, E),
        '~SharedPrograms.OrSeqToIntDL.P3'('ProgDefs.Or.F2'(A,B), E, F),
        '~SharedPrograms.CharDL.P3'('")', F, D).
'SharedPrograms.StatementInLanguage.P5'('MetaDefs.<-''.F2'(A,B), C, 'MetaDefs.<-''.F2'(D,E), F, G) :-
        'SharedPrograms.MeltedBodyTyping.P8'(B, C, [], H, E, I, J, K),
        user:goedel_freeze(ground([K]), (K=[],true->'SharedPrograms':'SharedPrograms.HeadAtom.P9'(A,C,H,_,D,J,[],L,M),user:goedel_freeze(ground([M]),(M=[],true->'SharedPrograms':'SharedPrograms.FixRationals.P2'(I,F),'SharedPrograms':'SharedPrograms.CheckHeadArgs.P4'(D,L,C,G);G=M));G=K)).
'~SharedPrograms.StatementInLanguage.P5'('MetaDefs.<-''.F2'(A,B), C, 'MetaDefs.<-''.F2'(D,E), F, G) :-
        '~SharedPrograms.MeltedBodyTyping.P8'(B, C, [], H, E, I, J, K),
        user:goedel_freeze(ground([K]), (K=[],true->'SharedPrograms':'~SharedPrograms.HeadAtom.P9'(A,C,H,_,D,J,[],L,M),user:goedel_freeze(ground([M]),(M=[],true->'SharedPrograms':'~SharedPrograms.FixRationals.P2'(I,F),'SharedPrograms':'~SharedPrograms.CheckHeadArgs.P4'(D,L,C,G);G=M));G=K)).
'SharedPrograms.TypeInLanguage.P2'('MetaDefs.Par.F2'(_,_), _).
'~SharedPrograms.TypeInLanguage.P2'('MetaDefs.Par.F2'(_,_), _).
'SharedPrograms.TypeInLanguage.P2'('MetaDefs.Par.F1'(_), _).
'~SharedPrograms.TypeInLanguage.P2'('MetaDefs.Par.F1'(_), _).
'SharedPrograms.TypeInLanguage.P2'('MetaDefs.BType.F1'(A), B) :-
        'SharedPrograms.BaseInLanguage.P2'(A, B).
'~SharedPrograms.TypeInLanguage.P2'('MetaDefs.BType.F1'(A), B) :-
        '~SharedPrograms.BaseInLanguage.P2'(A, B).
'SharedPrograms.TypeInLanguage.P2'('MetaDefs.XBType.F1'(A), B) :-
        'SharedPrograms.BaseInLanguage.P2'(A, B).
'~SharedPrograms.TypeInLanguage.P2'('MetaDefs.XBType.F1'(A), B) :-
        '~SharedPrograms.BaseInLanguage.P2'(A, B).
'SharedPrograms.TypeInLanguage.P2'('MetaDefs.Type.F2'(A,B), C) :-
        'SharedPrograms.ConstructorInLanguage.P3'(A, C, D),
        'Lists':'Lists.Length.P2'(B, D),
        'SharedPrograms.ListTypesInLanguage.P2'(B, C).
'~SharedPrograms.TypeInLanguage.P2'('MetaDefs.Type.F2'(A,B), C) :-
        '~SharedPrograms.ConstructorInLanguage.P3'(A, C, D),
        'Lists':'~Lists.Length.P2'(B, D),
        '~SharedPrograms.ListTypesInLanguage.P2'(B, C).
'SharedPrograms.TypeInLanguage.P2'('MetaDefs.XType.F2'(A,B), C) :-
        'SharedPrograms.ConstructorInLanguage.P3'(A, C, D),
        'Lists':'Lists.Length.P2'(B, D),
        'SharedPrograms.ListTypesInLanguage.P2'(B, C).
'~SharedPrograms.TypeInLanguage.P2'('MetaDefs.XType.F2'(A,B), C) :-
        '~SharedPrograms.ConstructorInLanguage.P3'(A, C, D),
        'Lists':'~Lists.Length.P2'(B, D),
        '~SharedPrograms.ListTypesInLanguage.P2'(B, C).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.Term.F2'(A,B), C, D, E, F, G, H, I, J) :-
        user:goedel_freeze(ground([A,C]), if(('SharedPrograms':'~SharedPrograms.FunctionInLanguage.P5'(A,C,K,_,_),true),('SharedPrograms':'SharedPrograms.TermToIntDLAux.P11'(K,A,B,C,D,E,L,G,H,I,J),F=L),(F='Syntax.NoFunctInd.C0',G='SharedPrograms.Bounded.C0',H='SharedPrograms.Bounded.C0','SharedPrograms':'SharedPrograms.TermInLanguage.P5'('MetaDefs.Term.F2'(A,B),D,'MetaDefs.VarTyping.F1'(E),_,M),'SharedPrograms':'SharedPrograms.CharDL.P3'('"<',I,N),'SharedPrograms':'SharedPrograms.TypeToIntDL.P4'(M,C,N,O),'SharedPrograms':'SharedPrograms.CharDL.P3'('">',O,J)))).
'~SharedPrograms.TermToIntDL.P9'('MetaDefs.Term.F2'(A,B), C, D, E, F, G, H, I, J) :-
        user:goedel_freeze(ground([A,C]), if(('SharedPrograms':'~SharedPrograms.FunctionInLanguage.P5'(A,C,K,_,_),true),('SharedPrograms':'~SharedPrograms.TermToIntDLAux.P11'(K,A,B,C,D,E,L,G,H,I,J),F=L),(F='Syntax.NoFunctInd.C0',G='SharedPrograms.Bounded.C0',H='SharedPrograms.Bounded.C0','SharedPrograms':'~SharedPrograms.TermInLanguage.P5'('MetaDefs.Term.F2'(A,B),D,'MetaDefs.VarTyping.F1'(E),_,M),'SharedPrograms':'~SharedPrograms.CharDL.P3'('"<',I,N),'SharedPrograms':'~SharedPrograms.TypeToIntDL.P4'(M,C,N,O),'SharedPrograms':'~SharedPrograms.CharDL.P3'('">',O,J)))).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.XTerm.F3'(_,_,A), B, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', C, D) :-
        'SharedPrograms.CharDL.P3'('"<', C, E),
        'SharedPrograms.TypeToIntDL.P4'(A, B, E, F),
        'SharedPrograms.CharDL.P3'('">', F, D).
'~SharedPrograms.TermToIntDL.P9'('MetaDefs.XTerm.F3'(_,_,A), B, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', C, D) :-
        '~SharedPrograms.CharDL.P3'('"<', C, E),
        '~SharedPrograms.TypeToIntDL.P4'(A, B, E, F),
        '~SharedPrograms.CharDL.P3'('">', F, D).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.CTerm.F1'(A), B, C, D, 'Syntax.NoFunctInd.C0', E, E, F, G) :-
        user:goedel_freeze(ground([A,B]), ('SharedPrograms':'~SharedPrograms.ConstantInLanguage.P3'(A,B,_),true->'SharedPrograms':'SharedPrograms.ConstantToIntDL.P4'(A,E,F,G);E='SharedPrograms.Bounded.C0','SharedPrograms':'SharedPrograms.TermInLanguage.P5'('MetaDefs.CTerm.F1'(A),C,'MetaDefs.VarTyping.F1'(D),_,H),'SharedPrograms':'SharedPrograms.CharDL.P3'('"<',F,I),'SharedPrograms':'SharedPrograms.TypeToIntDL.P4'(H,B,I,J),'SharedPrograms':'SharedPrograms.CharDL.P3'('">',J,G))).
'~SharedPrograms.TermToIntDL.P9'('MetaDefs.CTerm.F1'(A), B, C, D, 'Syntax.NoFunctInd.C0', E, E, F, G) :-
        user:goedel_freeze(ground([A,B]), ('SharedPrograms':'~SharedPrograms.ConstantInLanguage.P3'(A,B,_),true->'SharedPrograms':'~SharedPrograms.ConstantToIntDL.P4'(A,E,F,G);E='SharedPrograms.Bounded.C0','SharedPrograms':'~SharedPrograms.TermInLanguage.P5'('MetaDefs.CTerm.F1'(A),C,'MetaDefs.VarTyping.F1'(D),_,H),'SharedPrograms':'~SharedPrograms.CharDL.P3'('"<',F,I),'SharedPrograms':'~SharedPrograms.TypeToIntDL.P4'(H,B,I,J),'SharedPrograms':'~SharedPrograms.CharDL.P3'('">',J,G))).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.XCTerm.F2'(_,A), B, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', C, D) :-
        'SharedPrograms.CharDL.P3'('"<', C, E),
        'SharedPrograms.TypeToIntDL.P4'(A, B, E, F),
        'SharedPrograms.CharDL.P3'('">', F, D).
'~SharedPrograms.TermToIntDL.P9'('MetaDefs.XCTerm.F2'(_,A), B, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', C, D) :-
        '~SharedPrograms.CharDL.P3'('"<', C, E),
        '~SharedPrograms.TypeToIntDL.P4'(A, B, E, F),
        '~SharedPrograms.CharDL.P3'('">', F, D).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.Var.F2'(A,B), _, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.AlphaNum.C0', 'SharedPrograms.AlphaNum.C0', C, D) :-
        'Strings':'Strings.StringInts.P2'(A, E),
        'Lists':'Lists.Append.P3'(E, F, C),
        user:goedel_freeze(ground([B,A]), ((B=0;'Strings':concat('"_',_,G),A=G),true->D=F;'SharedPrograms':'SharedPrograms.CharDL.P3'('"_',F,H),'SharedSyntax':'SharedSyntax.IntegerToCharDL.P3'(B,H,D))).
'~SharedPrograms.TermToIntDL.P9'('MetaDefs.Var.F2'(A,B), _, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.AlphaNum.C0', 'SharedPrograms.AlphaNum.C0', C, D) :-
        'Strings':'~Strings.StringInts.P2'(A, E),
        'Lists':'~Lists.Append.P3'(E, F, C),
        user:goedel_freeze(ground([B,A]), ((B=0;'Strings':concat('"_',_,G),A=G),true->D=F;'SharedPrograms':'~SharedPrograms.CharDL.P3'('"_',F,H),'SharedSyntax':'~SharedSyntax.IntegerToCharDL.P3'(B,H,D))).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.Var.F1'(A), _, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.AlphaNum.C0', 'SharedPrograms.AlphaNum.C0', B, C) :-
        'Strings':'Strings.StringInts.P2'('"v', D),
        'Lists':'Lists.Append.P3'(D, E, B),
        user:goedel_freeze(ground([A]), (A=0,true->C=E;'SharedPrograms':'SharedPrograms.CharDL.P3'('"_',E,F),'SharedSyntax':'SharedSyntax.IntegerToCharDL.P3'(A,F,C))).
'~SharedPrograms.TermToIntDL.P9'('MetaDefs.Var.F1'(A), _, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.AlphaNum.C0', 'SharedPrograms.AlphaNum.C0', B, C) :-
        'Strings':'~Strings.StringInts.P2'('"v', D),
        'Lists':'~Lists.Append.P3'(D, E, B),
        user:goedel_freeze(ground([A]), (A=0,true->C=E;'SharedPrograms':'~SharedPrograms.CharDL.P3'('"_',E,F),'SharedSyntax':'~SharedSyntax.IntegerToCharDL.P3'(A,F,C))).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.Str.F1'(A), B, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', C, D) :-
        'SharedPrograms.BaseInLanguage.P2'('MetaDefs.Name.F4'('"Strings','"String','MetaDefs.Base.C0',0), B),
        'Strings':'Strings.StringInts.P2'(A, E),
        'SharedPrograms.ExpandString.P3'(E, C, D).
'~SharedPrograms.TermToIntDL.P9'('MetaDefs.Str.F1'(A), B, _, _, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', C, D) :-
        '~SharedPrograms.BaseInLanguage.P2'('MetaDefs.Name.F4'('"Strings','"String','MetaDefs.Base.C0',0), B),
        'Strings':'~Strings.StringInts.P2'(A, E),
        '~SharedPrograms.ExpandString.P3'(E, C, D).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.Int.F1'(A), B, _, _, C, D, 'SharedPrograms.AlphaNum.C0', E, F) :-
        'SharedPrograms.BaseInLanguage.P2'('MetaDefs.Name.F4'('"Integers','"Integer','MetaDefs.Base.C0',0), B),
        'SharedSyntax':'SharedSyntax.IntegerToCharDL.P3'(A, E, F),
        user:goedel_freeze(ground([A]), ('Integers':'~Integers.<.P2'(A,0),true->'SharedPrograms':'SharedPrograms.FunctionInLanguage.P5'('MetaDefs.Name.F4'('"Integers','"-','MetaDefs.Function.C0',1),B,C,_,_),D='SharedPrograms.Graphic.C0';C='Syntax.NoFunctInd.C0',D='SharedPrograms.AlphaNum.C0')).
'~SharedPrograms.TermToIntDL.P9'('MetaDefs.Int.F1'(A), B, _, _, C, D, 'SharedPrograms.AlphaNum.C0', E, F) :-
        '~SharedPrograms.BaseInLanguage.P2'('MetaDefs.Name.F4'('"Integers','"Integer','MetaDefs.Base.C0',0), B),
        'SharedSyntax':'~SharedSyntax.IntegerToCharDL.P3'(A, E, F),
        user:goedel_freeze(ground([A]), ('Integers':'~Integers.<.P2'(A,0),true->'SharedPrograms':'~SharedPrograms.FunctionInLanguage.P5'('MetaDefs.Name.F4'('"Integers','"-','MetaDefs.Function.C0',1),B,C,_,_),D='SharedPrograms.Graphic.C0';C='Syntax.NoFunctInd.C0',D='SharedPrograms.AlphaNum.C0')).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.Prm.F1'(A), B, C, D, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', E, F) :-
        'SharedPrograms.TermInLanguage.P5'('MetaDefs.Prm.F1'(A), C, 'MetaDefs.VarTyping.F1'(D), _, G),
        'SharedPrograms.CharDL.P3'('"<', E, H),
        'SharedPrograms.TypeToIntDL.P4'(G, B, H, I),
        'SharedPrograms.CharDL.P3'('">', I, F).
'~SharedPrograms.TermToIntDL.P9'('MetaDefs.Prm.F1'(A), B, C, D, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', E, F) :-
        '~SharedPrograms.TermInLanguage.P5'('MetaDefs.Prm.F1'(A), C, 'MetaDefs.VarTyping.F1'(D), _, G),
        '~SharedPrograms.CharDL.P3'('"<', E, H),
        '~SharedPrograms.TypeToIntDL.P4'(G, B, H, I),
        '~SharedPrograms.CharDL.P3'('">', I, F).
'SharedPrograms.TermToIntDL.P9'('MetaDefs.SuchThat.F2'(A,B), C, D, E, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', F, G) :-
        'SharedPrograms.CharDL.P3'('"{', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'Strings':'Strings.StringInts.P2'('" : ', J),
        'Lists':'Lists.Append.P3'(J, K, I),
        'SharedPrograms.FormulaToIntDL.P6'(B, C, D, E, K, L),
        'SharedPrograms.CharDL.P3'('"}', L, G).
'~SharedPrograms.TermToIntDL.P9'('MetaDefs.SuchThat.F2'(A,B), C, D, E, 'Syntax.NoFunctInd.C0', 'SharedPrograms.Bounded.C0', 'SharedPrograms.Bounded.C0', F, G) :-
        '~SharedPrograms.CharDL.P3'('"{', F, H),
        '~SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'Strings':'~Strings.StringInts.P2'('" : ', J),
        'Lists':'~Lists.Append.P3'(J, K, I),
        '~SharedPrograms.FormulaToIntDL.P6'(B, C, D, E, K, L),
        '~SharedPrograms.CharDL.P3'('"}', L, G).
'SharedPrograms.TermListToIntDL.P6'([A|B], C, D, E, F, G) :-
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, F, H),
        'SharedPrograms.TermListToIntDLAux.P6'(B, C, D, E, H, G).
'~SharedPrograms.TermListToIntDL.P6'([A|B], C, D, E, F, G) :-
        '~SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, F, H),
        '~SharedPrograms.TermListToIntDLAux.P6'(B, C, D, E, H, G).
'SharedPrograms.TermInLanguage.P5'(A, B, 'MetaDefs.VarTyping.F1'(C), 'MetaDefs.VarTyping.F1'(D), E) :-
        'SharedPrograms.MeltVariableTyping.P4'(C, B, [], F),
        'SharedPrograms.MeltedTermType.P9'(A, E, B, F, D, _, [], [], []),
        'SharedPrograms.FixType.P3'(E, 0, G),
        'SharedPrograms.FixVariableTyping.P2'(D, G).
'~SharedPrograms.TermInLanguage.P5'(A, B, 'MetaDefs.VarTyping.F1'(C), 'MetaDefs.VarTyping.F1'(D), E) :-
        '~SharedPrograms.MeltVariableTyping.P4'(C, B, [], F),
        '~SharedPrograms.MeltedTermType.P9'(A, E, B, F, D, _, [], [], []),
        '~SharedPrograms.FixType.P3'(E, 0, G),
        '~SharedPrograms.FixVariableTyping.P2'(D, G).
'SharedPrograms.TermListToIntDLAux.P6'([], _, _, _, A, A).
'~SharedPrograms.TermListToIntDLAux.P6'([], _, _, _, A, A).
'SharedPrograms.TermListToIntDLAux.P6'([A|B], C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('",', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'SharedPrograms.TermListToIntDLAux.P6'(B, C, D, E, I, G).
'~SharedPrograms.TermListToIntDLAux.P6'([A|B], C, D, E, F, G) :-
        '~SharedPrograms.CharDL.P3'('",', F, H),
        '~SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        '~SharedPrograms.TermListToIntDLAux.P6'(B, C, D, E, I, G).
'SharedPrograms.TermToIntList.P4'(A, B, C, D) :-
        'SharedPrograms.ProgramLanguage.P2'(A, E),
        'SharedPrograms.ModuleLanguage.P3'(A, B, F),
        'SharedPrograms.TermInLanguage.P5'(C, E, 'MetaDefs.VarTyping.F1'([]), 'MetaDefs.VarTyping.F1'(G), _),
        'SharedPrograms.TermToIntDL.P9'(C, F, E, G, _, _, _, D, []).
'~SharedPrograms.TermToIntList.P4'(A, B, C, D) :-
        '~SharedPrograms.ProgramLanguage.P2'(A, E),
        '~SharedPrograms.ModuleLanguage.P3'(A, B, F),
        '~SharedPrograms.TermInLanguage.P5'(C, E, 'MetaDefs.VarTyping.F1'([]), 'MetaDefs.VarTyping.F1'(G), _),
        '~SharedPrograms.TermToIntDL.P9'(C, F, E, G, _, _, _, D, []).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.NoFunctInd.C0', 'MetaDefs.Name.F4'(A,B,C,D), E, F, G, H, 'Syntax.NoFunctInd.C0', I, 'SharedPrograms.Bounded.C0', J, K) :-
        user:goedel_freeze(ground([D,C,A,B]), ((A='"Lists',B='"Cons',C='MetaDefs.Function.C0',D=2),true->I='SharedPrograms.Bounded.C0','SharedPrograms':'SharedPrograms.ListExprToIntDL.P6'(E,F,G,H,J,K);user:goedel_freeze(ground([D,C,A,B]),((A='"Sets',B='"Inc',C='MetaDefs.Function.C0',D=2),true->I='SharedPrograms.Bounded.C0','SharedPrograms':'SharedPrograms.SetExprToIntDL.P6'(E,F,G,H,J,K);'SharedPrograms':'SharedPrograms.ClassifyToken.P2'(B,I),'Strings':'Strings.StringInts.P2'(B,L),'Lists':'Lists.Append.P3'(L,M,J),'SharedPrograms':'SharedPrograms.CharDL.P3'('"(',M,N),'SharedPrograms':'SharedPrograms.TermListToIntDL.P6'(E,F,G,H,N,O),'SharedPrograms':'SharedPrograms.CharDL.P3'('")',O,K))))).
'~SharedPrograms.TermToIntDLAux.P11'('Syntax.NoFunctInd.C0', 'MetaDefs.Name.F4'(A,B,C,D), E, F, G, H, 'Syntax.NoFunctInd.C0', I, 'SharedPrograms.Bounded.C0', J, K) :-
        user:goedel_freeze(ground([D,C,A,B]), ((A='"Lists',B='"Cons',C='MetaDefs.Function.C0',D=2),true->I='SharedPrograms.Bounded.C0','SharedPrograms':'~SharedPrograms.ListExprToIntDL.P6'(E,F,G,H,J,K);user:goedel_freeze(ground([D,C,A,B]),((A='"Sets',B='"Inc',C='MetaDefs.Function.C0',D=2),true->I='SharedPrograms.Bounded.C0','SharedPrograms':'~SharedPrograms.SetExprToIntDL.P6'(E,F,G,H,J,K);'SharedPrograms':'~SharedPrograms.ClassifyToken.P2'(B,I),'Strings':'~Strings.StringInts.P2'(B,L),'Lists':'~Lists.Append.P3'(L,M,J),'SharedPrograms':'~SharedPrograms.CharDL.P3'('"(',M,N),'SharedPrograms':'~SharedPrograms.TermListToIntDL.P6'(E,F,G,H,N,O),'SharedPrograms':'~SharedPrograms.CharDL.P3'('")',O,K))))).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.XFX.F1'(A), B, [C,D], E, F, G, H, I, J, K, L) :-
        'SharedPrograms.BinaryInfixToIntDL.P12'('Syntax.XFX.F1'(A), B, C, D, E, F, G, H, I, J, K, L).
'~SharedPrograms.TermToIntDLAux.P11'('Syntax.XFX.F1'(A), B, [C,D], E, F, G, H, I, J, K, L) :-
        '~SharedPrograms.BinaryInfixToIntDL.P12'('Syntax.XFX.F1'(A), B, C, D, E, F, G, H, I, J, K, L).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.XFY.F1'(A), B, [C,D], E, F, G, H, I, J, K, L) :-
        'SharedPrograms.BinaryInfixToIntDL.P12'('Syntax.XFY.F1'(A), B, C, D, E, F, G, H, I, J, K, L).
'~SharedPrograms.TermToIntDLAux.P11'('Syntax.XFY.F1'(A), B, [C,D], E, F, G, H, I, J, K, L) :-
        '~SharedPrograms.BinaryInfixToIntDL.P12'('Syntax.XFY.F1'(A), B, C, D, E, F, G, H, I, J, K, L).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.YFX.F1'(A), B, [C,D], E, F, G, H, I, J, K, L) :-
        'SharedPrograms.BinaryInfixToIntDL.P12'('Syntax.YFX.F1'(A), B, C, D, E, F, G, H, I, J, K, L).
'~SharedPrograms.TermToIntDLAux.P11'('Syntax.YFX.F1'(A), B, [C,D], E, F, G, H, I, J, K, L) :-
        '~SharedPrograms.BinaryInfixToIntDL.P12'('Syntax.YFX.F1'(A), B, C, D, E, F, G, H, I, J, K, L).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.FX.F1'(A), B, [C], D, E, F, 'Syntax.FX.F1'(A), G, H, I, J) :-
        'SharedPrograms.UnaryPrefixToIntDL.P10'('Syntax.FX.F1'(A), B, C, D, E, F, G, H, I, J).
'~SharedPrograms.TermToIntDLAux.P11'('Syntax.FX.F1'(A), B, [C], D, E, F, 'Syntax.FX.F1'(A), G, H, I, J) :-
        '~SharedPrograms.UnaryPrefixToIntDL.P10'('Syntax.FX.F1'(A), B, C, D, E, F, G, H, I, J).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.FY.F1'(A), B, [C], D, E, F, 'Syntax.FY.F1'(A), G, H, I, J) :-
        'SharedPrograms.UnaryPrefixToIntDL.P10'('Syntax.FY.F1'(A), B, C, D, E, F, G, H, I, J).
'~SharedPrograms.TermToIntDLAux.P11'('Syntax.FY.F1'(A), B, [C], D, E, F, 'Syntax.FY.F1'(A), G, H, I, J) :-
        '~SharedPrograms.UnaryPrefixToIntDL.P10'('Syntax.FY.F1'(A), B, C, D, E, F, G, H, I, J).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.XF.F1'(A), B, [C], D, E, F, 'Syntax.XF.F1'(A), G, H, I, J) :-
        'SharedPrograms.UnaryPostfixToIntDL.P10'('Syntax.XF.F1'(A), B, C, D, E, F, G, H, I, J).
'~SharedPrograms.TermToIntDLAux.P11'('Syntax.XF.F1'(A), B, [C], D, E, F, 'Syntax.XF.F1'(A), G, H, I, J) :-
        '~SharedPrograms.UnaryPostfixToIntDL.P10'('Syntax.XF.F1'(A), B, C, D, E, F, G, H, I, J).
'SharedPrograms.TermToIntDLAux.P11'('Syntax.YF.F1'(A), B, [C], D, E, F, 'Syntax.YF.F1'(A), G, H, I, J) :-
        'SharedPrograms.UnaryPostfixToIntDL.P10'('Syntax.YF.F1'(A), B, C, D, E, F, G, H, I, J).
'~SharedPrograms.TermToIntDLAux.P11'('Syntax.YF.F1'(A), B, [C], D, E, F, 'Syntax.YF.F1'(A), G, H, I, J) :-
        '~SharedPrograms.UnaryPostfixToIntDL.P10'('Syntax.YF.F1'(A), B, C, D, E, F, G, H, I, J).
'SharedPrograms.ThenPartToIntDL.P6'(A, B, C, D, E, F) :-
        'SharedPrograms.FormulaToIntDL.P6'(A, B, C, D, G, H),
        user:goedel_freeze(ground([A]), ('SharedPrograms':'~SharedPrograms.MustBracketThen.P1'(A),true->'SharedPrograms':'SharedPrograms.CharDL.P3'('"(',E,G),'SharedPrograms':'SharedPrograms.CharDL.P3'('")',H,F);E=G,F=H)).
'~SharedPrograms.ThenPartToIntDL.P6'(A, B, C, D, E, F) :-
        '~SharedPrograms.FormulaToIntDL.P6'(A, B, C, D, G, H),
        user:goedel_freeze(ground([A]), ('SharedPrograms':'~SharedPrograms.MustBracketThen.P1'(A),true->'SharedPrograms':'~SharedPrograms.CharDL.P3'('"(',E,G),'SharedPrograms':'~SharedPrograms.CharDL.P3'('")',H,F);E=G,F=H)).
'SharedPrograms.TypeSymbolInLanguage.P3'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F) :-
        'SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(G,'ProgDefs.Categories.F2'(H,_),_)),
        'SharedPrograms.AVLFind.P3'(H, B, I),
        'SharedPrograms.FindDescriptor.P5'(I, G, C, D, F).
'~SharedPrograms.TypeSymbolInLanguage.P3'('MetaDefs.Name.F4'(A,B,C,D), 'ProgDefs.Language.F1'(E), F) :-
        '~SharedPrograms.AVLFind.P3'(E, A, 'ProgDefs.Module.F3'(G,'ProgDefs.Categories.F2'(H,_),_)),
        '~SharedPrograms.AVLFind.P3'(H, B, I),
        '~SharedPrograms.FindDescriptor.P5'(I, G, C, D, F).
'SharedPrograms.TypeListToIntDL.P4'([A|B], C, D, E) :-
        'SharedPrograms.TypeToIntDL.P4'(A, C, D, F),
        'SharedPrograms.TypeListToIntDLAux.P4'(B, C, F, E).
'~SharedPrograms.TypeListToIntDL.P4'([A|B], C, D, E) :-
        '~SharedPrograms.TypeToIntDL.P4'(A, C, D, F),
        '~SharedPrograms.TypeListToIntDLAux.P4'(B, C, F, E).
'SharedPrograms.TypeListToIntDLAux.P4'([], _, A, A).
'~SharedPrograms.TypeListToIntDLAux.P4'([], _, A, A).
'SharedPrograms.TypeListToIntDLAux.P4'([A|B], C, D, E) :-
        'SharedPrograms.CharDL.P3'('",', D, F),
        'SharedPrograms.TypeToIntDL.P4'(A, C, F, G),
        'SharedPrograms.TypeListToIntDLAux.P4'(B, C, G, E).
'~SharedPrograms.TypeListToIntDLAux.P4'([A|B], C, D, E) :-
        '~SharedPrograms.CharDL.P3'('",', D, F),
        '~SharedPrograms.TypeToIntDL.P4'(A, C, F, G),
        '~SharedPrograms.TypeListToIntDLAux.P4'(B, C, G, E).
'SharedPrograms.UnaryPrefixToIntDL.P10'(A, 'MetaDefs.Name.F4'(_,B,_,_), C, D, E, F, G, H, I, J) :-
        'SharedPrograms.TermToIntDL.P9'(C, D, E, F, K, L, M, N, O),
        'SharedPrograms.ClassifyToken.P2'(B, G),
        'Strings':'Strings.StringInts.P2'(B, P),
        'Lists':'Lists.Append.P3'(P, Q, I),
        user:goedel_freeze(ground([A,K]), (('SharedPrograms':'~SharedPrograms.LPrec.P2'(K,R),'SharedPrograms':'~SharedPrograms.RPrec.P2'(A,S),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(R,S)),true->H=M,'SharedPrograms':'SharedPrograms.SpaceIfNeeded.P4'(G,L,Q,N),J=O;H='SharedPrograms.Bounded.C0','SharedPrograms':'SharedPrograms.CharDL.P3'('"(',Q,N),'SharedPrograms':'SharedPrograms.CharDL.P3'('")',O,J))).
'~SharedPrograms.UnaryPrefixToIntDL.P10'(A, 'MetaDefs.Name.F4'(_,B,_,_), C, D, E, F, G, H, I, J) :-
        '~SharedPrograms.TermToIntDL.P9'(C, D, E, F, K, L, M, N, O),
        '~SharedPrograms.ClassifyToken.P2'(B, G),
        'Strings':'~Strings.StringInts.P2'(B, P),
        'Lists':'~Lists.Append.P3'(P, Q, I),
        user:goedel_freeze(ground([A,K]), (('SharedPrograms':'~SharedPrograms.LPrec.P2'(K,R),'SharedPrograms':'~SharedPrograms.RPrec.P2'(A,S),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(R,S)),true->H=M,'SharedPrograms':'~SharedPrograms.SpaceIfNeeded.P4'(G,L,Q,N),J=O;H='SharedPrograms.Bounded.C0','SharedPrograms':'~SharedPrograms.CharDL.P3'('"(',Q,N),'SharedPrograms':'~SharedPrograms.CharDL.P3'('")',O,J))).
'SharedPrograms.TypeToIntDL.P4'('MetaDefs.Type.F2'(A,B), C, D, E) :-
        'SharedPrograms.ConstructorInLanguage.P3'(A, C, F),
        'Lists':'Lists.Length.P2'(B, F),
        A='MetaDefs.Name.F4'(_,G,_,_),
        'Strings':'Strings.StringInts.P2'(G, H),
        'Lists':'Lists.Append.P3'(H, I, D),
        'SharedPrograms.CharDL.P3'('"(', I, J),
        'SharedPrograms.TypeListToIntDL.P4'(B, C, J, K),
        'SharedPrograms.CharDL.P3'('")', K, E).
'~SharedPrograms.TypeToIntDL.P4'('MetaDefs.Type.F2'(A,B), C, D, E) :-
        '~SharedPrograms.ConstructorInLanguage.P3'(A, C, F),
        'Lists':'~Lists.Length.P2'(B, F),
        A='MetaDefs.Name.F4'(_,G,_,_),
        'Strings':'~Strings.StringInts.P2'(G, H),
        'Lists':'~Lists.Append.P3'(H, I, D),
        '~SharedPrograms.CharDL.P3'('"(', I, J),
        '~SharedPrograms.TypeListToIntDL.P4'(B, C, J, K),
        '~SharedPrograms.CharDL.P3'('")', K, E).
'SharedPrograms.TypeToIntDL.P4'('MetaDefs.BType.F1'(A), B, C, D) :-
        'SharedPrograms.BaseInLanguage.P2'(A, B),
        A='MetaDefs.Name.F4'(_,E,_,_),
        'Strings':'Strings.StringInts.P2'(E, F),
        'Lists':'Lists.Append.P3'(F, D, C).
'~SharedPrograms.TypeToIntDL.P4'('MetaDefs.BType.F1'(A), B, C, D) :-
        '~SharedPrograms.BaseInLanguage.P2'(A, B),
        A='MetaDefs.Name.F4'(_,E,_,_),
        'Strings':'~Strings.StringInts.P2'(E, F),
        'Lists':'~Lists.Append.P3'(F, D, C).
'SharedPrograms.TypeToIntDL.P4'('MetaDefs.Par.F2'(A,B), _, C, D) :-
        'Strings':'Strings.StringInts.P2'(A, E),
        'Lists':'Lists.Append.P3'(E, F, C),
        user:goedel_freeze(ground([B]), (B=0,true->D=F;'SharedPrograms':'SharedPrograms.CharDL.P3'('"_',F,G),'SharedSyntax':'SharedSyntax.IntegerToCharDL.P3'(B,G,D))).
'~SharedPrograms.TypeToIntDL.P4'('MetaDefs.Par.F2'(A,B), _, C, D) :-
        'Strings':'~Strings.StringInts.P2'(A, E),
        'Lists':'~Lists.Append.P3'(E, F, C),
        user:goedel_freeze(ground([B]), (B=0,true->D=F;'SharedPrograms':'~SharedPrograms.CharDL.P3'('"_',F,G),'SharedSyntax':'~SharedSyntax.IntegerToCharDL.P3'(B,G,D))).
'SharedPrograms.TypeToIntDL.P4'('MetaDefs.Par.F1'(A), _, B, C) :-
        'SharedPrograms.CharDL.P3'('"p', B, D),
        user:goedel_freeze(ground([A]), (A=0,true->C=D;'SharedPrograms':'SharedPrograms.CharDL.P3'('"_',D,E),'SharedSyntax':'SharedSyntax.IntegerToCharDL.P3'(A,E,C))).
'~SharedPrograms.TypeToIntDL.P4'('MetaDefs.Par.F1'(A), _, B, C) :-
        '~SharedPrograms.CharDL.P3'('"p', B, D),
        user:goedel_freeze(ground([A]), (A=0,true->C=D;'SharedPrograms':'~SharedPrograms.CharDL.P3'('"_',D,E),'SharedSyntax':'~SharedSyntax.IntegerToCharDL.P3'(A,E,C))).
'SharedPrograms.UnaryPostfixToIntDL.P10'(A, 'MetaDefs.Name.F4'(_,B,_,_), C, D, E, F, G, H, I, J) :-
        'SharedPrograms.TermToIntDL.P9'(C, D, E, F, K, L, M, N, O),
        'SharedPrograms.ClassifyToken.P2'(B, H),
        user:goedel_freeze(ground([A,K]), (('SharedPrograms':'~SharedPrograms.RPrec.P2'(K,P),'SharedPrograms':'~SharedPrograms.LPrec.P2'(A,Q),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(P,Q)),true->G=L,I=N,'SharedPrograms':'SharedPrograms.SpaceIfNeeded.P4'(M,H,O,R);G='SharedPrograms.Bounded.C0','SharedPrograms':'SharedPrograms.CharDL.P3'('"(',I,N),'SharedPrograms':'SharedPrograms.CharDL.P3'('")',O,R))),
        'Strings':'Strings.StringInts.P2'(B, S),
        'Lists':'Lists.Append.P3'(S, J, R).
'~SharedPrograms.UnaryPostfixToIntDL.P10'(A, 'MetaDefs.Name.F4'(_,B,_,_), C, D, E, F, G, H, I, J) :-
        '~SharedPrograms.TermToIntDL.P9'(C, D, E, F, K, L, M, N, O),
        '~SharedPrograms.ClassifyToken.P2'(B, H),
        user:goedel_freeze(ground([A,K]), (('SharedPrograms':'~SharedPrograms.RPrec.P2'(K,P),'SharedPrograms':'~SharedPrograms.LPrec.P2'(A,Q),'SharedPrograms':'~SharedPrograms.BindsTighter.P2'(P,Q)),true->G=L,I=N,'SharedPrograms':'~SharedPrograms.SpaceIfNeeded.P4'(M,H,O,R);G='SharedPrograms.Bounded.C0','SharedPrograms':'~SharedPrograms.CharDL.P3'('"(',I,N),'SharedPrograms':'~SharedPrograms.CharDL.P3'('")',O,R))),
        'Strings':'~Strings.StringInts.P2'(B, S),
        'Lists':'~Lists.Append.P3'(S, J, R).
'SharedPrograms.UpdateDelays.P6'('ProgDefs.Export.C0', A, B, C, [A|B], C).
'~SharedPrograms.UpdateDelays.P6'('ProgDefs.Export.C0', A, B, C, [A|B], C).
'SharedPrograms.UpdateDelays.P6'('ProgDefs.Closed.C0', A, B, C, [A|B], C).
'~SharedPrograms.UpdateDelays.P6'('ProgDefs.Closed.C0', A, B, C, [A|B], C).
'SharedPrograms.UpdateDelays.P6'('ProgDefs.Local.C0', A, B, C, B, [A|C]).
'~SharedPrograms.UpdateDelays.P6'('ProgDefs.Local.C0', A, B, C, B, [A|C]).
'SharedPrograms.UpdateDelays.P6'('ProgDefs.Module.C0', A, B, C, B, [A|C]).
'~SharedPrograms.UpdateDelays.P6'('ProgDefs.Module.C0', A, B, C, B, [A|C]).
'SharedPrograms.VarListToIntDL.P6'([A|B], C, D, E, F, G) :-
        'SharedPrograms.CharDL.P3'('"[', F, H),
        'SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        'SharedPrograms.RestVarListToIntDL.P6'(B, C, D, E, I, J),
        'SharedPrograms.CharDL.P3'('"]', J, G).
'~SharedPrograms.VarListToIntDL.P6'([A|B], C, D, E, F, G) :-
        '~SharedPrograms.CharDL.P3'('"[', F, H),
        '~SharedPrograms.TermToIntDL.P9'(A, C, D, E, _, _, _, H, I),
        '~SharedPrograms.RestVarListToIntDL.P6'(B, C, D, E, I, J),
        '~SharedPrograms.CharDL.P3'('"]', J, G).
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
