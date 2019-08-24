:- module('Substs', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

'Substs.FullDerefType.P4'('MetaDefs.Par.F1'(A), B, _, C) :-
        user:goedel_freeze(ground([A,B]), (B='MetaDefs.Par.F1'(A),true->C='MetaDefs.Occ.F1'('MetaDefs.Par.F1'(A));C='MetaDefs.Par.F1'(A))).
'~Substs.FullDerefType.P4'('MetaDefs.Par.F1'(A), B, _, C) :-
        user:goedel_freeze(ground([A,B]), (B='MetaDefs.Par.F1'(A),true->C='MetaDefs.Occ.F1'('MetaDefs.Par.F1'(A));C='MetaDefs.Par.F1'(A))).
'Substs.FullDerefType.P4'('MetaDefs.Par.F2'(A,B), C, _, D) :-
        user:goedel_freeze(ground([B,C,A]), (C='MetaDefs.Par.F2'(A,B),true->D='MetaDefs.Occ.F1'('MetaDefs.Par.F2'(A,B));D='MetaDefs.Par.F2'(A,B))).
'~Substs.FullDerefType.P4'('MetaDefs.Par.F2'(A,B), C, _, D) :-
        user:goedel_freeze(ground([B,C,A]), (C='MetaDefs.Par.F2'(A,B),true->D='MetaDefs.Occ.F1'('MetaDefs.Par.F2'(A,B));D='MetaDefs.Par.F2'(A,B))).
'Substs.FullDerefType.P4'('MetaDefs.BType.F1'(A), _, _, 'MetaDefs.BType.F1'(A)).
'~Substs.FullDerefType.P4'('MetaDefs.BType.F1'(A), _, _, 'MetaDefs.BType.F1'(A)).
'Substs.FullDerefType.P4'('MetaDefs.Type.F2'(A,B), C, D, 'MetaDefs.Type.F2'(A,E)) :-
        'Substs.FullDerefType1.P4'(B, C, D, E).
'~Substs.FullDerefType.P4'('MetaDefs.Type.F2'(A,B), C, D, 'MetaDefs.Type.F2'(A,E)) :-
        '~Substs.FullDerefType1.P4'(B, C, D, E).
'Substs.FullDerefType.P4'('MetaDefs.XBType.F1'(A), _, _, 'MetaDefs.XBType.F1'(A)).
'~Substs.FullDerefType.P4'('MetaDefs.XBType.F1'(A), _, _, 'MetaDefs.XBType.F1'(A)).
'Substs.FullDerefType.P4'('MetaDefs.XType.F2'(A,B), C, D, 'MetaDefs.XType.F2'(A,E)) :-
        'Substs.FullDerefType1.P4'(B, C, D, E).
'~Substs.FullDerefType.P4'('MetaDefs.XType.F2'(A,B), C, D, 'MetaDefs.XType.F2'(A,E)) :-
        '~Substs.FullDerefType1.P4'(B, C, D, E).
'Substs.FullDerefType.P3'('MetaDefs.Par.F2'(A,B), _, 'MetaDefs.Par.F2'(A,B)).
'~Substs.FullDerefType.P3'('MetaDefs.Par.F2'(A,B), _, 'MetaDefs.Par.F2'(A,B)).
'Substs.FullDerefType.P3'('MetaDefs.Par.F1'(A), _, 'MetaDefs.Par.F1'(A)).
'~Substs.FullDerefType.P3'('MetaDefs.Par.F1'(A), _, 'MetaDefs.Par.F1'(A)).
'Substs.FullDerefType.P3'('MetaDefs.Type.F2'(A,B), C, 'MetaDefs.Type.F2'(A,D)) :-
        'Substs.ApplyTypeSubst.P3'(B, C, D).
'~Substs.FullDerefType.P3'('MetaDefs.Type.F2'(A,B), C, 'MetaDefs.Type.F2'(A,D)) :-
        '~Substs.ApplyTypeSubst.P3'(B, C, D).
'Substs.FullDerefType.P3'('MetaDefs.BType.F1'(A), _, 'MetaDefs.BType.F1'(A)).
'~Substs.FullDerefType.P3'('MetaDefs.BType.F1'(A), _, 'MetaDefs.BType.F1'(A)).
'Substs.FullDerefType.P3'('MetaDefs.XType.F2'(A,B), C, 'MetaDefs.XType.F2'(A,D)) :-
        'Substs.ApplyTypeSubst.P3'(B, C, D).
'~Substs.FullDerefType.P3'('MetaDefs.XType.F2'(A,B), C, 'MetaDefs.XType.F2'(A,D)) :-
        '~Substs.ApplyTypeSubst.P3'(B, C, D).
'Substs.FullDerefType.P3'('MetaDefs.XBType.F1'(A), _, 'MetaDefs.XBType.F1'(A)).
'~Substs.FullDerefType.P3'('MetaDefs.XBType.F1'(A), _, 'MetaDefs.XBType.F1'(A)).
'Substs.ComposeTypeSubsts2.P3'('Substs.TypeSubst.F2'(A,B), 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,F)) :-
        'Substs.ComposeHeaps.P3'(A, C, E),
        'Substs.ComposeLists.P3'(D, B, F).
'~Substs.ComposeTypeSubsts2.P3'('Substs.TypeSubst.F2'(A,B), 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,F)) :-
        '~Substs.ComposeHeaps.P3'(A, C, E),
        '~Substs.ComposeLists.P3'(D, B, F).
'Substs.ApplyTypeSubst.P3'([], _, []).
'~Substs.ApplyTypeSubst.P3'([], _, []).
'Substs.ApplyTypeSubst.P3'([A|B], C, [D|E]) :-
        'Substs.SubstApplyToType.P3'(A, C, D),
        'Substs.ApplyTypeSubst.P3'(B, C, E).
'~Substs.ApplyTypeSubst.P3'([A|B], C, [D|E]) :-
        '~Substs.SubstApplyToType.P3'(A, C, D),
        '~Substs.ApplyTypeSubst.P3'(B, C, E).
'Substs.AddTypeBinding1.P4'('MetaDefs.Par.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.V.F1'('MetaDefs.Par.F2'(A,B)), D, E).
'~Substs.AddTypeBinding1.P4'('MetaDefs.Par.F2'(A,B), C, D, E) :-
        '~Substs.AddBinding.P4'(C, 'Substs.V.F1'('MetaDefs.Par.F2'(A,B)), D, E).
'Substs.AddTypeBinding1.P4'('MetaDefs.Par.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.R.F1'(A), C, D).
'~Substs.AddTypeBinding1.P4'('MetaDefs.Par.F1'(A), B, C, D) :-
        '~Substs.AddBinding.P4'(B, 'Substs.R.F1'(A), C, D).
'Substs.AddTypeBinding1.P4'('MetaDefs.BType.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.BType.F1'(A)), C, D).
'~Substs.AddTypeBinding1.P4'('MetaDefs.BType.F1'(A), B, C, D) :-
        '~Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.BType.F1'(A)), C, D).
'Substs.AddTypeBinding1.P4'('MetaDefs.Type.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.Type.F2'(A,B)), D, E).
'~Substs.AddTypeBinding1.P4'('MetaDefs.Type.F2'(A,B), C, D, E) :-
        '~Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.Type.F2'(A,B)), D, E).
'Substs.AddTypeBinding1.P4'('MetaDefs.XType.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.XType.F2'(A,B)), D, E).
'~Substs.AddTypeBinding1.P4'('MetaDefs.XType.F2'(A,B), C, D, E) :-
        '~Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.XType.F2'(A,B)), D, E).
'Substs.AddTypeBinding1.P4'('MetaDefs.XBType.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.XBType.F1'(A)), C, D).
'~Substs.AddTypeBinding1.P4'('MetaDefs.XBType.F1'(A), B, C, D) :-
        '~Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.XBType.F1'(A)), C, D).
'Substs.AddTermBinding1.P4'('MetaDefs.Var.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.V.F1'('MetaDefs.Var.F2'(A,B)), D, E).
'~Substs.AddTermBinding1.P4'('MetaDefs.Var.F2'(A,B), C, D, E) :-
        '~Substs.AddBinding.P4'(C, 'Substs.V.F1'('MetaDefs.Var.F2'(A,B)), D, E).
'Substs.AddTermBinding1.P4'('MetaDefs.Var.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.R.F1'(A), C, D).
'~Substs.AddTermBinding1.P4'('MetaDefs.Var.F1'(A), B, C, D) :-
        '~Substs.AddBinding.P4'(B, 'Substs.R.F1'(A), C, D).
'Substs.AddTermBinding1.P4'('MetaDefs.CTerm.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.CTerm.F1'(A)), C, D).
'~Substs.AddTermBinding1.P4'('MetaDefs.CTerm.F1'(A), B, C, D) :-
        '~Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.CTerm.F1'(A)), C, D).
'Substs.AddTermBinding1.P4'('MetaDefs.Term.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.Term.F2'(A,B)), D, E).
'~Substs.AddTermBinding1.P4'('MetaDefs.Term.F2'(A,B), C, D, E) :-
        '~Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.Term.F2'(A,B)), D, E).
'Substs.AddTermBinding1.P4'('MetaDefs.XTerm.F3'(A,B,C), D, E, F) :-
        'Substs.AddBinding.P4'(D, 'Substs.T.F1'('MetaDefs.XTerm.F3'(A,B,C)), E, F).
'~Substs.AddTermBinding1.P4'('MetaDefs.XTerm.F3'(A,B,C), D, E, F) :-
        '~Substs.AddBinding.P4'(D, 'Substs.T.F1'('MetaDefs.XTerm.F3'(A,B,C)), E, F).
'Substs.AddTermBinding1.P4'('MetaDefs.XCTerm.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.XCTerm.F2'(A,B)), D, E).
'~Substs.AddTermBinding1.P4'('MetaDefs.XCTerm.F2'(A,B), C, D, E) :-
        '~Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.XCTerm.F2'(A,B)), D, E).
'Substs.AddTermBinding1.P4'('MetaDefs.Int.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.Int.F1'(A)), C, D).
'~Substs.AddTermBinding1.P4'('MetaDefs.Int.F1'(A), B, C, D) :-
        '~Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.Int.F1'(A)), C, D).
'Substs.AddTermBinding1.P4'('MetaDefs.Prm.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.Prm.F1'(A)), C, D).
'~Substs.AddTermBinding1.P4'('MetaDefs.Prm.F1'(A), B, C, D) :-
        '~Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.Prm.F1'(A)), C, D).
'Substs.AddTermBinding1.P4'('MetaDefs.Str.F1'(A), B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.Str.F1'(A)), C, D).
'~Substs.AddTermBinding1.P4'('MetaDefs.Str.F1'(A), B, C, D) :-
        '~Substs.AddBinding.P4'(B, 'Substs.T.F1'('MetaDefs.Str.F1'(A)), C, D).
'Substs.AddTermBinding1.P4'('MetaDefs.SuchThat.F2'(A,B), C, D, E) :-
        'Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.SuchThat.F2'(A,B)), D, E).
'~Substs.AddTermBinding1.P4'('MetaDefs.SuchThat.F2'(A,B), C, D, E) :-
        '~Substs.AddBinding.P4'(C, 'Substs.T.F1'('MetaDefs.SuchThat.F2'(A,B)), D, E).
'Substs.AddNewBinding.P5'(15, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D)) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(15, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D)) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(14, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(14, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(13, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(13, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(12, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(12, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(11, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(11, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(10, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(10, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(9, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(9, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(8, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(8, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(7, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(7, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(6, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(6, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(5, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(5, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(4, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(4, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(3, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(3, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(2, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(2, A, B, C, 'Substs.H.F16'('Substs.N.C0','Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(1, A, B, C, 'Substs.H.F16'('Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(1, A, B, C, 'Substs.H.F16'('Substs.N.C0',D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddNewBinding.P5'(0, A, B, C, 'Substs.H.F16'(D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        'Substs.AddNewBinding1.P4'(A, B, C, D).
'~Substs.AddNewBinding.P5'(0, A, B, C, 'Substs.H.F16'(D,'Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0')) :-
        '~Substs.AddNewBinding1.P4'(A, B, C, D).
'Substs.AddBinding2.P21'(15, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,T)) :-
        'Substs.AddBinding1.P5'(S, A, B, C, T).
'~Substs.AddBinding2.P21'(15, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,T)) :-
        '~Substs.AddBinding1.P5'(S, A, B, C, T).
'Substs.AddBinding2.P21'(14, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,O,P,Q,T,S)) :-
        'Substs.AddBinding1.P5'(R, A, B, C, T).
'~Substs.AddBinding2.P21'(14, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,O,P,Q,T,S)) :-
        '~Substs.AddBinding1.P5'(R, A, B, C, T).
'Substs.AddBinding2.P21'(13, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,O,P,T,R,S)) :-
        'Substs.AddBinding1.P5'(Q, A, B, C, T).
'~Substs.AddBinding2.P21'(13, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,O,P,T,R,S)) :-
        '~Substs.AddBinding1.P5'(Q, A, B, C, T).
'Substs.AddBinding2.P21'(12, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,O,T,Q,R,S)) :-
        'Substs.AddBinding1.P5'(P, A, B, C, T).
'~Substs.AddBinding2.P21'(12, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,O,T,Q,R,S)) :-
        '~Substs.AddBinding1.P5'(P, A, B, C, T).
'Substs.AddBinding2.P21'(11, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,T,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(O, A, B, C, T).
'~Substs.AddBinding2.P21'(11, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,N,T,P,Q,R,S)) :-
        '~Substs.AddBinding1.P5'(O, A, B, C, T).
'Substs.AddBinding2.P21'(10, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,T,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(N, A, B, C, T).
'~Substs.AddBinding2.P21'(10, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,M,T,O,P,Q,R,S)) :-
        '~Substs.AddBinding1.P5'(N, A, B, C, T).
'Substs.AddBinding2.P21'(9, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,T,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(M, A, B, C, T).
'~Substs.AddBinding2.P21'(9, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,L,T,N,O,P,Q,R,S)) :-
        '~Substs.AddBinding1.P5'(M, A, B, C, T).
'Substs.AddBinding2.P21'(8, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,T,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(L, A, B, C, T).
'~Substs.AddBinding2.P21'(8, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,K,T,M,N,O,P,Q,R,S)) :-
        '~Substs.AddBinding1.P5'(L, A, B, C, T).
'Substs.AddBinding2.P21'(7, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,T,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(K, A, B, C, T).
'~Substs.AddBinding2.P21'(7, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,J,T,L,M,N,O,P,Q,R,S)) :-
        '~Substs.AddBinding1.P5'(K, A, B, C, T).
'Substs.AddBinding2.P21'(6, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,T,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(J, A, B, C, T).
'~Substs.AddBinding2.P21'(6, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,I,T,K,L,M,N,O,P,Q,R,S)) :-
        '~Substs.AddBinding1.P5'(J, A, B, C, T).
'Substs.AddBinding2.P21'(5, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,T,J,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(I, A, B, C, T).
'~Substs.AddBinding2.P21'(5, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,H,T,J,K,L,M,N,O,P,Q,R,S)) :-
        '~Substs.AddBinding1.P5'(I, A, B, C, T).
'Substs.AddBinding2.P21'(4, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,T,I,J,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(H, A, B, C, T).
'~Substs.AddBinding2.P21'(4, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,G,T,I,J,K,L,M,N,O,P,Q,R,S)) :-
        '~Substs.AddBinding1.P5'(H, A, B, C, T).
'Substs.AddBinding2.P21'(3, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,T,H,I,J,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(G, A, B, C, T).
'~Substs.AddBinding2.P21'(3, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,F,T,H,I,J,K,L,M,N,O,P,Q,R,S)) :-
        '~Substs.AddBinding1.P5'(G, A, B, C, T).
'Substs.AddBinding2.P21'(2, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,T,G,H,I,J,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(F, A, B, C, T).
'~Substs.AddBinding2.P21'(2, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,E,T,G,H,I,J,K,L,M,N,O,P,Q,R,S)) :-
        '~Substs.AddBinding1.P5'(F, A, B, C, T).
'Substs.AddBinding2.P21'(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,T,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(E, A, B, C, T).
'~Substs.AddBinding2.P21'(1, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(D,T,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)) :-
        '~Substs.AddBinding1.P5'(E, A, B, C, T).
'Substs.AddBinding2.P21'(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(T,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)) :-
        'Substs.AddBinding1.P5'(D, A, B, C, T).
'~Substs.AddBinding2.P21'(0, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, 'Substs.H.F16'(T,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)) :-
        '~Substs.AddBinding1.P5'(D, A, B, C, T).
'Substs.AddTermBinding.P4'(A, B, 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,D)) :-
        'Substs.AddTermBinding1.P4'(A, B, C, E).
'~Substs.AddTermBinding.P4'(A, B, 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,D)) :-
        '~Substs.AddTermBinding1.P4'(A, B, C, E).
'Substs.AddTypeBinding.P4'(A, B, 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,D)) :-
        'Substs.AddTypeBinding1.P4'(A, B, C, E).
'~Substs.AddTypeBinding.P4'(A, B, 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,D)) :-
        '~Substs.AddTypeBinding1.P4'(A, B, C, E).
'Substs.ApplySubstToType.P4'(A, B, C, D) :-
        'Substs.DerefType.P3'(A, C, E),
        'Substs.FullDerefType.P4'(E, B, C, D).
'~Substs.ApplySubstToType.P4'(A, B, C, D) :-
        '~Substs.DerefType.P3'(A, C, E),
        '~Substs.FullDerefType.P4'(E, B, C, D).
'Substs.ApplySubstToTerm.P4'(A, B, C, D) :-
        'Substs.Dereference.P3'(A, C, E),
        'Substs.FullDerefTerm.P4'(E, B, C, D).
'~Substs.ApplySubstToTerm.P4'(A, B, C, D) :-
        '~Substs.Dereference.P3'(A, C, E),
        '~Substs.FullDerefTerm.P4'(E, B, C, D).
'Substs.ApplyTermSubst.P3'([], _, []).
'~Substs.ApplyTermSubst.P3'([], _, []).
'Substs.ApplyTermSubst.P3'([A|B], C, [D|E]) :-
        'Substs.SubstApplyToTerm.P3'(A, C, D),
        'Substs.ApplyTermSubst.P3'(B, C, E).
'~Substs.ApplyTermSubst.P3'([A|B], C, [D|E]) :-
        '~Substs.SubstApplyToTerm.P3'(A, C, D),
        '~Substs.ApplyTermSubst.P3'(B, C, E).
'Substs.ComposeTermSubsts1.P3'('Substs.TermSubst.F2'('Substs.Heap.F2'(A,B),C), D, 'Substs.TermSubst.F2'(E,F)) :-
        'Substs.RationaliseTermList.P3'(C, F, D),
        'Substs.EmptyHeap.P1'(G),
        'Substs.RationaliseTermHeap.P6'(B, A, 0, D, G, E).
'~Substs.ComposeTermSubsts1.P3'('Substs.TermSubst.F2'('Substs.Heap.F2'(A,B),C), D, 'Substs.TermSubst.F2'(E,F)) :-
        '~Substs.RationaliseTermList.P3'(C, F, D),
        '~Substs.EmptyHeap.P1'(G),
        '~Substs.RationaliseTermHeap.P6'(B, A, 0, D, G, E).
'Substs.ComposeHeaps.P3'(A, 'Substs.Heap.F2'(B,C), D) :-
        'Substs.ComposeHeaps1.P5'(C, B, 0, A, D).
'~Substs.ComposeHeaps.P3'(A, 'Substs.Heap.F2'(B,C), D) :-
        '~Substs.ComposeHeaps1.P5'(C, B, 0, A, D).
'Substs.BindVariable.P4'('MetaDefs.Var.F2'(A,B), C, 'Substs.TermSubst.F2'(D,E), 'Substs.TermSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),C)|E])).
'~Substs.BindVariable.P4'('MetaDefs.Var.F2'(A,B), C, 'Substs.TermSubst.F2'(D,E), 'Substs.TermSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),C)|E])).
'Substs.BindVariable.P4'('MetaDefs.Var.F1'(A), B, C, D) :-
        'Substs.AddTermBinding.P4'(B, A, C, D).
'~Substs.BindVariable.P4'('MetaDefs.Var.F1'(A), B, C, D) :-
        '~Substs.AddTermBinding.P4'(B, A, C, D).
'Substs.BindParameter.P4'('MetaDefs.Par.F2'(A,B), C, 'Substs.TypeSubst.F2'(D,E), 'Substs.TypeSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),C)|E])).
'~Substs.BindParameter.P4'('MetaDefs.Par.F2'(A,B), C, 'Substs.TypeSubst.F2'(D,E), 'Substs.TypeSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),C)|E])).
'Substs.BindParameter.P4'('MetaDefs.Par.F1'(A), B, C, D) :-
        'Substs.AddTypeBinding.P4'(B, A, C, D).
'~Substs.BindParameter.P4'('MetaDefs.Par.F1'(A), B, C, D) :-
        '~Substs.AddTypeBinding.P4'(B, A, C, D).
'Substs.BindingInHeap.P3'('Substs.Heap.F2'(A,B), C, D) :-
        'Substs.Address.P5'(B, A, 0, C, D).
'~Substs.BindingInHeap.P3'('Substs.Heap.F2'(A,B), C, D) :-
        '~Substs.Address.P5'(B, A, 0, C, D).
'Substs.ComposeLists.P3'([], A, A).
'~Substs.ComposeLists.P3'([], A, A).
'Substs.ComposeLists.P3'(['MetaDefs.!.F2'(B,C)|A], D, ['MetaDefs.!.F2'(B,C)|E]) :-
        'Substs.DB.P3'(D, B, F),
        'Substs.ComposeLists.P3'(A, F, E).
'~Substs.ComposeLists.P3'(['MetaDefs.!.F2'(B,C)|A], D, ['MetaDefs.!.F2'(B,C)|E]) :-
        '~Substs.DB.P3'(D, B, F),
        '~Substs.ComposeLists.P3'(A, F, E).
'Substs.ComposeHeaps1.P5'('Substs.N.C0', _, _, A, A).
'~Substs.ComposeHeaps1.P5'('Substs.N.C0', _, _, A, A).
'Substs.ComposeHeaps1.P5'('Substs.R.F1'(A), _, B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.R.F1'(A), C, D).
'~Substs.ComposeHeaps1.P5'('Substs.R.F1'(A), _, B, C, D) :-
        '~Substs.AddBinding.P4'(B, 'Substs.R.F1'(A), C, D).
'Substs.ComposeHeaps1.P5'('Substs.V.F1'(A), _, B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.V.F1'(A), C, D).
'~Substs.ComposeHeaps1.P5'('Substs.V.F1'(A), _, B, C, D) :-
        '~Substs.AddBinding.P4'(B, 'Substs.V.F1'(A), C, D).
'Substs.ComposeHeaps1.P5'('Substs.T.F1'(A), _, B, C, D) :-
        'Substs.AddBinding.P4'(B, 'Substs.T.F1'(A), C, D).
'~Substs.ComposeHeaps1.P5'('Substs.T.F1'(A), _, B, C, D) :-
        '~Substs.AddBinding.P4'(B, 'Substs.T.F1'(A), C, D).
'Substs.ComposeHeaps1.P5'('Substs.H.F16'(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), Q, R, S, T) :-
        'Integers':minus(Q, 1, V),
        U=V,
        'Integers':power(16, Q, X),
        W=X,
        'Substs.ComposeHeaps1.P5'(A, U, R, S, Y),
        'Integers':plus(W, R, Z),
        'Substs.ComposeHeaps1.P5'(B, U, Z, Y, A1),
        'Integers':times(2, W, D1),
        'Integers':plus(D1, R, B1),
        'Substs.ComposeHeaps1.P5'(C, U, B1, A1, C1),
        'Integers':times(3, W, G1),
        'Integers':plus(G1, R, E1),
        'Substs.ComposeHeaps1.P5'(D, U, E1, C1, F1),
        'Integers':times(4, W, J1),
        'Integers':plus(J1, R, H1),
        'Substs.ComposeHeaps1.P5'(E, U, H1, F1, I1),
        'Integers':times(5, W, M1),
        'Integers':plus(M1, R, K1),
        'Substs.ComposeHeaps1.P5'(F, U, K1, I1, L1),
        'Integers':times(6, W, P1),
        'Integers':plus(P1, R, N1),
        'Substs.ComposeHeaps1.P5'(G, U, N1, L1, O1),
        'Integers':times(7, W, S1),
        'Integers':plus(S1, R, Q1),
        'Substs.ComposeHeaps1.P5'(H, U, Q1, O1, R1),
        'Integers':times(8, W, V1),
        'Integers':plus(V1, R, T1),
        'Substs.ComposeHeaps1.P5'(I, U, T1, R1, U1),
        'Integers':times(9, W, Y1),
        'Integers':plus(Y1, R, W1),
        'Substs.ComposeHeaps1.P5'(J, U, W1, U1, X1),
        'Integers':times(10, W, B2),
        'Integers':plus(B2, R, Z1),
        'Substs.ComposeHeaps1.P5'(K, U, Z1, X1, A2),
        'Integers':times(11, W, E2),
        'Integers':plus(E2, R, C2),
        'Substs.ComposeHeaps1.P5'(L, U, C2, A2, D2),
        'Integers':times(12, W, H2),
        'Integers':plus(H2, R, F2),
        'Substs.ComposeHeaps1.P5'(M, U, F2, D2, G2),
        'Integers':times(13, W, K2),
        'Integers':plus(K2, R, I2),
        'Substs.ComposeHeaps1.P5'(N, U, I2, G2, J2),
        'Integers':times(14, W, N2),
        'Integers':plus(N2, R, L2),
        'Substs.ComposeHeaps1.P5'(O, U, L2, J2, M2),
        'Integers':times(15, W, P2),
        'Integers':plus(P2, R, O2),
        'Substs.ComposeHeaps1.P5'(P, U, O2, M2, T).
'~Substs.ComposeHeaps1.P5'('Substs.H.F16'(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), Q, R, S, T) :-
        'Integers':minus(Q, 1, V),
        U=V,
        'Integers':power(16, Q, X),
        W=X,
        '~Substs.ComposeHeaps1.P5'(A, U, R, S, Y),
        'Integers':plus(W, R, Z),
        '~Substs.ComposeHeaps1.P5'(B, U, Z, Y, A1),
        'Integers':times(2, W, D1),
        'Integers':plus(D1, R, B1),
        '~Substs.ComposeHeaps1.P5'(C, U, B1, A1, C1),
        'Integers':times(3, W, G1),
        'Integers':plus(G1, R, E1),
        '~Substs.ComposeHeaps1.P5'(D, U, E1, C1, F1),
        'Integers':times(4, W, J1),
        'Integers':plus(J1, R, H1),
        '~Substs.ComposeHeaps1.P5'(E, U, H1, F1, I1),
        'Integers':times(5, W, M1),
        'Integers':plus(M1, R, K1),
        '~Substs.ComposeHeaps1.P5'(F, U, K1, I1, L1),
        'Integers':times(6, W, P1),
        'Integers':plus(P1, R, N1),
        '~Substs.ComposeHeaps1.P5'(G, U, N1, L1, O1),
        'Integers':times(7, W, S1),
        'Integers':plus(S1, R, Q1),
        '~Substs.ComposeHeaps1.P5'(H, U, Q1, O1, R1),
        'Integers':times(8, W, V1),
        'Integers':plus(V1, R, T1),
        '~Substs.ComposeHeaps1.P5'(I, U, T1, R1, U1),
        'Integers':times(9, W, Y1),
        'Integers':plus(Y1, R, W1),
        '~Substs.ComposeHeaps1.P5'(J, U, W1, U1, X1),
        'Integers':times(10, W, B2),
        'Integers':plus(B2, R, Z1),
        '~Substs.ComposeHeaps1.P5'(K, U, Z1, X1, A2),
        'Integers':times(11, W, E2),
        'Integers':plus(E2, R, C2),
        '~Substs.ComposeHeaps1.P5'(L, U, C2, A2, D2),
        'Integers':times(12, W, H2),
        'Integers':plus(H2, R, F2),
        '~Substs.ComposeHeaps1.P5'(M, U, F2, D2, G2),
        'Integers':times(13, W, K2),
        'Integers':plus(K2, R, I2),
        '~Substs.ComposeHeaps1.P5'(N, U, I2, G2, J2),
        'Integers':times(14, W, N2),
        'Integers':plus(N2, R, L2),
        '~Substs.ComposeHeaps1.P5'(O, U, L2, J2, M2),
        'Integers':times(15, W, P2),
        'Integers':plus(P2, R, O2),
        '~Substs.ComposeHeaps1.P5'(P, U, O2, M2, T).
'Substs.ComposeTypeSubsts1.P3'('Substs.TypeSubst.F2'('Substs.Heap.F2'(A,B),C), D, 'Substs.TypeSubst.F2'(E,F)) :-
        'Substs.RationaliseTypeList.P3'(C, F, D),
        'Substs.EmptyHeap.P1'(G),
        'Substs.RationaliseTypeHeap.P6'(B, A, 0, D, G, E).
'~Substs.ComposeTypeSubsts1.P3'('Substs.TypeSubst.F2'('Substs.Heap.F2'(A,B),C), D, 'Substs.TypeSubst.F2'(E,F)) :-
        '~Substs.RationaliseTypeList.P3'(C, F, D),
        '~Substs.EmptyHeap.P1'(G),
        '~Substs.RationaliseTypeHeap.P6'(B, A, 0, D, G, E).
'Substs.ComposeTermSubsts2.P3'('Substs.TermSubst.F2'(A,B), 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,F)) :-
        'Substs.ComposeHeaps.P3'(A, C, E),
        'Substs.ComposeLists.P3'(D, B, F).
'~Substs.ComposeTermSubsts2.P3'('Substs.TermSubst.F2'(A,B), 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,F)) :-
        '~Substs.ComposeHeaps.P3'(A, C, E),
        '~Substs.ComposeLists.P3'(D, B, F).
'Substs.DerefType1.P6'('Substs.N.C0', A, _, _, _, A).
'~Substs.DerefType1.P6'('Substs.N.C0', A, _, _, _, A).
'Substs.DerefType1.P6'('Substs.R.F1'(A), _, B, C, D, E) :-
        'Substs.Contents.P4'(A, B, C, F),
        'Substs.DerefType1.P6'(F, 'MetaDefs.Par.F1'(A), B, C, D, E).
'~Substs.DerefType1.P6'('Substs.R.F1'(A), _, B, C, D, E) :-
        '~Substs.Contents.P4'(A, B, C, F),
        '~Substs.DerefType1.P6'(F, 'MetaDefs.Par.F1'(A), B, C, D, E).
'Substs.DerefType1.P6'('Substs.V.F1'(A), _, B, C, D, E) :-
        'Substs.DerefType.P3'(A, 'Substs.TypeSubst.F2'('Substs.Heap.F2'(B,C),D), E).
'~Substs.DerefType1.P6'('Substs.V.F1'(A), _, B, C, D, E) :-
        '~Substs.DerefType.P3'(A, 'Substs.TypeSubst.F2'('Substs.Heap.F2'(B,C),D), E).
'Substs.DerefType1.P6'('Substs.T.F1'(A), _, _, _, _, A).
'~Substs.DerefType1.P6'('Substs.T.F1'(A), _, _, _, _, A).
'Substs.DelTypeBinding.P4'('MetaDefs.Par.F2'(A,B), C, 'Substs.TypeSubst.F2'(D,E), 'Substs.TypeSubst.F2'(D,F)) :-
        'Lists':'Lists.DeleteFirst.P3'('MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),G), E, F),
        'Substs.SubstApplyToType.P3'(G, 'Substs.TypeSubst.F2'(D,E), C).
'~Substs.DelTypeBinding.P4'('MetaDefs.Par.F2'(A,B), C, 'Substs.TypeSubst.F2'(D,E), 'Substs.TypeSubst.F2'(D,F)) :-
        'Lists':'~Lists.DeleteFirst.P3'('MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),G), E, F),
        '~Substs.SubstApplyToType.P3'(G, 'Substs.TypeSubst.F2'(D,E), C).
'Substs.DelTypeBinding.P4'('MetaDefs.Par.F1'(A), B, 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,D)) :-
        'Substs.AddBinding.P4'(A, 'Substs.N.C0', C, E),
        'Substs.SubstApplyToType.P3'('MetaDefs.Par.F1'(A), 'Substs.TypeSubst.F2'(C,D), B),
        user:not_equal([], [B,A], B, 'MetaDefs.Par.F1'(A)).
'~Substs.DelTypeBinding.P4'('MetaDefs.Par.F1'(A), B, 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,D)) :-
        '~Substs.AddBinding.P4'(A, 'Substs.N.C0', C, E),
        '~Substs.SubstApplyToType.P3'('MetaDefs.Par.F1'(A), 'Substs.TypeSubst.F2'(C,D), B),
        user:not_equal([], [B,A], B, 'MetaDefs.Par.F1'(A)).
'Substs.DB.P3'([], _, []).
'~Substs.DB.P3'([], _, []).
'Substs.DB.P3'(['MetaDefs.!.F2'(B,C)|A], D, E) :-
        user:goedel_freeze(ground([B,D]), (B=D,true->E=A;E=['MetaDefs.!.F2'(B,C)|F],'Substs':'Substs.DB.P3'(A,D,F))).
'~Substs.DB.P3'(['MetaDefs.!.F2'(B,C)|A], D, E) :-
        user:goedel_freeze(ground([B,D]), (B=D,true->E=A;E=['MetaDefs.!.F2'(B,C)|F],'Substs':'~Substs.DB.P3'(A,D,F))).
'Substs.DelTermBinding.P4'('MetaDefs.Var.F2'(A,B), C, 'Substs.TermSubst.F2'(D,E), 'Substs.TermSubst.F2'(D,F)) :-
        'Lists':'Lists.DeleteFirst.P3'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),G), E, F),
        'Substs.SubstApplyToTerm.P3'(G, 'Substs.TermSubst.F2'(D,E), C).
'~Substs.DelTermBinding.P4'('MetaDefs.Var.F2'(A,B), C, 'Substs.TermSubst.F2'(D,E), 'Substs.TermSubst.F2'(D,F)) :-
        'Lists':'~Lists.DeleteFirst.P3'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),G), E, F),
        '~Substs.SubstApplyToTerm.P3'(G, 'Substs.TermSubst.F2'(D,E), C).
'Substs.DelTermBinding.P4'('MetaDefs.Var.F1'(A), B, 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,D)) :-
        'Substs.AddBinding.P4'(A, 'Substs.N.C0', C, E),
        'Substs.SubstApplyToTerm.P3'('MetaDefs.Var.F1'(A), 'Substs.TermSubst.F2'(C,D), B),
        user:not_equal([], [B,A], B, 'MetaDefs.Var.F1'(A)).
'~Substs.DelTermBinding.P4'('MetaDefs.Var.F1'(A), B, 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,D)) :-
        '~Substs.AddBinding.P4'(A, 'Substs.N.C0', C, E),
        '~Substs.SubstApplyToTerm.P3'('MetaDefs.Var.F1'(A), 'Substs.TermSubst.F2'(C,D), B),
        user:not_equal([], [B,A], B, 'MetaDefs.Var.F1'(A)).
'Substs.DerefType.P3'('MetaDefs.Occ.F1'(A), _, A).
'~Substs.DerefType.P3'('MetaDefs.Occ.F1'(A), _, A).
'Substs.DerefType.P3'('MetaDefs.Par.F1'(A), 'Substs.TypeSubst.F2'('Substs.Heap.F2'(B,C),D), E) :-
        'Substs.Contents.P4'(A, B, C, F),
        'Substs.DerefType1.P6'(F, 'MetaDefs.Par.F1'(A), B, C, D, E).
'~Substs.DerefType.P3'('MetaDefs.Par.F1'(A), 'Substs.TypeSubst.F2'('Substs.Heap.F2'(B,C),D), E) :-
        '~Substs.Contents.P4'(A, B, C, F),
        '~Substs.DerefType1.P6'(F, 'MetaDefs.Par.F1'(A), B, C, D, E).
'Substs.DerefType.P3'('MetaDefs.Type.F2'(A,B), _, 'MetaDefs.Type.F2'(A,B)).
'~Substs.DerefType.P3'('MetaDefs.Type.F2'(A,B), _, 'MetaDefs.Type.F2'(A,B)).
'Substs.DerefType.P3'('MetaDefs.BType.F1'(A), _, 'MetaDefs.BType.F1'(A)).
'~Substs.DerefType.P3'('MetaDefs.BType.F1'(A), _, 'MetaDefs.BType.F1'(A)).
'Substs.DerefType.P3'('MetaDefs.XType.F2'(A,B), _, 'MetaDefs.XType.F2'(A,B)).
'~Substs.DerefType.P3'('MetaDefs.XType.F2'(A,B), _, 'MetaDefs.XType.F2'(A,B)).
'Substs.DerefType.P3'('MetaDefs.XBType.F1'(A), _, 'MetaDefs.XBType.F1'(A)).
'~Substs.DerefType.P3'('MetaDefs.XBType.F1'(A), _, 'MetaDefs.XBType.F1'(A)).
'Substs.DerefType.P3'('MetaDefs.Par.F2'(A,B), 'Substs.TypeSubst.F2'(C,D), E) :-
        user:one_solution(user:goedel_freeze(ground([B,A,D]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),F),D),true),'Substs':'Substs.DerefType.P3'(F,'Substs.TypeSubst.F2'(C,D),E),E='MetaDefs.Par.F2'(A,B)))).
'~Substs.DerefType.P3'('MetaDefs.Par.F2'(A,B), 'Substs.TypeSubst.F2'(C,D), E) :-
        user:goedel_freeze(ground([B,A,D]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),F),D),true),'Substs':'~Substs.DerefType.P3'(F,'Substs.TypeSubst.F2'(C,D),E),E='MetaDefs.Par.F2'(A,B))).
'Substs.Dereference1.P6'('Substs.N.C0', A, _, _, _, A).
'~Substs.Dereference1.P6'('Substs.N.C0', A, _, _, _, A).
'Substs.Dereference1.P6'('Substs.R.F1'(A), _, B, C, D, E) :-
        'Substs.Contents.P4'(A, B, C, F),
        'Substs.Dereference1.P6'(F, 'MetaDefs.Var.F1'(A), B, C, D, E).
'~Substs.Dereference1.P6'('Substs.R.F1'(A), _, B, C, D, E) :-
        '~Substs.Contents.P4'(A, B, C, F),
        '~Substs.Dereference1.P6'(F, 'MetaDefs.Var.F1'(A), B, C, D, E).
'Substs.Dereference1.P6'('Substs.V.F1'(A), _, B, C, D, E) :-
        'Substs.Dereference.P3'(A, 'Substs.TermSubst.F2'('Substs.Heap.F2'(B,C),D), E).
'~Substs.Dereference1.P6'('Substs.V.F1'(A), _, B, C, D, E) :-
        '~Substs.Dereference.P3'(A, 'Substs.TermSubst.F2'('Substs.Heap.F2'(B,C),D), E).
'Substs.Dereference1.P6'('Substs.T.F1'(A), _, _, _, _, A).
'~Substs.Dereference1.P6'('Substs.T.F1'(A), _, _, _, _, A).
'Substs.Dereference.P3'('MetaDefs.Occ.F1'(A), _, A).
'~Substs.Dereference.P3'('MetaDefs.Occ.F1'(A), _, A).
'Substs.Dereference.P3'('MetaDefs.Var.F1'(A), 'Substs.TermSubst.F2'('Substs.Heap.F2'(B,C),D), E) :-
        'Substs.Contents.P4'(A, B, C, F),
        'Substs.Dereference1.P6'(F, 'MetaDefs.Var.F1'(A), B, C, D, E).
'~Substs.Dereference.P3'('MetaDefs.Var.F1'(A), 'Substs.TermSubst.F2'('Substs.Heap.F2'(B,C),D), E) :-
        '~Substs.Contents.P4'(A, B, C, F),
        '~Substs.Dereference1.P6'(F, 'MetaDefs.Var.F1'(A), B, C, D, E).
'Substs.Dereference.P3'('MetaDefs.Term.F2'(A,B), _, 'MetaDefs.Term.F2'(A,B)).
'~Substs.Dereference.P3'('MetaDefs.Term.F2'(A,B), _, 'MetaDefs.Term.F2'(A,B)).
'Substs.Dereference.P3'('MetaDefs.CTerm.F1'(A), _, 'MetaDefs.CTerm.F1'(A)).
'~Substs.Dereference.P3'('MetaDefs.CTerm.F1'(A), _, 'MetaDefs.CTerm.F1'(A)).
'Substs.Dereference.P3'('MetaDefs.XTerm.F3'(A,B,C), _, 'MetaDefs.XTerm.F3'(A,B,C)).
'~Substs.Dereference.P3'('MetaDefs.XTerm.F3'(A,B,C), _, 'MetaDefs.XTerm.F3'(A,B,C)).
'Substs.Dereference.P3'('MetaDefs.XCTerm.F2'(A,B), _, 'MetaDefs.XCTerm.F2'(A,B)).
'~Substs.Dereference.P3'('MetaDefs.XCTerm.F2'(A,B), _, 'MetaDefs.XCTerm.F2'(A,B)).
'Substs.Dereference.P3'('MetaDefs.Str.F1'(A), _, 'MetaDefs.Str.F1'(A)).
'~Substs.Dereference.P3'('MetaDefs.Str.F1'(A), _, 'MetaDefs.Str.F1'(A)).
'Substs.Dereference.P3'('MetaDefs.Int.F1'(A), _, 'MetaDefs.Int.F1'(A)).
'~Substs.Dereference.P3'('MetaDefs.Int.F1'(A), _, 'MetaDefs.Int.F1'(A)).
'Substs.Dereference.P3'('MetaDefs.Prm.F1'(A), _, 'MetaDefs.Prm.F1'(A)).
'~Substs.Dereference.P3'('MetaDefs.Prm.F1'(A), _, 'MetaDefs.Prm.F1'(A)).
'Substs.Dereference.P3'('MetaDefs.SuchThat.F2'(A,B), _, 'MetaDefs.SuchThat.F2'(A,B)).
'~Substs.Dereference.P3'('MetaDefs.SuchThat.F2'(A,B), _, 'MetaDefs.SuchThat.F2'(A,B)).
'Substs.Dereference.P3'('MetaDefs.Var.F2'(A,B), 'Substs.TermSubst.F2'(C,D), E) :-
        user:one_solution(user:goedel_freeze(ground([B,A,D]),if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),F),D),true),'Substs':'Substs.Dereference.P3'(F,'Substs.TermSubst.F2'(C,D),E),E='MetaDefs.Var.F2'(A,B)))).
'~Substs.Dereference.P3'('MetaDefs.Var.F2'(A,B), 'Substs.TermSubst.F2'(C,D), E) :-
        user:goedel_freeze(ground([B,A,D]), if(('Lists':'~Lists.MemberCheck.P2'('MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),F),D),true),'Substs':'~Substs.Dereference.P3'(F,'Substs.TermSubst.F2'(C,D),E),E='MetaDefs.Var.F2'(A,B))).
'Substs.FullDerefTerm.P4'('MetaDefs.Var.F1'(A), B, _, C) :-
        user:goedel_freeze(ground([A,B]), (B='MetaDefs.Var.F1'(A),true->C='MetaDefs.Occ.F1'(B);C='MetaDefs.Var.F1'(A))).
'~Substs.FullDerefTerm.P4'('MetaDefs.Var.F1'(A), B, _, C) :-
        user:goedel_freeze(ground([A,B]), (B='MetaDefs.Var.F1'(A),true->C='MetaDefs.Occ.F1'(B);C='MetaDefs.Var.F1'(A))).
'Substs.FullDerefTerm.P4'('MetaDefs.Var.F2'(A,B), C, _, D) :-
        user:goedel_freeze(ground([B,C,A]), (C='MetaDefs.Var.F2'(A,B),true->D='MetaDefs.Occ.F1'(C);D='MetaDefs.Var.F2'(A,B))).
'~Substs.FullDerefTerm.P4'('MetaDefs.Var.F2'(A,B), C, _, D) :-
        user:goedel_freeze(ground([B,C,A]), (C='MetaDefs.Var.F2'(A,B),true->D='MetaDefs.Occ.F1'(C);D='MetaDefs.Var.F2'(A,B))).
'Substs.FullDerefTerm.P4'('MetaDefs.CTerm.F1'(A), _, _, 'MetaDefs.CTerm.F1'(A)).
'~Substs.FullDerefTerm.P4'('MetaDefs.CTerm.F1'(A), _, _, 'MetaDefs.CTerm.F1'(A)).
'Substs.FullDerefTerm.P4'('MetaDefs.Term.F2'(A,B), C, D, 'MetaDefs.Term.F2'(A,E)) :-
        'Substs.FullDerefTerm1.P4'(B, C, D, E).
'~Substs.FullDerefTerm.P4'('MetaDefs.Term.F2'(A,B), C, D, 'MetaDefs.Term.F2'(A,E)) :-
        '~Substs.FullDerefTerm1.P4'(B, C, D, E).
'Substs.FullDerefTerm.P4'('MetaDefs.XCTerm.F2'(A,B), _, _, 'MetaDefs.XCTerm.F2'(A,B)).
'~Substs.FullDerefTerm.P4'('MetaDefs.XCTerm.F2'(A,B), _, _, 'MetaDefs.XCTerm.F2'(A,B)).
'Substs.FullDerefTerm.P4'('MetaDefs.XTerm.F3'(A,B,C), D, E, 'MetaDefs.XTerm.F3'(A,F,C)) :-
        'Substs.FullDerefTerm1.P4'(B, D, E, F).
'~Substs.FullDerefTerm.P4'('MetaDefs.XTerm.F3'(A,B,C), D, E, 'MetaDefs.XTerm.F3'(A,F,C)) :-
        '~Substs.FullDerefTerm1.P4'(B, D, E, F).
'Substs.FullDerefTerm.P4'('MetaDefs.Int.F1'(A), _, _, 'MetaDefs.Int.F1'(A)).
'~Substs.FullDerefTerm.P4'('MetaDefs.Int.F1'(A), _, _, 'MetaDefs.Int.F1'(A)).
'Substs.FullDerefTerm.P4'('MetaDefs.Prm.F1'(A), _, _, 'MetaDefs.Prm.F1'(A)).
'~Substs.FullDerefTerm.P4'('MetaDefs.Prm.F1'(A), _, _, 'MetaDefs.Prm.F1'(A)).
'Substs.FullDerefTerm.P4'('MetaDefs.Str.F1'(A), _, _, 'MetaDefs.Str.F1'(A)).
'~Substs.FullDerefTerm.P4'('MetaDefs.Str.F1'(A), _, _, 'MetaDefs.Str.F1'(A)).
'Substs.FullDerefTerm.P4'('MetaDefs.SuchThat.F2'(A,B), _, _, 'MetaDefs.SuchThat.F2'(A,B)).
'~Substs.FullDerefTerm.P4'('MetaDefs.SuchThat.F2'(A,B), _, _, 'MetaDefs.SuchThat.F2'(A,B)).
'Substs.EmptyHeap.P1'('Substs.Heap.F2'(0,'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0'))).
'~Substs.EmptyHeap.P1'('Substs.Heap.F2'(0,'Substs.H.F16'('Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0','Substs.N.C0'))).
'Substs.FullDerefTerm1.P4'([], _, _, []).
'~Substs.FullDerefTerm1.P4'([], _, _, []).
'Substs.FullDerefTerm1.P4'([A|B], C, D, [E|F]) :-
        'Substs.ApplySubstToTerm.P4'(A, C, D, E),
        'Substs.FullDerefTerm1.P4'(B, C, D, F).
'~Substs.FullDerefTerm1.P4'([A|B], C, D, [E|F]) :-
        '~Substs.ApplySubstToTerm.P4'(A, C, D, E),
        '~Substs.FullDerefTerm1.P4'(B, C, D, F).
'Substs.RationaliseTypeHeap.P6'('Substs.N.C0', _, _, _, A, A).
'~Substs.RationaliseTypeHeap.P6'('Substs.N.C0', _, _, _, A, A).
'Substs.RationaliseTypeHeap.P6'('Substs.R.F1'(A), _, B, C, D, E) :-
        'Substs.ApplySubstToType.P4'('MetaDefs.Par.F1'(A), 'MetaDefs.Par.F1'(B), C, F),
        user:goedel_freeze(ground([B,F]), (user:not_equal([],[F,B],F,'MetaDefs.Occ.F1'('MetaDefs.Par.F1'(B))),true->'Substs':'Substs.AddTypeBinding1.P4'(F,B,D,E);E=D)).
'~Substs.RationaliseTypeHeap.P6'('Substs.R.F1'(A), _, B, C, D, E) :-
        '~Substs.ApplySubstToType.P4'('MetaDefs.Par.F1'(A), 'MetaDefs.Par.F1'(B), C, F),
        user:goedel_freeze(ground([B,F]), (user:not_equal([],[F,B],F,'MetaDefs.Occ.F1'('MetaDefs.Par.F1'(B))),true->'Substs':'~Substs.AddTypeBinding1.P4'(F,B,D,E);E=D)).
'Substs.RationaliseTypeHeap.P6'('Substs.V.F1'(A), _, B, C, D, E) :-
        'Substs.ApplySubstToType.P4'(A, 'MetaDefs.Par.F1'(B), C, F),
        user:goedel_freeze(ground([B,F]), (user:not_equal([],[F,B],F,'MetaDefs.Occ.F1'('MetaDefs.Par.F1'(B))),true->'Substs':'Substs.AddTypeBinding1.P4'(F,B,D,E);E=D)).
'~Substs.RationaliseTypeHeap.P6'('Substs.V.F1'(A), _, B, C, D, E) :-
        '~Substs.ApplySubstToType.P4'(A, 'MetaDefs.Par.F1'(B), C, F),
        user:goedel_freeze(ground([B,F]), (user:not_equal([],[F,B],F,'MetaDefs.Occ.F1'('MetaDefs.Par.F1'(B))),true->'Substs':'~Substs.AddTypeBinding1.P4'(F,B,D,E);E=D)).
'Substs.RationaliseTypeHeap.P6'('Substs.T.F1'(A), _, B, C, D, E) :-
        'Substs.ApplySubstToType.P4'(A, 'MetaDefs.Par.F1'(B), C, F),
        'Substs.AddTypeBinding1.P4'(F, B, D, E).
'~Substs.RationaliseTypeHeap.P6'('Substs.T.F1'(A), _, B, C, D, E) :-
        '~Substs.ApplySubstToType.P4'(A, 'MetaDefs.Par.F1'(B), C, F),
        '~Substs.AddTypeBinding1.P4'(F, B, D, E).
'Substs.RationaliseTypeHeap.P6'('Substs.H.F16'(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), Q, R, S, T, U) :-
        'Integers':minus(Q, 1, W),
        V=W,
        'Integers':power(16, Q, Y),
        X=Y,
        'Substs.RationaliseTypeHeap.P6'(A, V, R, S, T, Z),
        'Integers':plus(X, R, A1),
        'Substs.RationaliseTypeHeap.P6'(B, V, A1, S, Z, B1),
        'Integers':times(2, X, E1),
        'Integers':plus(E1, R, C1),
        'Substs.RationaliseTypeHeap.P6'(C, V, C1, S, B1, D1),
        'Integers':times(3, X, H1),
        'Integers':plus(H1, R, F1),
        'Substs.RationaliseTypeHeap.P6'(D, V, F1, S, D1, G1),
        'Integers':times(4, X, K1),
        'Integers':plus(K1, R, I1),
        'Substs.RationaliseTypeHeap.P6'(E, V, I1, S, G1, J1),
        'Integers':times(5, X, N1),
        'Integers':plus(N1, R, L1),
        'Substs.RationaliseTypeHeap.P6'(F, V, L1, S, J1, M1),
        'Integers':times(6, X, Q1),
        'Integers':plus(Q1, R, O1),
        'Substs.RationaliseTypeHeap.P6'(G, V, O1, S, M1, P1),
        'Integers':times(7, X, T1),
        'Integers':plus(T1, R, R1),
        'Substs.RationaliseTypeHeap.P6'(H, V, R1, S, P1, S1),
        'Integers':times(8, X, W1),
        'Integers':plus(W1, R, U1),
        'Substs.RationaliseTypeHeap.P6'(I, V, U1, S, S1, V1),
        'Integers':times(9, X, Z1),
        'Integers':plus(Z1, R, X1),
        'Substs.RationaliseTypeHeap.P6'(J, V, X1, S, V1, Y1),
        'Integers':times(10, X, C2),
        'Integers':plus(C2, R, A2),
        'Substs.RationaliseTypeHeap.P6'(K, V, A2, S, Y1, B2),
        'Integers':times(11, X, F2),
        'Integers':plus(F2, R, D2),
        'Substs.RationaliseTypeHeap.P6'(L, V, D2, S, B2, E2),
        'Integers':times(12, X, I2),
        'Integers':plus(I2, R, G2),
        'Substs.RationaliseTypeHeap.P6'(M, V, G2, S, E2, H2),
        'Integers':times(13, X, L2),
        'Integers':plus(L2, R, J2),
        'Substs.RationaliseTypeHeap.P6'(N, V, J2, S, H2, K2),
        'Integers':times(14, X, O2),
        'Integers':plus(O2, R, M2),
        'Substs.RationaliseTypeHeap.P6'(O, V, M2, S, K2, N2),
        'Integers':times(15, X, Q2),
        'Integers':plus(Q2, R, P2),
        'Substs.RationaliseTypeHeap.P6'(P, V, P2, S, N2, U).
'~Substs.RationaliseTypeHeap.P6'('Substs.H.F16'(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), Q, R, S, T, U) :-
        'Integers':minus(Q, 1, W),
        V=W,
        'Integers':power(16, Q, Y),
        X=Y,
        '~Substs.RationaliseTypeHeap.P6'(A, V, R, S, T, Z),
        'Integers':plus(X, R, A1),
        '~Substs.RationaliseTypeHeap.P6'(B, V, A1, S, Z, B1),
        'Integers':times(2, X, E1),
        'Integers':plus(E1, R, C1),
        '~Substs.RationaliseTypeHeap.P6'(C, V, C1, S, B1, D1),
        'Integers':times(3, X, H1),
        'Integers':plus(H1, R, F1),
        '~Substs.RationaliseTypeHeap.P6'(D, V, F1, S, D1, G1),
        'Integers':times(4, X, K1),
        'Integers':plus(K1, R, I1),
        '~Substs.RationaliseTypeHeap.P6'(E, V, I1, S, G1, J1),
        'Integers':times(5, X, N1),
        'Integers':plus(N1, R, L1),
        '~Substs.RationaliseTypeHeap.P6'(F, V, L1, S, J1, M1),
        'Integers':times(6, X, Q1),
        'Integers':plus(Q1, R, O1),
        '~Substs.RationaliseTypeHeap.P6'(G, V, O1, S, M1, P1),
        'Integers':times(7, X, T1),
        'Integers':plus(T1, R, R1),
        '~Substs.RationaliseTypeHeap.P6'(H, V, R1, S, P1, S1),
        'Integers':times(8, X, W1),
        'Integers':plus(W1, R, U1),
        '~Substs.RationaliseTypeHeap.P6'(I, V, U1, S, S1, V1),
        'Integers':times(9, X, Z1),
        'Integers':plus(Z1, R, X1),
        '~Substs.RationaliseTypeHeap.P6'(J, V, X1, S, V1, Y1),
        'Integers':times(10, X, C2),
        'Integers':plus(C2, R, A2),
        '~Substs.RationaliseTypeHeap.P6'(K, V, A2, S, Y1, B2),
        'Integers':times(11, X, F2),
        'Integers':plus(F2, R, D2),
        '~Substs.RationaliseTypeHeap.P6'(L, V, D2, S, B2, E2),
        'Integers':times(12, X, I2),
        'Integers':plus(I2, R, G2),
        '~Substs.RationaliseTypeHeap.P6'(M, V, G2, S, E2, H2),
        'Integers':times(13, X, L2),
        'Integers':plus(L2, R, J2),
        '~Substs.RationaliseTypeHeap.P6'(N, V, J2, S, H2, K2),
        'Integers':times(14, X, O2),
        'Integers':plus(O2, R, M2),
        '~Substs.RationaliseTypeHeap.P6'(O, V, M2, S, K2, N2),
        'Integers':times(15, X, Q2),
        'Integers':plus(Q2, R, P2),
        '~Substs.RationaliseTypeHeap.P6'(P, V, P2, S, N2, U).
'Substs.HeapTerm.P2'('Substs.R.F1'(A), 'MetaDefs.Var.F1'(A)).
'~Substs.HeapTerm.P2'('Substs.R.F1'(A), 'MetaDefs.Var.F1'(A)).
'Substs.HeapTerm.P2'('Substs.V.F1'(A), A).
'~Substs.HeapTerm.P2'('Substs.V.F1'(A), A).
'Substs.HeapTerm.P2'('Substs.T.F1'(A), A).
'~Substs.HeapTerm.P2'('Substs.T.F1'(A), A).
'Substs.GetConstant1.P4'('MetaDefs.Var.F1'(A), B, 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,D)) :-
        'Substs.AddBinding.P4'(A, 'Substs.T.F1'(B), C, E).
'~Substs.GetConstant1.P4'('MetaDefs.Var.F1'(A), B, 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,D)) :-
        '~Substs.AddBinding.P4'(A, 'Substs.T.F1'(B), C, E).
'Substs.GetConstant1.P4'('MetaDefs.Var.F2'(A,B), C, 'Substs.TermSubst.F2'(D,E), 'Substs.TermSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),C)|E])).
'~Substs.GetConstant1.P4'('MetaDefs.Var.F2'(A,B), C, 'Substs.TermSubst.F2'(D,E), 'Substs.TermSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),C)|E])).
'Substs.GetConstant1.P4'('MetaDefs.CTerm.F1'(A), 'MetaDefs.CTerm.F1'(A), B, B).
'~Substs.GetConstant1.P4'('MetaDefs.CTerm.F1'(A), 'MetaDefs.CTerm.F1'(A), B, B).
'Substs.GetConstant1.P4'('MetaDefs.XCTerm.F2'(A,B), 'MetaDefs.XCTerm.F2'(A,B), C, C).
'~Substs.GetConstant1.P4'('MetaDefs.XCTerm.F2'(A,B), 'MetaDefs.XCTerm.F2'(A,B), C, C).
'Substs.GetConstant1.P4'('MetaDefs.Int.F1'(A), 'MetaDefs.Int.F1'(A), B, B).
'~Substs.GetConstant1.P4'('MetaDefs.Int.F1'(A), 'MetaDefs.Int.F1'(A), B, B).
'Substs.GetConstant1.P4'('MetaDefs.Prm.F1'(A), 'MetaDefs.Prm.F1'(A), B, B).
'~Substs.GetConstant1.P4'('MetaDefs.Prm.F1'(A), 'MetaDefs.Prm.F1'(A), B, B).
'Substs.GetConstant1.P4'('MetaDefs.Str.F1'(A), 'MetaDefs.Str.F1'(A), B, B).
'~Substs.GetConstant1.P4'('MetaDefs.Str.F1'(A), 'MetaDefs.Str.F1'(A), B, B).
'Substs.GetBase.P4'(A, B, C, D) :-
        'Substs.DerefType.P3'(A, C, E),
        'Substs.GetBase1.P4'(E, B, C, D).
'~Substs.GetBase.P4'(A, B, C, D) :-
        '~Substs.DerefType.P3'(A, C, E),
        '~Substs.GetBase1.P4'(E, B, C, D).
'Substs.FullDereference.P3'('MetaDefs.Var.F2'(A,B), _, 'MetaDefs.Var.F2'(A,B)).
'~Substs.FullDereference.P3'('MetaDefs.Var.F2'(A,B), _, 'MetaDefs.Var.F2'(A,B)).
'Substs.FullDereference.P3'('MetaDefs.Var.F1'(A), _, 'MetaDefs.Var.F1'(A)).
'~Substs.FullDereference.P3'('MetaDefs.Var.F1'(A), _, 'MetaDefs.Var.F1'(A)).
'Substs.FullDereference.P3'('MetaDefs.Term.F2'(A,B), C, 'MetaDefs.Term.F2'(A,D)) :-
        'Substs.ApplyTermSubst.P3'(B, C, D).
'~Substs.FullDereference.P3'('MetaDefs.Term.F2'(A,B), C, 'MetaDefs.Term.F2'(A,D)) :-
        '~Substs.ApplyTermSubst.P3'(B, C, D).
'Substs.FullDereference.P3'('MetaDefs.CTerm.F1'(A), _, 'MetaDefs.CTerm.F1'(A)).
'~Substs.FullDereference.P3'('MetaDefs.CTerm.F1'(A), _, 'MetaDefs.CTerm.F1'(A)).
'Substs.FullDereference.P3'('MetaDefs.XTerm.F3'(A,B,C), D, 'MetaDefs.XTerm.F3'(A,E,C)) :-
        'Substs.ApplyTermSubst.P3'(B, D, E).
'~Substs.FullDereference.P3'('MetaDefs.XTerm.F3'(A,B,C), D, 'MetaDefs.XTerm.F3'(A,E,C)) :-
        '~Substs.ApplyTermSubst.P3'(B, D, E).
'Substs.FullDereference.P3'('MetaDefs.XCTerm.F2'(A,B), _, 'MetaDefs.XCTerm.F2'(A,B)).
'~Substs.FullDereference.P3'('MetaDefs.XCTerm.F2'(A,B), _, 'MetaDefs.XCTerm.F2'(A,B)).
'Substs.FullDereference.P3'('MetaDefs.Str.F1'(A), _, 'MetaDefs.Str.F1'(A)).
'~Substs.FullDereference.P3'('MetaDefs.Str.F1'(A), _, 'MetaDefs.Str.F1'(A)).
'Substs.FullDereference.P3'('MetaDefs.Int.F1'(A), _, 'MetaDefs.Int.F1'(A)).
'~Substs.FullDereference.P3'('MetaDefs.Int.F1'(A), _, 'MetaDefs.Int.F1'(A)).
'Substs.FullDereference.P3'('MetaDefs.Prm.F1'(A), _, 'MetaDefs.Prm.F1'(A)).
'~Substs.FullDereference.P3'('MetaDefs.Prm.F1'(A), _, 'MetaDefs.Prm.F1'(A)).
'Substs.FullDereference.P3'('MetaDefs.SuchThat.F2'(A,B), _, 'MetaDefs.SuchThat.F2'(A,B)).
'~Substs.FullDereference.P3'('MetaDefs.SuchThat.F2'(A,B), _, 'MetaDefs.SuchThat.F2'(A,B)).
'Substs.FullDerefType1.P4'([], _, _, []).
'~Substs.FullDerefType1.P4'([], _, _, []).
'Substs.FullDerefType1.P4'([A|B], C, D, [E|F]) :-
        'Substs.ApplySubstToType.P4'(A, C, D, E),
        'Substs.FullDerefType1.P4'(B, C, D, F).
'~Substs.FullDerefType1.P4'([A|B], C, D, [E|F]) :-
        '~Substs.ApplySubstToType.P4'(A, C, D, E),
        '~Substs.FullDerefType1.P4'(B, C, D, F).
'Substs.GetConstant.P4'(A, B, C, D) :-
        'Substs.Dereference.P3'(A, C, E),
        'Substs.GetConstant1.P4'(E, B, C, D).
'~Substs.GetConstant.P4'(A, B, C, D) :-
        '~Substs.Dereference.P3'(A, C, E),
        '~Substs.GetConstant1.P4'(E, B, C, D).
'Substs.GetBase1.P4'('MetaDefs.Par.F1'(A), B, 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,D)) :-
        'Substs.AddBinding.P4'(A, 'Substs.T.F1'(B), C, E).
'~Substs.GetBase1.P4'('MetaDefs.Par.F1'(A), B, 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,D)) :-
        '~Substs.AddBinding.P4'(A, 'Substs.T.F1'(B), C, E).
'Substs.GetBase1.P4'('MetaDefs.Par.F2'(A,B), C, 'Substs.TypeSubst.F2'(D,E), 'Substs.TypeSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),C)|E])).
'~Substs.GetBase1.P4'('MetaDefs.Par.F2'(A,B), C, 'Substs.TypeSubst.F2'(D,E), 'Substs.TypeSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),C)|E])).
'Substs.GetBase1.P4'('MetaDefs.BType.F1'(A), 'MetaDefs.BType.F1'(A), B, B).
'~Substs.GetBase1.P4'('MetaDefs.BType.F1'(A), 'MetaDefs.BType.F1'(A), B, B).
'Substs.GetBase1.P4'('MetaDefs.XBType.F1'(A), 'MetaDefs.XBType.F1'(A), B, B).
'~Substs.GetBase1.P4'('MetaDefs.XBType.F1'(A), 'MetaDefs.XBType.F1'(A), B, B).
'Substs.GetType.P5'(A, B, C, D, E) :-
        'Substs.DerefType.P3'(A, D, F),
        'Substs.GetType1.P5'(F, B, C, D, E).
'~Substs.GetType.P5'(A, B, C, D, E) :-
        '~Substs.DerefType.P3'(A, D, F),
        '~Substs.GetType1.P5'(F, B, C, D, E).
'Substs.GetFunction.P5'(A, B, C, D, E) :-
        'Substs.Dereference.P3'(A, D, F),
        'Substs.GetFunction1.P5'(F, B, C, D, E).
'~Substs.GetFunction.P5'(A, B, C, D, E) :-
        '~Substs.Dereference.P3'(A, D, F),
        '~Substs.GetFunction1.P5'(F, B, C, D, E).
'Substs.GetFunction1.P5'('MetaDefs.Var.F1'(A), B, 'Substs.Write.C0', 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,D)) :-
        'Substs.AddBinding.P4'(A, 'Substs.T.F1'(B), C, E).
'~Substs.GetFunction1.P5'('MetaDefs.Var.F1'(A), B, 'Substs.Write.C0', 'Substs.TermSubst.F2'(C,D), 'Substs.TermSubst.F2'(E,D)) :-
        '~Substs.AddBinding.P4'(A, 'Substs.T.F1'(B), C, E).
'Substs.GetFunction1.P5'('MetaDefs.Var.F2'(A,B), C, 'Substs.Write.C0', 'Substs.TermSubst.F2'(D,E), 'Substs.TermSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),C)|E])).
'~Substs.GetFunction1.P5'('MetaDefs.Var.F2'(A,B), C, 'Substs.Write.C0', 'Substs.TermSubst.F2'(D,E), 'Substs.TermSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Var.F2'(A,B),C)|E])).
'Substs.GetFunction1.P5'('MetaDefs.Term.F2'(A,B), 'MetaDefs.Term.F2'(A,B), 'Substs.Read.C0', C, C).
'~Substs.GetFunction1.P5'('MetaDefs.Term.F2'(A,B), 'MetaDefs.Term.F2'(A,B), 'Substs.Read.C0', C, C).
'Substs.GetFunction1.P5'('MetaDefs.XTerm.F3'(A,B,C), 'MetaDefs.XTerm.F3'(A,B,C), 'Substs.Read.C0', D, D).
'~Substs.GetFunction1.P5'('MetaDefs.XTerm.F3'(A,B,C), 'MetaDefs.XTerm.F3'(A,B,C), 'Substs.Read.C0', D, D).
'Substs.GetType1.P5'('MetaDefs.Par.F1'(A), B, 'Substs.Write.C0', 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,D)) :-
        'Substs.AddBinding.P4'(A, 'Substs.T.F1'(B), C, E).
'~Substs.GetType1.P5'('MetaDefs.Par.F1'(A), B, 'Substs.Write.C0', 'Substs.TypeSubst.F2'(C,D), 'Substs.TypeSubst.F2'(E,D)) :-
        '~Substs.AddBinding.P4'(A, 'Substs.T.F1'(B), C, E).
'Substs.GetType1.P5'('MetaDefs.Par.F2'(A,B), C, 'Substs.Write.C0', 'Substs.TypeSubst.F2'(D,E), 'Substs.TypeSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),C)|E])).
'~Substs.GetType1.P5'('MetaDefs.Par.F2'(A,B), C, 'Substs.Write.C0', 'Substs.TypeSubst.F2'(D,E), 'Substs.TypeSubst.F2'(D,['MetaDefs.!.F2'('MetaDefs.Par.F2'(A,B),C)|E])).
'Substs.GetType1.P5'('MetaDefs.Type.F2'(A,B), 'MetaDefs.Type.F2'(A,B), 'Substs.Read.C0', C, C).
'~Substs.GetType1.P5'('MetaDefs.Type.F2'(A,B), 'MetaDefs.Type.F2'(A,B), 'Substs.Read.C0', C, C).
'Substs.GetType1.P5'('MetaDefs.XType.F2'(A,B), 'MetaDefs.XType.F2'(A,B), 'Substs.Read.C0', C, C).
'~Substs.GetType1.P5'('MetaDefs.XType.F2'(A,B), 'MetaDefs.XType.F2'(A,B), 'Substs.Read.C0', C, C).
'Substs.ParameterInSubst.P3'('Substs.TypeSubst.F2'(_,A), B, C) :-
        'Lists':'Lists.Member.P2'('MetaDefs.!.F2'(B,C), A).
'~Substs.ParameterInSubst.P3'('Substs.TypeSubst.F2'(_,A), B, C) :-
        'Lists':'~Lists.Member.P2'('MetaDefs.!.F2'(B,C), A).
'Substs.ParameterInSubst.P3'('Substs.TypeSubst.F2'(A,_), 'MetaDefs.Par.F1'(B), C) :-
        'Substs.BindingInHeap.P3'(A, B, D),
        'Substs.HeapType.P2'(D, C).
'~Substs.ParameterInSubst.P3'('Substs.TypeSubst.F2'(A,_), 'MetaDefs.Par.F1'(B), C) :-
        '~Substs.BindingInHeap.P3'(A, B, D),
        '~Substs.HeapType.P2'(D, C).
'Substs.HeapType.P2'('Substs.R.F1'(A), 'MetaDefs.Par.F1'(A)).
'~Substs.HeapType.P2'('Substs.R.F1'(A), 'MetaDefs.Par.F1'(A)).
'Substs.HeapType.P2'('Substs.V.F1'(A), A).
'~Substs.HeapType.P2'('Substs.V.F1'(A), A).
'Substs.HeapType.P2'('Substs.T.F1'(A), A).
'~Substs.HeapType.P2'('Substs.T.F1'(A), A).
'Substs.RationaliseTermHeap.P6'('Substs.N.C0', _, _, _, A, A).
'~Substs.RationaliseTermHeap.P6'('Substs.N.C0', _, _, _, A, A).
'Substs.RationaliseTermHeap.P6'('Substs.R.F1'(A), _, B, C, D, E) :-
        'Substs.ApplySubstToTerm.P4'('MetaDefs.Var.F1'(A), 'MetaDefs.Var.F1'(B), C, F),
        user:goedel_freeze(ground([B,F]), (user:not_equal([],[F,B],F,'MetaDefs.Occ.F1'('MetaDefs.Var.F1'(B))),true->'Substs':'Substs.AddTermBinding1.P4'(F,B,D,E);E=D)).
'~Substs.RationaliseTermHeap.P6'('Substs.R.F1'(A), _, B, C, D, E) :-
        '~Substs.ApplySubstToTerm.P4'('MetaDefs.Var.F1'(A), 'MetaDefs.Var.F1'(B), C, F),
        user:goedel_freeze(ground([B,F]), (user:not_equal([],[F,B],F,'MetaDefs.Occ.F1'('MetaDefs.Var.F1'(B))),true->'Substs':'~Substs.AddTermBinding1.P4'(F,B,D,E);E=D)).
'Substs.RationaliseTermHeap.P6'('Substs.V.F1'(A), _, B, C, D, E) :-
        'Substs.ApplySubstToTerm.P4'(A, 'MetaDefs.Var.F1'(B), C, F),
        user:goedel_freeze(ground([B,F]), (user:not_equal([],[F,B],F,'MetaDefs.Occ.F1'('MetaDefs.Var.F1'(B))),true->'Substs':'Substs.AddTermBinding1.P4'(F,B,D,E);E=D)).
'~Substs.RationaliseTermHeap.P6'('Substs.V.F1'(A), _, B, C, D, E) :-
        '~Substs.ApplySubstToTerm.P4'(A, 'MetaDefs.Var.F1'(B), C, F),
        user:goedel_freeze(ground([B,F]), (user:not_equal([],[F,B],F,'MetaDefs.Occ.F1'('MetaDefs.Var.F1'(B))),true->'Substs':'~Substs.AddTermBinding1.P4'(F,B,D,E);E=D)).
'Substs.RationaliseTermHeap.P6'('Substs.T.F1'(A), _, B, C, D, E) :-
        'Substs.ApplySubstToTerm.P4'(A, 'MetaDefs.Var.F1'(B), C, F),
        'Substs.AddTermBinding1.P4'(F, B, D, E).
'~Substs.RationaliseTermHeap.P6'('Substs.T.F1'(A), _, B, C, D, E) :-
        '~Substs.ApplySubstToTerm.P4'(A, 'MetaDefs.Var.F1'(B), C, F),
        '~Substs.AddTermBinding1.P4'(F, B, D, E).
'Substs.RationaliseTermHeap.P6'('Substs.H.F16'(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), Q, R, S, T, U) :-
        'Integers':minus(Q, 1, W),
        V=W,
        'Integers':power(16, Q, Y),
        X=Y,
        'Substs.RationaliseTermHeap.P6'(A, V, R, S, T, Z),
        'Integers':plus(X, R, A1),
        'Substs.RationaliseTermHeap.P6'(B, V, A1, S, Z, B1),
        'Integers':times(2, X, E1),
        'Integers':plus(E1, R, C1),
        'Substs.RationaliseTermHeap.P6'(C, V, C1, S, B1, D1),
        'Integers':times(3, X, H1),
        'Integers':plus(H1, R, F1),
        'Substs.RationaliseTermHeap.P6'(D, V, F1, S, D1, G1),
        'Integers':times(4, X, K1),
        'Integers':plus(K1, R, I1),
        'Substs.RationaliseTermHeap.P6'(E, V, I1, S, G1, J1),
        'Integers':times(5, X, N1),
        'Integers':plus(N1, R, L1),
        'Substs.RationaliseTermHeap.P6'(F, V, L1, S, J1, M1),
        'Integers':times(6, X, Q1),
        'Integers':plus(Q1, R, O1),
        'Substs.RationaliseTermHeap.P6'(G, V, O1, S, M1, P1),
        'Integers':times(7, X, T1),
        'Integers':plus(T1, R, R1),
        'Substs.RationaliseTermHeap.P6'(H, V, R1, S, P1, S1),
        'Integers':times(8, X, W1),
        'Integers':plus(W1, R, U1),
        'Substs.RationaliseTermHeap.P6'(I, V, U1, S, S1, V1),
        'Integers':times(9, X, Z1),
        'Integers':plus(Z1, R, X1),
        'Substs.RationaliseTermHeap.P6'(J, V, X1, S, V1, Y1),
        'Integers':times(10, X, C2),
        'Integers':plus(C2, R, A2),
        'Substs.RationaliseTermHeap.P6'(K, V, A2, S, Y1, B2),
        'Integers':times(11, X, F2),
        'Integers':plus(F2, R, D2),
        'Substs.RationaliseTermHeap.P6'(L, V, D2, S, B2, E2),
        'Integers':times(12, X, I2),
        'Integers':plus(I2, R, G2),
        'Substs.RationaliseTermHeap.P6'(M, V, G2, S, E2, H2),
        'Integers':times(13, X, L2),
        'Integers':plus(L2, R, J2),
        'Substs.RationaliseTermHeap.P6'(N, V, J2, S, H2, K2),
        'Integers':times(14, X, O2),
        'Integers':plus(O2, R, M2),
        'Substs.RationaliseTermHeap.P6'(O, V, M2, S, K2, N2),
        'Integers':times(15, X, Q2),
        'Integers':plus(Q2, R, P2),
        'Substs.RationaliseTermHeap.P6'(P, V, P2, S, N2, U).
'~Substs.RationaliseTermHeap.P6'('Substs.H.F16'(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), Q, R, S, T, U) :-
        'Integers':minus(Q, 1, W),
        V=W,
        'Integers':power(16, Q, Y),
        X=Y,
        '~Substs.RationaliseTermHeap.P6'(A, V, R, S, T, Z),
        'Integers':plus(X, R, A1),
        '~Substs.RationaliseTermHeap.P6'(B, V, A1, S, Z, B1),
        'Integers':times(2, X, E1),
        'Integers':plus(E1, R, C1),
        '~Substs.RationaliseTermHeap.P6'(C, V, C1, S, B1, D1),
        'Integers':times(3, X, H1),
        'Integers':plus(H1, R, F1),
        '~Substs.RationaliseTermHeap.P6'(D, V, F1, S, D1, G1),
        'Integers':times(4, X, K1),
        'Integers':plus(K1, R, I1),
        '~Substs.RationaliseTermHeap.P6'(E, V, I1, S, G1, J1),
        'Integers':times(5, X, N1),
        'Integers':plus(N1, R, L1),
        '~Substs.RationaliseTermHeap.P6'(F, V, L1, S, J1, M1),
        'Integers':times(6, X, Q1),
        'Integers':plus(Q1, R, O1),
        '~Substs.RationaliseTermHeap.P6'(G, V, O1, S, M1, P1),
        'Integers':times(7, X, T1),
        'Integers':plus(T1, R, R1),
        '~Substs.RationaliseTermHeap.P6'(H, V, R1, S, P1, S1),
        'Integers':times(8, X, W1),
        'Integers':plus(W1, R, U1),
        '~Substs.RationaliseTermHeap.P6'(I, V, U1, S, S1, V1),
        'Integers':times(9, X, Z1),
        'Integers':plus(Z1, R, X1),
        '~Substs.RationaliseTermHeap.P6'(J, V, X1, S, V1, Y1),
        'Integers':times(10, X, C2),
        'Integers':plus(C2, R, A2),
        '~Substs.RationaliseTermHeap.P6'(K, V, A2, S, Y1, B2),
        'Integers':times(11, X, F2),
        'Integers':plus(F2, R, D2),
        '~Substs.RationaliseTermHeap.P6'(L, V, D2, S, B2, E2),
        'Integers':times(12, X, I2),
        'Integers':plus(I2, R, G2),
        '~Substs.RationaliseTermHeap.P6'(M, V, G2, S, E2, H2),
        'Integers':times(13, X, L2),
        'Integers':plus(L2, R, J2),
        '~Substs.RationaliseTermHeap.P6'(N, V, J2, S, H2, K2),
        'Integers':times(14, X, O2),
        'Integers':plus(O2, R, M2),
        '~Substs.RationaliseTermHeap.P6'(O, V, M2, S, K2, N2),
        'Integers':times(15, X, Q2),
        'Integers':plus(Q2, R, P2),
        '~Substs.RationaliseTermHeap.P6'(P, V, P2, S, N2, U).
'Substs.RationaliseTermList.P3'([], [], _).
'~Substs.RationaliseTermList.P3'([], [], _).
'Substs.RationaliseTermList.P3'(['MetaDefs.!.F2'(B,C)|A], D, E) :-
        'Substs.ApplySubstToTerm.P4'(C, B, E, F),
        user:goedel_freeze(ground([C,B]), (user:not_equal([],[C,B],C,'MetaDefs.Occ.F1'(B)),true->D=['MetaDefs.!.F2'(B,F)|G];D=G)),
        'Substs.RationaliseTermList.P3'(A, G, E).
'~Substs.RationaliseTermList.P3'(['MetaDefs.!.F2'(B,C)|A], D, E) :-
        '~Substs.ApplySubstToTerm.P4'(C, B, E, F),
        user:goedel_freeze(ground([C,B]), (user:not_equal([],[C,B],C,'MetaDefs.Occ.F1'(B)),true->D=['MetaDefs.!.F2'(B,F)|G];D=G)),
        '~Substs.RationaliseTermList.P3'(A, G, E).
'Substs.SubstsComposeType.P3'(A, B, C) :-
        'Substs.ComposeTypeSubsts1.P3'(A, A, D),
        'Substs.ComposeTypeSubsts1.P3'(D, B, E),
        'Substs.ComposeTypeSubsts1.P3'(B, B, F),
        'Substs.ComposeTypeSubsts2.P3'(E, F, C).
'~Substs.SubstsComposeType.P3'(A, B, C) :-
        '~Substs.ComposeTypeSubsts1.P3'(A, A, D),
        '~Substs.ComposeTypeSubsts1.P3'(D, B, E),
        '~Substs.ComposeTypeSubsts1.P3'(B, B, F),
        '~Substs.ComposeTypeSubsts2.P3'(E, F, C).
'Substs.SubstApplyToType.P3'(A, B, C) :-
        'Substs.DerefType.P3'(A, B, D),
        'Substs.FullDerefType.P3'(D, B, C).
'~Substs.SubstApplyToType.P3'(A, B, C) :-
        '~Substs.DerefType.P3'(A, B, D),
        '~Substs.FullDerefType.P3'(D, B, C).
'Substs.RationaliseTypeList.P3'([], [], _).
'~Substs.RationaliseTypeList.P3'([], [], _).
'Substs.RationaliseTypeList.P3'(['MetaDefs.!.F2'(B,C)|A], D, E) :-
        'Substs.ApplySubstToType.P4'(C, B, E, F),
        user:goedel_freeze(ground([C,B]), (user:not_equal([],[C,B],C,'MetaDefs.Occ.F1'(B)),true->D=['MetaDefs.!.F2'(B,F)|G];D=G)),
        'Substs.RationaliseTypeList.P3'(A, G, E).
'~Substs.RationaliseTypeList.P3'(['MetaDefs.!.F2'(B,C)|A], D, E) :-
        '~Substs.ApplySubstToType.P4'(C, B, E, F),
        user:goedel_freeze(ground([C,B]), (user:not_equal([],[C,B],C,'MetaDefs.Occ.F1'(B)),true->D=['MetaDefs.!.F2'(B,F)|G];D=G)),
        '~Substs.RationaliseTypeList.P3'(A, G, E).
'Substs.SubstApplyToTerm.P3'(A, B, C) :-
        'Substs.Dereference.P3'(A, B, D),
        'Substs.FullDereference.P3'(D, B, C).
'~Substs.SubstApplyToTerm.P3'(A, B, C) :-
        '~Substs.Dereference.P3'(A, B, D),
        '~Substs.FullDereference.P3'(D, B, C).
'Substs.SubstsComposeTerm.P3'(A, B, C) :-
        'Substs.ComposeTermSubsts1.P3'(A, A, D),
        'Substs.ComposeTermSubsts1.P3'(D, B, E),
        'Substs.ComposeTermSubsts1.P3'(B, B, F),
        'Substs.ComposeTermSubsts2.P3'(E, F, C).
'~Substs.SubstsComposeTerm.P3'(A, B, C) :-
        '~Substs.ComposeTermSubsts1.P3'(A, A, D),
        '~Substs.ComposeTermSubsts1.P3'(D, B, E),
        '~Substs.ComposeTermSubsts1.P3'(B, B, F),
        '~Substs.ComposeTermSubsts2.P3'(E, F, C).
'Substs.UnifyParameter.P5'('Substs.Write.C0', A, A, B, B).
'~Substs.UnifyParameter.P5'('Substs.Write.C0', A, A, B, B).
'Substs.UnifyParameter.P5'('Substs.Read.C0', A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), (user:not_equal([],[A,B],A,B),true->'Substs':'Substs.BindParameter.P4'(B,A,C,D);D=C)).
'~Substs.UnifyParameter.P5'('Substs.Read.C0', A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), (user:not_equal([],[A,B],A,B),true->'Substs':'~Substs.BindParameter.P4'(B,A,C,D);D=C)).
'Substs.UnifyConstant.P5'('Substs.Write.C0', A, A, B, B).
'~Substs.UnifyConstant.P5'('Substs.Write.C0', A, A, B, B).
'Substs.UnifyConstant.P5'('Substs.Read.C0', A, B, C, D) :-
        'Substs.Dereference.P3'(A, C, E),
        'Substs.GetConstant1.P4'(E, B, C, D).
'~Substs.UnifyConstant.P5'('Substs.Read.C0', A, B, C, D) :-
        '~Substs.Dereference.P3'(A, C, E),
        '~Substs.GetConstant1.P4'(E, B, C, D).
'Substs.UnifyBase.P5'('Substs.Write.C0', A, A, B, B).
'~Substs.UnifyBase.P5'('Substs.Write.C0', A, A, B, B).
'Substs.UnifyBase.P5'('Substs.Read.C0', A, B, C, D) :-
        'Substs.DerefType.P3'(A, C, E),
        'Substs.GetBase1.P4'(E, B, C, D).
'~Substs.UnifyBase.P5'('Substs.Read.C0', A, B, C, D) :-
        '~Substs.DerefType.P3'(A, C, E),
        '~Substs.GetBase1.P4'(E, B, C, D).
'Substs.UnifyKnownVariable.P5'('Substs.Write.C0', A, A, B, B).
'~Substs.UnifyKnownVariable.P5'('Substs.Write.C0', A, A, B, B).
'Substs.UnifyKnownVariable.P5'('Substs.Read.C0', A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), (user:not_equal([],[A,B],A,B),true->'Substs':'Substs.BindVariable.P4'(B,A,C,D);D=C)).
'~Substs.UnifyKnownVariable.P5'('Substs.Read.C0', A, B, C, D) :-
        user:goedel_freeze(ground([A,B]), (user:not_equal([],[A,B],A,B),true->'Substs':'~Substs.BindVariable.P4'(B,A,C,D);D=C)).
'Substs.UnifyFunction.P6'('Substs.Write.C0', A, A, 'Substs.Write.C0', B, B).
'~Substs.UnifyFunction.P6'('Substs.Write.C0', A, A, 'Substs.Write.C0', B, B).
'Substs.UnifyFunction.P6'('Substs.Read.C0', A, B, C, D, E) :-
        'Substs.Dereference.P3'(A, D, F),
        'Substs.GetFunction1.P5'(F, B, C, D, E).
'~Substs.UnifyFunction.P6'('Substs.Read.C0', A, B, C, D, E) :-
        '~Substs.Dereference.P3'(A, D, F),
        '~Substs.GetFunction1.P5'(F, B, C, D, E).
'Substs.UnifyVariable.P4'('Substs.Write.C0', 'MetaDefs.Var.F1'(A), A, B) :-
        'Integers':plus(A, 1, B),
        true.
'~Substs.UnifyVariable.P4'('Substs.Write.C0', 'MetaDefs.Var.F1'(A), A, B) :-
        'Integers':plus(A, 1, B),
        true.
'Substs.UnifyVariable.P4'('Substs.Read.C0', _, A, A).
'~Substs.UnifyVariable.P4'('Substs.Read.C0', _, A, A).
'Substs.UnifyType.P6'('Substs.Write.C0', A, A, 'Substs.Write.C0', B, B).
'~Substs.UnifyType.P6'('Substs.Write.C0', A, A, 'Substs.Write.C0', B, B).
'Substs.UnifyType.P6'('Substs.Read.C0', A, B, C, D, E) :-
        'Substs.DerefType.P3'(A, D, F),
        'Substs.GetType1.P5'(F, B, C, D, E).
'~Substs.UnifyType.P6'('Substs.Read.C0', A, B, C, D, E) :-
        '~Substs.DerefType.P3'(A, D, F),
        '~Substs.GetType1.P5'(F, B, C, D, E).
'Substs.VariableInSubst.P3'('Substs.TermSubst.F2'(_,A), B, C) :-
        'Lists':'Lists.Member.P2'('MetaDefs.!.F2'(B,C), A).
'~Substs.VariableInSubst.P3'('Substs.TermSubst.F2'(_,A), B, C) :-
        'Lists':'~Lists.Member.P2'('MetaDefs.!.F2'(B,C), A).
'Substs.VariableInSubst.P3'('Substs.TermSubst.F2'(A,_), 'MetaDefs.Var.F1'(B), C) :-
        'Substs.BindingInHeap.P3'(A, B, D),
        'Substs.HeapTerm.P2'(D, C).
'~Substs.VariableInSubst.P3'('Substs.TermSubst.F2'(A,_), 'MetaDefs.Var.F1'(B), C) :-
        '~Substs.BindingInHeap.P3'(A, B, D),
        '~Substs.HeapTerm.P2'(D, C).
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
   'Substs.BindingInTypeSubst.P3'(Subst, Param, Type).


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

