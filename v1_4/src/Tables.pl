:- module('Tables', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

'Tables.ListTable.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)or(nonvar(B)and nonvar(C)), 'Tables':'Tables.ListTable.P3.0'(A,B,C)).
'~Tables.ListTable.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)or(nonvar(B)and nonvar(C)), 'Tables':'~Tables.ListTable.P3.0'(A,B,C)).
'Tables.ListTable.P3.0'(A, B, C) :-
        'Tables.ListTableAux.P3'(A, B, C).
'~Tables.ListTable.P3.0'(A, B, C) :-
        '~Tables.ListTableAux.P3'(A, B, C).
'Tables.FirstNode.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A), 'Tables':'Tables.FirstNode.P3.0'(A,B,C)).
'~Tables.FirstNode.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A), 'Tables':'~Tables.FirstNode.P3.0'(A,B,C)).
'Tables.FirstNode.P3.0'('Tables.Node.F5'('Tables.Null.C0',A,B,_,_), A, B) :- !.
'~Tables.FirstNode.P3.0'('Tables.Node.F5'('Tables.Null.C0',A,B,_,_), A, B).
'Tables.FirstNode.P3.0'('Tables.Node.F5'(A,_,_,_,_), B, C) :-
        call_residue(user:not_equal([],[A],A,'Tables.Null.C0'), D),
        (   D=[] ->
            !
        ;   user:release_suspended(D)
        ),
        'Tables.FirstNode.P3'(A, B, C).
'~Tables.FirstNode.P3.0'('Tables.Node.F5'(A,_,_,_,_), B, C) :-
        user:not_equal([], [A], A, 'Tables.Null.C0'),
        '~Tables.FirstNode.P3'(A, B, C).
'Tables.Combine.P5'('Tables.Null.C0', _, A, A, 'Tables.Yes.C0') :- !.
'~Tables.Combine.P5'('Tables.Null.C0', _, A, A, 'Tables.Yes.C0').
'Tables.Combine.P5'(A, _, 'Tables.Null.C0', A, 'Tables.Yes.C0') :- !.
'~Tables.Combine.P5'(A, _, 'Tables.Null.C0', A, 'Tables.Yes.C0').
'Tables.Combine.P5'(A, B, C, D, E) :-
        call_residue((user:not_equal([],[A],A,'Tables.Null.C0'),user:not_equal([],[C],C,'Tables.Null.C0')), F),
        (   F=[] ->
            !
        ;   user:release_suspended(F)
        ),
        'Tables.RemoveRightmost.P5'(A, G, H, I, J),
        'Tables.Adjust.P6'(J, 'Tables.Node.F5'(I,G,H,B,C), B, 'Tables.Right.C0', K, D),
        'Tables.HeightDecreased.P3'(B, K, E).
'~Tables.Combine.P5'(A, B, C, D, E) :-
        user:not_equal([], [A], A, 'Tables.Null.C0'),
        user:not_equal([], [C], C, 'Tables.Null.C0'),
        '~Tables.RemoveRightmost.P5'(A, F, G, H, I),
        '~Tables.Adjust.P6'(I, 'Tables.Node.F5'(H,F,G,B,C), B, 'Tables.Right.C0', J, D),
        '~Tables.HeightDecreased.P3'(B, J, E).
'Tables.AmendTable.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'Tables':'Tables.AmendTable.P6.0'(A,B,C,D,E,F)).
'~Tables.AmendTable.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'Tables':'~Tables.AmendTable.P6.0'(A,B,C,D,E,F)).
'Tables.AmendTable.P6.0'(A, B, C, D, E, F) :-
        'Tables.Amend.P7'(A, B, C, D, E, F, _).
'~Tables.AmendTable.P6.0'(A, B, C, D, E, F) :-
        '~Tables.Amend.P7'(A, B, C, D, E, F, _).
'Tables.Adjustment.P4'('Tables.EQ.C0', 'Tables.Left.C0', 'Tables.LH.C0', 'Tables.No.C0') :- !.
'~Tables.Adjustment.P4'('Tables.EQ.C0', 'Tables.Left.C0', 'Tables.LH.C0', 'Tables.No.C0').
'Tables.Adjustment.P4'('Tables.EQ.C0', 'Tables.Right.C0', 'Tables.RH.C0', 'Tables.No.C0') :- !.
'~Tables.Adjustment.P4'('Tables.EQ.C0', 'Tables.Right.C0', 'Tables.RH.C0', 'Tables.No.C0').
'Tables.Adjustment.P4'('Tables.LH.C0', 'Tables.Left.C0', _, 'Tables.Yes.C0') :- !.
'~Tables.Adjustment.P4'('Tables.LH.C0', 'Tables.Left.C0', _, 'Tables.Yes.C0').
'Tables.Adjustment.P4'('Tables.LH.C0', 'Tables.Right.C0', 'Tables.EQ.C0', 'Tables.No.C0') :- !.
'~Tables.Adjustment.P4'('Tables.LH.C0', 'Tables.Right.C0', 'Tables.EQ.C0', 'Tables.No.C0').
'Tables.Adjustment.P4'('Tables.RH.C0', 'Tables.Left.C0', 'Tables.EQ.C0', 'Tables.No.C0') :- !.
'~Tables.Adjustment.P4'('Tables.RH.C0', 'Tables.Left.C0', 'Tables.EQ.C0', 'Tables.No.C0').
'Tables.Adjustment.P4'('Tables.RH.C0', 'Tables.Right.C0', _, 'Tables.Yes.C0') :- !.
'~Tables.Adjustment.P4'('Tables.RH.C0', 'Tables.Right.C0', _, 'Tables.Yes.C0').
'Tables.Adjust.P6'('Tables.No.C0', A, B, _, B, A).
'~Tables.Adjust.P6'('Tables.No.C0', A, B, _, B, A).
'Tables.Adjust.P6'('Tables.Yes.C0', A, B, C, D, E) :-
        'Tables.Adjustment.P4'(B, C, D, F),
        'Tables.Rebalance.P4'(F, A, D, E).
'~Tables.Adjust.P6'('Tables.Yes.C0', A, B, C, D, E) :-
        '~Tables.Adjustment.P4'(B, C, D, F),
        '~Tables.Rebalance.P4'(F, A, D, E).
'Tables.Amend.P7'('Tables.Null.C0', A, B, C, 'Tables.Node.F5'('Tables.Null.C0',A,B,'Tables.EQ.C0','Tables.Null.C0'), C, 'Tables.Yes.C0').
'~Tables.Amend.P7'('Tables.Null.C0', A, B, C, 'Tables.Node.F5'('Tables.Null.C0',A,B,'Tables.EQ.C0','Tables.Null.C0'), C, 'Tables.Yes.C0').
'Tables.Amend.P7'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I, J, K) :-
        call_residue('Strings':'Strings.<.P2'(F,B), L),
        (   L=[] ->
            !
        ;   user:release_suspended(L)
        ),
        'Tables.Amend.P7'(A, F, G, H, M, J, N),
        'Tables.Adjust.P6'(N, 'Tables.Node.F5'(M,B,C,D,E), D, 'Tables.Left.C0', O, I),
        'Tables.HeightIncreased.P3'(D, O, K).
'~Tables.Amend.P7'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I, J, K) :-
        'Strings':'~Strings.<.P2'(F, B),
        '~Tables.Amend.P7'(A, F, G, H, L, J, M),
        '~Tables.Adjust.P6'(M, 'Tables.Node.F5'(L,B,C,D,E), D, 'Tables.Left.C0', N, I),
        '~Tables.HeightIncreased.P3'(D, N, K).
'Tables.Amend.P7'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I, J, K) :-
        call_residue('Strings':'Strings.>.P2'(F,B), L),
        (   L=[] ->
            !
        ;   user:release_suspended(L)
        ),
        'Tables.Amend.P7'(E, F, G, H, M, J, N),
        'Tables.Adjust.P6'(N, 'Tables.Node.F5'(A,B,C,D,M), D, 'Tables.Right.C0', O, I),
        'Tables.HeightIncreased.P3'(D, O, K).
'~Tables.Amend.P7'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I, J, K) :-
        'Strings':'~Strings.>.P2'(F, B),
        '~Tables.Amend.P7'(E, F, G, H, L, J, M),
        '~Tables.Adjust.P6'(M, 'Tables.Node.F5'(A,B,C,D,L), D, 'Tables.Right.C0', N, I),
        '~Tables.HeightIncreased.P3'(D, N, K).
'Tables.Amend.P7'('Tables.Node.F5'(A,B,C,D,E), F, G, _, H, I, J) :-
        call_residue(F=B, K),
        (   K=[] ->
            !
        ;   user:release_suspended(K)
        ),
        H='Tables.Node.F5'(A,F,G,D,E),
        I=C,
        J='Tables.No.C0'.
'~Tables.Amend.P7'('Tables.Node.F5'(A,B,C,D,E), F, G, _, H, I, J) :-
        F=B,
        H='Tables.Node.F5'(A,F,G,D,E),
        I=C,
        J='Tables.No.C0'.
'Tables.BalanceTable1.P3'('Tables.EQ.C0', 'Tables.RH.C0', 'Tables.LH.C0').
'~Tables.BalanceTable1.P3'('Tables.EQ.C0', 'Tables.RH.C0', 'Tables.LH.C0').
'Tables.BalanceTable1.P3'('Tables.LH.C0', 'Tables.EQ.C0', 'Tables.EQ.C0').
'~Tables.BalanceTable1.P3'('Tables.LH.C0', 'Tables.EQ.C0', 'Tables.EQ.C0').
'Tables.BalanceTable1.P3'('Tables.RH.C0', 'Tables.EQ.C0', 'Tables.EQ.C0').
'~Tables.BalanceTable1.P3'('Tables.RH.C0', 'Tables.EQ.C0', 'Tables.EQ.C0').
'Tables.BalanceTable2.P3'('Tables.LH.C0', 'Tables.EQ.C0', 'Tables.RH.C0').
'~Tables.BalanceTable2.P3'('Tables.LH.C0', 'Tables.EQ.C0', 'Tables.RH.C0').
'Tables.BalanceTable2.P3'('Tables.RH.C0', 'Tables.LH.C0', 'Tables.EQ.C0').
'~Tables.BalanceTable2.P3'('Tables.RH.C0', 'Tables.LH.C0', 'Tables.EQ.C0').
'Tables.BalanceTable2.P3'('Tables.EQ.C0', 'Tables.EQ.C0', 'Tables.EQ.C0').
'~Tables.BalanceTable2.P3'('Tables.EQ.C0', 'Tables.EQ.C0', 'Tables.EQ.C0').
'Tables.DeleteNode.P4'(A, B, C, D) :-
        user:goedel_freeze(nonvar(A), 'Tables':'Tables.DeleteNode.P4.0'(A,B,C,D)).
'~Tables.DeleteNode.P4'(A, B, C, D) :-
        user:goedel_freeze(nonvar(A), 'Tables':'~Tables.DeleteNode.P4.0'(A,B,C,D)).
'Tables.DeleteNode.P4.0'(A, B, C, D) :-
        'Tables.DeleteKey.P5'(A, B, C, D, _).
'~Tables.DeleteNode.P4.0'(A, B, C, D) :-
        '~Tables.DeleteKey.P5'(A, B, C, D, _).
'Tables.DeleteKey.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings':'Strings.<.P2'(F,B), J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        'Tables.DeleteKey.P5'(A, F, G, K, L),
        'Tables.Adjust.P6'(L, 'Tables.Node.F5'(K,B,C,D,E), D, 'Tables.Right.C0', M, H),
        'Tables.HeightDecreased.P3'(D, M, I).
'~Tables.DeleteKey.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        'Strings':'~Strings.<.P2'(F, B),
        '~Tables.DeleteKey.P5'(A, F, G, J, K),
        '~Tables.Adjust.P6'(K, 'Tables.Node.F5'(J,B,C,D,E), D, 'Tables.Right.C0', L, H),
        '~Tables.HeightDecreased.P3'(D, L, I).
'Tables.DeleteKey.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings':'Strings.>.P2'(F,B), J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        'Tables.DeleteKey.P5'(E, F, G, K, L),
        'Tables.Adjust.P6'(L, 'Tables.Node.F5'(A,B,C,D,K), D, 'Tables.Left.C0', M, H),
        'Tables.HeightDecreased.P3'(D, M, I).
'~Tables.DeleteKey.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        'Strings':'~Strings.>.P2'(F, B),
        '~Tables.DeleteKey.P5'(E, F, G, J, K),
        '~Tables.Adjust.P6'(K, 'Tables.Node.F5'(A,B,C,D,J), D, 'Tables.Left.C0', L, H),
        '~Tables.HeightDecreased.P3'(D, L, I).
'Tables.DeleteKey.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue(F=B, J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        G=C,
        'Tables.Combine.P5'(A, D, E, H, I).
'~Tables.DeleteKey.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        F=B,
        G=C,
        '~Tables.Combine.P5'(A, D, E, H, I).
'Tables.EmptyTable.P1'('Tables.Null.C0').
'~Tables.EmptyTable.P1'('Tables.Null.C0').
'Tables.InsertNode.P4'(A, B, C, D) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'Tables':'Tables.InsertNode.P4.0'(A,B,C,D)).
'~Tables.InsertNode.P4'(A, B, C, D) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'Tables':'~Tables.InsertNode.P4.0'(A,B,C,D)).
'Tables.InsertNode.P4.0'(A, B, C, D) :-
        'Tables.Insert.P5'(A, B, C, D, _).
'~Tables.InsertNode.P4.0'(A, B, C, D) :-
        '~Tables.Insert.P5'(A, B, C, D, _).
'Tables.HeightIncreased.P3'('Tables.RH.C0', 'Tables.EQ.C0', 'Tables.No.C0') :- !.
'~Tables.HeightIncreased.P3'('Tables.RH.C0', 'Tables.EQ.C0', 'Tables.No.C0').
'Tables.HeightIncreased.P3'('Tables.LH.C0', 'Tables.EQ.C0', 'Tables.No.C0') :- !.
'~Tables.HeightIncreased.P3'('Tables.LH.C0', 'Tables.EQ.C0', 'Tables.No.C0').
'Tables.HeightIncreased.P3'('Tables.EQ.C0', 'Tables.LH.C0', 'Tables.Yes.C0') :- !.
'~Tables.HeightIncreased.P3'('Tables.EQ.C0', 'Tables.LH.C0', 'Tables.Yes.C0').
'Tables.HeightIncreased.P3'('Tables.EQ.C0', 'Tables.RH.C0', 'Tables.Yes.C0') :- !.
'~Tables.HeightIncreased.P3'('Tables.EQ.C0', 'Tables.RH.C0', 'Tables.Yes.C0').
'Tables.HeightIncreased.P3'(A, A, 'Tables.No.C0') :- !.
'~Tables.HeightIncreased.P3'(A, A, 'Tables.No.C0').
'Tables.HeightDecreased.P3'('Tables.LH.C0', 'Tables.RH.C0', 'Tables.No.C0') :- !.
'~Tables.HeightDecreased.P3'('Tables.LH.C0', 'Tables.RH.C0', 'Tables.No.C0').
'Tables.HeightDecreased.P3'('Tables.RH.C0', 'Tables.LH.C0', 'Tables.No.C0') :- !.
'~Tables.HeightDecreased.P3'('Tables.RH.C0', 'Tables.LH.C0', 'Tables.No.C0').
'Tables.HeightDecreased.P3'('Tables.LH.C0', 'Tables.EQ.C0', 'Tables.Yes.C0') :- !.
'~Tables.HeightDecreased.P3'('Tables.LH.C0', 'Tables.EQ.C0', 'Tables.Yes.C0').
'Tables.HeightDecreased.P3'('Tables.RH.C0', 'Tables.EQ.C0', 'Tables.Yes.C0') :- !.
'~Tables.HeightDecreased.P3'('Tables.RH.C0', 'Tables.EQ.C0', 'Tables.Yes.C0').
'Tables.HeightDecreased.P3'('Tables.EQ.C0', _, 'Tables.No.C0') :- !.
'~Tables.HeightDecreased.P3'('Tables.EQ.C0', _, 'Tables.No.C0').
'Tables.HeightDecreased.P3'(A, A, 'Tables.No.C0') :- !.
'~Tables.HeightDecreased.P3'(A, A, 'Tables.No.C0').
'Tables.Insert.P5'('Tables.Null.C0', A, B, 'Tables.Node.F5'('Tables.Null.C0',A,B,'Tables.EQ.C0','Tables.Null.C0'), 'Tables.Yes.C0').
'~Tables.Insert.P5'('Tables.Null.C0', A, B, 'Tables.Node.F5'('Tables.Null.C0',A,B,'Tables.EQ.C0','Tables.Null.C0'), 'Tables.Yes.C0').
'Tables.Insert.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings':'Strings.<.P2'(F,B), J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        'Tables.Insert.P5'(A, F, G, K, L),
        'Tables.Adjust.P6'(L, 'Tables.Node.F5'(K,B,C,D,E), D, 'Tables.Left.C0', M, H),
        'Tables.HeightIncreased.P3'(D, M, I).
'~Tables.Insert.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        'Strings':'~Strings.<.P2'(F, B),
        '~Tables.Insert.P5'(A, F, G, J, K),
        '~Tables.Adjust.P6'(K, 'Tables.Node.F5'(J,B,C,D,E), D, 'Tables.Left.C0', L, H),
        '~Tables.HeightIncreased.P3'(D, L, I).
'Tables.Insert.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings':'Strings.>.P2'(F,B), J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        'Tables.Insert.P5'(E, F, G, K, L),
        'Tables.Adjust.P6'(L, 'Tables.Node.F5'(A,B,C,D,K), D, 'Tables.Right.C0', M, H),
        'Tables.HeightIncreased.P3'(D, M, I).
'~Tables.Insert.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        'Strings':'~Strings.>.P2'(F, B),
        '~Tables.Insert.P5'(E, F, G, J, K),
        '~Tables.Adjust.P6'(K, 'Tables.Node.F5'(A,B,C,D,J), D, 'Tables.Right.C0', L, H),
        '~Tables.HeightIncreased.P3'(D, L, I).
'Tables.Insert.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue(F=B, J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        G=C,
        H='Tables.Node.F5'(A,B,C,D,E),
        I='Tables.No.C0'.
'~Tables.Insert.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        F=B,
        G=C,
        H='Tables.Node.F5'(A,B,C,D,E),
        I='Tables.No.C0'.
'Tables.JoinTables.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and nonvar(B), 'Tables':'Tables.JoinTables.P3.0'(A,B,C)).
'~Tables.JoinTables.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and nonvar(B), 'Tables':'~Tables.JoinTables.P3.0'(A,B,C)).
'Tables.JoinTables.P3.0'(A, B, C) :-
        'Tables.JoinAux.P3'(B, A, C).
'~Tables.JoinTables.P3.0'(A, B, C) :-
        '~Tables.JoinAux.P3'(B, A, C).
'Tables.JoinAux.P3'('Tables.Null.C0', A, A).
'~Tables.JoinAux.P3'('Tables.Null.C0', A, A).
'Tables.JoinAux.P3'('Tables.Node.F5'(A,B,C,_,D), E, F) :-
        'Tables.Amend.P7'(E, B, C, _, G, _, _),
        'Tables.JoinAux.P3'(A, G, H),
        'Tables.JoinAux.P3'(D, H, F).
'~Tables.JoinAux.P3'('Tables.Node.F5'(A,B,C,_,D), E, F) :-
        '~Tables.Amend.P7'(E, B, C, _, G, _, _),
        '~Tables.JoinAux.P3'(A, G, H),
        '~Tables.JoinAux.P3'(D, H, F).
'Tables.LastNode.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A), 'Tables':'Tables.LastNode.P3.0'(A,B,C)).
'~Tables.LastNode.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A), 'Tables':'~Tables.LastNode.P3.0'(A,B,C)).
'Tables.LastNode.P3.0'('Tables.Node.F5'(_,A,B,_,'Tables.Null.C0'), A, B) :- !.
'~Tables.LastNode.P3.0'('Tables.Node.F5'(_,A,B,_,'Tables.Null.C0'), A, B).
'Tables.LastNode.P3.0'('Tables.Node.F5'(_,_,_,_,A), B, C) :-
        call_residue(user:not_equal([],[A],A,'Tables.Null.C0'), D),
        (   D=[] ->
            !
        ;   user:release_suspended(D)
        ),
        'Tables.LastNode.P3'(A, B, C).
'~Tables.LastNode.P3.0'('Tables.Node.F5'(_,_,_,_,A), B, C) :-
        user:not_equal([], [A], A, 'Tables.Null.C0'),
        '~Tables.LastNode.P3'(A, B, C).
'Tables.Restructure.P3'('Tables.Node.F5'(A,B,C,'Tables.RH.C0','Tables.Node.F5'(D,E,F,G,H)), I, J) :-
        call_residue(user:not_equal([],[G],G,'Tables.LH.C0'), K),
        (   K=[] ->
            !
        ;   user:release_suspended(K)
        ),
        J='Tables.Node.F5'('Tables.Node.F5'(A,B,C,L,D),E,F,I,H),
        'Tables.BalanceTable1.P3'(G, L, I).
'~Tables.Restructure.P3'('Tables.Node.F5'(A,B,C,'Tables.RH.C0','Tables.Node.F5'(D,E,F,G,H)), I, J) :-
        user:not_equal([], [G], G, 'Tables.LH.C0'),
        J='Tables.Node.F5'('Tables.Node.F5'(A,B,C,K,D),E,F,I,H),
        '~Tables.BalanceTable1.P3'(G, K, I).
'Tables.Restructure.P3'('Tables.Node.F5'('Tables.Node.F5'(A,B,C,D,E),F,G,'Tables.LH.C0',H), I, J) :-
        call_residue(user:not_equal([],[D],D,'Tables.RH.C0'), K),
        (   K=[] ->
            !
        ;   user:release_suspended(K)
        ),
        J='Tables.Node.F5'(A,B,C,I,'Tables.Node.F5'(E,F,G,L,H)),
        'Tables.BalanceTable1.P3'(D, I, L).
'~Tables.Restructure.P3'('Tables.Node.F5'('Tables.Node.F5'(A,B,C,D,E),F,G,'Tables.LH.C0',H), I, J) :-
        user:not_equal([], [D], D, 'Tables.RH.C0'),
        J='Tables.Node.F5'(A,B,C,I,'Tables.Node.F5'(E,F,G,K,H)),
        '~Tables.BalanceTable1.P3'(D, I, K).
'Tables.Restructure.P3'('Tables.Node.F5'(A,B,C,'Tables.RH.C0','Tables.Node.F5'('Tables.Node.F5'(D,E,F,G,H),I,J,'Tables.LH.C0',K)), 'Tables.EQ.C0', 'Tables.Node.F5'('Tables.Node.F5'(A,B,C,L,D),E,F,'Tables.EQ.C0','Tables.Node.F5'(H,I,J,M,K))) :- !,
        'Tables.BalanceTable2.P3'(G, L, M).
'~Tables.Restructure.P3'('Tables.Node.F5'(A,B,C,'Tables.RH.C0','Tables.Node.F5'('Tables.Node.F5'(D,E,F,G,H),I,J,'Tables.LH.C0',K)), 'Tables.EQ.C0', 'Tables.Node.F5'('Tables.Node.F5'(A,B,C,L,D),E,F,'Tables.EQ.C0','Tables.Node.F5'(H,I,J,M,K))) :-
        true,
        '~Tables.BalanceTable2.P3'(G, L, M).
'Tables.Restructure.P3'('Tables.Node.F5'('Tables.Node.F5'(A,B,C,'Tables.RH.C0','Tables.Node.F5'(D,E,F,G,H)),I,J,'Tables.LH.C0',K), 'Tables.EQ.C0', 'Tables.Node.F5'('Tables.Node.F5'(A,B,C,L,D),E,F,'Tables.EQ.C0','Tables.Node.F5'(H,I,J,M,K))) :- !,
        'Tables.BalanceTable2.P3'(G, L, M).
'~Tables.Restructure.P3'('Tables.Node.F5'('Tables.Node.F5'(A,B,C,'Tables.RH.C0','Tables.Node.F5'(D,E,F,G,H)),I,J,'Tables.LH.C0',K), 'Tables.EQ.C0', 'Tables.Node.F5'('Tables.Node.F5'(A,B,C,L,D),E,F,'Tables.EQ.C0','Tables.Node.F5'(H,I,J,M,K))) :-
        true,
        '~Tables.BalanceTable2.P3'(G, L, M).
'Tables.PreviousNode.P4'(A, B, C, D) :-
        user:goedel_freeze(nonvar(A)and nonvar(B), 'Tables':'Tables.PreviousNode.P4.0'(A,B,C,D)).
'~Tables.PreviousNode.P4'(A, B, C, D) :-
        user:goedel_freeze(nonvar(A)and nonvar(B), 'Tables':'~Tables.PreviousNode.P4.0'(A,B,C,D)).
'Tables.PreviousNode.P4.0'('Tables.Node.F5'(A,B,_,_,_), C, D, E) :-
        call_residue(C=B, F),
        (   F=[] ->
            !
        ;   user:release_suspended(F)
        ),
        'Tables.LastNode.P3'(A, D, E).
'~Tables.PreviousNode.P4.0'('Tables.Node.F5'(A,B,_,_,_), C, D, E) :-
        C=B,
        '~Tables.LastNode.P3'(A, D, E).
'Tables.PreviousNode.P4.0'('Tables.Node.F5'(A,B,_,_,_), C, D, E) :-
        call_residue('Strings':'Strings.<.P2'(C,B), F),
        (   F=[] ->
            !
        ;   user:release_suspended(F)
        ),
        'Tables.PreviousNode.P4'(A, C, D, E).
'~Tables.PreviousNode.P4.0'('Tables.Node.F5'(A,B,_,_,_), C, D, E) :-
        'Strings':'~Strings.<.P2'(C, B),
        '~Tables.PreviousNode.P4'(A, C, D, E).
'Tables.PreviousNode.P4.0'('Tables.Node.F5'(_,A,B,_,C), D, E, F) :-
        call_residue('Strings':'Strings.>.P2'(D,A), G),
        (   G=[] ->
            !
        ;   user:release_suspended(G)
        ),
        user:goedel_freeze(ground([C,D]), if(('Tables':'~Tables.PreviousNode.P4'(C,D,H,I),true),(E=H,F=I),('Tables':'Tables.FirstNode.P3'(C,D,_),E=A,F=B))).
'~Tables.PreviousNode.P4.0'('Tables.Node.F5'(_,A,B,_,C), D, E, F) :-
        'Strings':'~Strings.>.P2'(D, A),
        user:goedel_freeze(ground([C,D]), if(('Tables':'~Tables.PreviousNode.P4'(C,D,G,H),true),(E=G,F=H),('Tables':'~Tables.FirstNode.P3'(C,D,_),E=A,F=B))).
'Tables.NextNode.P4'(A, B, C, D) :-
        user:goedel_freeze(nonvar(A)and nonvar(B), 'Tables':'Tables.NextNode.P4.0'(A,B,C,D)).
'~Tables.NextNode.P4'(A, B, C, D) :-
        user:goedel_freeze(nonvar(A)and nonvar(B), 'Tables':'~Tables.NextNode.P4.0'(A,B,C,D)).
'Tables.NextNode.P4.0'('Tables.Node.F5'(_,A,_,_,B), C, D, E) :-
        call_residue(C=A, F),
        (   F=[] ->
            !
        ;   user:release_suspended(F)
        ),
        'Tables.FirstNode.P3'(B, D, E).
'~Tables.NextNode.P4.0'('Tables.Node.F5'(_,A,_,_,B), C, D, E) :-
        C=A,
        '~Tables.FirstNode.P3'(B, D, E).
'Tables.NextNode.P4.0'('Tables.Node.F5'(A,B,C,_,_), D, E, F) :-
        call_residue('Strings':'Strings.<.P2'(D,B), G),
        (   G=[] ->
            !
        ;   user:release_suspended(G)
        ),
        user:goedel_freeze(ground([A,D]), if(('Tables':'~Tables.NextNode.P4'(A,D,H,I),true),(E=H,F=I),('Tables':'Tables.LastNode.P3'(A,D,_),E=B,F=C))).
'~Tables.NextNode.P4.0'('Tables.Node.F5'(A,B,C,_,_), D, E, F) :-
        'Strings':'~Strings.<.P2'(D, B),
        user:goedel_freeze(ground([A,D]), if(('Tables':'~Tables.NextNode.P4'(A,D,G,H),true),(E=G,F=H),('Tables':'~Tables.LastNode.P3'(A,D,_),E=B,F=C))).
'Tables.NextNode.P4.0'('Tables.Node.F5'(_,A,_,_,B), C, D, E) :-
        call_residue('Strings':'Strings.>.P2'(C,A), F),
        (   F=[] ->
            !
        ;   user:release_suspended(F)
        ),
        'Tables.NextNode.P4'(B, C, D, E).
'~Tables.NextNode.P4.0'('Tables.Node.F5'(_,A,_,_,B), C, D, E) :-
        'Strings':'~Strings.>.P2'(C, A),
        '~Tables.NextNode.P4'(B, C, D, E).
'Tables.ListsToTable.P3'([], [], 'Tables.Null.C0').
'~Tables.ListsToTable.P3'([], [], 'Tables.Null.C0').
'Tables.ListsToTable.P3'([A|B], C, D) :-
        'Tables.ListsToTable1.P5'(B, A, C, 'Tables.Null.C0', D).
'~Tables.ListsToTable.P3'([A|B], C, D) :-
        '~Tables.ListsToTable1.P5'(B, A, C, 'Tables.Null.C0', D).
'Tables.ListsToTable1.P5'([], A, [B], C, D) :-
        'Tables.InsertNode.P4'(C, A, B, D).
'~Tables.ListsToTable1.P5'([], A, [B], C, D) :-
        '~Tables.InsertNode.P4'(C, A, B, D).
'Tables.ListsToTable1.P5'([A|B], C, [D|E], F, G) :-
        'Strings':'Strings.<.P2'(C, A),
        'Tables.InsertNode.P4'(F, C, D, H),
        'Tables.ListsToTable1.P5'(B, A, E, H, G).
'~Tables.ListsToTable1.P5'([A|B], C, [D|E], F, G) :-
        'Strings':'~Strings.<.P2'(C, A),
        '~Tables.InsertNode.P4'(F, C, D, H),
        '~Tables.ListsToTable1.P5'(B, A, E, H, G).
'Tables.NodeInTable.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A), 'Tables':'Tables.NodeInTable.P3.0'(A,B,C)).
'~Tables.NodeInTable.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A), 'Tables':'~Tables.NodeInTable.P3.0'(A,B,C)).
'Tables.NodeInTable.P3.0'(A, B, C) :-
        'Tables.NodeInTableAux.P3'(A, B, C).
'~Tables.NodeInTable.P3.0'(A, B, C) :-
        '~Tables.NodeInTableAux.P3'(A, B, C).
'Tables.RemoveRightmost.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue(user:not_equal([],[E],E,'Tables.Null.C0'), J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        'Tables.RemoveRightmost.P5'(E, F, G, K, L),
        'Tables.Adjust.P6'(L, 'Tables.Node.F5'(A,B,C,D,K), D, 'Tables.Left.C0', M, H),
        'Tables.HeightDecreased.P3'(D, M, I).
'~Tables.RemoveRightmost.P5'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        user:not_equal([], [E], E, 'Tables.Null.C0'),
        '~Tables.RemoveRightmost.P5'(E, F, G, J, K),
        '~Tables.Adjust.P6'(K, 'Tables.Node.F5'(A,B,C,D,J), D, 'Tables.Left.C0', L, H),
        '~Tables.HeightDecreased.P3'(D, L, I).
'Tables.RemoveRightmost.P5'('Tables.Node.F5'(A,B,C,_,'Tables.Null.C0'), B, C, A, 'Tables.Yes.C0') :- !.
'~Tables.RemoveRightmost.P5'('Tables.Node.F5'(A,B,C,_,'Tables.Null.C0'), B, C, A, 'Tables.Yes.C0').
'Tables.Rebalance.P4'('Tables.No.C0', 'Tables.Node.F5'(A,B,C,_,D), E, 'Tables.Node.F5'(A,B,C,E,D)).
'~Tables.Rebalance.P4'('Tables.No.C0', 'Tables.Node.F5'(A,B,C,_,D), E, 'Tables.Node.F5'(A,B,C,E,D)).
'Tables.Rebalance.P4'('Tables.Yes.C0', A, B, C) :-
        'Tables.Restructure.P3'(A, B, C).
'~Tables.Rebalance.P4'('Tables.Yes.C0', A, B, C) :-
        '~Tables.Restructure.P3'(A, B, C).
'Tables.TableSearch.P3'('Tables.Node.F5'(_,A,B,_,_), C, D) :-
        call_residue(C=A, E),
        (   E=[] ->
            !
        ;   user:release_suspended(E)
        ),
        D=B.
'~Tables.TableSearch.P3'('Tables.Node.F5'(_,A,B,_,_), C, D) :-
        C=A,
        D=B.
'Tables.TableSearch.P3'('Tables.Node.F5'(A,B,_,_,_), C, D) :-
        call_residue('Strings':'Strings.<.P2'(C,B), E),
        (   E=[] ->
            !
        ;   user:release_suspended(E)
        ),
        'Tables.TableSearch.P3'(A, C, D).
'~Tables.TableSearch.P3'('Tables.Node.F5'(A,B,_,_,_), C, D) :-
        'Strings':'~Strings.<.P2'(C, B),
        '~Tables.TableSearch.P3'(A, C, D).
'Tables.TableSearch.P3'('Tables.Node.F5'(_,A,_,_,B), C, D) :-
        call_residue('Strings':'Strings.>.P2'(C,A), E),
        (   E=[] ->
            !
        ;   user:release_suspended(E)
        ),
        'Tables.TableSearch.P3'(B, C, D).
'~Tables.TableSearch.P3'('Tables.Node.F5'(_,A,_,_,B), C, D) :-
        'Strings':'~Strings.>.P2'(C, A),
        '~Tables.TableSearch.P3'(B, C, D).
'Tables.TableMember.P3'('Tables.Node.F5'(_,A,B,_,_), A, B).
'~Tables.TableMember.P3'('Tables.Node.F5'(_,A,B,_,_), A, B).
'Tables.TableMember.P3'('Tables.Node.F5'(A,_,_,_,_), B, C) :-
        'Tables.TableMember.P3'(A, B, C).
'~Tables.TableMember.P3'('Tables.Node.F5'(A,_,_,_,_), B, C) :-
        '~Tables.TableMember.P3'(A, B, C).
'Tables.TableMember.P3'('Tables.Node.F5'(_,_,_,_,A), B, C) :-
        'Tables.TableMember.P3'(A, B, C).
'~Tables.TableMember.P3'('Tables.Node.F5'(_,_,_,_,A), B, C) :-
        '~Tables.TableMember.P3'(A, B, C).
'Tables.UpdateTable.P5'(A, B, C, D, E) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'Tables':'Tables.UpdateTable.P5.0'(A,B,C,D,E)).
'~Tables.UpdateTable.P5'(A, B, C, D, E) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'Tables':'~Tables.UpdateTable.P5.0'(A,B,C,D,E)).
'Tables.UpdateTable.P5.0'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue(F=B, J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        H='Tables.Node.F5'(A,B,G,D,E),
        I=C.
'~Tables.UpdateTable.P5.0'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        F=B,
        H='Tables.Node.F5'(A,B,G,D,E),
        I=C.
'Tables.UpdateTable.P5.0'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings':'Strings.<.P2'(F,B), J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        'Tables.UpdateTable.P5'(A, F, G, K, I),
        H='Tables.Node.F5'(K,B,C,D,E).
'~Tables.UpdateTable.P5.0'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        'Strings':'~Strings.<.P2'(F, B),
        '~Tables.UpdateTable.P5'(A, F, G, J, I),
        H='Tables.Node.F5'(J,B,C,D,E).
'Tables.UpdateTable.P5.0'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings':'Strings.>.P2'(F,B), J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        'Tables.UpdateTable.P5'(E, F, G, K, I),
        H='Tables.Node.F5'(A,B,C,D,K).
'~Tables.UpdateTable.P5.0'('Tables.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        'Strings':'~Strings.>.P2'(F, B),
        '~Tables.UpdateTable.P5'(E, F, G, J, I),
        H='Tables.Node.F5'(A,B,C,D,J).
'Tables.TableToLists.P5'('Tables.Null.C0', A, A, B, B).
'~Tables.TableToLists.P5'('Tables.Null.C0', A, A, B, B).
'Tables.TableToLists.P5'('Tables.Node.F5'(A,B,C,_,D), E, F, G, H) :-
        'Tables.TableToLists.P5'(A, E, [B|I], G, [C|J]),
        'Tables.TableToLists.P5'(D, I, F, J, H).
'~Tables.TableToLists.P5'('Tables.Node.F5'(A,B,C,_,D), E, F, G, H) :-
        '~Tables.TableToLists.P5'(A, E, [B|I], G, [C|J]),
        '~Tables.TableToLists.P5'(D, I, F, J, H).
%------------------------------------------------------------------------------
% Supplementary routines for Tables.
%------------------------------------------------------------------------------

% %Z%%M% %I% last updated %E% %U% by %Q%
 
'Tables.NodeInTableAux.P3'(Tree, Key, Item) :-
   ( var(Key) ->
     'Tables.TableMember.P3'(Tree, Key, Item)
   ; 'Tables.TableSearch.P3'(Tree, Key, Item)
   ).

'~Tables.NodeInTableAux.P3'(Tree, Key, Item) :-
   'Tables.NodeInTableAux.P3'(Tree, Key, Item).


'Tables.ListTableAux.P3'(Tree, Keys, Items) :-
   ( var(Tree) ->
     'Tables.ListsToTable.P3'(Keys, Items, Tree)
   ; 'Tables.TableToLists.P5'(Tree, Keys, [], Items, [])
   ).

'~Tables.ListTableAux.P3'(Tree, Keys, Items) :-
   'Tables.ListTableAux.P3'(Tree, Keys, Items).
