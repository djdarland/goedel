:- module('AVLTrees', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

'AVLTrees.Adjustment.P4'('AVLTrees.EQ.C0', 'AVLTrees.Left.C0', 'AVLTrees.LH.C0', 'AVLTrees.No.C0') :- !.
'~AVLTrees.Adjustment.P4'('AVLTrees.EQ.C0', 'AVLTrees.Left.C0', 'AVLTrees.LH.C0', 'AVLTrees.No.C0').
'AVLTrees.Adjustment.P4'('AVLTrees.EQ.C0', 'AVLTrees.Right.C0', 'AVLTrees.RH.C0', 'AVLTrees.No.C0') :- !.
'~AVLTrees.Adjustment.P4'('AVLTrees.EQ.C0', 'AVLTrees.Right.C0', 'AVLTrees.RH.C0', 'AVLTrees.No.C0').
'AVLTrees.Adjustment.P4'('AVLTrees.LH.C0', 'AVLTrees.Left.C0', _, 'AVLTrees.Yes.C0') :- !.
'~AVLTrees.Adjustment.P4'('AVLTrees.LH.C0', 'AVLTrees.Left.C0', _, 'AVLTrees.Yes.C0').
'AVLTrees.Adjustment.P4'('AVLTrees.LH.C0', 'AVLTrees.Right.C0', 'AVLTrees.EQ.C0', 'AVLTrees.No.C0') :- !.
'~AVLTrees.Adjustment.P4'('AVLTrees.LH.C0', 'AVLTrees.Right.C0', 'AVLTrees.EQ.C0', 'AVLTrees.No.C0').
'AVLTrees.Adjustment.P4'('AVLTrees.RH.C0', 'AVLTrees.Left.C0', 'AVLTrees.EQ.C0', 'AVLTrees.No.C0') :- !.
'~AVLTrees.Adjustment.P4'('AVLTrees.RH.C0', 'AVLTrees.Left.C0', 'AVLTrees.EQ.C0', 'AVLTrees.No.C0').
'AVLTrees.Adjustment.P4'('AVLTrees.RH.C0', 'AVLTrees.Right.C0', _, 'AVLTrees.Yes.C0') :- !.
'~AVLTrees.Adjustment.P4'('AVLTrees.RH.C0', 'AVLTrees.Right.C0', _, 'AVLTrees.Yes.C0').
'AVLTrees.AVLJoin.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and nonvar(B), 'AVLTrees':'AVLTrees.AVLJoin.P3.0'(A,B,C)).
'~AVLTrees.AVLJoin.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and nonvar(B), 'AVLTrees':'~AVLTrees.AVLJoin.P3.0'(A,B,C)).
'AVLTrees.AVLJoin.P3.0'(A, B, C) :-
        'AVLTrees.AVLJoinAux.P3'(B, A, C).
'~AVLTrees.AVLJoin.P3.0'(A, B, C) :-
        '~AVLTrees.AVLJoinAux.P3'(B, A, C).
'AVLTrees.AVLInsert.P4'(A, B, C, D) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'AVLTrees':'AVLTrees.AVLInsert.P4.0'(A,B,C,D)).
'~AVLTrees.AVLInsert.P4'(A, B, C, D) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'AVLTrees':'~AVLTrees.AVLInsert.P4.0'(A,B,C,D)).
'AVLTrees.AVLInsert.P4.0'(A, B, C, D) :-
        'AVLTrees.Insert.P5'(A, B, C, D, _).
'~AVLTrees.AVLInsert.P4.0'(A, B, C, D) :-
        '~AVLTrees.Insert.P5'(A, B, C, D, _).
'AVLTrees.AVLAmend.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'AVLTrees':'AVLTrees.AVLAmend.P6.0'(A,B,C,D,E,F)).
'~AVLTrees.AVLAmend.P6'(A, B, C, D, E, F) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'AVLTrees':'~AVLTrees.AVLAmend.P6.0'(A,B,C,D,E,F)).
'AVLTrees.AVLAmend.P6.0'(A, B, C, D, E, F) :-
        'AVLTrees.Amend.P7'(A, B, C, D, E, F, _).
'~AVLTrees.AVLAmend.P6.0'(A, B, C, D, E, F) :-
        '~AVLTrees.Amend.P7'(A, B, C, D, E, F, _).
'AVLTrees.AVLDelete.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'AVLTrees':'AVLTrees.AVLDelete.P3.0'(A,B,C)).
'~AVLTrees.AVLDelete.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'AVLTrees':'~AVLTrees.AVLDelete.P3.0'(A,B,C)).
'AVLTrees.AVLDelete.P3.0'(A, B, C) :-
        'AVLTrees.DeleteKey.P4'(A, B, C, _).
'~AVLTrees.AVLDelete.P3.0'(A, B, C) :-
        '~AVLTrees.DeleteKey.P4'(A, B, C, _).
'AVLTrees.AVLIsEmpty.P1'('AVLTrees.Null.C0').
'~AVLTrees.AVLIsEmpty.P1'('AVLTrees.Null.C0').
'AVLTrees.AVLSearch.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'AVLTrees':'AVLTrees.AVLSearch.P3.0'(A,B,C)).
'~AVLTrees.AVLSearch.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'AVLTrees':'~AVLTrees.AVLSearch.P3.0'(A,B,C)).
'AVLTrees.AVLSearch.P3.0'('AVLTrees.Node.F5'(_,A,B,_,_), C, D) :-
        call_residue(C=A, E),
        (   E=[] ->
            !
        ;   user:release_suspended(E)
        ),
        D=B.
'~AVLTrees.AVLSearch.P3.0'('AVLTrees.Node.F5'(_,A,B,_,_), C, D) :-
        C=A,
        D=B.
'AVLTrees.AVLSearch.P3.0'('AVLTrees.Node.F5'(A,B,_,_,_), C, D) :-
        call_residue('Strings':'Strings.<.P2'(C,B), E),
        (   E=[] ->
            !
        ;   user:release_suspended(E)
        ),
        'AVLTrees.AVLSearch.P3'(A, C, D).
'~AVLTrees.AVLSearch.P3.0'('AVLTrees.Node.F5'(A,B,_,_,_), C, D) :-
        'Strings':'~Strings.<.P2'(C, B),
        '~AVLTrees.AVLSearch.P3'(A, C, D).
'AVLTrees.AVLSearch.P3.0'('AVLTrees.Node.F5'(_,A,_,_,B), C, D) :-
        call_residue('Strings':'Strings.>.P2'(C,A), E),
        (   E=[] ->
            !
        ;   user:release_suspended(E)
        ),
        'AVLTrees.AVLSearch.P3'(B, C, D).
'~AVLTrees.AVLSearch.P3.0'('AVLTrees.Node.F5'(_,A,_,_,B), C, D) :-
        'Strings':'~Strings.>.P2'(C, A),
        '~AVLTrees.AVLSearch.P3'(B, C, D).
'AVLTrees.AVLMember.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A), 'AVLTrees':'AVLTrees.AVLMember.P3.0'(A,B,C)).
'~AVLTrees.AVLMember.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A), 'AVLTrees':'~AVLTrees.AVLMember.P3.0'(A,B,C)).
'AVLTrees.AVLMember.P3.0'('AVLTrees.Node.F5'(_,A,B,_,_), A, B).
'~AVLTrees.AVLMember.P3.0'('AVLTrees.Node.F5'(_,A,B,_,_), A, B).
'AVLTrees.AVLMember.P3.0'('AVLTrees.Node.F5'(A,_,_,_,_), B, C) :-
        'AVLTrees.AVLMember.P3'(A, B, C).
'~AVLTrees.AVLMember.P3.0'('AVLTrees.Node.F5'(A,_,_,_,_), B, C) :-
        '~AVLTrees.AVLMember.P3'(A, B, C).
'AVLTrees.AVLMember.P3.0'('AVLTrees.Node.F5'(_,_,_,_,A), B, C) :-
        'AVLTrees.AVLMember.P3'(A, B, C).
'~AVLTrees.AVLMember.P3.0'('AVLTrees.Node.F5'(_,_,_,_,A), B, C) :-
        '~AVLTrees.AVLMember.P3'(A, B, C).
'AVLTrees.AVLJoinAux.P3'('AVLTrees.Null.C0', A, A).
'~AVLTrees.AVLJoinAux.P3'('AVLTrees.Null.C0', A, A).
'AVLTrees.AVLJoinAux.P3'('AVLTrees.Node.F5'(A,B,C,_,D), E, F) :-
        'AVLTrees.Amend.P7'(E, B, C, _, G, _, _),
        'AVLTrees.AVLJoinAux.P3'(A, G, H),
        'AVLTrees.AVLJoinAux.P3'(D, H, F).
'~AVLTrees.AVLJoinAux.P3'('AVLTrees.Node.F5'(A,B,C,_,D), E, F) :-
        '~AVLTrees.Amend.P7'(E, B, C, _, G, _, _),
        '~AVLTrees.AVLJoinAux.P3'(A, G, H),
        '~AVLTrees.AVLJoinAux.P3'(D, H, F).
'AVLTrees.AVLUpdate.P5'(A, B, C, D, E) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'AVLTrees':'AVLTrees.AVLUpdate.P5.0'(A,B,C,D,E)).
'~AVLTrees.AVLUpdate.P5'(A, B, C, D, E) :-
        user:goedel_freeze(nonvar(A)and ground([B]), 'AVLTrees':'~AVLTrees.AVLUpdate.P5.0'(A,B,C,D,E)).
'AVLTrees.AVLUpdate.P5.0'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue(F=B, J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        H='AVLTrees.Node.F5'(A,B,G,D,E),
        I=C.
'~AVLTrees.AVLUpdate.P5.0'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        F=B,
        H='AVLTrees.Node.F5'(A,B,G,D,E),
        I=C.
'AVLTrees.AVLUpdate.P5.0'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings':'Strings.<.P2'(F,B), J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        'AVLTrees.AVLUpdate.P5'(A, F, G, K, I),
        H='AVLTrees.Node.F5'(K,B,C,D,E).
'~AVLTrees.AVLUpdate.P5.0'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        'Strings':'~Strings.<.P2'(F, B),
        '~AVLTrees.AVLUpdate.P5'(A, F, G, J, I),
        H='AVLTrees.Node.F5'(J,B,C,D,E).
'AVLTrees.AVLUpdate.P5.0'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings':'Strings.>.P2'(F,B), J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        'AVLTrees.AVLUpdate.P5'(E, F, G, K, I),
        H='AVLTrees.Node.F5'(A,B,C,D,K).
'~AVLTrees.AVLUpdate.P5.0'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        'Strings':'~Strings.>.P2'(F, B),
        '~AVLTrees.AVLUpdate.P5'(E, F, G, J, I),
        H='AVLTrees.Node.F5'(A,B,C,D,J).
'AVLTrees.AVLToBinary.P2'(A, B) :-
        user:goedel_freeze(nonvar(A), 'AVLTrees':'AVLTrees.AVLToBinary.P2.0'(A,B)).
'~AVLTrees.AVLToBinary.P2'(A, B) :-
        user:goedel_freeze(nonvar(A), 'AVLTrees':'~AVLTrees.AVLToBinary.P2.0'(A,B)).
'AVLTrees.AVLToBinary.P2.0'('AVLTrees.Null.C0', 'AVLTrees.Empty.C0').
'~AVLTrees.AVLToBinary.P2.0'('AVLTrees.Null.C0', 'AVLTrees.Empty.C0').
'AVLTrees.AVLToBinary.P2.0'('AVLTrees.Node.F5'(A,B,C,_,D), 'AVLTrees.Tree.F4'(E,B,C,F)) :-
        'AVLTrees.AVLToBinary.P2'(A, E),
        'AVLTrees.AVLToBinary.P2'(D, F).
'~AVLTrees.AVLToBinary.P2.0'('AVLTrees.Node.F5'(A,B,C,_,D), 'AVLTrees.Tree.F4'(E,B,C,F)) :-
        '~AVLTrees.AVLToBinary.P2'(A, E),
        '~AVLTrees.AVLToBinary.P2'(D, F).
'AVLTrees.Adjust.P6'('AVLTrees.No.C0', A, B, _, B, A).
'~AVLTrees.Adjust.P6'('AVLTrees.No.C0', A, B, _, B, A).
'AVLTrees.Adjust.P6'('AVLTrees.Yes.C0', A, B, C, D, E) :-
        'AVLTrees.Adjustment.P4'(B, C, D, F),
        'AVLTrees.Rebalance.P4'(F, A, D, E).
'~AVLTrees.Adjust.P6'('AVLTrees.Yes.C0', A, B, C, D, E) :-
        '~AVLTrees.Adjustment.P4'(B, C, D, F),
        '~AVLTrees.Rebalance.P4'(F, A, D, E).
'AVLTrees.DeleteKey.P4'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H) :-
        call_residue('Strings':'Strings.<.P2'(F,B), I),
        (   I=[] ->
            !
        ;   user:release_suspended(I)
        ),
        'AVLTrees.DeleteKey.P4'(A, F, J, K),
        'AVLTrees.Adjust.P6'(K, 'AVLTrees.Node.F5'(J,B,C,D,E), D, 'AVLTrees.Right.C0', L, G),
        'AVLTrees.HeightDecreased.P3'(D, L, H).
'~AVLTrees.DeleteKey.P4'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H) :-
        'Strings':'~Strings.<.P2'(F, B),
        '~AVLTrees.DeleteKey.P4'(A, F, I, J),
        '~AVLTrees.Adjust.P6'(J, 'AVLTrees.Node.F5'(I,B,C,D,E), D, 'AVLTrees.Right.C0', K, G),
        '~AVLTrees.HeightDecreased.P3'(D, K, H).
'AVLTrees.DeleteKey.P4'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H) :-
        call_residue('Strings':'Strings.>.P2'(F,B), I),
        (   I=[] ->
            !
        ;   user:release_suspended(I)
        ),
        'AVLTrees.DeleteKey.P4'(E, F, J, K),
        'AVLTrees.Adjust.P6'(K, 'AVLTrees.Node.F5'(A,B,C,D,J), D, 'AVLTrees.Left.C0', L, G),
        'AVLTrees.HeightDecreased.P3'(D, L, H).
'~AVLTrees.DeleteKey.P4'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H) :-
        'Strings':'~Strings.>.P2'(F, B),
        '~AVLTrees.DeleteKey.P4'(E, F, I, J),
        '~AVLTrees.Adjust.P6'(J, 'AVLTrees.Node.F5'(A,B,C,D,I), D, 'AVLTrees.Left.C0', K, G),
        '~AVLTrees.HeightDecreased.P3'(D, K, H).
'AVLTrees.DeleteKey.P4'('AVLTrees.Node.F5'(A,B,_,C,D), E, F, G) :-
        call_residue(E=B, H),
        (   H=[] ->
            !
        ;   user:release_suspended(H)
        ),
        'AVLTrees.Combine.P5'(A, C, D, F, G).
'~AVLTrees.DeleteKey.P4'('AVLTrees.Node.F5'(A,B,_,C,D), E, F, G) :-
        E=B,
        '~AVLTrees.Combine.P5'(A, C, D, F, G).
'AVLTrees.BalanceTable2.P3'('AVLTrees.LH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.RH.C0').
'~AVLTrees.BalanceTable2.P3'('AVLTrees.LH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.RH.C0').
'AVLTrees.BalanceTable2.P3'('AVLTrees.RH.C0', 'AVLTrees.LH.C0', 'AVLTrees.EQ.C0').
'~AVLTrees.BalanceTable2.P3'('AVLTrees.RH.C0', 'AVLTrees.LH.C0', 'AVLTrees.EQ.C0').
'AVLTrees.BalanceTable2.P3'('AVLTrees.EQ.C0', 'AVLTrees.EQ.C0', 'AVLTrees.EQ.C0').
'~AVLTrees.BalanceTable2.P3'('AVLTrees.EQ.C0', 'AVLTrees.EQ.C0', 'AVLTrees.EQ.C0').
'AVLTrees.BalanceTable1.P3'('AVLTrees.EQ.C0', 'AVLTrees.RH.C0', 'AVLTrees.LH.C0').
'~AVLTrees.BalanceTable1.P3'('AVLTrees.EQ.C0', 'AVLTrees.RH.C0', 'AVLTrees.LH.C0').
'AVLTrees.BalanceTable1.P3'('AVLTrees.LH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.EQ.C0').
'~AVLTrees.BalanceTable1.P3'('AVLTrees.LH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.EQ.C0').
'AVLTrees.BalanceTable1.P3'('AVLTrees.RH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.EQ.C0').
'~AVLTrees.BalanceTable1.P3'('AVLTrees.RH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.EQ.C0').
'AVLTrees.Amend.P7'('AVLTrees.Null.C0', A, B, C, 'AVLTrees.Node.F5'('AVLTrees.Null.C0',A,B,'AVLTrees.EQ.C0','AVLTrees.Null.C0'), C, 'AVLTrees.Yes.C0').
'~AVLTrees.Amend.P7'('AVLTrees.Null.C0', A, B, C, 'AVLTrees.Node.F5'('AVLTrees.Null.C0',A,B,'AVLTrees.EQ.C0','AVLTrees.Null.C0'), C, 'AVLTrees.Yes.C0').
'AVLTrees.Amend.P7'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I, J, K) :-
        call_residue('Strings':'Strings.<.P2'(F,B), L),
        (   L=[] ->
            !
        ;   user:release_suspended(L)
        ),
        'AVLTrees.Amend.P7'(A, F, G, H, M, J, N),
        'AVLTrees.Adjust.P6'(N, 'AVLTrees.Node.F5'(M,B,C,D,E), D, 'AVLTrees.Left.C0', O, I),
        'AVLTrees.HeightIncreased.P3'(D, O, K).
'~AVLTrees.Amend.P7'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I, J, K) :-
        'Strings':'~Strings.<.P2'(F, B),
        '~AVLTrees.Amend.P7'(A, F, G, H, L, J, M),
        '~AVLTrees.Adjust.P6'(M, 'AVLTrees.Node.F5'(L,B,C,D,E), D, 'AVLTrees.Left.C0', N, I),
        '~AVLTrees.HeightIncreased.P3'(D, N, K).
'AVLTrees.Amend.P7'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I, J, K) :-
        call_residue('Strings':'Strings.>.P2'(F,B), L),
        (   L=[] ->
            !
        ;   user:release_suspended(L)
        ),
        'AVLTrees.Amend.P7'(E, F, G, H, M, J, N),
        'AVLTrees.Adjust.P6'(N, 'AVLTrees.Node.F5'(A,B,C,D,M), D, 'AVLTrees.Right.C0', O, I),
        'AVLTrees.HeightIncreased.P3'(D, O, K).
'~AVLTrees.Amend.P7'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I, J, K) :-
        'Strings':'~Strings.>.P2'(F, B),
        '~AVLTrees.Amend.P7'(E, F, G, H, L, J, M),
        '~AVLTrees.Adjust.P6'(M, 'AVLTrees.Node.F5'(A,B,C,D,L), D, 'AVLTrees.Right.C0', N, I),
        '~AVLTrees.HeightIncreased.P3'(D, N, K).
'AVLTrees.Amend.P7'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, _, H, I, J) :-
        call_residue(F=B, K),
        (   K=[] ->
            !
        ;   user:release_suspended(K)
        ),
        H='AVLTrees.Node.F5'(A,F,G,D,E),
        I=C,
        J='AVLTrees.No.C0'.
'~AVLTrees.Amend.P7'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, _, H, I, J) :-
        F=B,
        H='AVLTrees.Node.F5'(A,F,G,D,E),
        I=C,
        J='AVLTrees.No.C0'.
'AVLTrees.Combine.P5'('AVLTrees.Null.C0', _, A, A, 'AVLTrees.Yes.C0') :- !.
'~AVLTrees.Combine.P5'('AVLTrees.Null.C0', _, A, A, 'AVLTrees.Yes.C0').
'AVLTrees.Combine.P5'(A, _, 'AVLTrees.Null.C0', A, 'AVLTrees.Yes.C0') :- !.
'~AVLTrees.Combine.P5'(A, _, 'AVLTrees.Null.C0', A, 'AVLTrees.Yes.C0').
'AVLTrees.Combine.P5'(A, B, C, D, E) :-
        call_residue((user:not_equal([],[A],A,'AVLTrees.Null.C0'),user:not_equal([],[C],C,'AVLTrees.Null.C0')), F),
        (   F=[] ->
            !
        ;   user:release_suspended(F)
        ),
        'AVLTrees.RemoveRightmost.P5'(A, G, H, I, J),
        'AVLTrees.Adjust.P6'(J, 'AVLTrees.Node.F5'(I,G,H,B,C), B, 'AVLTrees.Right.C0', K, D),
        'AVLTrees.HeightDecreased.P3'(B, K, E).
'~AVLTrees.Combine.P5'(A, B, C, D, E) :-
        user:not_equal([], [A], A, 'AVLTrees.Null.C0'),
        user:not_equal([], [C], C, 'AVLTrees.Null.C0'),
        '~AVLTrees.RemoveRightmost.P5'(A, F, G, H, I),
        '~AVLTrees.Adjust.P6'(I, 'AVLTrees.Node.F5'(H,F,G,B,C), B, 'AVLTrees.Right.C0', J, D),
        '~AVLTrees.HeightDecreased.P3'(B, J, E).
'AVLTrees.Insert.P5'('AVLTrees.Null.C0', A, B, 'AVLTrees.Node.F5'('AVLTrees.Null.C0',A,B,'AVLTrees.EQ.C0','AVLTrees.Null.C0'), 'AVLTrees.Yes.C0').
'~AVLTrees.Insert.P5'('AVLTrees.Null.C0', A, B, 'AVLTrees.Node.F5'('AVLTrees.Null.C0',A,B,'AVLTrees.EQ.C0','AVLTrees.Null.C0'), 'AVLTrees.Yes.C0').
'AVLTrees.Insert.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings':'Strings.<.P2'(F,B), J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        'AVLTrees.Insert.P5'(A, F, G, K, L),
        'AVLTrees.Adjust.P6'(L, 'AVLTrees.Node.F5'(K,B,C,D,E), D, 'AVLTrees.Left.C0', M, H),
        'AVLTrees.HeightIncreased.P3'(D, M, I).
'~AVLTrees.Insert.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        'Strings':'~Strings.<.P2'(F, B),
        '~AVLTrees.Insert.P5'(A, F, G, J, K),
        '~AVLTrees.Adjust.P6'(K, 'AVLTrees.Node.F5'(J,B,C,D,E), D, 'AVLTrees.Left.C0', L, H),
        '~AVLTrees.HeightIncreased.P3'(D, L, I).
'AVLTrees.Insert.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings':'Strings.>.P2'(F,B), J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        'AVLTrees.Insert.P5'(E, F, G, K, L),
        'AVLTrees.Adjust.P6'(L, 'AVLTrees.Node.F5'(A,B,C,D,K), D, 'AVLTrees.Right.C0', M, H),
        'AVLTrees.HeightIncreased.P3'(D, M, I).
'~AVLTrees.Insert.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        'Strings':'~Strings.>.P2'(F, B),
        '~AVLTrees.Insert.P5'(E, F, G, J, K),
        '~AVLTrees.Adjust.P6'(K, 'AVLTrees.Node.F5'(A,B,C,D,J), D, 'AVLTrees.Right.C0', L, H),
        '~AVLTrees.HeightIncreased.P3'(D, L, I).
'AVLTrees.Insert.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue(F=B, J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        G=C,
        H='AVLTrees.Node.F5'(A,B,C,D,E),
        I='AVLTrees.No.C0'.
'~AVLTrees.Insert.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        F=B,
        G=C,
        H='AVLTrees.Node.F5'(A,B,C,D,E),
        I='AVLTrees.No.C0'.
'AVLTrees.HeightIncreased.P3'('AVLTrees.RH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.No.C0') :- !.
'~AVLTrees.HeightIncreased.P3'('AVLTrees.RH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.No.C0').
'AVLTrees.HeightIncreased.P3'('AVLTrees.LH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.No.C0') :- !.
'~AVLTrees.HeightIncreased.P3'('AVLTrees.LH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.No.C0').
'AVLTrees.HeightIncreased.P3'('AVLTrees.EQ.C0', 'AVLTrees.LH.C0', 'AVLTrees.Yes.C0') :- !.
'~AVLTrees.HeightIncreased.P3'('AVLTrees.EQ.C0', 'AVLTrees.LH.C0', 'AVLTrees.Yes.C0').
'AVLTrees.HeightIncreased.P3'('AVLTrees.EQ.C0', 'AVLTrees.RH.C0', 'AVLTrees.Yes.C0') :- !.
'~AVLTrees.HeightIncreased.P3'('AVLTrees.EQ.C0', 'AVLTrees.RH.C0', 'AVLTrees.Yes.C0').
'AVLTrees.HeightIncreased.P3'(A, A, 'AVLTrees.No.C0') :- !.
'~AVLTrees.HeightIncreased.P3'(A, A, 'AVLTrees.No.C0').
'AVLTrees.HeightDecreased.P3'('AVLTrees.LH.C0', 'AVLTrees.RH.C0', 'AVLTrees.No.C0') :- !.
'~AVLTrees.HeightDecreased.P3'('AVLTrees.LH.C0', 'AVLTrees.RH.C0', 'AVLTrees.No.C0').
'AVLTrees.HeightDecreased.P3'('AVLTrees.RH.C0', 'AVLTrees.LH.C0', 'AVLTrees.No.C0') :- !.
'~AVLTrees.HeightDecreased.P3'('AVLTrees.RH.C0', 'AVLTrees.LH.C0', 'AVLTrees.No.C0').
'AVLTrees.HeightDecreased.P3'('AVLTrees.LH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.Yes.C0') :- !.
'~AVLTrees.HeightDecreased.P3'('AVLTrees.LH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.Yes.C0').
'AVLTrees.HeightDecreased.P3'('AVLTrees.RH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.Yes.C0') :- !.
'~AVLTrees.HeightDecreased.P3'('AVLTrees.RH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.Yes.C0').
'AVLTrees.HeightDecreased.P3'('AVLTrees.EQ.C0', _, 'AVLTrees.No.C0') :- !.
'~AVLTrees.HeightDecreased.P3'('AVLTrees.EQ.C0', _, 'AVLTrees.No.C0').
'AVLTrees.HeightDecreased.P3'(A, A, 'AVLTrees.No.C0') :- !.
'~AVLTrees.HeightDecreased.P3'(A, A, 'AVLTrees.No.C0').
'AVLTrees.RemoveRightmost.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue(user:not_equal([],[E],E,'AVLTrees.Null.C0'), J),
        (   J=[] ->
            !
        ;   user:release_suspended(J)
        ),
        'AVLTrees.RemoveRightmost.P5'(E, F, G, K, L),
        'AVLTrees.Adjust.P6'(L, 'AVLTrees.Node.F5'(A,B,C,D,K), D, 'AVLTrees.Left.C0', M, H),
        'AVLTrees.HeightDecreased.P3'(D, M, I).
'~AVLTrees.RemoveRightmost.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        user:not_equal([], [E], E, 'AVLTrees.Null.C0'),
        '~AVLTrees.RemoveRightmost.P5'(E, F, G, J, K),
        '~AVLTrees.Adjust.P6'(K, 'AVLTrees.Node.F5'(A,B,C,D,J), D, 'AVLTrees.Left.C0', L, H),
        '~AVLTrees.HeightDecreased.P3'(D, L, I).
'AVLTrees.RemoveRightmost.P5'('AVLTrees.Node.F5'(A,B,C,_,'AVLTrees.Null.C0'), B, C, A, 'AVLTrees.Yes.C0') :- !.
'~AVLTrees.RemoveRightmost.P5'('AVLTrees.Node.F5'(A,B,C,_,'AVLTrees.Null.C0'), B, C, A, 'AVLTrees.Yes.C0').
'AVLTrees.Rebalance.P4'('AVLTrees.No.C0', 'AVLTrees.Node.F5'(A,B,C,_,D), E, 'AVLTrees.Node.F5'(A,B,C,E,D)).
'~AVLTrees.Rebalance.P4'('AVLTrees.No.C0', 'AVLTrees.Node.F5'(A,B,C,_,D), E, 'AVLTrees.Node.F5'(A,B,C,E,D)).
'AVLTrees.Rebalance.P4'('AVLTrees.Yes.C0', A, B, C) :-
        'AVLTrees.Restructure.P3'(A, B, C).
'~AVLTrees.Rebalance.P4'('AVLTrees.Yes.C0', A, B, C) :-
        '~AVLTrees.Restructure.P3'(A, B, C).
'AVLTrees.Restructure.P3'('AVLTrees.Node.F5'(A,B,C,'AVLTrees.RH.C0','AVLTrees.Node.F5'(D,E,F,G,H)), I, J) :-
        call_residue(user:not_equal([],[G],G,'AVLTrees.LH.C0'), K),
        (   K=[] ->
            !
        ;   user:release_suspended(K)
        ),
        J='AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,L,D),E,F,I,H),
        'AVLTrees.BalanceTable1.P3'(G, L, I).
'~AVLTrees.Restructure.P3'('AVLTrees.Node.F5'(A,B,C,'AVLTrees.RH.C0','AVLTrees.Node.F5'(D,E,F,G,H)), I, J) :-
        user:not_equal([], [G], G, 'AVLTrees.LH.C0'),
        J='AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,K,D),E,F,I,H),
        '~AVLTrees.BalanceTable1.P3'(G, K, I).
'AVLTrees.Restructure.P3'('AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,D,E),F,G,'AVLTrees.LH.C0',H), I, J) :-
        call_residue(user:not_equal([],[D],D,'AVLTrees.RH.C0'), K),
        (   K=[] ->
            !
        ;   user:release_suspended(K)
        ),
        J='AVLTrees.Node.F5'(A,B,C,I,'AVLTrees.Node.F5'(E,F,G,L,H)),
        'AVLTrees.BalanceTable1.P3'(D, I, L).
'~AVLTrees.Restructure.P3'('AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,D,E),F,G,'AVLTrees.LH.C0',H), I, J) :-
        user:not_equal([], [D], D, 'AVLTrees.RH.C0'),
        J='AVLTrees.Node.F5'(A,B,C,I,'AVLTrees.Node.F5'(E,F,G,K,H)),
        '~AVLTrees.BalanceTable1.P3'(D, I, K).
'AVLTrees.Restructure.P3'('AVLTrees.Node.F5'(A,B,C,'AVLTrees.RH.C0','AVLTrees.Node.F5'('AVLTrees.Node.F5'(D,E,F,G,H),I,J,'AVLTrees.LH.C0',K)), 'AVLTrees.EQ.C0', 'AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,L,D),E,F,'AVLTrees.EQ.C0','AVLTrees.Node.F5'(H,I,J,M,K))) :- !,
        'AVLTrees.BalanceTable2.P3'(G, L, M).
'~AVLTrees.Restructure.P3'('AVLTrees.Node.F5'(A,B,C,'AVLTrees.RH.C0','AVLTrees.Node.F5'('AVLTrees.Node.F5'(D,E,F,G,H),I,J,'AVLTrees.LH.C0',K)), 'AVLTrees.EQ.C0', 'AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,L,D),E,F,'AVLTrees.EQ.C0','AVLTrees.Node.F5'(H,I,J,M,K))) :-
        true,
        '~AVLTrees.BalanceTable2.P3'(G, L, M).
'AVLTrees.Restructure.P3'('AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,'AVLTrees.RH.C0','AVLTrees.Node.F5'(D,E,F,G,H)),I,J,'AVLTrees.LH.C0',K), 'AVLTrees.EQ.C0', 'AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,L,D),E,F,'AVLTrees.EQ.C0','AVLTrees.Node.F5'(H,I,J,M,K))) :- !,
        'AVLTrees.BalanceTable2.P3'(G, L, M).
'~AVLTrees.Restructure.P3'('AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,'AVLTrees.RH.C0','AVLTrees.Node.F5'(D,E,F,G,H)),I,J,'AVLTrees.LH.C0',K), 'AVLTrees.EQ.C0', 'AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,L,D),E,F,'AVLTrees.EQ.C0','AVLTrees.Node.F5'(H,I,J,M,K))) :-
        true,
        '~AVLTrees.BalanceTable2.P3'(G, L, M).
