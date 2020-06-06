:- multifile '$$module'/1.

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


/* 
Subject:	the compiled AVLTree routines.
Date:		20 Feb 1992
	
================================================================================
*/

%% '$$module'('@(#)avltrees.pl 1.5 last updated 93/05/20 18:13:38 by jiwei').


'AVLTrees.Adjustment.P4'('AVLTrees.EQ.C0', 'AVLTrees.Left.C0', 'AVLTrees.LH.C0', 'AVLTrees.No.C0') :- !.
'AVLTrees.Adjustment.P4'('AVLTrees.EQ.C0', 'AVLTrees.Right.C0', 'AVLTrees.RH.C0', 'AVLTrees.No.C0') :- !.
'AVLTrees.Adjustment.P4'('AVLTrees.LH.C0', 'AVLTrees.Left.C0', _, 'AVLTrees.Yes.C0') :- !.
'AVLTrees.Adjustment.P4'('AVLTrees.LH.C0', 'AVLTrees.Right.C0', 'AVLTrees.EQ.C0', 'AVLTrees.No.C0') :- !.
'AVLTrees.Adjustment.P4'('AVLTrees.RH.C0', 'AVLTrees.Left.C0', 'AVLTrees.EQ.C0', 'AVLTrees.No.C0') :- !.
'AVLTrees.Adjustment.P4'('AVLTrees.RH.C0', 'AVLTrees.Right.C0', _, 'AVLTrees.Yes.C0') :- !.
'AVLTrees.AVLJoin.P3'(A, B, C) :-
        'AVLTrees.AVLJoinAux.P3'(B, A, C).
'AVLTrees.AVLInsert.P4'(A, B, C, D) :-
        'AVLTrees.Insert.P5'(A, B, C, D, _).
'AVLTrees.AVLAmend.P6'(A, B, C, D, E, F) :-
        'AVLTrees.Amend.P7'(A, B, C, D, E, F, _).
'AVLTrees.AVLDelete.P3'(A, B, C) :-
        'AVLTrees.DeleteKey.P4'(A, B, C, _).
'AVLTrees.AVLIsEmpty.P1'('AVLTrees.Null.C0').
'AVLTrees.AVLSearch.P3'('AVLTrees.Node.F5'(_,A,B,_,_), C, D) :-
        call_residue(C=A, E),
        (   E=[] ->
            true
        ;   user:flounder_commit(E)
        ), !,
        D=B.
'AVLTrees.AVLSearch.P3'('AVLTrees.Node.F5'(A,B,_,_,_), C, D) :-
        call_residue('Strings.<.P2'(C,B), E),
        (   E=[] ->
            true
        ;   user:flounder_commit(E)
        ), !,
        'AVLTrees.AVLSearch.P3'(A, C, D).
'AVLTrees.AVLSearch.P3'('AVLTrees.Node.F5'(_,A,_,_,B), C, D) :-
        call_residue('Strings.>.P2'(C,A), E),
        (   E=[] ->
            true
        ;   user:flounder_commit(E)
        ), !,
        'AVLTrees.AVLSearch.P3'(B, C, D).
'AVLTrees.AVLMember.P3'('AVLTrees.Node.F5'(_,A,B,_,_), A, B).
'AVLTrees.AVLMember.P3'('AVLTrees.Node.F5'(A,_,_,_,_), B, C) :-
        'AVLTrees.AVLMember.P3'(A, B, C).
'AVLTrees.AVLMember.P3'('AVLTrees.Node.F5'(_,_,_,_,A), B, C) :-
        'AVLTrees.AVLMember.P3'(A, B, C).
'AVLTrees.AVLJoinAux.P3'('AVLTrees.Null.C0', A, A).
'AVLTrees.AVLJoinAux.P3'('AVLTrees.Node.F5'(A,B,C,_,D), E, F) :-
        'AVLTrees.Amend.P7'(E, B, C, _, G, _, _),
        'AVLTrees.AVLJoinAux.P3'(A, G, H),
        'AVLTrees.AVLJoinAux.P3'(D, H, F).
'AVLTrees.AVLUpdate.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue(F=B, J),
        (   J=[] ->
            true
        ;   user:flounder_commit(J)
        ), !,
        H='AVLTrees.Node.F5'(A,B,G,D,E),
        I=C.
'AVLTrees.AVLUpdate.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings.<.P2'(F,B), J),
        (   J=[] ->
            true
        ;   user:flounder_commit(J)
        ), !,
        'AVLTrees.AVLUpdate.P5'(A, F, G, K, I),
        H='AVLTrees.Node.F5'(K,B,C,D,E).
'AVLTrees.AVLUpdate.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings.>.P2'(F,B), J),
        (   J=[] ->
            true
        ;   user:flounder_commit(J)
        ), !,
        'AVLTrees.AVLUpdate.P5'(E, F, G, K, I),
        H='AVLTrees.Node.F5'(A,B,C,D,K).
'AVLTrees.AVLToBinary.P2'('AVLTrees.Null.C0', 'AVLTrees.Empty.C0').
'AVLTrees.AVLToBinary.P2'('AVLTrees.Node.F5'(A,B,C,_,D), 'AVLTrees.Tree.F4'(E,B,C,F)) :-
        'AVLTrees.AVLToBinary.P2'(A, E),
        'AVLTrees.AVLToBinary.P2'(D, F).
'AVLTrees.Adjust.P6'('AVLTrees.No.C0', A, B, _, B, A).
'AVLTrees.Adjust.P6'('AVLTrees.Yes.C0', A, B, C, D, E) :-
        'AVLTrees.Adjustment.P4'(B, C, D, F),
        'AVLTrees.Rebalance.P4'(F, A, D, E).
'AVLTrees.DeleteKey.P4'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H) :-
        call_residue('Strings.<.P2'(F,B), I),
        (   I=[] ->
            true
        ;   user:flounder_commit(I)
        ), !,
        'AVLTrees.DeleteKey.P4'(A, F, J, K),
        'AVLTrees.Adjust.P6'(K, 'AVLTrees.Node.F5'(J,B,C,D,E), D, 'AVLTrees.Right.C0', L, G),
        'AVLTrees.HeightDecreased.P3'(D, L, H).
'AVLTrees.DeleteKey.P4'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H) :-
        call_residue('Strings.>.P2'(F,B), I),
        (   I=[] ->
            true
        ;   user:flounder_commit(I)
        ), !,
        'AVLTrees.DeleteKey.P4'(E, F, J, K),
        'AVLTrees.Adjust.P6'(K, 'AVLTrees.Node.F5'(A,B,C,D,J), D, 'AVLTrees.Left.C0', L, G),
        'AVLTrees.HeightDecreased.P3'(D, L, H).
'AVLTrees.DeleteKey.P4'('AVLTrees.Node.F5'(A,B,_,C,D), E, F, G) :-
        call_residue(E=B, H),
        (   H=[] ->
            true
        ;   user:flounder_commit(H)
        ), !,
        'AVLTrees.Combine.P5'(A, C, D, F, G).
'AVLTrees.BalanceTable2.P3'('AVLTrees.LH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.RH.C0').
'AVLTrees.BalanceTable2.P3'('AVLTrees.RH.C0', 'AVLTrees.LH.C0', 'AVLTrees.EQ.C0').
'AVLTrees.BalanceTable2.P3'('AVLTrees.EQ.C0', 'AVLTrees.EQ.C0', 'AVLTrees.EQ.C0').
'AVLTrees.BalanceTable1.P3'('AVLTrees.EQ.C0', 'AVLTrees.RH.C0', 'AVLTrees.LH.C0').
'AVLTrees.BalanceTable1.P3'('AVLTrees.LH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.EQ.C0').
'AVLTrees.BalanceTable1.P3'('AVLTrees.RH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.EQ.C0').
'AVLTrees.Amend.P7'('AVLTrees.Null.C0', A, B, C, 'AVLTrees.Node.F5'('AVLTrees.Null.C0',A,B,'AVLTrees.EQ.C0','AVLTrees.Null.C0'), C, 'AVLTrees.Yes.C0').
'AVLTrees.Amend.P7'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I, J, K) :-
        call_residue('Strings.<.P2'(F,B), L),
        (   L=[] ->
            true
        ;   user:flounder_commit(L)
        ), !,
        'AVLTrees.Amend.P7'(A, F, G, H, M, J, N),
        'AVLTrees.Adjust.P6'(N, 'AVLTrees.Node.F5'(M,B,C,D,E), D, 'AVLTrees.Left.C0', O, I),
        'AVLTrees.HeightIncreased.P3'(D, O, K).
'AVLTrees.Amend.P7'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I, J, K) :-
        call_residue('Strings.>.P2'(F,B), L),
        (   L=[] ->
            true
        ;   user:flounder_commit(L)
        ), !,
        'AVLTrees.Amend.P7'(E, F, G, H, M, J, N),
        'AVLTrees.Adjust.P6'(N, 'AVLTrees.Node.F5'(A,B,C,D,M), D, 'AVLTrees.Right.C0', O, I),
        'AVLTrees.HeightIncreased.P3'(D, O, K).
'AVLTrees.Amend.P7'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, _, H, I, J) :-
        call_residue(F=B, K),
        (   K=[] ->
            true
        ;   user:flounder_commit(K)
        ), !,
        H='AVLTrees.Node.F5'(A,F,G,D,E),
        I=C,
        J='AVLTrees.No.C0'.
'AVLTrees.Combine.P5'('AVLTrees.Null.C0', _, A, A, 'AVLTrees.Yes.C0') :- !.
'AVLTrees.Combine.P5'(A, _, 'AVLTrees.Null.C0', A, 'AVLTrees.Yes.C0') :- !.
'AVLTrees.Combine.P5'(A, B, C, D, E) :-
        call_residue((user:not_equal([],[A],A,'AVLTrees.Null.C0'),user:not_equal([],[C],C,'AVLTrees.Null.C0')), F),
        (   F=[] ->
            true
        ;   user:flounder_commit(F)
        ), !,
        'AVLTrees.RemoveRightmost.P5'(A, G, H, I, J),
        'AVLTrees.Adjust.P6'(J, 'AVLTrees.Node.F5'(I,G,H,B,C), B, 'AVLTrees.Right.C0', K, D),
        'AVLTrees.HeightDecreased.P3'(B, K, E).
'AVLTrees.Insert.P5'('AVLTrees.Null.C0', A, B, 'AVLTrees.Node.F5'('AVLTrees.Null.C0',A,B,'AVLTrees.EQ.C0','AVLTrees.Null.C0'), 'AVLTrees.Yes.C0').
'AVLTrees.Insert.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings.<.P2'(F,B), J),
        (   J=[] ->
            true
        ;   user:flounder_commit(J)
        ), !,
        'AVLTrees.Insert.P5'(A, F, G, K, L),
        'AVLTrees.Adjust.P6'(L, 'AVLTrees.Node.F5'(K,B,C,D,E), D, 'AVLTrees.Left.C0', M, H),
        'AVLTrees.HeightIncreased.P3'(D, M, I).
'AVLTrees.Insert.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue('Strings.>.P2'(F,B), J),
        (   J=[] ->
            true
        ;   user:flounder_commit(J)
        ), !,
        'AVLTrees.Insert.P5'(E, F, G, K, L),
        'AVLTrees.Adjust.P6'(L, 'AVLTrees.Node.F5'(A,B,C,D,K), D, 'AVLTrees.Right.C0', M, H),
        'AVLTrees.HeightIncreased.P3'(D, M, I).
'AVLTrees.Insert.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue(F=B, J),
        (   J=[] ->
            true
        ;   user:flounder_commit(J)
        ), !,
        G=C,
        H='AVLTrees.Node.F5'(A,B,C,D,E),
        I='AVLTrees.No.C0'.
'AVLTrees.HeightIncreased.P3'('AVLTrees.RH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.No.C0') :- !.
'AVLTrees.HeightIncreased.P3'('AVLTrees.LH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.No.C0') :- !.
'AVLTrees.HeightIncreased.P3'('AVLTrees.EQ.C0', 'AVLTrees.LH.C0', 'AVLTrees.Yes.C0') :- !.
'AVLTrees.HeightIncreased.P3'('AVLTrees.EQ.C0', 'AVLTrees.RH.C0', 'AVLTrees.Yes.C0') :- !.
'AVLTrees.HeightIncreased.P3'(A, A, 'AVLTrees.No.C0') :- !.
'AVLTrees.HeightDecreased.P3'('AVLTrees.LH.C0', 'AVLTrees.RH.C0', 'AVLTrees.No.C0') :- !.
'AVLTrees.HeightDecreased.P3'('AVLTrees.RH.C0', 'AVLTrees.LH.C0', 'AVLTrees.No.C0') :- !.
'AVLTrees.HeightDecreased.P3'('AVLTrees.LH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.Yes.C0') :- !.
'AVLTrees.HeightDecreased.P3'('AVLTrees.RH.C0', 'AVLTrees.EQ.C0', 'AVLTrees.Yes.C0') :- !.
'AVLTrees.HeightDecreased.P3'('AVLTrees.EQ.C0', _, 'AVLTrees.No.C0') :- !.
'AVLTrees.HeightDecreased.P3'(A, A, 'AVLTrees.No.C0') :- !.
'AVLTrees.RemoveRightmost.P5'('AVLTrees.Node.F5'(A,B,C,D,E), F, G, H, I) :-
        call_residue(user:not_equal([],[E],E,'AVLTrees.Null.C0'), J),
        (   J=[] ->
            true
        ;   user:flounder_commit(J)
        ), !,
        'AVLTrees.RemoveRightmost.P5'(E, F, G, K, L),
        'AVLTrees.Adjust.P6'(L, 'AVLTrees.Node.F5'(A,B,C,D,K), D, 'AVLTrees.Left.C0', M, H),
        'AVLTrees.HeightDecreased.P3'(D, M, I).
'AVLTrees.RemoveRightmost.P5'('AVLTrees.Node.F5'(A,B,C,_,'AVLTrees.Null.C0'), B, C, A, 'AVLTrees.Yes.C0') :- !.
'AVLTrees.Rebalance.P4'('AVLTrees.No.C0', 'AVLTrees.Node.F5'(A,B,C,_,D), E, 'AVLTrees.Node.F5'(A,B,C,E,D)).
'AVLTrees.Rebalance.P4'('AVLTrees.Yes.C0', A, B, C) :-
        'AVLTrees.Restructure.P3'(A, B, C).
'AVLTrees.Restructure.P3'('AVLTrees.Node.F5'(A,B,C,'AVLTrees.RH.C0','AVLTrees.Node.F5'(D,E,F,G,H)), I, J) :-
        call_residue(user:not_equal([],[G],G,'AVLTrees.LH.C0'), K),
        (   K=[] ->
            true
        ;   user:flounder_commit(K)
        ), !,
        J='AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,L,D),E,F,I,H),
        'AVLTrees.BalanceTable1.P3'(G, L, I).
'AVLTrees.Restructure.P3'('AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,D,E),F,G,'AVLTrees.LH.C0',H), I, J) :-
        call_residue(user:not_equal([],[D],D,'AVLTrees.RH.C0'), K),
        (   K=[] ->
            true
        ;   user:flounder_commit(K)
        ), !,
        J='AVLTrees.Node.F5'(A,B,C,I,'AVLTrees.Node.F5'(E,F,G,L,H)),
        'AVLTrees.BalanceTable1.P3'(D, I, L).
'AVLTrees.Restructure.P3'('AVLTrees.Node.F5'(A,B,C,'AVLTrees.RH.C0','AVLTrees.Node.F5'('AVLTrees.Node.F5'(D,E,F,G,H),I,J,'AVLTrees.LH.C0',K)), 'AVLTrees.EQ.C0', 'AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,L,D),E,F,'AVLTrees.EQ.C0','AVLTrees.Node.F5'(H,I,J,M,K))) :- !,
        'AVLTrees.BalanceTable2.P3'(G, L, M).
'AVLTrees.Restructure.P3'('AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,'AVLTrees.RH.C0','AVLTrees.Node.F5'(D,E,F,G,H)),I,J,'AVLTrees.LH.C0',K), 'AVLTrees.EQ.C0', 'AVLTrees.Node.F5'('AVLTrees.Node.F5'(A,B,C,L,D),E,F,'AVLTrees.EQ.C0','AVLTrees.Node.F5'(H,I,J,M,K))) :- !,
        'AVLTrees.BalanceTable2.P3'(G, L, M).
