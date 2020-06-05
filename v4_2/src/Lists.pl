:- module('Lists', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

'Lists.Member.P2'(A, B) :-
        user:goedel_freeze(nonvar(B), 'Lists':'Lists.Member.P2.0'(A,B)).
'~Lists.Member.P2'(A, B) :-
        user:goedel_freeze(nonvar(B), 'Lists':'~Lists.Member.P2.0'(A,B)).
'Lists.Member.P2.0'(A, [A|_]).
'~Lists.Member.P2.0'(A, [A|_]).
'Lists.Member.P2.0'(A, [_|B]) :-
        'Lists.Member.P2'(A, B).
'~Lists.Member.P2.0'(A, [_|B]) :-
        '~Lists.Member.P2'(A, B).
'Lists.Delete.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(B)or nonvar(C), 'Lists':'Lists.Delete.P3.0'(A,B,C)).
'~Lists.Delete.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(B)or nonvar(C), 'Lists':'~Lists.Delete.P3.0'(A,B,C)).
'Lists.Delete.P3.0'(A, [A|B], B).
'~Lists.Delete.P3.0'(A, [A|B], B).
'Lists.Delete.P3.0'(A, [B|C], [B|D]) :-
        'Lists.Delete.P3'(A, C, D).
'~Lists.Delete.P3.0'(A, [B|C], [B|D]) :-
        '~Lists.Delete.P3'(A, C, D).
'Lists.Append.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)or nonvar(C), 'Lists':'Lists.Append.P3.0'(A,B,C)).
'~Lists.Append.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)or nonvar(C), 'Lists':'~Lists.Append.P3.0'(A,B,C)).
'Lists.Append.P3.0'([], A, A).
'~Lists.Append.P3.0'([], A, A).
'Lists.Append.P3.0'([A|B], C, [A|D]) :-
        'Lists.Append.P3'(B, C, D).
'~Lists.Append.P3.0'([A|B], C, [A|D]) :-
        '~Lists.Append.P3'(B, C, D).
'Lists.DeleteFirst.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and nonvar(B)or nonvar(C), 'Lists':'Lists.DeleteFirst.P3.0'(A,B,C)).
'~Lists.DeleteFirst.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and nonvar(B)or nonvar(C), 'Lists':'~Lists.DeleteFirst.P3.0'(A,B,C)).
'Lists.DeleteFirst.P3.0'(A, [A|B], B) :- !.
'~Lists.DeleteFirst.P3.0'(A, [A|B], B).
'Lists.DeleteFirst.P3.0'(A, [B|C], [B|D]) :-
        call_residue(user:not_equal([],[A,B],A,B), E),
        (   E=[] ->
            !
        ;   user:release_suspended(E)
        ),
        'Lists.DeleteFirst.P3'(A, C, D).
'~Lists.DeleteFirst.P3.0'(A, [B|C], [B|D]) :-
        user:not_equal([], [A,B], A, B),
        '~Lists.DeleteFirst.P3'(A, C, D).
'Lists.Length.P2'(A, B) :-
        user:goedel_freeze(nonvar(A)or nonvar(B), 'Lists':'Lists.Length.P2.0'(A,B)).
'~Lists.Length.P2'(A, B) :-
        user:goedel_freeze(nonvar(A)or nonvar(B), 'Lists':'~Lists.Length.P2.0'(A,B)).
'Lists.Length.P2.0'([], 0).
'~Lists.Length.P2.0'([], 0).
'Lists.Length.P2.0'([_|A], B) :-
        'Integers':plus(C, 1, B),
        'Integers':'Integers.>=.P2'(C, 0),
        'Lists.Length.P2'(A, C).
'~Lists.Length.P2.0'([_|A], B) :-
        'Integers':plus(C, 1, B),
        'Integers':'~Integers.>=.P2'(C, 0),
        '~Lists.Length.P2'(A, C).
'Lists.Reverse.P2'(A, B) :-
        user:goedel_freeze(nonvar(A)or nonvar(B), 'Lists':'Lists.Reverse.P2.0'(A,B)).
'~Lists.Reverse.P2'(A, B) :-
        user:goedel_freeze(nonvar(A)or nonvar(B), 'Lists':'~Lists.Reverse.P2.0'(A,B)).
'Lists.Reverse.P2.0'(A, B) :-
        'Lists.Reverse3.P3'(A, [], B).
'~Lists.Reverse.P2.0'(A, B) :-
        '~Lists.Reverse3.P3'(A, [], B).
'Lists.Permutation.P2'(A, B) :-
        user:goedel_freeze(nonvar(A)or nonvar(B), 'Lists':'Lists.Permutation.P2.0'(A,B)).
'~Lists.Permutation.P2'(A, B) :-
        user:goedel_freeze(nonvar(A)or nonvar(B), 'Lists':'~Lists.Permutation.P2.0'(A,B)).
'Lists.Permutation.P2.0'([], []).
'~Lists.Permutation.P2.0'([], []).
'Lists.Permutation.P2.0'([A|B], [C|D]) :-
        'Lists.Delete.P3'(C, [A|B], E),
        'Lists.Permutation.P2'(E, D).
'~Lists.Permutation.P2.0'([A|B], [C|D]) :-
        '~Lists.Delete.P3'(C, [A|B], E),
        '~Lists.Permutation.P2'(E, D).
'Lists.Merge.P3'(A, B, C) :-
        user:goedel_freeze((nonvar(A)and nonvar(B))or nonvar(C), 'Lists':'Lists.Merge.P3.0'(A,B,C)).
'~Lists.Merge.P3'(A, B, C) :-
        user:goedel_freeze((nonvar(A)and nonvar(B))or nonvar(C), 'Lists':'~Lists.Merge.P3.0'(A,B,C)).
'Lists.Merge.P3.0'([], A, A).
'~Lists.Merge.P3.0'([], A, A).
'Lists.Merge.P3.0'(A, [], A).
'~Lists.Merge.P3.0'(A, [], A).
'Lists.Merge.P3.0'([A|B], [C|D], [A|E]) :-
        'Integers':'Integers.<.P2'(A, C),
        'Lists.Merge.P3'(B, [C|D], E).
'~Lists.Merge.P3.0'([A|B], [C|D], [A|E]) :-
        'Integers':'~Integers.<.P2'(A, C),
        '~Lists.Merge.P3'(B, [C|D], E).
'Lists.Merge.P3.0'([A|B], [C|D], [C|E]) :-
        'Integers':'Integers.>=.P2'(A, C),
        'Lists.Merge.P3'([A|B], D, E).
'~Lists.Merge.P3.0'([A|B], [C|D], [C|E]) :-
        'Integers':'~Integers.>=.P2'(A, C),
        '~Lists.Merge.P3'([A|B], D, E).
'Lists.MemberCheck.P2'(A, B) :-
        user:goedel_freeze(nonvar(B), 'Lists':'Lists.MemberCheck.P2.1'(A,B)).
'~Lists.MemberCheck.P2'(A, B) :-
        user:goedel_freeze(nonvar(B), 'Lists':'~Lists.MemberCheck.P2.1'(A,B)).
'Lists.MemberCheck.P2.1'(A, [B|C]) :- !,
        user:goedel_freeze(nonvar(A)and nonvar(B), 'Lists':'Lists.MemberCheck.P2.0'(A,[B|C])).
'~Lists.MemberCheck.P2.1'(A, [B|C]) :- !,
        user:goedel_freeze(nonvar(A)and nonvar(B), 'Lists':'~Lists.MemberCheck.P2.0'(A,[B|C])).
'Lists.MemberCheck.P2.1'(A, []) :- !,
        'Lists.MemberCheck.P2.0'(A, []).
'~Lists.MemberCheck.P2.1'(A, []) :- !,
        '~Lists.MemberCheck.P2.0'(A, []).
'Lists.MemberCheck.P2.1'(A, B) :-
        'Lists.MemberCheck.P2.0'(A, B).
'~Lists.MemberCheck.P2.1'(A, B) :-
        '~Lists.MemberCheck.P2.0'(A, B).
'Lists.MemberCheck.P2.0'(A, [A|_]) :- !.
'~Lists.MemberCheck.P2.0'(A, [A|_]).
'Lists.MemberCheck.P2.0'(A, [B|C]) :-
        call_residue(user:not_equal([],[A,B],A,B), D),
        (   D=[] ->
            !
        ;   user:release_suspended(D)
        ),
        'Lists.MemberCheck.P2'(A, C).
'~Lists.MemberCheck.P2.0'(A, [B|C]) :-
        user:not_equal([], [A,B], A, B),
        '~Lists.MemberCheck.P2'(A, C).
'Lists.Partition.P4'([], _, [], []).
'~Lists.Partition.P4'([], _, [], []).
'Lists.Partition.P4'([A|B], C, [A|D], E) :-
        'Integers':'Integers.=<.P2'(A, C),
        'Lists.Partition.P4'(B, C, D, E).
'~Lists.Partition.P4'([A|B], C, [A|D], E) :-
        'Integers':'~Integers.=<.P2'(A, C),
        '~Lists.Partition.P4'(B, C, D, E).
'Lists.Partition.P4'([A|B], C, D, [A|E]) :-
        'Integers':'Integers.>.P2'(A, C),
        'Lists.Partition.P4'(B, C, D, E).
'~Lists.Partition.P4'([A|B], C, D, [A|E]) :-
        'Integers':'~Integers.>.P2'(A, C),
        '~Lists.Partition.P4'(B, C, D, E).
'Lists.Prefix.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and nonvar(B)or nonvar(C), 'Lists':'Lists.Prefix.P3.0'(A,B,C)).
'~Lists.Prefix.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and nonvar(B)or nonvar(C), 'Lists':'~Lists.Prefix.P3.0'(A,B,C)).
'Lists.Prefix.P3.0'(A, B, C) :-
        'Integers':'Integers.>=.P2'(B, 0),
        'Lists.Length.P2'(A, D),
        'Integers':'Integers.=<.P2'(B, D),
        'Lists.Split.P4'(B, A, C, _).
'~Lists.Prefix.P3.0'(A, B, C) :-
        'Integers':'~Integers.>=.P2'(B, 0),
        '~Lists.Length.P2'(A, D),
        'Integers':'~Integers.=<.P2'(B, D),
        '~Lists.Split.P4'(B, A, C, _).
'Lists.Sorted.P1'(A) :-
        user:goedel_freeze(nonvar(A), 'Lists':'Lists.Sorted.P1.1'(A)).
'~Lists.Sorted.P1'(A) :-
        user:goedel_freeze(nonvar(A), 'Lists':'~Lists.Sorted.P1.1'(A)).
'Lists.Sorted.P1.1'([A|B]) :- !,
        user:goedel_freeze(nonvar(B), 'Lists':'Lists.Sorted.P1.0'([A|B])).
'~Lists.Sorted.P1.1'([A|B]) :- !,
        user:goedel_freeze(nonvar(B), 'Lists':'~Lists.Sorted.P1.0'([A|B])).
'Lists.Sorted.P1.1'([]) :- !,
        'Lists.Sorted.P1.0'([]).
'~Lists.Sorted.P1.1'([]) :- !,
        '~Lists.Sorted.P1.0'([]).
'Lists.Sorted.P1.1'(A) :-
        'Lists.Sorted.P1.0'(A).
'~Lists.Sorted.P1.1'(A) :-
        '~Lists.Sorted.P1.0'(A).
'Lists.Sorted.P1.0'([]).
'~Lists.Sorted.P1.0'([]).
'Lists.Sorted.P1.0'([_]).
'~Lists.Sorted.P1.0'([_]).
'Lists.Sorted.P1.0'([A,B|C]) :-
        'Integers':'Integers.=<.P2'(A, B),
        'Lists.Sorted.P1'([B|C]).
'~Lists.Sorted.P1.0'([A,B|C]) :-
        'Integers':'~Integers.=<.P2'(A, B),
        '~Lists.Sorted.P1'([B|C]).
'Lists.Sort.P2'(A, B) :-
        user:goedel_freeze(nonvar(A), 'Lists':'Lists.Sort.P2.0'(A,B)).
'~Lists.Sort.P2'(A, B) :-
        user:goedel_freeze(nonvar(A), 'Lists':'~Lists.Sort.P2.0'(A,B)).
'Lists.Sort.P2.0'([], []).
'~Lists.Sort.P2.0'([], []).
'Lists.Sort.P2.0'([A|B], C) :-
        'Lists.Partition.P4'(B, A, D, E),
        'Lists.Sort.P2'(D, F),
        'Lists.Sort.P2'(E, G),
        'Lists.Append.P3'(F, [A|G], C).
'~Lists.Sort.P2.0'([A|B], C) :-
        '~Lists.Partition.P4'(B, A, D, E),
        '~Lists.Sort.P2'(D, F),
        '~Lists.Sort.P2'(E, G),
        '~Lists.Append.P3'(F, [A|G], C).
'Lists.Reverse3.P3'([], A, A).
'~Lists.Reverse3.P3'([], A, A).
'Lists.Reverse3.P3'([A|B], C, D) :-
        'Lists.Reverse3.P3'(B, [A|C], D).
'~Lists.Reverse3.P3'([A|B], C, D) :-
        '~Lists.Reverse3.P3'(B, [A|C], D).
'Lists.Suffix.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and nonvar(B)or nonvar(C), 'Lists':'Lists.Suffix.P3.0'(A,B,C)).
'~Lists.Suffix.P3'(A, B, C) :-
        user:goedel_freeze(nonvar(A)and nonvar(B)or nonvar(C), 'Lists':'~Lists.Suffix.P3.0'(A,B,C)).
'Lists.Suffix.P3.0'(A, B, C) :-
        'Integers':'Integers.>=.P2'(B, 0),
        'Lists.Length.P2'(A, D),
        'Integers':'Integers.=<.P2'(B, D),
        'Integers':minus(D, B, F),
        E=F,
        'Lists.Split.P4'(E, A, _, C).
'~Lists.Suffix.P3.0'(A, B, C) :-
        'Integers':'~Integers.>=.P2'(B, 0),
        '~Lists.Length.P2'(A, D),
        'Integers':'~Integers.=<.P2'(B, D),
        'Integers':minus(D, B, F),
        E=F,
        '~Lists.Split.P4'(E, A, _, C).
'Lists.Split.P4'(0, A, [], A).
'~Lists.Split.P4'(0, A, [], A).
'Lists.Split.P4'(A, [B|C], [B|D], E) :-
        'Integers':'Integers.>.P2'(A, 0),
        'Integers':minus(A, 1, F),
        'Lists.Split.P4'(F, C, D, E).
'~Lists.Split.P4'(A, [B|C], [B|D], E) :-
        'Integers':'~Integers.>.P2'(A, 0),
        'Integers':minus(A, 1, F),
        '~Lists.Split.P4'(F, C, D, E).
