:- module('Flocks', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

'Flocks.Extent.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Flocks':'Flocks.Extent.P2.0'(A,B)).
'~Flocks.Extent.P2'(A, B) :-
        user:goedel_freeze(ground([A]), 'Flocks':'~Flocks.Extent.P2.0'(A,B)).
'Flocks.Extent.P2.0'('Flocks.Flock.F1'(A), B) :-
        'Lists':'Lists.Length.P2'(A, B).
'~Flocks.Extent.P2.0'('Flocks.Flock.F1'(A), B) :-
        'Lists':'~Lists.Length.P2'(A, B).
'Flocks.DeleteUnit.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Flocks':'Flocks.DeleteUnit.P4.0'(A,B,C,D)).
'~Flocks.DeleteUnit.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Flocks':'~Flocks.DeleteUnit.P4.0'(A,B,C,D)).
'Flocks.DeleteUnit.P4.0'('Flocks.Flock.F1'(A), B, C, 'Flocks.Flock.F1'(D)) :-
        'Flocks.DelNthElement.P4'(A, C, B, D).
'~Flocks.DeleteUnit.P4.0'('Flocks.Flock.F1'(A), B, C, 'Flocks.Flock.F1'(D)) :-
        '~Flocks.DelNthElement.P4'(A, C, B, D).
'Flocks.DelNthElement.P4'([A|B], 1, A, B).
'~Flocks.DelNthElement.P4'([A|B], 1, A, B).
'Flocks.DelNthElement.P4'([A|B], C, D, [A|E]) :-
        'Integers':'Integers.>.P2'(C, 1),
        'Integers':minus(C, 1, F),
        'Flocks.DelNthElement.P4'(B, F, D, E).
'~Flocks.DelNthElement.P4'([A|B], C, D, [A|E]) :-
        'Integers':'~Integers.>.P2'(C, 1),
        'Integers':minus(C, 1, F),
        '~Flocks.DelNthElement.P4'(B, F, D, E).
'Flocks.EmptyFlock.P1'('Flocks.Flock.F1'([])).
'~Flocks.EmptyFlock.P1'('Flocks.Flock.F1'([])).
'Flocks.UnitInFlock.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Flocks':'Flocks.UnitInFlock.P3.0'(A,B,C)).
'~Flocks.UnitInFlock.P3'(A, B, C) :-
        user:goedel_freeze(ground([A]), 'Flocks':'~Flocks.UnitInFlock.P3.0'(A,B,C)).
'Flocks.UnitInFlock.P3.0'('Flocks.Flock.F1'(A), B, C) :-
        'Flocks.NthElement.P3'(A, C, B).
'~Flocks.UnitInFlock.P3.0'('Flocks.Flock.F1'(A), B, C) :-
        '~Flocks.NthElement.P3'(A, C, B).
'Flocks.InsertUnit.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Flocks':'Flocks.InsertUnit.P4.0'(A,B,C,D)).
'~Flocks.InsertUnit.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A,B,C]), 'Flocks':'~Flocks.InsertUnit.P4.0'(A,B,C,D)).
'Flocks.InsertUnit.P4.0'('Flocks.Flock.F1'(A), B, C, 'Flocks.Flock.F1'(D)) :-
        'Flocks.InsNthElement.P4'(A, C, B, D).
'~Flocks.InsertUnit.P4.0'('Flocks.Flock.F1'(A), B, C, 'Flocks.Flock.F1'(D)) :-
        '~Flocks.InsNthElement.P4'(A, C, B, D).
'Flocks.InsNthElement.P4'(A, 1, B, [B|A]) :- !.
'~Flocks.InsNthElement.P4'(A, 1, B, [B|A]).
'Flocks.InsNthElement.P4'([A|B], C, D, [A|E]) :-
        call_residue('Integers':'Integers.>.P2'(C,1), F),
        (   F=[] ->
            !
        ;   user:release_suspended(F)
        ),
        'Integers':minus(C, 1, G),
        'Flocks.InsNthElement.P4'(B, G, D, E).
'~Flocks.InsNthElement.P4'([A|B], C, D, [A|E]) :-
        'Integers':'~Integers.>.P2'(C, 1),
        'Integers':minus(C, 1, F),
        '~Flocks.InsNthElement.P4'(B, F, D, E).
'Flocks.NthElement.P3'([A|_], 1, A).
'~Flocks.NthElement.P3'([A|_], 1, A).
'Flocks.NthElement.P3'([_|A], B, C) :-
        'Integers':'Integers.>.P2'(B, 1),
        'Integers':minus(B, 1, D),
        'Flocks.NthElement.P3'(A, D, C).
'~Flocks.NthElement.P3'([_|A], B, C) :-
        'Integers':'~Integers.>.P2'(B, 1),
        'Integers':minus(B, 1, D),
        '~Flocks.NthElement.P3'(A, D, C).
'Flocks.UnitWithIdentifier.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Flocks':'Flocks.UnitWithIdentifier.P4.0'(A,B,C,D)).
'~Flocks.UnitWithIdentifier.P4'(A, B, C, D) :-
        user:goedel_freeze(ground([A]), 'Flocks':'~Flocks.UnitWithIdentifier.P4.0'(A,B,C,D)).
'Flocks.UnitWithIdentifier.P4.0'('Flocks.Flock.F1'(A), B, C, D) :-
        'Flocks.NthElement.P3'(A, D, C),
        'Units':'Units.UnitParts.P3'(C, B, _).
'~Flocks.UnitWithIdentifier.P4.0'('Flocks.Flock.F1'(A), B, C, D) :-
        '~Flocks.NthElement.P3'(A, D, C),
        'Units':'~Units.UnitParts.P3'(C, B, _).
