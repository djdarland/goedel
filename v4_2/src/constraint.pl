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
File:		constraint.pl
Subject:	a constraint compiler
Author: 	Jiwei Wang
Date:		31 January 1992

================================================================================
*/

%% '$$module'('@(#)constraint.pl 1.18 last updated 93/12/14 11:58:15 by jiwei').


build_constraints([], null, VarDict, VarDict).
build_constraints([pair(Var, Expr)], Goal, VarDict, NewVarDict) :- !,
   build_constraints_aux(Expr, Var, Goal, VarDict, NewVarDict).

build_constraints([pair(Var, Expr)|Constraints], (Goal, Goals),
		VarDict, NewVarDict) :- 
   build_constraints_aux(Expr, Var, Goal, VarDict, VarDict2),
   build_constraints(Constraints, Goals, VarDict2, NewVarDict).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

build_constraints_aux('MetaDefs.CTerm.F1'(
		'MetaDefs.Name.F4'('"Sets', '"Null', 'MetaDefs.Constant.C0', _)),
	Var, BuiltVar = set([]), VarDict, NewVarDict) :-
   build_term(Var, BuiltVar, VarDict, NewVarDict).

build_constraints_aux('MetaDefs.Term.F2'(
		'MetaDefs.Name.F4'(GModule, Functor, 'MetaDefs.Function.C0', _),
		Expr),
	Var, Goal, VarDict, NewVarDict) :-
   gstring2string(GModule, Module),
   build_constraints_aux(Functor, Expr, Var, Module, Goal, VarDict, NewVarDict).

build_constraints_aux('MetaDefs.SuchThat.F3'(FreeVars, T, W), Var,
	'Sets':set_of(BuiltExpr, BuiltGoal, BuiltVar, BuiltFreeVars), VarDict,
	NewVarDict) :-
   replace_evaluatable(T, T2, [], Constraints),
   ( Constraints = []
     -> compile_formula(W, BuiltGoal, VarDict, VarDict1, [])
     ;  build_constraints(Constraints, Equations, VarDict, VarDict0),
	BuiltGoal = (BuiltGoal0, Equations),
        compile_formula(W, BuiltGoal0, VarDict0, VarDict1, [0'~])
		% the constraints are more likely be solved after W
   ),
   build_term(Var, BuiltVar, VarDict1, VarDict2),
   build_term(T2, BuiltExpr, VarDict2, VarDict3),
   build_term(FreeVars, BuiltFreeVars, VarDict3, NewVarDict).

%------------------------------------------------------------------------------
% For Integers and Rationals
%------------------------------------------------------------------------------

build_constraints_aux('"-', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   !,
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:negative(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:negative(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"-', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:minus(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:minus(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"+', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:plus(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:plus(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"Round', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:roundg(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:roundg(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"Truncate', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:truncateg(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:truncateg(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"*', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:times(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:times(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"^', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:power(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:power(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"Div', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:divides(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:divides(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"Mod', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:mod(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:mod(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"Rem', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:rem(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:rem(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"Abs', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:absolute(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:absolute(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).


build_constraints_aux('"Sign', [Expr], Var, Module, Goal, VarDict, NewVarDict):-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:sign(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:sign(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"Max', [Expr1, Expr2], Var, Module, Goal, 
		VarDict, NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:maximum(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:maximum(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"Min', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:minimum(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:minimum(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).


build_constraints_aux('"/', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:divides(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:divides(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"//', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:rational(BuiltExpr1 // BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:rational(BuiltExpr1 // BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).
%------------------------------------------------------------------------------
% For Floats
%------------------------------------------------------------------------------
build_constraints_aux('"-', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   !,
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:negative(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:negative(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"-', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:minus(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:minus(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"+', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:plus(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:plus(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"*', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:times(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:times(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"^', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:power(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:power(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

build_constraints_aux('"Sin', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:sinrad(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:sinrad(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

/*
build_constraints_aux('"IntegerToFloat', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:integertofloatg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:integertofloatg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).
*/
build_constraints_aux('"Log', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:logg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:logg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"Log10', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:log10g(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:log10g(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"Tan', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:tanrad(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:tanrad(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"Sqrt', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:sqrtg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:sqrtg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"Successor', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:successorg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:successorg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"FractionalPart', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:fractionalpartg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:fractionalpartg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"Predecessor', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:predecessorg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:predecessorg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"Truncate', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:truncateg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:truncateg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).



build_constraints_aux('"ArcSin', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:arcsinrad(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:arcsinrad(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"ArcTan', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:arctanrad(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:arctanrad(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).


build_constraints_aux('"IntegerPart', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:integerpartg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:integerpartg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).
build_constraints_aux('"Floor', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:floorg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:floorg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"Ceiling', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:ceilingg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:ceilingg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"Cos', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:cosrad(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:cosrad(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"Cot', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:cotanrad(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:cotanrad(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"Sinh', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:sinhg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:sinhg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"Cosh', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:coshg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:coshg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"Tanh', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:tanhg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:tanhg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"ArcCos', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:acosrad(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:acosrad(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"ArcCot', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:acotrad(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:acotrad(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"ArcSinh', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:asinhg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:asinhg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"ArcCosh', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:acoshg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:acoshg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"ArcTanh', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:atanhg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:atanhg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

build_constraints_aux('"ArcCoth', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:acothg(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:acothg(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).


build_constraints_aux('"Exp', [Expr], Var, Module, Goal, VarDict, NewVarDict) :-
   replace_evaluatable(Expr, Expr2, [], Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr2, BuiltExpr, VarDict2, VarDict3),
   ( Constraints = []
     -> NewVarDict = VarDict3,
	Goal = Module:expe(BuiltExpr, BuiltVar) 
     ;  Goal = ( Goals, Module:expe(BuiltExpr, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict3, NewVarDict)
   ).

%------------------------------------------------------------------------------
% For Sets
%------------------------------------------------------------------------------

build_constraints_aux('"Inc', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   build_set_term(Expr2, Expr20, TailVar),
   replace_evaluatable(Expr20, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   prolog:term_variables([BuiltExpr1|BuiltExpr2], Vlist),
   ( var(TailVar) 
     -> VarDict5 = VarDict4,
        Goal2 = Module:normalise(Vlist, [BuiltExpr1|BuiltExpr2], BuiltVar) 
     ;  build_term(TailVar, BuiltTailVar, VarDict4, VarDict5), 
        Goal2 = ( Module:plus(set([BuiltExpr1|BuiltExpr2]), BuiltTailVar,
				set(Any)),
	          Module:normalise([Any], Any, BuiltVar) )
   ),
   ( Constraints = []
     -> NewVarDict = VarDict5,
	Goal = Goal2
     ;  Goal = ( Goals, Goal2),
	build_constraints(Constraints, Goals, VarDict5, NewVarDict)
   ).

build_set_term('MetaDefs.CTerm.F1'(_), [], _).	% Null
build_set_term('MetaDefs.Var.F2'(X, Y), [], 'MetaDefs.Var.F2'(X,Y)).  % set tail
build_set_term('MetaDefs.Term.F2'(_, [A, B]), [A|C], TailVar) :-
   build_set_term(B, C, TailVar).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

build_constraints_aux('"\\', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:diff(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:diff(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

%------------------------------------------------------------------------------
% For Strings
%------------------------------------------------------------------------------

build_constraints_aux('"++', [Expr1, Expr2], Var, Module, Goal, VarDict,
	NewVarDict) :-
   replace_evaluatable(Expr1, Expr12, [], Constraints1),
   replace_evaluatable(Expr2, Expr22, Constraints1, Constraints),
   build_term(Var, BuiltVar, VarDict, VarDict2),
   build_term(Expr12, BuiltExpr1, VarDict2, VarDict3),
   build_term(Expr22, BuiltExpr2, VarDict3, VarDict4),
   ( Constraints = []
     -> NewVarDict = VarDict4,
	Goal = Module:concat(BuiltExpr1, BuiltExpr2, BuiltVar) 
     ;  Goal = ( Goals, Module:concat(BuiltExpr1, BuiltExpr2, BuiltVar) ),
	build_constraints(Constraints, Goals, VarDict4, NewVarDict)
   ).

