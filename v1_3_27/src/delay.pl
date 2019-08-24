
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


/*==============================================================================
 * Compile delay declarations.
 *		Author:	Jiwei Wang
 *		Date:	14 August 1991
 *==============================================================================
 */

'$$module'('@(#)delay.pl 1.18 last updated 93/05/18 16:26:15 by jiwei
').

:- op(500, yfx, and).
:- op(400, yfx, or).


/* Method of transformation

Delay declarations:
-------------------

   For a Goedel procedure and delay declaration:
  
   DELAY P(A11, A12, ..., A1n) UNTIL TRUE;
         P(A21, A22, ..., A2n) UNTIL C.
  	 
   	P(...) <- Body1.
   	...	...
   	P(...) <- Bodyn.
  
   If all the Aij are variables (then there should be only one delay),
   it can be transformed to Prolog:
  
   	p(A11, A12, ..., A1n) :-
  		p_0(A11, A12, ..., A1n).
  		% backtracking should be allowed
  	p(A21, A22, ..., A2n) :-
   		goedel_freeze(C, p_0(A21, A22, ..., A2n)).
   
  
   If there is some, say two Aij and Aik, in the delay declaration which are
   not variables, the procedure can be transformed to:
   
   	p(X, Y, ..., Z) :-
  		goedel_freeze(nonvar(Aij) and nonvar(Aik), p_1(X, Y, ..., Z)).
  
   	p_1(A11, A12, ..., A1n) :-
  		p_0(A11, A12, ..., A1n).
  		% backtracking should be allowed
  	p_1(A21, A22, ..., A2n) :-
   		goedel_freeze(C2, p_0(A21, A22, ..., A2n)).
  
  
   And the procedure itself should always be transformed to:
  
  	p_0(...) :- body1.
  	...	...
  	p_0(...) :- bodyn.

   Goals in bodyi are not altered.

   Note the structures in the delay declaration can be much deeper.  Variables
   for such structures should be waited till nonvar. More than one layer of
   the transformed structure may be built.

*/


:- dynamic special_delay/1.

/*------------------------------------------------------------------------------
 * compile_delays(Delays, ModuleName)
 *------------------------------------------------------------------------------
 */

compile_delays([], _):- !.
compile_delays(Delays, ModuleName):-
   build_delays(Delays, BuiltDelays),
   retractall(special_delay),
   analyse_delays(BuiltDelays, 0, ModuleName),
   handle_special_delay.	% create delay clause to capture failures


/*------------------------------------------------------------------------------
 * analyse_delays(BuiltDelays, Layer, ModuleName)
 * This solves the general form of delay declaration, lays a layer of
 * goedel_freeze if necessary.
 *------------------------------------------------------------------------------
 */

analyse_delays([], _, _).
analyse_delays([Delay|Delays], Layer, ModuleName):-
   Delay = 'ProgDefs.Delay.F2'('MetaDefs.Atom.F2'(Predicate, Argus), _), 
   analyse_argus_struct(Argus, NewArgus, DelayVars, Layer),
   ( DelayVars = []
     -> Layer2 = Layer
     ;  Layer2 is Layer + 1,
	name(Layer2, Postfix2),
	( DelayVars = [Var]
	  -> NewCond = nonvar(Var)
	  ;  NewCond = nonvar_and(DelayVars)
	),
	( Layer = 0
	  -> generate_delay_clauses(NewCond, Predicate, NewArgus,
					[], Postfix2, ModuleName)
	  ;  name(Layer, Postfix1),
	     generate_delay_clauses(NewCond, Predicate, NewArgus,
					Postfix1, Postfix2, ModuleName)
	)
   ),
   analyse_delays_aux([Delay|Delays], DeeperDelays, Layer2, ModuleName),
   ( DeeperDelays = []
     -> true
     ;  analyse_delays(DeeperDelays, Layer2, ModuleName)
   ).

/*------------------------------------------------------------------------------
 * analyse_delays_aux(BuiltDelays, DeeperDelays, Layer, ModuleName)
 * This decides if a delay declaration belongs to a certain structure layer.
 * If it does, it is output, otherwise it's kept in DeeperDelays.
 *------------------------------------------------------------------------------
 */

analyse_delays_aux([], [], _, _).
analyse_delays_aux([Delay|Delays], DeeperDelays, Layer, ModuleName):-
   Delay = 'ProgDefs.Delay.F2'('MetaDefs.Atom.F2'(Name, Argus), Condition),
   analyse_argus_struct(Argus, _, NewCondition, Layer),
   ( NewCondition = []
     -> DeeperDelays = DeeperDelays2,
	( Layer = 0
	  -> Postfix = []
          ;  name(Layer, Postfix)
	),
        generate_delay_clauses(Condition, Name, Argus, Postfix, [0'0],
		ModuleName)
     ;  DeeperDelays = [Delay|DeeperDelays2]
   ),
   analyse_delays_aux(Delays, DeeperDelays2, Layer, ModuleName).


/*------------------------------------------------------------------------------
 */

generate_delay_clauses(Condition, Name, Argus, Postfix1, Postfix2, ModuleName):-
   ( create_tracer
     -> generate_delay_clauses_trace(Condition, Name, Argus, Postfix1, Postfix2,
		ModuleName)
     ;  generate_delay_clauses_normal(Condition, Name, Argus, Postfix1,
		Postfix2, ModuleName)
   ).

%------------------------------------------------------------------------------

generate_delay_clauses_normal(Condition, Name, Argus, Postfix1, Postfix2,
		ModuleName):-
   make_flat_name(Name, Predicate1, [], Postfix1),
   Head =.. [Predicate1|Argus],
   make_flat_name(Name, Predicate2, [], Postfix2),
   Body =.. [Predicate2|Argus],

   make_flat_name(Name, NegPredicate1, [0'~], Postfix1),
   NegHead =.. [NegPredicate1|Argus],
   make_flat_name(Name, NegPredicate2, [0'~], Postfix2),
   NegBody =.. [NegPredicate2|Argus],

   ( var_list(Argus)
     -> ( Condition = true
          -> Clause = (Head :- Body)
          ;  Clause = (Head :- user:goedel_freeze(Condition, ModuleName:Body))
        ),
        portray_clause(Clause),

	( Condition = true
	  -> NegClause = (NegHead :- NegBody)
  	  ;  NegClause = (NegHead :-
			    user:goedel_freeze(Condition, ModuleName:NegBody))
   	),
   	portray_clause(NegClause)
     ;  % this handles the case when the goal doesn't match any delay decl.
	length(Argus, ArguLength),
	assert(special_delay(Predicate1/ArguLength)),
		% this will be used to capture failed delay conditions

	( Condition = true
	  -> Clause = (Head :- !, Body)
	  ;  Clause = (Head :- !, user:goedel_freeze(Condition,ModuleName:Body))
	),
        portray_clause(Clause),
	( Condition = true
  	  -> NegClause = (NegHead :- !, NegBody)
  	  ;  NegClause = (NegHead :- !,
			    user:goedel_freeze(Condition, ModuleName:NegBody))
        ),
	portray_clause(NegClause)
   ).

/*------------------------------------------------------------------------------
 */

generate_delay_clauses_trace(Condition, Name, Argus, Postfix1, Postfix2,
	ModuleName):-
   make_flat_name(Name, Predicate1, [], Postfix1),
   Head =.. [Predicate1|Argus],
   make_flat_name(Name, Predicate2, [], Postfix2),
   Body =.. [Predicate2|Argus],

   make_flat_name(Name, NegPredicate1, [0'~], Postfix1),
   NegHead =.. [NegPredicate1|Argus],
   make_flat_name(Name, NegPredicate2, [0'~], Postfix2),
   NegBody =.. [NegPredicate2|Argus],

% for the tracer
   Name = 'MetaDefs.Name.F4'(GM, GSymbol, _, _),
   gstring2string(GSymbol, Symbol),
   gstring2string(GM, M),

   ( var_list(Argus)
     -> ( Condition = true
          -> Clause = (Head :- Body)
          ;  Clause = (Head :-
		user:trace_suspension(Condition, M:Symbol, Argus, Signal),
	        user:goedel_freeze(Condition,
			( user:trace_wakening(M:Symbol, Argus, Signal),
			  ModuleName:Body )))
        ),
        portray_clause(Clause),

	( Condition = true
	  -> NegClause = (NegHead :- NegBody)
  	  ;  NegClause = (NegHead :-
		user:trace_suspension(Condition, M:Symbol, Argus, Signal),
		user:goedel_freeze(Condition,
			( user:trace_wakening(M:Symbol, Argus, Signal),
			  ModuleName:NegBody )))
   	),
   	portray_clause(NegClause)
     ;  % this handles the case when the goal doesn't match any delay decl.
	length(Argus, ArguLength),
	assert(special_delay(Predicate1/ArguLength)),
		% this will be used to capture failed delay conditions

	( Condition = true
	  -> Clause = (Head :- !, Body)
	  ;  Clause = (Head :- !,
		user:trace_suspension(Condition, M:Symbol, Argus, Signal),
		user:goedel_freeze(Condition,
			( user:trace_wakening(M:Symbol, Argus, Signal),
			  ModuleName:Body )))
	),
        portray_clause(Clause),
	( Condition = true
  	  -> NegClause = (NegHead :- !, NegBody)
  	  ;  NegClause = (NegHead :- !,
		user:trace_suspension(Condition, M:Symbol, Argus, Signal),
		user:goedel_freeze(Condition,
			( user:trace_wakening(M:Symbol, Argus, Signal),
			  ModuleName:NegBody )))
        ),
	portray_clause(NegClause)
   ).

/*------------------------------------------------------------------------------
 */

handle_special_delay :-
   findall(X, retract(special_delay(X)), List),
   sort(List, List2),
   handle_special_delay_aux(List2).

handle_special_delay_aux([]).
handle_special_delay_aux([Predicate/Arity|List]) :-
   functor(Head, Predicate, Arity),
   Head =.. [Predicate|VarArgus],
   make_flat_name(Name, Predicate, _, Postfix),
   make_flat_name(Name, Predicate2, [], [0'0]),
   Body =.. [Predicate2|VarArgus],
   portray_clause( (Head :- Body) ),

   make_flat_name(Name, Predicate3, [0'~], Postfix),
   NegHead =.. [Predicate3|VarArgus],
   make_flat_name(Name, Predicate4, [0'~], [0'0]),
   NegBody =.. [Predicate4|VarArgus],
   portray_clause( (NegHead :- NegBody) ),

   handle_special_delay_aux(List).


/*------------------------------------------------------------------------------
 */

analyse_argus_struct([], [], [], _) :- !.
analyse_argus_struct(Argus, NewArgus, DelayVars, 0):- !,
   analyse_argus_var(Argus, NewArgus, DelayVars).

analyse_argus_struct([Argu|Argus], [NewArgu|NewArgus], DelayVars, Layer):-
   nonvar(Argu), !,
   down_to_the_layer(Layer, Argu, NewArgu, DelayVars0),
   analyse_argus_struct(Argus, NewArgus, DelayVars1, Layer),
   append(DelayVars0, DelayVars1, DelayVars).
analyse_argus_struct([Argu|Argus], [Argu|NewArgus], Rest, Layer):-
   analyse_argus_struct(Argus, NewArgus, Rest, Layer).


analyse_argus_var([], [], []).
analyse_argus_var([Argu|Argus], [NewArgu|NewArgus], [NewArgu|Rest]):-
   nonvar(Argu), !,
   analyse_argus_var(Argus, NewArgus, Rest).
analyse_argus_var([Argu|Argus], [Argu|NewArgus], Rest):-
   analyse_argus_var(Argus, NewArgus, Rest).


down_to_the_layer(1, NonVarArgu, NewArgu, DelayVars):-  !,
   NonVarArgu =.. [H|T],
   analyse_argus_var(T, NewT, DelayVars),
   NewArgu =.. [H|NewT].
down_to_the_layer(Layer, NonVarArgu, NewArgu, DelayVars):-
   Layer > 1,
   NonVarArgu =.. [H|T],
   ( T = []
     -> NewArgu = NonVarArgu,
	DelayVars = []
     ;  Layer2 is Layer - 1,
        analyse_argus_struct(T, NewArgus, DelayVars, Layer2),
        NewArgu =.. [H|NewArgus]
   ).


/*------------------------------------------------------------------------------
 * build_delays(Delays)
 *------------------------------------------------------------------------------
 */

build_delays([], []).

build_delays(['ProgDefs.Delay.F2'('MetaDefs.PAtom.F1'(_), _)|Delays],
		BuiltDelays) :-
   !,
   build_delays(Delays, BuiltDelays).

build_delays(['ProgDefs.Delay.F2'('MetaDefs.Atom.F2'(Name, Argus), Condition)
		|Delays],
		 ['ProgDefs.Delay.F2'('MetaDefs.Atom.F2'(Name, BuiltArgus),
			BuiltCondition)|BuiltDelays]) :-
   build_term(Argus, BuiltArgus, [], VarDict),
   build_delay_condition(Condition, BuiltCondition2, VarDict, _),
   optimise_condition(BuiltCondition2, BuiltCondition),
   build_delays(Delays, BuiltDelays).


/*------------------------------------------------------------------------------
 * build_delay_condition(Condition, BuiltCondition, VarDict, NewVarDict)
 *------------------------------------------------------------------------------
 */
 
build_delay_condition('ProgDefs.TrueCond.C0', true, VarDict, VarDict).

build_delay_condition('ProgDefs.Nonvar.F1'(Var), nonvar(BuiltVar), VarDict,
		NewVarDict ):-
   update_variable_dictionary(Var, BuiltVar, VarDict, NewVarDict).

build_delay_condition('ProgDefs.Ground.F1'(Var), ground([BuiltVar]), VarDict,
		NewVarDict) :-
   update_variable_dictionary(Var, BuiltVar, VarDict, NewVarDict).

build_delay_condition('ProgDefs.And.F2'(Cond1, Cond2), BuiltCond1 and BuiltCond2,
		VarDict, NewVarDict) :-
   build_delay_condition(Cond1, BuiltCond1, VarDict, VarDict1),
   build_delay_condition(Cond2, BuiltCond2, VarDict1, NewVarDict).

build_delay_condition('ProgDefs.Or.F2'(Cond1, Cond2), BuiltCond1 or BuiltCond2,
		VarDict, NewVarDict) :-
   build_delay_condition(Cond1, BuiltCond1, VarDict, VarDict2),
   build_delay_condition(Cond2, BuiltCond2, VarDict2, NewVarDict).


/*------------------------------------------------------------------------------
 * optimise_condition(Condition, NewCondition)
 *------------------------------------------------------------------------------
 */

optimise_condition(Condition1 or Condition2, NewCondition1 or NewCondition2):-
   !,
   optimise_condition(Condition1, NewCondition1),
   optimise_condition(Condition2, NewCondition2).

optimise_condition(Condition, NewCondition):-
   ground_and(Condition, NewCondition), !.

optimise_condition(Condition, Condition).

% nonvar_or and nonvar_and optimization are omitted

%nonvar_or(nonvar(X) or nonvar(Y), nonvar_or([X,Y])) :- !.
%nonvar_or(Condition or nonvar(X), nonvar_or([X|Y])) :-
%   nonvar_or(Condition, nonvar_or(Y)).

%nonvar_and(nonvar(X) and  nonvar(Y), nonvar_and([X,Y])) :- !.
%nonvar_and(Condition and nonvar(X), nonvar_and([X|Y])) :-
%   nonvar_and(Condition, nonvar_and(Y)).

ground_and(Condition, ground(VList)) :-
   ground_and_aux(Condition),
   prolog:term_variables(Condition, VList).

ground_and_aux(ground(_)).
ground_and_aux(Cond1 and Cond2) :-
   ground_and_aux(Cond1),
   ground_and_aux(Cond2).

