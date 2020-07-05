%% :- module(compiler, []).
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


/*------------------------------------------------------------------------------
 * A program which compiles parsed Goedel programs into SICStus.
 *	Author: Jiwei Wang
 *	Date: 	23 January 1992
 *------------------------------------------------------------------------------
 */

%% '$$module'('@(#)compiler.pl 1.58 last updated 94/04/18 22:34:11 by jiwei').


/* Methods of transformation:
============================

Plain Goedel programs:
----------------------

   Transformation for plain Goedel programs (without delay declarations and
   commit) is straight-away.  A procedure P:
   	P(...) <- Body1.
   	...	...
   	P(...) <- Bodyn.

   is transformed to:
   	p(...) :- Body1.
   	...	...
   	p(...) :- Bodyn.


Delay declarations:
-------------------
   See delay.pl


Bar commit:
-----------

A bar commit in a clause body is transformed in the following way:
   1. If the scope of a bar commit is empty, it's replaced by a cut.
   2. When the scope of a bar commit is not empty, and the program is of
      the form:

	P(...) <-  Q | R.  where Q and R are sequence of sub-goals.

      it is transformed to:
	p(...) :-
		call_residue(q, Residue),
		( Residue = []
		  -> true
		  ;  flounder_commit(Residue)
		),
		!,
		r.


Negation:
---------

   Another copy of the program should be provided with bar-commits removed
   for running negations.


One Solution Operator:
----------------------

   Goedel program:
	P(...):- { Q } R.
   should be transformed to:
	p_1(..):-
		one_solution(q), r.


IF_THEN and IF_SOME_THEN constructs: 
------------------------------------

   IF C THEN S1 ELSE S2 is transformed to:
	goedel_freeze(ground(C), (C -> S1 ; S2) ).


   IF SOME VLIST C THEN S1 ELSE S2 is transformed to:
	goedel_freeze(ground(FreeVarList), if(C, S1, S2))
   where FreeVarList = freevar(C) - VLIST

   Note that C should use the version of program which have commits switched
   off.

*/


:- op(500, yfx, and).
:- op(400, yfx, or).

/*------------------------------------------------------------------------------
 * compile_program(Program, ModuleName)
 * note that the AVLtree structure is pull apart here.  Violating the
 * modulality.
 *------------------------------------------------------------------------------
 */
 
compile_program(Program, ModuleName):-
   retractall(there_is_error),
   string2Gstring(ModuleName, GModuleName),
   format(user_output, 'Compiling module "~a" ...~n', [ModuleName]),
%   trace, % DENNIS
   Program = 'ProgDefs.Program.F4'(_, ModuleDefAVL, Language,
					StatementsAndDelays),
   'AVLTrees.AVLSearch.P3'(StatementsAndDelays, GModuleName, ModuleCode),
   ModuleCode = 'ProgDefs.Code.F2'(_, Code),
		% takes out the statements and delays of the module.
   'AVLTrees.AVLSearch.P3'(ModuleDefAVL, GModuleName, ModuleDef),
		% takes out the module definition of the module.
   Language = 'ProgDefs.Language.F1'(ModuleDescriptorAVL),
   'AVLTrees.AVLSearch.P3'(ModuleDescriptorAVL, GModuleName, ModuleDescriptor),
		% takes out the module descriptor of the module.
   declared_but_undefined(ModuleDescriptor, Code, PredicateSpecs),
		% check predicates which are declared but not defined
   ( PredicateSpecs = []
     -> true
     ;  format(user_error, '~nWarning: predicates ~w are declared but not defined.~n', [PredicateSpecs])
   ),
   compile_program_aux(ModuleName, Code, ModuleDef, ModuleDescriptor, ordinary).

%------------------------------------------------------------------------------

compile_script(Script, ModuleName):-
   retractall(there_is_error),
   format(user_output, 'Compiling script "~a" ...~n', [ModuleName]),
   Script = 'Scripts.Script.F5'(_ModuleName, ModuleDefAVL, Language,
					_GoalLanguage, CodeAVL),
   compile_program_aux(ModuleName, CodeAVL, ModuleDefAVL, Language,
	script).
		% note that the Language is passed on as ModuleDescriptor.
		% it was GoalLanguage.

%------------------------------------------------------------------------------
% When Switch = script, Code is a AVLTree of AVLTrees, otherwise it's
% an AVLTree of List of clause definitions.

compile_program_aux(ModuleName, Code, ModuleDef, ModuleDescriptor, Switch) :-
   sappend(ModuleName, '.pl', FileName2),
   sappend('GL/',FileName2, FileName),
   ( tell(FileName)
     -> ( is_runtime_system	% a switch for the runtime system
	  -> format('module(~q).~n', [ModuleName])
	  ;  format(':- module(~q, []).~n~n', [ModuleName]),
	     format(':- op(500, yfx, and).~n:- op(400, yfx, or).~n~n', [])
	),
        general_compile_module(Code, ModuleName, Switch),
        told,
	( there_is_error
	-> true
	    %sappend('rm ', FileName, UnixComm),
	    % unix(system(UnixComm))
	        % DOS/PC version should have these two lines replaced by "true"
	; true,  %%( sappend(ModuleName, '.sup', SupFile),
	    %% djd_cat_1(SupFile, FileName, 'djd_work')),

/*	    sappend(ModuleName, '.sup', SupFile),
	     ( open(SupFile, read, SupStream, [type(binary)])
	       -> close(SupStream),
		  sappend(' >> ', FileName, CommString1),
		  sappend(SupFile, CommString1, CommString2),
		  sappend('cat ', CommString2, CommString),
		  unix(system(CommString))
	       ;  true
	     ), 
	     ( is_runtime_system     % a switch for the runtime system
	       -> true
	       ;  fcompile(FileName)	% creating quick load format
	     ), */
	format(user_output, 'Module "~a" compiled.~n', [ModuleName]),
	    
	    sappend(ModuleName, '.lng', LanFileName2),
	    sappend('GL/', LanFileName2, LanFileName),
             ( open(LanFileName, write, Stream, [type(text)])
               -> string2Gstring(ModuleName, GModuleName),
		  ( system_module_name(GModuleName)
		    -> ( open_system_module(GModuleName)
			 -> Code = Code2
			 ;  'ParserPrograms.ExportDelaysOnly.P2'(Code, Code2)
		       ),
		       write_canonical(Stream, system_module(GModuleName,
		       		ModuleDef, ModuleDescriptor, Code2) ),
		       format(Stream, '.~n', [])
		    ;  file_version(V),
		       format(Stream, 'version(''~a'', ~a).~n', [V, Switch]),
		       write_canonical(Stream, ModuleDef),
	               format(Stream, '.~n', []),
                       write_canonical(Stream, ModuleDescriptor),
	               format(Stream, '.~n', [])
		  ),
		 close(Stream)
               ;  format(user_error,'~nError: cannot open file "~a".~n', [LanFileName])
	     )
	)
   ;  format(user_error, '~nError: cannot open file "~a".~n', [FileName])
   ).

%------------------------------------------------------------------------------

general_compile_module(Code, ModuleName, Switch) :-
   ( Switch = script 
     -> make_script_code(Code, ModuleName, Switch)
     ;  compile_module(Code, ModuleName, Switch)
   ).

%------------------------------------------------------------------------------

make_script_code('AVLTrees.Null.C0', _, _).
make_script_code('AVLTrees.Node.F5'(LeftTree, _, ModuleCode, _,RightTree),
		ModuleName, Switch) :-
   ModuleCode = 'ProgDefs.Code.F2'(_, Code),
		% takes out the statements and delays of the module.
   compile_module(Code, ModuleName, Switch),
   make_script_code(LeftTree, ModuleName, Switch),
   make_script_code(RightTree, ModuleName, Switch).


/*-----------------------------------------------------------------------------
 * compile_goal(Goal, BuiltGoal, FreeVars, VarDict) 
 *-----------------------------------------------------------------------------
 * entry point of the toplevel query evaluation
 */

compile_goal(Goal, BuiltGoal, FreeVars, VarDict) :-
   underscore_init,
   ( trace_is_on
     -> assert(create_tracer)
     ;  true
   ),
   transform_body(Goal, TransformedBody, FreeVars),
   compile_formula(TransformedBody, BuiltGoal, [], VarDict, []),
   retractall(create_tracer).

/*-----------------------------------------------------------------------------
 * compile_object_module(GModuleName, Code)
 *-----------------------------------------------------------------------------
 * entry point of Succeed/Fail etc.
 */

compile_object_module(GModuleName, Code):-
   retractall(there_is_error),
   retractall(create_tracer),		% just to make sure no create_tracer
					% which is not normally there.
   gstring2string(GModuleName, ModuleName),
   sappend('%%', ModuleName, ModuleName2),
   sappend(ModuleName, '.pl', FileName2),
   sappend('GL/', FileName2, FileName),
   ( tell(FileName)
     -> ( is_runtime_system
	  -> format('module(~q).~n', [ModuleName2])
	  ;  format(':- module(~q, []).~n~n', [ModuleName2]),
	     format(':- op(500, yfx, and).~n:- op(400, yfx, or).~n~n', [])
	),
        compile_module(Code, ModuleName, object),
        told,
	( there_is_error
	  -> raise_exception(catch_in_query)
	  ;  ( is_runtime_system
	       -> my_load(ModuleName2)
	       ;  compile(FileName)
	     )
	),
	sappend('rm ', FileName, UnixComm),
	unix(system(UnixComm))
	        % DOS/PC version should have these two lines removed.
   ;  format(user_error, '~nError: cannot open file "~a".~n', [FileName]),
        raise_exception(catch_in_query)
   ).

/*-----------------------------------------------------------------------------
 * compile_object_goal(Goal, BuiltGoal, FreeVars, VarDict) 
 *-----------------------------------------------------------------------------
 * entry point of the toplevel query evaluation
 */

compile_object_goal(Goal, BuiltGoal, FreeVars, VarDict, Prefix) :-
   transform_body(Goal, TransformedBody, FreeVars),
   compile_formula(TransformedBody, BuiltGoal2, [], VarDict, Prefix),
   object_module_prefix(BuiltGoal2, BuiltGoal).

/*------------------------------------------------------------------------------
 */

declared_but_undefined(ModuleDescriptor, Code, PredicateSpecs) :-
   ModuleDescriptor = 'ProgDefs.Module.F3'(_,
			'ProgDefs.Categories.F2'(_, Symbols), _),
   declared_symbols(Symbols, Code, [], PredicateSpecs).

declared_symbols('AVLTrees.Null.C0', _, PredicateSpecs, PredicateSpecs).
declared_symbols('AVLTrees.Node.F5'(LeftTree, Symbol, Decls, _, RightTree),
		Code, PredicateSpecs, PredicateSpecs2) :-
   ( 'AVLTrees.AVLSearch.P3'(Code, Symbol, Results)
     -> true
     ;  Results = []
   ),
   gstring2string(Symbol, S),
   find_not_defined(Decls, Results, S, PredicateSpecs, PredicateSpecs3), 
   declared_symbols(LeftTree, Code, PredicateSpecs3, PredicateSpecs4),
   declared_symbols(RightTree, Code, PredicateSpecs4, PredicateSpecs2).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * This finds out not-defined proposition and predicates.
 */

find_not_defined([], _, _, PredicateSpecs, PredicateSpecs).
find_not_defined(['ProgDefs.Symbol.F2'(_, Decl)|Decls], Statements, S,
		PredicateSpecs, PredicateSpecs2) :-
   find_not_defined_aux(Decl, Statements, S, PredicateSpecs, PredicateSpecs3),
   find_not_defined(Decls, Statements, S, PredicateSpecs3, PredicateSpecs2).

% Base and constructor symbols won't appear here
find_not_defined_aux('ProgDefs.ConstantDecl.F1'(_), _, _, PredicateSpecs,
	PredicateSpecs).
find_not_defined_aux('ProgDefs.FunctionDecl.F4'(_,_,_,_), _, _, PredicateSpecs,
	PredicateSpecs).

find_not_defined_aux('ProgDefs.PropositionDecl.C0', Statements, S,
	PredicateSpecs, PredicateSpecs2) :-
   ( member('ProgDefs.PredDef.F4'(0, Formulae, _, _), Statements),
     Formulae \== []
     -> PredicateSpecs2 = PredicateSpecs
     ;  PredicateSpecs2 = [S/0|PredicateSpecs]
   ).

find_not_defined_aux('ProgDefs.PredicateDecl.F3'(Arity, _, _), Statements, S,
	PredicateSpecs, PredicateSpecs2) :-
   ( member('ProgDefs.PredDef.F4'(Arity, Formulae, _, _), Statements),
     Formulae \== []
     -> PredicateSpecs2 = PredicateSpecs
     ;  PredicateSpecs2 = [S/Arity|PredicateSpecs]
   ).

/*------------------------------------------------------------------------------
 */

make_program(Program):-
   Program = 'ProgDefs.Program.F4'(_, _, _, StatementsAndDelays),
   make_program_aux(StatementsAndDelays, Program).

make_program_aux('AVLTrees.Null.C0', _).
make_program_aux('AVLTrees.Node.F5'(LeftTree, GModuleName,
		'ProgDefs.Code.F2'(_, Code), _, RightTree), Program) :-
   ( system_module_name(GModuleName)
     -> true
     ;  Program = 'ProgDefs.Program.F4'(_, ModuleDefAVL, Language, _),
        Language = 'ProgDefs.Language.F1'(ModuleDescriptorAVL),
	'AVLTrees.AVLSearch.P3'(ModuleDescriptorAVL, GModuleName,
					ModuleDescriptor),
		% takes out the module descriptor of the module.
        'AVLTrees.AVLSearch.P3'(ModuleDefAVL, GModuleName, ModuleDef),
		% takes out the module definition of the module.
        gstring2string(GModuleName, ModuleName),
        format(user_output, 'Compiling module "~a" ...~n', [ModuleName]),
        compile_program_aux(ModuleName, Code, ModuleDef, ModuleDescriptor,
			ordinary)
   ),
   make_program_aux(LeftTree, Program),
   make_program_aux(RightTree, Program).

/*-----------------------------------------------------------------------------
 * compile_module(AVLTree, ModuleName, Switch)
 * Switch can be:
 *	ordinary - compiling an ordinary program
 *	object   - compiling an object program from Succeed/Fail
 *	script   - compiling a script
 *-----------------------------------------------------------------------------
 */

compile_module('AVLTrees.Null.C0', _, _).

compile_module('AVLTrees.Node.F5'(LeftTree, _, PredicateDef, _, RightTree),
		ModuleName, Switch) :- 
   compile_predicates(PredicateDef, ModuleName, Switch),
   compile_module(LeftTree, ModuleName, Switch),
   compile_module(RightTree, ModuleName, Switch).
   
/*------------------------------------------------------------------------------
 * compile_predicates(PredicateDef, ModuleName, Switch)
 *------------------------------------------------------------------------------
 */
 
compile_predicates([], _, _).

compile_predicates(['ProgDefs.PredDef.F4'(_, Statements, ExpDelays, LocDelays)
	            | PredicateDef], ModuleName, Switch) :-
   append(ExpDelays, LocDelays, Delays),
   order_statements(Statements, Statements2),
   ( Delays = []
     -> compile_statement_list(Statements2, without_delay, ModuleName, Switch)
     ;  ( Statements2 = []
     	  -> true
	  ;  ( Switch = object
	       -> sappend('%%', ModuleName, ModuleName2),
		  compile_delays(Delays, ModuleName2)
	       ;  compile_delays(Delays, ModuleName)
			% include the case of compiling scripts
	     ),
             compile_statement_list(Statements2, with_delay, ModuleName, Switch)
	)
   ),
   compile_predicates(PredicateDef, ModuleName, Switch).

/*------------------------------------------------------------------------------
 * order_statements(Statements, NewStatements)
 * this puts statements of no commits in front of those with commits in the
 * context of a procedure
 *------------------------------------------------------------------------------
 */

order_statements(Statements, NewStatements) :-
  sieve_statements(Statements, WithCommit, WithoutCommit),
  append(WithCommit, WithoutCommit, NewStatements2),
  reverse(NewStatements2, NewStatements).

sieve_statements([], [], []).
sieve_statements([Statement|Statements], WithCommit, WithoutCommit) :-
   ( contains_commit(Statement)
     -> WithCommit = [Statement|WithCommit2],
	sieve_statements(Statements, WithCommit2, WithoutCommit)
     ;  WithoutCommit = [Statement|WithoutCommit2],
	sieve_statements(Statements, WithCommit, WithoutCommit2)
   ).

contains_commit('MetaDefs.<-''.F2'(_, Body)):-
   contains_commit_aux(Body).

contains_commit_aux('MetaDefs.Commit.F2'(_, _)).
contains_commit_aux('MetaDefs.&''.F2'(Left, Right)):- 
   ( contains_commit_aux(Left);
     contains_commit_aux(Right)
   ), !.

/*------------------------------------------------------------------------------
 * compile_statement_list(StatementList, DelayList, ThereIsDelay, ModuleName,
				Switch)
 * ThereIsDelay can have one of the two values:
 *		without_delay or with_delay 
 *------------------------------------------------------------------------------
 */
 
compile_statement_list([], _, _, _).
compile_statement_list([Statement|Statements], ThereIsDelay, ModuleName,
	Switch):-
   compile_statement(Statement, ThereIsDelay, ModuleName, Switch),
   compile_statement_list(Statements, ThereIsDelay, ModuleName, Switch).

/*------------------------------------------------------------------------------
 * compile_statement(Statement, ThereIsDelay, ModuleName, Switch)
 *------------------------------------------------------------------------------
 */

compile_statement('MetaDefs.<-''.F2'(HeadAtom, Body), ThereIsDelay,
		ModuleName, Switch) :-
   transform_body(HeadAtom, TrdHeadAtom, _),
   transform_body(Body, TransformedBody, _),
   ( ThereIsDelay = without_delay
     -> build_head(TrdHeadAtom, BuiltHead, VarDict, [], [], Constraints)
     ;  build_head(TrdHeadAtom, BuiltHead, VarDict, [], [0'0], Constraints)
   ),
   build_constraints(Constraints, BuiltConstraints3, VarDict, VarDict3),
   compile_formula(TransformedBody, BuiltBody3, VarDict3, _, []),
   remove_redundant(BuiltBody3, BuiltBody5, ModuleName),
   ( Switch = object
     -> object_module_prefix(BuiltBody5, BuiltBody),
        object_module_prefix(BuiltConstraints3, BuiltConstraints)
     ;  ( Switch = script
	  -> script_module_prefix(BuiltBody5, BuiltBody, ModuleName),
	     script_module_prefix(BuiltConstraints3, BuiltConstraints,
			ModuleName)
	  ;  BuiltBody = BuiltBody5,
   	     BuiltConstraints = BuiltConstraints3
	)
   ),
   ( BuiltBody = true,
     BuiltConstraints = null
     -> portray_clause(BuiltHead)
     ;  ( BuiltConstraints = null
	  -> portray_clause( (BuiltHead :- BuiltBody) )
	  ;  portray_clause( (BuiltHead :- BuiltConstraints, BuiltBody) )
	)
   ),

% generate a copy for processing negation as failure
   ( ThereIsDelay = without_delay
     -> build_head(TrdHeadAtom, BuiltHead2, VarDict2, [0'~], [], Constraints2)
     ;  build_head(TrdHeadAtom, BuiltHead2, VarDict2, [0'~], [0'0], Constraints2)
   ),
   build_constraints(Constraints2, BuiltConstraints4, VarDict2, VarDict4),
   compile_formula(TransformedBody, BuiltBody4, VarDict4, _, [0'~]),
   remove_redundant(BuiltBody4, BuiltBody6, ModuleName),
   ( Switch = object
     -> object_module_prefix(BuiltBody6, BuiltBody2),
        object_module_prefix(BuiltConstraints4, BuiltConstraints2)
     ;  ( Switch = script
	  -> script_module_prefix(BuiltBody6, BuiltBody2, ModuleName),
	     script_module_prefix(BuiltConstraints4, BuiltConstraints2,
			ModuleName)
	  ;  BuiltBody2 = BuiltBody6,
             BuiltConstraints2 = BuiltConstraints4
	)
   ),
   ( BuiltBody2 = true,
     BuiltConstraints2 = null
     -> portray_clause(BuiltHead2)
     ;  ( BuiltConstraints2 = null
          -> portray_clause( (BuiltHead2 :- BuiltBody2) )
	  ;  portray_clause( (BuiltHead2 :- BuiltConstraints2, BuiltBody2) )
	)
   ).

/*------------------------------------------------------------------------------
 * build_head(+Head, -BuiltHead, -VarDict, +Prefix, +Postfix, -Constraints)
 *------------------------------------------------------------------------------
 */
 
% this head can not be True and False
build_head('MetaDefs.PAtom.F1'(Name), BuiltHead, [], Prefix, Postfix, []) :-
   make_flat_name(Name, BuiltHead, Prefix, Postfix).


build_head('MetaDefs.Atom.F2'(Name, Argus), BuiltHead, VarDict, Prefix, Postfix,
		Constraints) :-
   make_flat_name(Name, FlatPredicate, Prefix, Postfix),
   term_with_constraints(Argus, BuiltTermList, [], VarDict, Constraints),
		% no constraints is allowed in the head
   BuiltHead =.. [FlatPredicate|BuiltTermList].

/*------------------------------------------------------------------------------
 * compile_formula(Formula, BuiltFormula, VarDict, NewVarDict, Prefix)
 *------------------------------------------------------------------------------
 */
  
% empty formula is compiled as 'true' 
compile_formula('MetaDefs.Empty.C0', true, VarDict, VarDict, _).

compile_formula('MetaDefs.PAtom.F1'('MetaDefs.Name.F4'('"', '"True',
	'MetaDefs.Proposition.C0', 0)), Goal, VarDict, VarDict, _):- !,
   ( create_tracer
     -> Goal = ( user:trace_call('True', [], N),
		 true,
		 user:trace_exit('True', [], N)
	       )
     ;  Goal = true
   ).
compile_formula('MetaDefs.PAtom.F1'('MetaDefs.Name.F4'('"', '"False',
	'MetaDefs.Proposition.C0', 0)), Goal, VarDict, VarDict, _):- !,
   ( create_tracer
     -> Goal = ( user:trace_call('False', [], N),
		 fail,
		 user:trace_exit('False', [], N)
	       )
     ;  Goal = fail
   ).
compile_formula('MetaDefs.PAtom.F1'(Name), Goal, VarDict,
	VarDict, Prefix):-
   make_flat_name(Name, BuiltPAtom, Prefix, []),
   get_module_name(Name, ModuleName),
   ( create_tracer
     -> Name = 'MetaDefs.Name.F4'(GM, GSymbol, _, _),
	gstring2string(GSymbol, Symbol),
	gstring2string(GM, M),
	Goal = ( user:trace_call(M:Symbol, [], N),
		 ModuleName:BuiltPAtom,
		 user:trace_exit(M:Symbol, [], N)
	       )
     ;  Goal = ModuleName:BuiltPAtom
   ).

compile_formula('MetaDefs.Atom.F2'(Predicate, Argus), Goal, VarDict, NewVarDict,
	Prefix):-
   replace_evaluatable(Argus, Argus2, [], Constraints), 
   build_term(Argus2, BuiltTermList, VarDict, VarDict2),
   ( Predicate == 'MetaDefs.Name.F4'('"', '"~=', 'MetaDefs.Predicate.C0', 2)
     -> term_non_underscore_variables(Argus2, FreeVars, []),
        term_underscore_variables(Argus2, UnderscoreVars, []),
        build_term(FreeVars, BuiltFreeVars, VarDict2, _),
        build_term(UnderscoreVars, BuiltUnderscoreVars, VarDict2, _),
        BuiltTermList = [BuiltTerm1, BuiltTerm2],
	Goal2 = user:not_equal(BuiltUnderscoreVars, BuiltFreeVars,
			BuiltTerm1, BuiltTerm2)

     ;  ( Predicate == 'MetaDefs.Name.F4'('"', '"=', 'MetaDefs.Predicate.C0', 2)
	  -> Goal2 =.. ['='|BuiltTermList]
	  ;  make_flat_name(Predicate, FlatPredicate, Prefix, []),
	     BuiltAtom =.. [FlatPredicate|BuiltTermList],
	     get_module_name(Predicate, ModuleName),
	     Goal2 = ModuleName:BuiltAtom
	)
   ),
   ( create_tracer
     -> Predicate = 'MetaDefs.Name.F4'(GM, GSymbol, _, _),
	gstring2string(GSymbol, Symbol),
	gstring2string(GM, M),
	Goal3 = ( user:trace_call(M:Symbol, BuiltTermList, N),
		  Goal2,
		  user:trace_exit(M:Symbol, BuiltTermList, N)
	        )
     ;  Goal3 = Goal2
   ),
   build_constraints(Constraints, BuiltConstraints, VarDict2, NewVarDict),
   ( BuiltConstraints = null
     -> Goal = Goal3
     ;  Goal = (BuiltConstraints, Goal3)
   ).

compile_formula('MetaDefs.&''.F2'(Formula1, Formula2),
	(BuiltFormula1, BuiltFormula2), VarDict, NewVarDict, Prefix):-
   compile_formula(Formula1, BuiltFormula1, VarDict, VarDict1, Prefix),
   compile_formula(Formula2, BuiltFormula2, VarDict1, NewVarDict, Prefix).

compile_formula('MetaDefs.\\/''.F2'(Formula1, Formula2),
	(BuiltFormula1; BuiltFormula2), VarDict, NewVarDict, Prefix) :-
   compile_formula(Formula1, BuiltFormula1, VarDict, VarDict1, Prefix),
   compile_formula(Formula2, BuiltFormula2, VarDict1, NewVarDict, Prefix).

/* all negative goals use a '~' prefix */
% How are we going to trace negation !
compile_formula('MetaDefs.Neg.F2'(FreeVars, Formula), Goal, VarDict,
	NewVarDict, _) :-
   compile_formula(Formula, BuiltFormula, VarDict, VarDict2, [0'~]),
   ( FreeVars = []
     -> NewVarDict = VarDict2,
	Goal = user:goedel_not(BuiltFormula)
     ;  Goal = user:goedel_freeze(ground(BuiltFreeVars),
			user:goedel_not(BuiltFormula)),
        build_term(FreeVars, BuiltFreeVars, VarDict2, NewVarDict)
   ).

% not clear for the time being
compile_formula('MetaDefs.Commit.F2'(Id, Formula), BuiltFormula, VarDict,
		NewVarDict, Prefix) :-
   ( Prefix = [0'~]
     -> compile_formula(Formula, BuiltFormula, VarDict, NewVarDict, Prefix)
			% commits are switched off in negation
     ;  ( Id = 1000000000	% temporary solution to one-solution commit
          -> BuiltFormula= user:one_solution(BuiltFormula2),
	     compile_formula(Formula, BuiltFormula2, VarDict, NewVarDict,Prefix)
          ;  ( Id = 0		% not sure what to do with this yet
	       -> compile_formula(Formula, BuiltFormula2, VarDict, NewVarDict,
				Prefix),
                  ( BuiltFormula2 = true
		    -> BuiltFormula = !
		    ;  BuiltFormula = ( call_residue(BuiltFormula2, Residue),
				        ( Residue = []
					  -> !
					  ;  user:release_suspended(Residue)
					)
				      )
		  )
	       ;  NewVarDict = VarDict,
		  BuiltFormula = true,
		  assert(there_is_error),
		  format(user_error, "~nSorry: full commits are not supported.~n",
				[]),
		  nl(user_error)
             )
        )
   ).

% true is inserted to the condition part to cope with the order of awakening
% coroutines.

compile_formula('MetaDefs.IfSome.F4'(FreeVars, Condition, Then, Else), 
	user:goedel_freeze(ground(BuiltFreeVars),
			if( (BuiltCondition, true), BuiltThen, BuiltElse)),
	VarDict, NewVarDict, Prefix) :-
   build_term(FreeVars, BuiltFreeVars, VarDict, VarDict1),
   compile_formula(Condition, BuiltCondition, VarDict1, VarDict2, [0'~]),
   compile_formula(Then, BuiltThen, VarDict2, VarDict3, Prefix),
   compile_formula(Else, BuiltElse, VarDict3, NewVarDict, Prefix).

compile_formula('MetaDefs.If.F4'(FreeVars, Condition, Then, Else), 
	user:goedel_freeze(ground(BuiltFreeVars),
			(BuiltCondition, true -> BuiltThen; BuiltElse)),
	VarDict, NewVarDict, Prefix) :-
   build_term(FreeVars, BuiltFreeVars, VarDict, VarDict1),
   compile_formula(Condition, BuiltCondition, VarDict1, VarDict2, [0'~]),
   compile_formula(Then, BuiltThen, VarDict2, VarDict3, Prefix),
   compile_formula(Else, BuiltElse, VarDict3, NewVarDict, Prefix).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

get_module_name('MetaDefs.Name.F4'(GModuleName, _, _, _), ModuleName) :-
   gstring2string(GModuleName, ModuleName).

/*------------------------------------------------------------------------------
 * remove_redundant(BuiltGoal, NewBuiltGoal, ModuleName)
 *   remove module prefix if the goal is in the top module.
 *------------------------------------------------------------------------------
 */

remove_redundant( (Goal1; Goal2), (NewGoal1; NewGoal2), ModuleName):- !,
   remove_redundant(Goal1, NewGoal1, ModuleName),
   remove_redundant(Goal2, NewGoal2, ModuleName).
 
remove_redundant( (Goal1, Goal2), (NewGoal1, NewGoal2), ModuleName):- !,
   remove_redundant(Goal1, NewGoal1, ModuleName),
   remove_redundant(Goal2, NewGoal2, ModuleName).

remove_redundant(ModuleName:Goal, Goal, ModuleName):-  !.
	% this removes redundant module prefix

remove_redundant(Goal, Goal, _).
	% this captures goals in "user" module

/*------------------------------------------------------------------------------
 * object_module_prefix(BuiltGoal, NewBuiltGoal)
 *   This changes the user-defined module prefix by adding '%%' 
 *------------------------------------------------------------------------------
 */

object_module_prefix( (Goal1, Goal2), (NewGoal1, NewGoal2)):-
   !,
   object_module_prefix(Goal1, NewGoal1),
   object_module_prefix(Goal2, NewGoal2).

object_module_prefix( (Goal1; Goal2), (NewGoal1; NewGoal2)):-
   !,
   object_module_prefix(Goal1, NewGoal1),
   object_module_prefix(Goal2, NewGoal2).
 
object_module_prefix( if(Goal1, Goal2, Goal3),
	if(NewGoal1, NewGoal2, NewGoal3) ):-
   !,
   object_module_prefix(Goal1, NewGoal1),
   object_module_prefix(Goal2, NewGoal2),
   object_module_prefix(Goal3, NewGoal3).

object_module_prefix( (Goal1 -> Goal2), (NewGoal1 -> NewGoal2) ):-
   !,
   object_module_prefix(Goal1, NewGoal1),
   object_module_prefix(Goal2, NewGoal2).

object_module_prefix(call_residue(Goal, L), user:call_residue(NewGoal, L)):- !,
   object_module_prefix(Goal, NewGoal).

object_module_prefix(user:flounder_commit(F), user:flounder_commit(F)) :-
   !.

object_module_prefix(user:not_equal(Vars, FreeVars, Term1, Term2),
		user:not_equal(Vars, FreeVars, Term1, Term2)):-
   !.

object_module_prefix(user:one_solution(Goal), user:one_solution(NewGoal)):- !,
   object_module_prefix(Goal, NewGoal).

object_module_prefix(user:goedel_not(Goal), user:goedel_not(NewGoal)):-  !,
   object_module_prefix(Goal, NewGoal).

object_module_prefix(user:goedel_freeze(Vlist, Goal),
		user:goedel_freeze(Vlist, NewGoal)):-  !,
   object_module_prefix(Goal, NewGoal).
 
% this is to capture Goals in intensional set.
object_module_prefix('Sets':set_of(A, Goal, B, C),
		'Sets':set_of(A, NewGoal, B, C) ):- !,
   object_module_prefix(Goal, NewGoal).

 
object_module_prefix(ModuleName:Goal, ModuleName2:Goal):-  !,
   ( string2Gstring(ModuleName, GModuleName),
     system_module_name(GModuleName)
     -> ModuleName2 = ModuleName
     ;  sappend('%%', ModuleName, ModuleName2)
   ).

object_module_prefix(Goal, Goal).
	% to capture goals without prefix, e.g. !, 1=2

/*------------------------------------------------------------------------------
 * script_module_prefix(BuiltGoal, NewBuiltGoal)
 *   This changes the module prefix complying to a script
 *------------------------------------------------------------------------------
 */

script_module_prefix( (Goal1, Goal2), (NewGoal1, NewGoal2), ModuleName):-
   !,
   script_module_prefix(Goal1, NewGoal1, ModuleName),
   script_module_prefix(Goal2, NewGoal2, ModuleName).

script_module_prefix( (Goal1; Goal2), (NewGoal1; NewGoal2), ModuleName):-
   !,
   script_module_prefix(Goal1, NewGoal1, ModuleName),
   script_module_prefix(Goal2, NewGoal2, ModuleName).
 
script_module_prefix( if(Goal1, Goal2, Goal3),
	if(NewGoal1, NewGoal2, NewGoal3), ModuleName ):-
   !,
   script_module_prefix(Goal1, NewGoal1, ModuleName),
   script_module_prefix(Goal2, NewGoal2, ModuleName),
   script_module_prefix(Goal3, NewGoal3, ModuleName).

script_module_prefix( (Goal1 -> Goal2), (NewGoal1 -> NewGoal2), ModuleName):-
   !,
   script_module_prefix(Goal1, NewGoal1, ModuleName),
   script_module_prefix(Goal2, NewGoal2, ModuleName).

script_module_prefix(call_residue(Goal, L), user:call_residue(NewGoal, L),
		ModuleName):- !,
   script_module_prefix(Goal, NewGoal, ModuleName).

script_module_prefix(user:flounder_commit(F), user:flounder_commit(F), _) :-
   !.

script_module_prefix(user:not_equal(Vars, FreeVars, Term1, Term2),
		user:not_equal(Vars, FreeVars, Term1, Term2), _):-
   !.

script_module_prefix(user:one_solution(Goal), user:one_solution(NewGoal),
		ModuleName):- !,
   script_module_prefix(Goal, NewGoal, ModuleName).

script_module_prefix(user:goedel_not(Goal), user:goedel_not(NewGoal),
		ModuleName):-  !,
   script_module_prefix(Goal, NewGoal, ModuleName).

script_module_prefix(user:goedel_freeze(Vlist, Goal),
		user:goedel_freeze(Vlist, NewGoal), ModuleName):-  !,
   script_module_prefix(Goal, NewGoal, ModuleName).
 
% this is to capture Goals in intensional set.
script_module_prefix('Sets':set_of(A, Goal, B, C),
		'Sets':set_of(A, NewGoal, B, C), ModuleName ):- !,
   script_module_prefix(Goal, NewGoal, ModuleName).

 
script_module_prefix(Module:Goal, Module2:Goal, ModuleName):-  !,
   ( string2Gstring(Module, GModule),
     system_module_name(GModule),
     \+ open_system_module(GModule)
     -> Module2 = Module
     ;  Module2 = ModuleName
   ).

script_module_prefix(Goal, Goal, _).
	% to capture goals without prefix, e.g. !, 1=2

/*------------------------------------------------------------------------------
 * build_term(Term, BuiltTerm, VarDict, NewVarDict)
 */
 
build_term([], [], VarDict, VarDict).

build_term([Term|Terms], [BuiltTerm|BuiltTerms], VarDict, NewVarDict):-
   build_term(Term, BuiltTerm, VarDict, VarDict1),
   build_term(Terms, BuiltTerms, VarDict1, NewVarDict).

build_term('MetaDefs.Var.F1'(N), BuiltVariable, VarDict, NewVarDict) :-
   update_variable_dictionary('MetaDefs.Var.F1'(N), BuiltVariable,
		VarDict, NewVarDict).
 
build_term('MetaDefs.Var.F2'(Var, N), BuiltVariable, VarDict, NewVarDict) :-
   update_variable_dictionary('MetaDefs.Var.F2'(Var, N), BuiltVariable,
		VarDict, NewVarDict).
 
build_term('MetaDefs.Int.F1'(Int), Int, VarDict, VarDict).
build_term('MetaDefs.Str.F1'(Str), Str, VarDict, VarDict).
build_term('MetaDefs.Flo.F1'(Flo), Flo, VarDict, VarDict).

build_term('MetaDefs.Prm.F1'(Prm), 'ProgDefs.CachedProgram.F1'(Prm),
   VarDict, VarDict).

build_term('MetaDefs.CTerm.F1'('MetaDefs.Name.F4'('"Lists', '"Nil',
		'MetaDefs.Constant.C0', 0)), [], VarDict, VarDict):- !.
build_term('MetaDefs.CTerm.F1'(Constant), BuiltTerm, VarDict, VarDict):-
   make_flat_name(Constant, BuiltTerm, [], []).

build_term('MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Lists', '"Cons',
		'MetaDefs.Function.C0', 2), [Head, Tail]),
		[BuiltHead|BuiltTail], VarDict, NewVarDict):- !,
   build_term(Head, BuiltHead, VarDict, VarDict2),
   build_term(Tail, BuiltTail, VarDict2, NewVarDict).

build_term('MetaDefs.Term.F2'(Functor, Argus), BuiltTerm, VarDict, NewVarDict):-
   make_flat_name(Functor, FlatFunctor, [], []),
   build_term(Argus, BuiltArgus, VarDict, NewVarDict),
   BuiltTerm =.. [FlatFunctor|BuiltArgus].

/*------------------------------------------------------------------------------
 * term_with_constraints(TermList, BuiltTermList, VarDict, NewVarDict,
 *		Constraints)
 * 	Constraints contains the evaluatable constraints.
 *------------------------------------------------------------------------------
 */

term_with_constraints(TermList, BuiltTermList, VarDict, NewVarDict,
		Constraints) :-
   replace_evaluatable(TermList, TermList2, [], Constraints), 
   build_term(TermList2, BuiltTermList, VarDict, NewVarDict).

/*------------------------------------------------------------------------------
 */

replace_evaluatable([], [], Constraints, Constraints).
replace_evaluatable([Term|TermList], [NewTerm|NewTermList], Constraints,
	Constraints2):-
   replace_evaluatable(Term, NewTerm, Constraints, Constraints3),
   replace_evaluatable(TermList, NewTermList, Constraints3, Constraints2).

replace_evaluatable('MetaDefs.Var.F1'(N),
	'MetaDefs.Var.F1'(N), Constraints, Constraints).

replace_evaluatable('MetaDefs.Var.F2'(Var, N),
	'MetaDefs.Var.F2'(Var, N), Constraints, Constraints).

replace_evaluatable('MetaDefs.Int.F1'(Int), 'MetaDefs.Int.F1'(Int),
	Constraints, Constraints).
replace_evaluatable('MetaDefs.Flo.F1'(Flo), 'MetaDefs.Flo.F1'(Flo),
	Constraints, Constraints).
replace_evaluatable('MetaDefs.Str.F1'(Str), 'MetaDefs.Str.F1'(Str),
	Constraints, Constraints).
replace_evaluatable('MetaDefs.Prm.F1'(Str), 'MetaDefs.Prm.F1'(Str),
	Constraints, Constraints).
replace_evaluatable('MetaDefs.CTerm.F1'('MetaDefs.Name.F4'(M,S,C,A)),
	NewTerm, Constraints, Constraints2) :-
   ( M = '"Sets'
     -> NewTerm = 'MetaDefs.Var.F2'(String, 0),
	Constraints2 = [pair('MetaDefs.Var.F2'(String, 0),
		'MetaDefs.CTerm.F1'('MetaDefs.Name.F4'(M,S,C,A)))|Constraints],
        underscore_label(L),    % just to get a new string
        sappend('.sys', L, String)
     ;  NewTerm = 'MetaDefs.CTerm.F1'('MetaDefs.Name.F4'(M,S,C,A)),
	Constraints2 = Constraints
   ).

replace_evaluatable('MetaDefs.SuchThat.F3'(Vars,T,W), 'MetaDefs.Var.F2'(String, 0),
	Constraints, [pair('MetaDefs.Var.F2'(String, 0),
			'MetaDefs.SuchThat.F3'(Vars, T, W))|Constraints]) :-
   underscore_label(L),    % just to get a new string
   sappend('.sys', L, String).

replace_evaluatable('MetaDefs.Term.F2'(Functor, Argus), NewTerm, Constraints,
		Constraints2) :-
   ( evaluatable_functor(Functor)
     -> underscore_label(L),	% just to get a new string
	NewTerm = 'MetaDefs.Var.F2'(String, 0),
	Constraints2 = [pair('MetaDefs.Var.F2'(String, 0),
			'MetaDefs.Term.F2'(Functor, Argus)) | Constraints],
	sappend('.sys', L, String)
     ;  NewTerm = 'MetaDefs.Term.F2'(Functor, Argus2),
	replace_evaluatable(Argus, Argus2, Constraints, Constraints2)
   ).


evaluatable_functor('MetaDefs.Name.F4'(Module, _, _, _)) :-
   evaluatable_functor(Module).


evaluatable_functor('"Integers').
evaluatable_functor('"Rationals').
evaluatable_functor('"Sets').
evaluatable_functor('"Strings').

/*------------------------------------------------------------------------------
 * make_flat_name(OName, FlatName, Prefix, Postfix)
 * Prefix and Postfix are lists of Ascii codes.
 * should work for both directions
 * A flat name is of the form:
 * 	{<Prefix>}<ModuleName>.<SymbolName>.<Category&Arity>{.<Postfix>}
 *------------------------------------------------------------------------------
 */

make_flat_name('MetaDefs.Name.F4'(Module, Symbol, Category, Arity), FlatName,
		Prefix, Postfix) :-
   nonvar(Symbol), nonvar(Prefix), nonvar(Postfix), !,
   name(Module, [_|ModuleString]),
   name(Symbol, [_|SymbolString]),
   category_char(Category, Cat),
   name(Arity, ArityString),
   ( Postfix = []
     -> String1 = [Cat|ArityString]
     ;  append([Cat|ArityString], [0'.|Postfix], String1)
   ),
   append(SymbolString, [0'.|String1], String2),
   append(ModuleString, [0'.|String2], String3),
   append(Prefix, String3, FlatString),
   name(FlatName, FlatString).

make_flat_name('MetaDefs.Name.F4'(Module, Symbol, Category, Arity), FlatName,
		Prefix, Postfix) :-
   nonvar(FlatName), !,
   name(FlatName, FlatString),
   split_list(FlatString, 0'., ModuleString, FlatString2),
   split_list(FlatString2, 0'., SymbolString, FlatString3),
   split_list(FlatString3, 0'., [Cat|ArityString], Postfix),
   ( ModuleString = [0'~|MString]
     -> Prefix = [0'~]
     ;  MString = ModuleString,
	Prefix = []
   ),
   name(Module, [0'"|MString]),
   name(Symbol, [0'"|SymbolString]),
   name(Arity , ArityString),
   category_char(Category, Cat).


% category_char(Category, CategoryChar).
category_char('MetaDefs.Base.C0', 0'B).
category_char('MetaDefs.Constructor.C0', 0'S).
category_char('MetaDefs.Constant.C0', 0'C).
category_char('MetaDefs.Function.C0', 0'F).
category_char('MetaDefs.Predicate.C0', 0'P).
category_char('MetaDefs.Proposition.C0', 0'O).

/*------------------------------------------------------------------------------
 * term_non_underscore_variables(Term, Vars, VarsT)
 *------------------------------------------------------------------------------
 */
 
term_non_underscore_variables([], Vs, Vs).

term_non_underscore_variables([Term|Terms], Vs, VsT) :-
   term_non_underscore_variables(Term, Vs, Vs1),
   term_non_underscore_variables(Terms, Vs1, VsT).
 
term_non_underscore_variables('MetaDefs.CTerm.F1'(_), Vs, Vs).
term_non_underscore_variables('MetaDefs.Int.F1'(_), Vs, Vs).
term_non_underscore_variables('MetaDefs.Flo.F1'(_), Vs, Vs).
term_non_underscore_variables('MetaDefs.Str.F1'(_), Vs, Vs).

term_non_underscore_variables('MetaDefs.Term.F2'(_, Argus), Vs, VsT) :-
   term_non_underscore_variables(Argus, Vs, VsT).

term_non_underscore_variables('MetaDefs.Var.F1'(N),
		['MetaDefs.Var.F1'(N)|Vs], Vs).

term_non_underscore_variables('MetaDefs.Var.F2'(GVar, N), Vs2, Vs) :-
   ( name(GVar, [0'",0'_|_])
     -> Vs2 = Vs
     ;  Vs2 = ['MetaDefs.Var.F2'(GVar, N)|Vs]
   ).

/*------------------------------------------------------------------------------
 * term_underscore_variables(Term, Vars, VarsT)
 *------------------------------------------------------------------------------
 */
 
term_underscore_variables([], Vs, Vs).

term_underscore_variables([Term|Terms], Vs, VsT) :-
   term_underscore_variables(Term, Vs, Vs1),
   term_underscore_variables(Terms, Vs1, VsT).
 
term_underscore_variables('MetaDefs.CTerm.F1'(_), Vs, Vs).
term_underscore_variables('MetaDefs.Int.F1'(_), Vs, Vs).
term_underscore_variables('MetaDefs.Flo.F1'(_), Vs, Vs).
term_underscore_variables('MetaDefs.Str.F1'(_), Vs, Vs).

term_underscore_variables('MetaDefs.Term.F2'(_, Argus), Vs, VsT) :-
   term_underscore_variables(Argus, Vs, VsT).

term_underscore_variables('MetaDefs.Var.F1'(N), ['MetaDefs.Var.F1'(N)|Vs], Vs).
 
term_underscore_variables('MetaDefs.Var.F2'(GVar, N), Vs2, Vs) :-
   ( name(GVar, [0'",0'_|_])
     -> Vs2 = ['MetaDefs.Var.F2'(GVar, N)|Vs]
     ;  Vs2 = Vs
   ).

/*------------------------------------------------------------------------------
 * update_variable_dictionary(Variable, NewVariable, Dictionary, NewDictionary)
 *------------------------------------------------------------------------------
 */

update_variable_dictionary('MetaDefs.Var.F1'(N), NewVariable, VarDict,
		NewVarDict) :-
   update_dictionary('MetaDefs.Var.F1'(N), NewVariable, VarDict, NewVarDict).

update_variable_dictionary('MetaDefs.Var.F2'(GVar,N), NewVariable, VarDict,
		NewVarDict) :-
   ( name(GVar, [0'",0'_|_])
			% was Variable == 'MetaDefs.Var.F2'('"_', _)
     -> NewVarDict = VarDict
     ;  update_dictionary('MetaDefs.Var.F2'(GVar,N), NewVariable, VarDict,
				NewVarDict)
   ).

/*------------------------------------------------------------------------------
 * update_dictionary(Item, NewItem, Dictionary, NewDictionary)
 *------------------------------------------------------------------------------
 */
 
update_dictionary(Item, NewItem, Dictionary, NewDictionary) :-
   ( member(Item/NewItem, Dictionary)
     -> NewDictionary = Dictionary
     ;  NewDictionary = [Item/NewItem|Dictionary]
   ).

