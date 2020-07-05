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
File:		formula.pl
Subject:	handling formula
Author: 	Jiwei Wang
Date:		18 November 1991, starting the ambiguous parser.

================================================================================
*/

%% '$$module'('@(#)formula.pl 1.34 last updated 93/05/23 18:24:43 by jiwei').


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Again we use the recursive descent parsing method.
 *
 * The original grammars for formula are transformed by unfolding and assigning
 * precedences to connectives as:

  Formula(8) ->   Proposition
  	  	| PredicateNoInd(n) ( TermSeq(n) )
  	  	| PredicateInd(1,Pz) Term
  	  	| Term PredicateInd(1,zP)
  	  	| Term PredicateInd(2,zPz) Term
  	  	| Term Less Variable Less Term
		| Term Greater Variable Greater Term
		| Term = Term
		| Term ~= Term
		| ~ Formula(8)
  		| SOME [ VariableSeq ] Formula(8) 
  		| ALL [ VariableSeq ] Formula(8) 
  		| ( Formula(f1) )
		| Conditional

  Formula(f) ->   Formula(f2) & Formula(f3)
		| Formula(f2) \/ Formula(f3)
		| Formula(f2) <- Formula(f3)
		| Formula(f2) -> Formula(f3)
		| Formula(f2) <-> Formula(f3)
  Precedence:
		&   : xFy(100);
		\/  : xFy(90); 
		->  : xFy(80);
		<-  : xFy(80);
		<-> : xFx(80);

  Less ->	 < | =<

  Greater ->	 > | >=


 * These grammars can then be rewritten as:
 *
 *	F -> a | F c1 F | F c2 F | F c3 F | F c4 F| F c5 F
 *
 * where a stands for formulas in Formula(8), ci is a connective.
 *
 * The transformed grammars (with left recursion removed) are as follows:
 *
 *	F  -> a F' 
 *	F' -> c1 F F'| c2 F F' | c3 F F' | c4 F F' | c5 F F' | e
 *
 * where e is the empty string.
 * Replace F F' by F (because they are equivalent), we have
 *
 *	F  -> a F' 
 *	F' -> c1 F | c2 F | c3 F | c4 F | c5 F | e
 *
 *
 *	The parser for formulae is very close to that for terms, except that
 * there is no ambiguous connectives.
 */


/*------------------------------------------------------------------------------
 * All predicates in this file should be assume to be always succeed (unless
 * stated) and leave no choice points behind.
 *----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
 * formula(+Tokens, -Return, +Language).
 * formula(+Tokens, -Return, +SpecLeft, +PrecLeft, +Language):-
 *	Token:		one look ahead token
 *	Tokens:		tokens to be parsed
 *	Language:	language of the module
 *	Return:		the returned parse result.  It is a list of:
 *				Stack/Remains or error(A,B)/Remains
 *	  Remains:	tokens left unparsed
 *	  Stack:	a list of: 
 *			stack(Spec, Prec, Hole, ParseTree)
 * 			where Spec and Prec is the indicator of the major
 *			operator in the parse tree, Hole is an unfilled position
 *			in ParseTree.
 *	  ParseTree:	a structure of the form:
 *			see ground representation
 *	  Indicator:	Indicator of the major connective in the formula,
 *			consisting Spec and Prec. Spec can be a specifier and
 *			'null'.
 *			
 *	SpecLeft:	specifier of the immediately left operator, can be
 *			'null' when there is no operator at the left.
 *	PrecLeft:	precedence of the immediately left operator, can be
 *			a positive integer or uninstantiated, indicating
 *			not applicable.
 */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * formula/3
 */

formula(Tokens, Return, Language):-
	formula(Tokens, Return, null, _, Language).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * formula(+Tokens, -Return, +SpecLeft, +PrecLeft, +Language):-
 */

formula([], [], _, _, _).

formula([Token|Tokens], Return, SpecLeft, PrecLeft, Language):-
   ( check_formula_token(Token, SymbolList, Language)
     -> formula_aux(SymbolList, Tokens, Return2, SpecLeft, PrecLeft, Language),
        ( Return2 = []
	  -> length(Tokens, Position),
	     Return = [error('formula expected', [])/Position]
	  ;  Return = Return2
	)
     ;  length(Tokens, Position),
	Return = [error('undeclared or illegal symbol: "~w"', [Token])/Position]
   ).

/*------------------------------------------------------------------------------
 * formula_aux(+SymbolList, +Tokens, -Return, +SpecLeft, +PrecLeft, +Language)
 *	for predicate of no indicator and proposition, Decl is a list
 *
 * error handling in formulas of predicate_no_ind, (Formula):
 *	if there is an valid parsing for this formula, the errors encountered
 *	inside the formula is discarded, otherwise they are returned.
 *
 * Just to speed things up, a list of symbol and an element is not
 * distinguished in formula_aux
 */


formula_aux([], _, [], _, _, _).

formula_aux([Decl|SymbolList], Tokens, Return, SpecLeft, PrecLeft, Language) :-
   formula_aux(Decl, Tokens, Return2, SpecLeft, PrecLeft, Language),
   formula_aux(SymbolList, Tokens, Return3, SpecLeft, PrecLeft, Language),
   append(Return3, Return2, Return).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

formula_aux(proposition(LeftAtoms), Tokens, Return, SpecLeft, PrecLeft,
		Language) :-
   formula_infinite(Tokens, Return, LeftAtoms, SpecLeft, PrecLeft, Language).

formula_aux(term(Token), Tokens, Return, SpecLeft, PrecLeft, Language) :-
   atom(term(Token), Tokens, Remains, LeftAtoms, ErrorReturn, Language),
   ( LeftAtoms = []
     -> Return = ErrorReturn
     ;  formula_infinite(Remains, Return, LeftAtoms, SpecLeft, PrecLeft,
			Language)
   ).

formula_aux(predicate_no_ind(Decls), Tokens, Return, SpecLeft, PrecLeft,
		Language) :-
   atom(predicate_no_ind(Decls), Tokens, Remains, LeftAtoms, ErrorReturn,
		Language),
   ( LeftAtoms = []
     -> Return = ErrorReturn
     ;  formula_infinite(Remains, Return, LeftAtoms, SpecLeft, PrecLeft,
			Language)
   ).

formula_aux(prefix_predicate(Decls), Tokens, Return, SpecLeft, PrecLeft,
		Language) :-
   atom(prefix_predicate(Decls), Tokens, Remains, LeftAtoms, ErrorReturn,
			Language),
   ( LeftAtoms = []
     -> Return = ErrorReturn
     ;  formula_infinite(Remains, Return, LeftAtoms, SpecLeft, PrecLeft,
			Language)
   ).

% This bracket can be a bracket for either a term or a formula 
formula_aux(bracket(Token), Tokens, Return, SpecLeft, PrecLeft, Language) :-
   atom(bracket(Token), Tokens, Remains, LeftAtoms, ErrorReturn, Language),
   ( LeftAtoms = []
     -> Return = ErrorReturn
     ;  formula_infinite(Remains, Return, LeftAtoms, SpecLeft, PrecLeft,
			Language)
   ).

% Formula(0) and Formula(1) are taken as Atoms.
% This is only for the convenience reasons.
formula_aux('~', Tokens, Return, SpecLeft, PrecLeft, Language) :-
   atom('~', Tokens, Remains, LeftAtoms, ErrorReturn, Language),
   ( LeftAtoms = []
     -> Return = ErrorReturn
     ;  formula_infinite(Remains, Return, LeftAtoms, SpecLeft, PrecLeft,
			Language)
   ).

formula_aux('SOME', Tokens, Return, SpecLeft, PrecLeft, Language) :-
   atom('SOME', Tokens, Remains, LeftAtoms, ErrorReturn, Language),
   ( LeftAtoms = []
     -> Return = ErrorReturn
     ;  formula_infinite(Remains, Return, LeftAtoms, SpecLeft, PrecLeft,
			Language)
   ).

formula_aux('ALL', Tokens, Return, SpecLeft, PrecLeft, Language) :-
   atom('ALL', Tokens, Remains, LeftAtoms, ErrorReturn, Language),
   ( LeftAtoms = []
     -> Return = ErrorReturn
     ;  formula_infinite(Remains, Return, LeftAtoms, SpecLeft, PrecLeft,
			Language)
   ).

formula_aux('IF', Tokens, Return, SpecLeft, PrecLeft, Language) :-
   conditional(Tokens, Remains, LeftAtoms, ErrorReturn, Language),
   ( LeftAtoms = []
     -> Return = ErrorReturn
     ;  formula_infinite(Remains, Return, LeftAtoms, SpecLeft, PrecLeft,
			Language)
   ).

% symbols missed out in F
formula_aux(infix_predicate(_), Tokens,
		[error('unexpected infix predicate', [])/Position], _, _, _) :-
   length(Tokens, Position).
formula_aux(postfix_predicate(_), Tokens,
		[error('unexpected postfix predicate', [])/Position], _, _, _):-
   length(Tokens, Position).
formula_aux(end_of_formula(_), _, [], _, _, _).
formula_aux(connective(_, _), Tokens,
		[error('unexpected connective', [])/Position], _, _, _):-
   length(Tokens, Position).

/*------------------------------------------------------------------------------
 * variable_seq sucks in the left and right square brackets
 *	VarList = [] is the error signal.
 */

variable_seq([], [], [], [error('left square bracket expected', [])/0]).
variable_seq([Token|Tokens], Remains, VarList, ErrorReturn) :-
	( Token = '['
	  -> variable_seq_aux(Tokens, Remains2, VarList2, ErrorReturn2),
	     ( ErrorReturn2 = []
	       -> ( Remains2 = [ ']'|Remains ]
	            -> VarList = VarList2,
		       ErrorReturn = ErrorReturn2
	            ;  VarList = [],
		       length(Remains2, Position),
		       ErrorReturn = [error('missing "]"', [])/Position]
	          )
	       ;  VarList = [],
		  ErrorReturn = ErrorReturn2
	     )
	  ;  VarList = [],
	     length(Tokens, Position),
	     ErrorReturn = [error('missing "["', [])/Position]
	).


% ErrorReturn = [] is the correct signal.
% make sure this won't fail
variable_seq_aux([], [], [], [error('incomplete variable sequence', [])/0]).
variable_seq_aux([Token|Tokens], Remains, VarList, ErrorReturn) :-
   ( ( Token = little_name(Var), Label = 0;
       Token = '_'(Var), underscore_label(Label)
     )
     -> var_conversion(Var, Label, VarRep),
        VarList = [VarRep|VarList2],
	variable_seq_aux(Tokens, Remains, VarList2, ErrorReturn)
     ;  ( Token = ','
	  -> variable_seq_aux(Tokens, Remains, VarList, ErrorReturn)
	  ;  ( Token = ']'
	       -> VarList = [],
		  Remains = [ ']'|Tokens],
		  ErrorReturn = []
	       ;  length(Tokens, Position),
		  ErrorReturn = [error('nonvariable symbol in variable sequence', [])/Position]
	     )
	)
   ).

/*------------------------------------------------------------------------------
 * formula_infinite:
 *	LeftAtoms is a list of formulas of Term(8) on the immediate left side.
 */

formula_infinite(Tokens, Return, LeftAtoms, SpecLeft, PrecLeft, Language) :-
   formula_prime(Tokens, Return2, SpecLeft, PrecLeft, Language),
   formula_infinite_aux(Return2, Return, Tokens, LeftAtoms, SpecLeft, PrecLeft).


formula_infinite_aux([], [], _, _, _, _).
formula_infinite_aux([Stack/Remains|Return], NewReturn, Tokens, LeftAtoms,
			SpecLeft, PrecLeft) :-
   ( Stack = error(_, _)
     -> NewReturn = [Stack/Remains|NewReturn2]
     ;  ( pop(stack(SpecRight, PrecRight, _, _), Stack, _)
          -> ( null(SpecRight, PrecRight)
               -> length(Tokens, Position),
		  NewReturn = [error('connective expected', [])/Position|NewReturn2]
	       ;  ( ( null(SpecLeft, PrecLeft);
	              less_than(SpecLeft, PrecLeft, SpecRight, PrecRight)
	            )
	            -> fill_holes(LeftAtoms, Stack, NewStacks),
		       stack_to_return(NewStacks, Tokens, Remains, NewReturn,
					NewReturn2)
	            ;  ( bigger_than(SpecLeft, PrecLeft, SpecRight, PrecRight)
		         -> push_in_constants(LeftAtoms, Stack, NewStacks2),
		            stack_to_return(NewStacks2, Tokens, Remains,
						NewReturn, NewReturn2)
		         ;  length(Tokens, Position),
			    NewReturn = [error('misuse of connectives', [])/Position
					 |NewReturn2]
		       )
	         )
	     )
          ;  push_in_constants(LeftAtoms, Stack, NewStacks3),
	     stack_to_return(NewStacks3, Tokens, Remains, NewReturn, NewReturn2)
			% equivalent to a push
        )
   ),
   formula_infinite_aux(Return, NewReturn2, Tokens, LeftAtoms, SpecLeft,
		PrecLeft).


/*------------------------------------------------------------------------------
 * formula_prime(+Tokens, -Return, +SpecLeft, +PrecLeft, +Language).
 *	parameters are defined the same as formula/6
 */

% partly corresponding to the grammar F' -> e
formula_prime([], [[]/[]], _, _, _).

% corresponding to the grammar F' -> f1 F | f2 F | f3 F | f4 F | f5 F | e
formula_prime([Token|Tokens], Return, SpecLeft, PrecLeft, Language):-
   ( end_of_formula([Token|Tokens], Language)
     -> Return = [[]/[Token|Tokens]]
     ;  ( connective(Token, SymbolList)
	  -> formula_prime_aux(SymbolList, Tokens, Return, SpecLeft, PrecLeft,
			Language)
	  ;  length(Tokens, Position),
	     Return = [error('connective expected', [])/Position]
	)
   ).

end_of_formula([Token|Tokens], Language) :-
   ( end_of_formula_aux(Token)
     -> true
     ;  Token = graphic_name('&'),	% the start of a CFormula
	start_of_cformula(Tokens, Language)
   ).

end_of_formula_aux(')').
end_of_formula_aux('}'(_)).
end_of_formula_aux(graphic_name('|')).
end_of_formula_aux(big_name(Name)) :-
   end_of_formula_aux(Name).

end_of_formula_aux('THEN').
end_of_formula_aux('ELSE').
end_of_formula_aux('UNTIL').


start_of_cformula([Token|Tokens], Language) :-
   ( Token = '{'
     -> term_aux(set, Tokens, Return, null, _, Language),
	( select_return(Return, null, Terms, _, _)
	  -> Terms = []
	  ;  write('Internal error No. 10'), nl, abort
	)
		% a test that it's not set.
     ;  Token = '(',
	start_of_cformula(Tokens, Language)
   ).


connective(graphic_name(Name), SymbolList) :-  connective(Name, SymbolList).
connective('&', connective('MetaDefs.&''.F2', 'Syntax.XFY.F1'(150))).
connective('\\/', connective('MetaDefs.\\/''.F2', 'Syntax.XFY.F1'(140))).
connective('->', connective('MetaDefs.->''.F2', 'Syntax.XFY.F1'(120))).
connective('<-', connective('MetaDefs.<-''.F2', 'Syntax.XFY.F1'(120))).
connective('<->', connective('MetaDefs.<->''.F2', 'Syntax.XFX.F1'(120))).

/*------------------------------------------------------------------------------
 */

% corresponding to the grammar F' -> f5 F | f6 F | f7 F
formula_prime_aux(connective(Connective, Indicator), Tokens, Return, SpecLeft,
		PrecLeft, Language):-
   Indicator =.. [SpecOp, PrecOp],
   formula(Tokens, Return2, SpecOp, PrecOp, Language),
   formula_prime_connective(Return2, Return, Tokens, SpecLeft, PrecLeft,
			Connective, SpecOp, PrecOp).


formula_prime_connective([], [], _, _, _, _, _, _).
formula_prime_connective([Stack/Remains|Return], NewReturn, Tokens, SpecLeft,
	PrecLeft, Connective, SpecOp, PrecOp):-
   ( Stack = error(_, _)
     -> NewReturn = [Stack/Remains|NewReturn2]
     ; ( pop(stack(SpecRight, PrecRight, HoleRight, TreeRight), Stack, Stack2),
         nonvar(HoleRight)
         -> ( ( null(SpecRight, PrecRight);
	        less_than(SpecOp, PrecOp, SpecRight, PrecRight)
	      )
	      -> Formula =.. [Connective, Hole, TreeRight],
	         contract(Stack2, SpecLeft, PrecLeft, 
			  stack(SpecOp, PrecOp, Hole, Formula), Stacks),
		 stack_to_return(Stacks, Tokens, Remains, NewReturn, NewReturn2)
	      ;  length(Tokens, Position),
		 NewReturn = [error('misuse of connective "~w"', [Connective])/Position | NewReturn2]
	    )
         ;  length(Tokens, Position),
	    NewReturn = [error('misuse of connective "~w"', [Connective])/Position
			 |NewReturn2]
       )
   ),
   formula_prime_connective(Return, NewReturn2, Tokens, SpecLeft, PrecLeft,
			Connective, SpecOp, PrecOp).

/*------------------------------------------------------------------------------
 * check_formula_token(+Token, -SymbolList, +Language)
 * decides whether it's a term token, a reserved word, a proposition,
 * a prefix_predicate, an infix_predicate, or a postfix_predicate.
 * Symbols of other kinds (base and constructor names) should not be included
 * in SymbolList.
 * It succeeds when Token is a valid in formulae (bases and constructors are
 * not regarded as valid token in formulae), and fails when Token is not
 * declared.
 * 	SymbolList is a list of elements in the form:
 *		Attribute/Token
 *		or when Attribute denoting connective, it is of the form
 *			Attribute/decl(Connective, Indicator))
 *		or null (not applicable)
 *	when Attribute is predicate_no_ind (or constant), the declaration part
 *	is a list of declarations, all predicate_no_ind (or constants) specified
 *	with that symbol.
 */

check_formula_token(little_name(A), term(little_name(A)), _).
check_formula_token('_'(A), term('_'(A)), _).
check_formula_token(string(A), term(string(A)), _).
check_formula_token(float(A), term(float(A)), _).
check_formula_token(number(A), term(number(A)), _).
check_formula_token('[', term('['), _).
check_formula_token('{', term('{'), _).
check_formula_token('(', bracket('('), _).
check_formula_token(')', end_of_formula(')'), _).
check_formula_token('}'(N), end_of_formula('}'(N)), _).

check_formula_token(big_name(Name), SymbolList, Language) :-
   ( lookup_reserved_word(Name, SymbolList)
     -> true
     ;  lookup_predicate_dict(big_name(Name), SymbolList, Language)
   ).

check_formula_token(graphic_name(Name), SymbolList, Language) :-
   ( lookup_reserved_word(Name, SymbolList)
     -> true
     ;  lookup_predicate_dict(graphic_name(Name), SymbolList, Language)
   ).

lookup_reserved_word('ALL', 'ALL').
lookup_reserved_word('SOME', 'SOME').
lookup_reserved_word('~', '~').
lookup_reserved_word('IF', 'IF').

/* This is now provided by the builtin language

lookup_reserved_word('True', proposition(
	['MetaDefs.PAtom.F1'('MetaDefs.Name.F4'('"', '"True',
			'MetaDefs.Proposition.C0', 0))])).
lookup_reserved_word('False', proposition(
	['MetaDefs.PAtom.F1'('MetaDefs.Name.F4'('"', '"False',
			'MetaDefs.Proposition.C0', 0))])).
*/

lookup_reserved_word('&',
	connective('MetaDefs.&''.F2', 'Syntax.YFX.F1'(150))).
lookup_reserved_word('\\/',
	connective('MetaDefs.\\/''.F2', 'Syntax.YFX.F1'(140))).
lookup_reserved_word('->',
	connective('MetaDefs.->''.F2', 'Syntax.XFY.F1'(120))).
lookup_reserved_word('<-',
	connective('MetaDefs.<-''.F2', 'Syntax.XFY.F1'(120))).
lookup_reserved_word('<->',
	connective('MetaDefs.<->''.F2', 'Syntax.XFX.F1'(120))).

lookup_reserved_word('THEN', end_of_formula(big_name('THEN'))).
lookup_reserved_word('ELSE', end_of_formula(big_name('ELSE'))).
lookup_reserved_word('UNTIL', end_of_formula(big_name('UNTIL'))).
lookup_reserved_word('|', end_of_formula(graphic_name('|'))).

/*------------------------------------------------------------------------------
 * atom(+Tokens, -Remains, -Formulae, -ErrorReturn, +Language):-
 *	Formulae = [] is the signal indicating errors
 *	ErrorReturn should always be instantiated.
 */

atom([], [], [], [], _).
atom([Token|Tokens], Remains, Formulae, ErrorReturn, Language):-
   ( check_formula_token(Token, SymbolList, Language)
     -> atom(SymbolList, Tokens, Remains, Formulae, ErrorReturn, Language)
     ;  Formulae = [],
	length(Tokens, Position),
	ErrorReturn = [error('undeclared or illegal symbol: "~w"',
				[Token])/Position]
   ).

/*------------------------------------------------------------------------------
 * atom(+SymbolList, +Tokens, -Remains, -LeftAtoms, -ErrorReturn,
 *		+Language)
 *	LeftAtoms = [] is the signal indicating errors
 *	ErrorReturn should always be instantiated.
 *
 * Be very careful !!  The following procedure takes more than just Atoms.
 * For efficiency reason, (Formula), ~ Formula, SOME Var_seq Formula, and
 * ALL Var_seq Formula are all taken as an atom.
 * If only a headatom is wanted, use headatom/5. 
 */

atom([], _, _, [], [], _).

atom([Decl|SymbolList], Tokens, Remains, Atoms, ErrorReturn, Language) :-
	atom(Decl, Tokens, Remains2, Atoms2, ErrorReturn2, Language),
	atom(SymbolList, Tokens, Remains, Atoms3, ErrorReturn3, Language),
	( var(Remains2)
	  -> Atoms = Atoms3,
	     append(ErrorReturn3, ErrorReturn2, ErrorReturn)
	  ;  ( end_of_atom(Remains2)
	       -> ( Remains = Remains2
	            -> true
	            ;  write('Internal error No 11.'), nl, abort
		  ),
		  append(Atoms3, Atoms2, Atoms),
		  append(ErrorReturn3, ErrorReturn2, ErrorReturn)
	       ;  Atoms = Atoms3,	% remove inproper atoms
	     	  append(ErrorReturn3, ErrorReturn2, ErrorReturn)
	     )
	).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */


atom(term(Token), Tokens, Remains, LeftAtoms, ErrorReturn, Language) :-
   term([Token|Tokens], TermReturn, Language),
   process_term_return(TermReturn, Remains, LeftAtoms2, [], ErrorReturn,
				Language),
   quantify_underscore(LeftAtoms2, LeftAtoms).

atom(proposition(LeftAtoms), Tokens, Tokens, LeftAtoms, [], _).

atom(predicate_no_ind(Decls), Tokens, Remains, LeftAtoms, ErrorReturn,
		Language) :-
   ( Tokens = ['('|Remains2]
     -> term_seq(Remains2, Remains3, Term_seqs, Language, ErrorReturn2),
	( Term_seqs = []
	  -> LeftAtoms = [],
	     ErrorReturn = ErrorReturn2
	  ;  ( Remains3 = [')'|Remains]
	       -> Term_seqs = [Term_seq|_],
		  length(Term_seq, Arity),
		  create_predicates(Decls, Term_seqs, Arity, LeftAtoms2, []),
		  ( LeftAtoms2 = []
		    -> LeftAtoms = [],
		       Decls = ['MetaDefs.Name.F4'(_, GPred, _, _)|_],
		       gstring2string(GPred, Pred),
		       length(Remains3, Position2),
		       Position is Position2 - 1,
		       ErrorReturn = [error('predicate "~w/~d" undeclared',
					[Pred, Arity])/Position]
		    ;  ErrorReturn = [],
	  	       quantify_underscore(LeftAtoms2, LeftAtoms)
		  )
	       ;  LeftAtoms = [],
		  length(Remains3, Position),
		  ErrorReturn = [error('missing ")"', [])/Position]
	     )
	)
     ;  LeftAtoms = [],
	length(Tokens, Position),
	ErrorReturn = [error('misuse of predicate, "(" expected', [])/Position]
   ).


atom(prefix_predicate(Predicate), Tokens, Remains, LeftAtoms, ErrorReturn,
		Language):-
   term(Tokens, TermReturn, Language),
   ( select_return(TermReturn, end_of_atom, Terms, Remains, ErrorReturn2)
     -> ( Terms = []
	  -> LeftAtoms = [],
	     ErrorReturn = ErrorReturn2 
	  ;  list2listOflist(Terms, TermLists),
	     create_formula('MetaDefs.Atom.F2', [ [Predicate], TermLists],
			LeftAtoms2),
	     ErrorReturn = [],
	     quantify_underscore(LeftAtoms2, LeftAtoms)
	     
	)
     ;  write('Internal error No 12.'), nl, abort
   ).

% For brackets, we first parse it as an atom, if we succeed, we won't parse
% it as a (Formula)
atom(bracket(_), Tokens, Remains, LeftAtoms, ErrorReturn, Language):-
	atom(term('('), Tokens, Remains2, LeftAtoms2, ErrorReturn2, Language),
	( LeftAtoms2 = []
	  -> formula(Tokens, Return, Language),
	     ( select_return(Return, [')'], Formulas, Remains3, ErrorReturn3)
	       -> ( Formulas = []
	            -> LeftAtoms = [],
		       ( select_return(Return, null, Formulas2, Remains5, _)
			 -> true
			 ;  write('Internal error No 13.'), nl, abort
		       ),
		       ( Formulas2 = []
		         -> append(ErrorReturn2, ErrorReturn3, ErrorReturn)
		         ;  length(Remains5, Position),
			    append(ErrorReturn2,
			      [error('")" expected', [])/Position|ErrorReturn3],
			      ErrorReturn)
		       )
	            ;  ( Remains3 = [')'|Remains4]
		         -> LeftAtoms = Formulas,
		            Remains = Remains4,
	     		    ErrorReturn = []
		         ;  LeftAtoms = [],
			    length(Remains3, Position),
		            ErrorReturn = [error('missing ")"', [])/Position]
		       )	       
                  ) 
	       ;  write('Internal error No 14.'), nl, abort 
	     )
	  ;  Remains = Remains2,
	     ErrorReturn = [],
	     LeftAtoms = LeftAtoms2
	     % the old version quantify_underscore(LeftAtoms2, LeftAtoms)
	     
	).


atom('~', Tokens, Remains, LeftAtoms, ErrorReturn, Language) :-
	atom(Tokens, Remains, Formulae, ErrorReturn, Language),
	( Formulae = []
	  -> LeftAtoms = []
	  ;  create_formula('MetaDefs.~''.F1', [Formulae], LeftAtoms)
	).


atom('SOME', Tokens, Remains, LeftAtoms, ErrorReturn, Language) :-
	variable_seq(Tokens, Remains2, VarList, ErrorReturn2),
	( VarList = []
	  -> LeftAtoms = [],
	     ErrorReturn = ErrorReturn2
	  ;  atom(Remains2, Remains, Formulae, ErrorReturn3, Language),
	     ( Formulae = []
	       -> LeftAtoms = [],
		  ErrorReturn = ErrorReturn3
	       ;  ErrorReturn = [],
		  create_formula('MetaDefs.Some.F2', [[VarList], Formulae],
				LeftAtoms)
	     )
	).


atom('ALL', Tokens, Remains, LeftAtoms, ErrorReturn, Language) :-
	variable_seq(Tokens, Remains2, VarList, ErrorReturn2),
	( VarList = []
	  -> LeftAtoms = [],
	     ErrorReturn = ErrorReturn2
	  ;  atom(Remains2, Remains, Formulae, ErrorReturn3, Language),
	     ( Formulae = []
	       -> LeftAtoms = [],
		  ErrorReturn = ErrorReturn3
	       ;  ErrorReturn = [],
		  create_formula('MetaDefs.All.F2', [[VarList], Formulae],
				LeftAtoms)
	     )
	).


% The following should not be the start of an atom
atom('IF', Tokens, Tokens, [], 
		[error('unexpected conditional', [])/Position], _) :-
	length(Tokens, Position).
atom(infix_predicate(_), Tokens, Tokens, [],
		[error('unexpected infix predicate', [])/Position], _) :-
	length(Tokens, Position).
atom(postfix_predicate(_), Tokens, Tokens, [],
		[error('unexpected postfix predicate', [])/Position], _) :-
	length(Tokens, Position).
atom(connective(_, _), Tokens, Tokens, [],
		[error('unexpected connective', [])/Position], _) :-
	length(Tokens, Position).
atom(end_of_formula(Token), Tokens, [Token|Tokens], [], [], _).

/*------------------------------------------------------------------------------
 */
	     
conditional(Tokens, Remains, Formulae, ErrorReturn, Language) :-
   conditional_aux(Tokens, Remains2, Formulae2, ErrorReturn2, Language),
   ( Formulae2 = []
     -> ErrorReturn = ErrorReturn2,
	Formulae = []
     ;  ( end_of_conditional(Remains2, Language)
	  -> Formulae = Formulae2,
	     Remains = Remains2,
	     ErrorReturn = []
	  ;  ( Remains2 = [graphic_name('&')|Remains3]
	       -> formula(Remains3, Return3, Language),
		  ( select_return(Return3, null, Formulae4, Remains,
				ErrorReturn4)
		    -> ( Formulae4 = []
		         -> ErrorReturn = ErrorReturn4,
		            Formulae = []
		         ;  ErrorReturn = [],
		            create_formula('MetaDefs.&''.F2', 
					    [Formulae2, Formulae4], Formulae)
		       )
		    ;  write('Internal error No 15.'), nl, abort 
		  )
	       ;  Formulae = [],
		  length(Remains2, Position),
		  Remains2 = [Symb|_],
		  ErrorReturn = [error('illegal symbol: "~w"', [Symb])/Position]
	     )
	)
   ).


conditional_aux(Tokens, Remains, Formulae, ErrorReturn, Language) :-
   ( Tokens = [big_name('SOME')|Tokens2]
     -> variable_seq(Tokens2, Remains2, VarList, ErrorReturn2),
	( VarList = []
	  -> Formulae = [],
	     ErrorReturn = ErrorReturn2
          ;  conditional_rest(Remains2, Remains, VarList, Formulae, ErrorReturn,
				Language)
	)
     ;  conditional_rest(Tokens, Remains, [], Formulae, ErrorReturn, Language)
   ).

%------------------------------------------------------------------------------

end_of_conditional([], _).
end_of_conditional([Token|Tokens], Language) :-
   ( end_of_conditional_aux(Token)
     -> true
     ;  Token = graphic_name('&'),      % the start of a Commit
	Tokens = ['{'|Tokens2],
	term_aux(set, Tokens2, Return, null, _, Language),
	( select_return(Return, null, Terms, _, _)
	  -> Terms = []
	  ;  write('Internal error No 16.'), nl, abort 
	)
		% a test that it's not set
   ).

end_of_conditional_aux(')').
end_of_conditional_aux('}'(_)).
end_of_conditional_aux(graphic_name(N)) :-
   end_of_conditional_aux(N).
end_of_conditional_aux('|').
end_of_conditional_aux('\\/').
end_of_conditional_aux('<-').
end_of_conditional_aux('->').
end_of_conditional_aux('<->').
end_of_conditional_aux(big_name(N)) :-
   end_of_conditional_aux(N).
end_of_conditional_aux('ELSE').
end_of_conditional_aux('THEN').

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

conditional_rest(Tokens, Remains, VarList, Formulae, ErrorReturn, Language) :-
   formula(Tokens, CondReturn, Language),
   ( select_return(CondReturn, [big_name('THEN')], Conds, Remains2,
		ErrorReturn2)
     -> true
      ;  write('Internal error No 17.'), nl, abort
   ),
   ( Conds = []
     -> Formulae = [],
        ( ErrorReturn2 = []
	  -> length(Tokens, Position),
	     ErrorReturn = [error('invalid formula or "THEN" missing', [])/Position]
	  ;  ErrorReturn = ErrorReturn2
	)
     ;  ( Remains2 = [big_name('THEN')|Remains3]
          -> then_part(Remains3, Remains4, Thens, ErrorReturn4, Language),
	     ( Thens = []
	       -> Formulae = [],
		  ( ErrorReturn4 = []
		    -> length(Remains3, Position),
		       ErrorReturn = [error('formula expected', [])/Position]
		    ;  ErrorReturn = ErrorReturn4
		  )
	       ;  ( Remains4 = [big_name('ELSE')|Remains5]
		    -> else_part(Remains5, Remains, Elses, ErrorReturn5,
					Language),
		       ( Elses = [],
			 ErrorReturn5 = []
			 -> Formulae = [],
			    length(Remains5, Position),
			    ErrorReturn = [error('formula expected', [])/Position]
			 ;  ( Elses = []
			      -> Formulae = [],
		    	         ErrorReturn = ErrorReturn5
			      ;  ErrorReturn = [],
			         ( VarList = []
			           -> create_formula('MetaDefs.ITE.F3',
				      	[Conds, Thens, Elses], Formulae)
			           ;  create_formula('MetaDefs.ISTE.F4',
					[[VarList], Conds, Thens, Elses],
					Formulae)
			    	 )
			    )
		       )
		    ;  Remains = Remains4,
		       ErrorReturn = [],
		       ( VarList = []
			 -> create_formula('MetaDefs.IT.F2',
				      [Conds, Thens], Formulae)
			 ;  create_formula('MetaDefs.IST.F3',
				      [[VarList], Conds, Thens], Formulae)
		       )
		  )
	     )
	  ;  Formulae =[],
	     length(Remains2, Position),
	     ErrorReturn = [error('"THEN" expected', [])/Position]
       )
   ).

%------------------------------------------------------------------------------
% ElsePart ->   Atom | IF A THEN B | IF A THEN B ELSE C | Atom & ElsePart 

else_part([big_name('IF')|Tokens], Remains, Formulae, ErrorReturn, Language) :-
   !,
   conditional(Tokens, Remains, Formulae, ErrorReturn, Language).

% it must be a Formula by now.
else_part(Tokens, Remains, Formulae, ErrorReturn, Language) :-
   atom(Tokens, Remains2, Formulae2, ErrorReturn2, Language),
   ( Formulae2 = []
     -> ErrorReturn = ErrorReturn2,
	Formulae = []
     ;  ( end_of_conditional(Remains2, Language)
	  -> Formulae = Formulae2,
	     ErrorReturn = [],
	     Remains = Remains2
	  ;  ( Remains2 = [graphic_name('&')|Remains3]
	       -> else_part(Remains3, Remains, Formulae3, ErrorReturn3,
				Language),
		  ( Formulae3 = []
		    -> ErrorReturn = ErrorReturn3,
		       Formulae = []
		    ;  ErrorReturn = [],
		       create_formula('MetaDefs.&''.F2', [Formulae2, Formulae3],
					Formulae)
		  )
	       ;  Formulae = [],
	          length(Remains2, Position),
		  Remains2 = [Symb|_],
		  ErrorReturn = [error('illegal symbol in conditional: "~w"', [Symb])/Position]
	     )
	)
   ).

%------------------------------------------------------------------------------
% ThenPart ->   Atom | IF A THEN B | Atom & ThenPart 

then_part([big_name('IF')|Tokens], Remains, Formulae, ErrorReturn, Language) :-
   !,
   ( Tokens = [big_name('SOME')|Tokens2]
     -> variable_seq(Tokens2, Remains2, VarList, ErrorReturn2),
	( VarList = []
	  -> Formulae = [],
	     ErrorReturn = ErrorReturn2
          ;  then_part_aux(Remains2, Remains, VarList, Formulae, ErrorReturn,
				Language)
	)
     ;  then_part_aux(Tokens, Remains, [], Formulae, ErrorReturn, Language)
   ).

% it must be a Formula by now.
then_part(Tokens, Remains, Formulae, ErrorReturn, Language) :-
   atom(Tokens, Remains2, Formulae2, ErrorReturn2, Language),
   ( Formulae2 = []
     -> ErrorReturn = ErrorReturn2,
	Formulae = []
     ;  ( end_of_conditional(Remains2, Language)
	  -> Formulae = Formulae2,
	     ErrorReturn = [],
	     Remains = Remains2
	  ;  ( Remains2 = [graphic_name('&')|Remains3]
	       -> then_part(Remains3, Remains, Formulae3, ErrorReturn3,Language),
		  ( Formulae3 = []
		    -> ErrorReturn = ErrorReturn3,
		       Formulae = []
		    ;  ErrorReturn = [],
		       create_formula('MetaDefs.&''.F2', [Formulae2, Formulae3],
					Formulae)
		  )
	       ;  Formulae = [],
	          length(Remains2, Position),
		  Remains2 = [Symb|_],
		  ErrorReturn = [error('illegal symbol in conditional: "~w"', [Symb])/Position]
	     )
	)
   ).

%------------------------------------------------------------------------------

then_part_aux(Tokens, Remains, VarList, Formulae, ErrorReturn, Language) :-
   formula(Tokens, CondReturn, Language),
   ( select_return(CondReturn, [big_name('THEN')], Conds, Remains2,
		ErrorReturn2)
     -> true
     ;  write('Internal error No 18.'), nl, abort
   ),
   ( Conds = []
     -> Formulae = [],
        ( ErrorReturn2 = []
	  -> length(Tokens, Position),
	     ErrorReturn = [error('not formula or "THEN" missing', [])/Position]
	  ;  ErrorReturn = ErrorReturn2
	)
     ;  ( Remains2 = [big_name('THEN')|Remains3]
          -> then_part(Remains3, Remains4, Thens, ErrorReturn4, Language),
	     ( Thens = []
	       -> Formulae = [],
		  ( ErrorReturn4 = []
		    -> length(Remains3, Position),
		       ErrorReturn = [error('formula expected', [])/Position]
		    ;  ErrorReturn = ErrorReturn4
		  )
	       ;  Remains = Remains4,
		  ErrorReturn = [],
		  ( VarList = []
		    -> create_formula('MetaDefs.IT.F2', [Conds, Thens], Formulae)
		    ;  create_formula('MetaDefs.IST.F3',
				      [[VarList], Conds, Thens], Formulae)
		  )
	     )
	  ;  Formulae =[],
	     length(Remains2, Position),
	     ErrorReturn = [error('"THEN" expected', [])/Position]
       )
   ).

/*------------------------------------------------------------------------------
 * create_predicates(+Predicates, +Term_seqs, +Term_seq_Arity, -Atoms, +Atoms2)
 * Giving a list of normal predicate declarations and a list of formula_seqs,
 * this returns the parse trees of the predicate. Arity of the predicate is
 * checked. 
 */

create_predicates([], _, _, Atoms, Atoms).
create_predicates(['MetaDefs.Name.F4'(Module, Predicate, Cat, Arity)|Decls],
		Term_seqs, Term_seq_Arity, Atoms, Atoms2) :-
	( Arity = Term_seq_Arity
	  -> create_predicates_aux(Term_seqs,
			      'MetaDefs.Name.F4'(Module, Predicate, Cat, Arity),
			      Atoms, Atoms3)
	  ;  Atoms = Atoms3
	),
	create_predicates(Decls, Term_seqs, Term_seq_Arity, Atoms3, Atoms2).


create_predicates_aux([], _, Atoms, Atoms).
create_predicates_aux([Term_seq|Term_seqs], Predicate, 
		['MetaDefs.Atom.F2'(Predicate, Term_seq)|Atoms], Atoms2) :-
	create_predicates_aux(Term_seqs, Predicate, Atoms, Atoms2).


/*------------------------------------------------------------------------------
 * process_term_return(+TermReturn, -NewRemains, -LeftAtoms, +LeftAtoms2,
 *		-ErrorReturn, +Language)
 *	LeftAtoms = [] is the signal indicating errors
 *	ErrorReturn should always be instantiated.
 */

process_term_return([], _, LeftAtoms, LeftAtoms, [], _).
process_term_return([Stack/Remains|TermReturn], NewRemains, LeftAtoms,
			LeftAtoms2, ErrorReturn, Language) :-
	( Stack = error(_, _)
	  -> ErrorReturn = [Stack/Remains|ErrorReturn2],
	     LeftAtoms = LeftAtoms3
	  ;  ( Stack = [stack(_, _, Hole, Term)],
	       nonvar(Hole)
	       -> ( Remains = [Token|Remains3],
		    change_symbol(Token, Symbol)
		    -> atom_middleway(Symbol, Remains3, NewRemains, Term,
				LeftAtoms, LeftAtoms3, ErrorReturn3, Language),
		       append(ErrorReturn3, ErrorReturn2, ErrorReturn)
		  ;  length(Remains, Position),
		      (Token = halt) -> halt
		       ; ErrorReturn = [error('predicate expected', [])/Position|ErrorReturn2],
		       LeftAtoms = LeftAtoms3
		  )
	       ;  length(Remains, Position),
		  ErrorReturn = [error('term expected', [])/Position|ErrorReturn2],
		  LeftAtoms = LeftAtoms3
	     )
	),
	process_term_return(TermReturn, NewRemains, LeftAtoms3, LeftAtoms2,
			ErrorReturn2, Language).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% this is to enable the use of indexing

change_symbol(A, B) :-
   ( A = graphic_name(S),
     change_symbol_aux(S, B);
     B = A
   ).

change_symbol_aux('=', '=').
change_symbol_aux('~=', '~=').
change_symbol_aux('=<', '=<').
change_symbol_aux('<', '<').

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

create_formula(Predicates, Terms, Formulae) :-
	cartesian_product(Terms, Arguments),
	create_formula_aux(Arguments, Predicates, Formulae, []).


create_formula_aux([], _, Formulae, Formulae).
create_formula_aux([Argus|Arguments], Predicate, [Formula|Formulae],
		Formulae2) :-
	Formula =.. [Predicate|Argus],
	create_formula_aux(Arguments, Predicate, Formulae, Formulae2).

			
/*------------------------------------------------------------------------------
 * atom_middleway(+Symbol, +Tokens, -NewRemains, +LeftTerm, -Atoms, +Atoms2,
 *		-ErrorReturn, +Language)
 *	Atoms = [] is the signal indicating errors
 *	ErrorReturn should always be instantiated.
 */

atom_middleway(Symbol, Tokens, NewRemains, LeftTerm, Atoms, Atoms2, ErrorReturn,
			Language) :-
	atom_aux(Symbol, Tokens, NewRemains, Predicates, RightTerms,
			ErrorReturn, Language),
	( Predicates = []
	  -> Atoms = Atoms2
	  ;  ( Predicates = Predicates2/adjust(A, B)
	       -> adjust_interval(A, B, LeftTerm, LeftTerm2, RightTerms,
			RightTerms2)
	       ;  Predicates2 = Predicates,
		  LeftTerm = LeftTerm2,
		  RightTerms = RightTerms2
	     ),
	     cartesian_product([[LeftTerm2]|RightTerms2], Term_seqs),
	     create_predicates(Predicates2, Term_seqs, _, Atoms, Atoms2)
	),
	( var(NewRemains)
	  -> NewRemains = Remains
	  ;  ( NewRemains = Remains
	       -> true
	       ;  write('Internal error No 19.'), nl, abort
	     )
	).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * atom_aux(+Symbol, +Tokens, -NewRemains, -Predicates, -RightTerms,
 *		-ErrorReturn, +Language)
 *	Predicates = [] is the signal indicating errors
 *	RightTerms is a list of lists.
 *	ErrorReturn should always be instantiated.
 */

atom_aux('~=', Tokens, NewRemains, Predicates, [RightTerms], ErrorReturn,
		Language) :-
   !,
   term(Tokens, Return, Language),
   ( select_return(Return, end_of_atom, RightTerms, NewRemains, ErrorReturn2)
     -> ( RightTerms = []
	  -> Predicates = [],
	     ( ErrorReturn2 = []
	       -> length(Tokens, Position),
		  ErrorReturn = [error('term expected', [])/Position]
	       ;  ErrorReturn = ErrorReturn2
	     )
	  ;  ErrorReturn = ErrorReturn2,
	     Predicates = ['MetaDefs.Name.F4'('"', '"~=',
					'MetaDefs.Predicate.C0', 2)]
	)
     ;  write('Internal error No 20.'), nl, abort  
   ).


atom_aux('=', Tokens, NewRemains, Predicates, [RightTerms], ErrorReturn,
		Language) :-
   !,
   term(Tokens, Return, Language),
   ( select_return(Return, end_of_atom, RightTerms, NewRemains, ErrorReturn2)
     -> ( RightTerms = []
	  -> Predicates = [],
	     ( ErrorReturn2 = []
	       -> length(Tokens, Position),
		  ErrorReturn = [error('term expected', [])/Position]
	       ;  ErrorReturn = ErrorReturn2
	     )
	  ;  ErrorReturn = [],
	     Predicates = ['MetaDefs.Name.F4'('"', '"=',
					'MetaDefs.Predicate.C0', 2)]
	)
     ;  write('Internal error No 21.'), nl, abort  
   ).


atom_aux('<', Tokens, NewRemains, Predicates, RightTerms, ErrorReturn,
		Language) :-
   !,
   term(Tokens, Return, Language),
   ( select_return(Return, [graphic_name('<'), graphic_name('=<')], MidTerms,
		Remains, _ErrorReturn3)
     -> true
     ;  write('Internal error No 22.'), nl, abort  
   ),
   ( ( MidTerms = [];
       \+ inequation(Remains, _, _)	% make sure it's not an interval
     )
     -> ( select_return(Return, end_of_atom, RightTerms2, NewRemains,
				ErrorReturn2)
          -> true
          ;  write('Internal error No 42.'), nl, abort  
        ),
        ( RightTerms2 = []
	  -> Predicates = [],
	     length(Tokens, Position),
	     ErrorReturn = [error('term expected',[])/Position|ErrorReturn2]
	  ;  RightTerms = [RightTerms2],
	     ( lookup_predicate_dict(graphic_name('<'), SymbolList, Language)
	       -> ErrorReturn = [],
		  select_binary_infix(SymbolList, Predicates)
	       ;  Predicates = [],
		  length(Tokens, Position),
		  ErrorReturn = [error('undeclared symbol: "<"',[])/Position]
	     )
	)
     ;  inequation(Remains, Remains2, Operator),
	term(Remains2, Return2, Language),
        ( select_return(Return2, end_of_atom, RightTerms2, NewRemains,
				ErrorReturn2)
          -> true
          ;  write('Internal error No 43.'), nl, abort  
        ),
	( RightTerms2 = []
	  -> Predicates = [],
	     length(Remains2, Position),
	     ErrorReturn = [error('term expected',[])/Position|ErrorReturn2]
	  ;  RightTerms = [MidTerms, RightTerms2],
	     ( lookup_base('"Integer', '"Integers', Language)
	       -> Predicates = ['MetaDefs.Name.F4'('"Integers',
				'"Interval', 'MetaDefs.Predicate.C0', 3)]/
				adjust('<', Operator),
				% note the special structure of Predicates
		  ErrorReturn = []
	       ;  length(Tokens, Position),
		  ErrorReturn = [error('system module "Intergers" not imported', [])/Position]
	     )
	)
   ).


atom_aux('=<', Tokens, NewRemains, Predicates, RightTerms, ErrorReturn,
		Language) :-
   !,
   term(Tokens, Return, Language),
   ( select_return(Return, [graphic_name('<'), graphic_name('=<')], MidTerms,
		Remains, _ErrorReturn3)
     -> true
     ;  write('Internal error No 23.'), nl, abort  
   ),
   ( ( MidTerms = [];
       \+ inequation(Remains, _, _)   % make sure it's not an interval
     )
     -> ( select_return(Return, end_of_atom, RightTerms2, NewRemains,
				ErrorReturn2)
          -> true
          ;  write('Internal error No 44.'), nl, abort  
        ),
        ( RightTerms2 = []
	  -> Predicates = [],
	     length(Tokens, Position),
	     ErrorReturn = [error('term expected',[])/Position|ErrorReturn2]
	  ;  RightTerms = [RightTerms2],
	     ( lookup_predicate_dict(graphic_name('=<'), SymbolList, Language)
	       -> ErrorReturn = [],
		  select_binary_infix(SymbolList, Predicates)
	       ;  Predicates = [],
		  length(Tokens, Position),
		  ErrorReturn = [error('undeclared symbol: "=<"',[])/Position]
	     )
	)
     ;  inequation(Remains, Remains2, Operator),
	term(Remains2, Return2, Language),
        ( select_return(Return2, end_of_atom, RightTerms2, NewRemains,
				ErrorReturn2)
          -> true
          ;  write('Internal error No 45.'), nl, abort  
        ),
	( RightTerms2 = []
	  -> Predicates = [],
	     length(Remains2, Position),
	     ErrorReturn = [error('term expected',[])/Position|ErrorReturn2]
	  ;  RightTerms = [MidTerms, RightTerms2],
	     ( lookup_base('"Integer', '"Integers', Language)
	       -> Predicates = ['MetaDefs.Name.F4'('"Integers',
				'"Interval', 'MetaDefs.Predicate.C0', 3)]/
				adjust('=<', Operator),
				% note the special structure of Predicates
		  ErrorReturn = []
	       ;  length(Tokens, Position),
		  ErrorReturn = [error('system module "Intergers" not imported', [])/Position]
	     )
	)
   ).


atom_aux(Symbol, Tokens, NewRemains, Predicates, RightTerms, ErrorReturn,
		Language) :-
   ( lookup_predicate_dict(Symbol, SymbolList, Language)
     -> ( ( Tokens = [Token|_],
	    end_of_atom(Token);
	    Tokens = []
	  )
	  -> NewRemains = Tokens,
	     select_predicate_operator(SymbolList, postfix_predicate,
						Predicates),
	     RightTerms = [],
	     ( Predicates = []
	       -> length(Tokens, Position),
		  ErrorReturn = [error('misuse of postfix predicate operator "~w"', [Symbol])/Position]
	       ;  ErrorReturn = []
	     )
	  ;  term(Tokens, Return, Language),
	     ( select_return(Return, end_of_atom, RightTerms2, NewRemains,
			ErrorReturn2)
	       -> ( RightTerms2 = []
	            -> ErrorReturn = ErrorReturn2,
		       RightTerms = [],
		       Predicates = []
		    ;  select_predicate_operator(SymbolList,
						infix_predicate, Predicates),
	     	       ( Predicates = []
	       		 -> RightTerms = [],
		   	    length(Tokens, Position),
			    ErrorReturn = [error('misuse of infix predicate operator "~w"', [Symbol])/Position]
	       		 ;  RightTerms = [RightTerms2],
			    ErrorReturn = []
	     	       )
		  )
	       ;  write('Internal error No 24.'), nl, abort  
	     )
	 )
      ;  RightTerms = [],
	 length(Tokens, Position),
	 ErrorReturn = [error('infix or postfix predicate operator expected', [])/Position]
   ).


end_of_atom([]).
end_of_atom([Token|_]) :-
   end_of_atom(Token).

end_of_atom(')').
end_of_atom('}'(_)).

end_of_atom(graphic_name(Name)) :-
   end_of_atom(Name).
end_of_atom(big_name(Name)) :-
   end_of_atom(Name).

end_of_atom('&').
end_of_atom('->').
end_of_atom('\\/').
end_of_atom('<-').
end_of_atom('<->').
end_of_atom('|').
end_of_atom('THEN').
end_of_atom('ELSE').
end_of_atom('UNTIL').

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * this assume infix, postfix, and prefix predicates are all in the dictionary
 * format of Attribute/decl(Predicate-name, null).
 */

select_predicate_operator([], _, []).
select_predicate_operator([Decl|SymbolList], Switch, Predicates):-
	( Decl =.. [Switch, Predicate]
	  -> Predicates = [Predicate|Predicates2]
	  ;  Predicates = Predicates2
	),
	select_predicate_operator(SymbolList, Switch, Predicates2).
	

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * select_binary_infix picks out binary infix predicates
 */

select_binary_infix([], []).
select_binary_infix([infix_predicate(Name)|SymbolList], [Name|Predicates]) :-
	!,
	select_binary_infix(SymbolList, Predicates).
select_binary_infix([_|SymbolList], Predicates) :-
	select_binary_infix(SymbolList, Predicates).

%------------------------------------------------------------------------------

inequation([graphic_name('<')|Remains], Remains, '<') :- !.
inequation([graphic_name('=<')|Remains], Remains, '=<').


/*------------------------------------------------------------------------------
 * adjust_interval adds +1 to intervals when the relation operator is <.
 */

adjust_interval('<', SecondOp, LeftTerm,
		'MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Integers', '"+',
			'MetaDefs.Function.C0', 2),
			[LeftTerm, 'MetaDefs.Int.F1'(1)]),
		RightTerms, RightTerms2) :-
   adjust_interval2(SecondOp, RightTerms, RightTerms2).

adjust_interval('=<', SecondOp, LeftTerm, LeftTerm, RightTerms, RightTerms2) :-
   adjust_interval2(SecondOp, RightTerms, RightTerms2).


adjust_interval2('<', [Var, RightTerms], [Var, RightTerms2]) :-
   adjust_interval2_aux(RightTerms, RightTerms2).

adjust_interval2('=<', RightTerms, RightTerms).


adjust_interval2_aux([], []).
adjust_interval2_aux([T|RightTerms],
		['MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Integers', '"-',
			'MetaDefs.Function.C0', 2), [T, 'MetaDefs.Int.F1'(1)])
		 |RightTerms2]) :-
   adjust_interval2_aux(RightTerms, RightTerms2).

/*------------------------------------------------------------------------------
 */

quantify_underscore([], []).
quantify_underscore([Formula|Formulae], [NewFormula|NewFormulae]):-
	process_underscore(Formula, NewFormula),
	quantify_underscore(Formulae, NewFormulae).


process_underscore('MetaDefs.Empty.C0', 'MetaDefs.Empty.C0').
process_underscore('MetaDefs.PAtom.F1'(B), 'MetaDefs.PAtom.F1'(B)).
process_underscore('MetaDefs.Atom.F2'(Name, Terms), NewFormula) :-
	collect_underscores(Terms, Underscores, []),
	( Underscores = []
	  -> NewFormula = 'MetaDefs.Atom.F2'(Name, Terms)
	  ;  NewFormula = 'MetaDefs.Some.F2'(Underscores,
				'MetaDefs.Atom.F2'(Name, Terms))
	).

% 'MetaDefs.Var.F1'(Label) is definitely not an underscore
collect_underscores('MetaDefs.Var.F2'(GVar, Label),
		['MetaDefs.Var.F2'(GVar, Label)|Underscores], Underscores) :-
	name(GVar, [0'",0'_|_]), !.
collect_underscores([H|T], Underscores, Underscores2) :-
	!,
	collect_underscores(H, Underscores, Underscores3),
	collect_underscores(T, Underscores3, Underscores2).

collect_underscores('MetaDefs.Term.F2'(_, Terms), Underscores, Underscores2) :-
	!,
	collect_underscores(Terms, Underscores, Underscores2).

collect_underscores(_, Underscores, Underscores).

