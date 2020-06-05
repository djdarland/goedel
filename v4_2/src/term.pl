
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
File:		term.pl
Subject:	parsing terms
Author: 	Jiwei Wang
Date:		25 October 1991
		15 November 1991, the first fully working version of the
		ambiguous parser
Comments:	the new design of the parser which copes with all kinds of
		ambiguity.

================================================================================
*/

'$$module'('@(#)term.pl 1.42 last updated 93/12/14 11:58:33 by jiwei
').

/*------------------------------------------------------------------------------
  The parsing method follows the general idea of recursive descent parsing
  method. Firstly the sequence of term is decided by recursive procedures.
  Then, the order of operators is sorted out by their precedence and
  associativity. Multiple parse trees are output from the parser.

 
  The original grammars for term can be rewritten as:
 
 	T -> c | f1 T | f2 T | T f3 | T f4 | T f5 T | T f6 T | T f7 T
 
  where c stands for terms in Term(infinite), fi stands for a function.
 
  The transformed grammars (with left recursion removed) are as follows:
 
 	T  -> c T' | f1 T T' | f2 T T'
 	T' -> f3 T'| f4 T'   | f5 T T' | f6 T T' | f7 T T' | e
 
  where e is the empty string.
  In fact sequence T T' is just T.  Replace T T' by T, we have the grammar: 
 
 	T  -> c T' | f1 T | f2 T
 	T' -> f3 T'| f4 T'| f5 T | f6 T | f7 T | e
 
  The parsing strategy is first to get the sequence of tokens in the term
  right, and then sort out the order of operators by their precedences.
 
  Sorting out the order of operators.
 	T and T' carry the indicator of its immediate-left operator (can be
 	'null') and a stack whose elements contain a parse tree and the
 	indicator of the major function in the parse tree. Therefore in T and T'
 	Let (IndLeft), (IndOp) and (IndRight)
 	be the indicators of the left, the current and the right operators
 	respectively. Let the current operator be Op and the parsetree on the
 	top of the stack be TreeRight.
 	Let the specifier for Term(8) be 'null'.
 
	We define three relations between two indicators, namely bigger_than,
	less_than and ambiguous.  Indicators (IndLeft and IndRight) have
	two components, namely specifier (Spec) and precedence (Prec).


	bigger_than(IndLeft, IndRight) <-
		PrecLeft > PrecRight \/
 		( PrecLeft = PrecRight &
 		  ( ( SpecLeft = 'YFX' & SpecRight = 'YFX' ) \/
 		    ( SpecLeft = 'YFX' & SpecRight = 'YF' ) \/
 		    ( SpecLeft = 'XFX' & SpecRight = 'YFX' ) \/
 		    ( SpecLeft = 'XFX' & SpecRight = 'YF' ) \/
 		    ( SpecLeft = 'FX' & SpecRight = 'YFX' ) \/
 		    ( SpecLeft = 'FX' & SpecRight = 'YF' ) \/
 		    ( SpecLeft = 'XF' & SpecRight = 'YFX' ) \/
 		    ( SpecLeft = 'XF' & SpecRight = 'YF' ) \/
 		    ( SpecLeft = 'YF' & SpecRight = 'YFX' ) \/
 		    ( SpecLeft = 'YF' & SpecRight = 'YF' )
		  )
		).

	less_than(IndLeft, IndRight) <-
		PrecLeft < PrecRight \/
 		( PrecLeft = PrecRight &
 	  	  ( ( SpecLeft = 'XFY' & SpecRight = 'XFX') \/
 	            ( SpecLeft = 'XFY' & SpecRight = 'XFY') \/
 	            ( SpecLeft = 'XFY' & SpecRight = 'XF') \/
 	            ( SpecLeft = 'FY' & SpecRight = 'XFX') \/
 	            ( SpecLeft = 'FY' & SpecRight = 'XFY') \/
 	            ( SpecLeft = 'FY' & SpecRight = 'XF') \/
 	            ( SpecLeft = 'XFY' & SpecRight = 'FX') \/
 	            ( SpecLeft = 'XFY' & SpecRight = 'FY') \/
 	            ( SpecLeft = 'FY' & SpecRight = 'FX') \/
 	            ( SpecLeft = 'FY' & SpecRight = 'FY')
		  )
		).

	ambiguous(IndLeft, IndRight) <-
 		PrecLeft = PrecRight &
 	  	( ( SpecLeft = 'XFY' & SpecRight = 'YFX') \/
 	          ( SpecLeft = 'XFY' & SpecRight = 'YF') \/
 	          ( SpecLeft = 'FY' & SpecRight = 'YFX') \/
 	          ( SpecLeft = 'FY' & SpecRight = 'YF')
		).

	In the parse tree of an expression, adjacent operators not satisfying
	one of bigger_than, less_than or ambiguous cause syntax errors.
	There are 25 combinations of this kind.

	The following operations are considered with the condition that the
	stack is always kept in normal form.  A stack is in normal form if
	there is no expression (Op) in the stack satisfying the condition:
		less_than(IndUp, IndOp) &
		( bigger_than(IndOp, IndDown) \/ ambiguous(IndOp, IndDown) ) &
		less_than(IndUp, IndDown).
	This is maintained by contracting the stack, in which Op should
	be filled into the hole of the expression at its right.

	There are two stack operations, namely push and pop. pop fails if the
	stack is empty.

 	1. When Op is a term(8): (it happens in T -> c T' )

 	If empty(Stack)
	   push(stack('null', _, term(Op, [])).
	else
 	   pop(stack(IndRight, HoleRight, TreeRight)),
	   if null(IndRight)
	      error		% no two constants can be put together
	   else if null(IndLeft) \/ less_than(IndLeft, IndRight) \/
	   		ambiguous(IndLeft, IndRight)
			% ambiguity is dealt later.
 		   HoleRight = term(Op, []).
 	   	   push(stack(IndRight, HoleRight, TreeRight)),
	   else if bigger_than(IndLeft, IndRight)
 	   	   push(stack(IndRight, HoleRight, TreeRight)),
 		   push(stack('null', _, term(Op, [])).
 	   otherwise
		   error.


 	2. When Op is unary prefix: (it happens in T -> f1 T | f2 T )

 	If empty(Stack)
	   error	% expecting a term
	else
	   if null(IndLeft) \/ less_than(IndLeft, IndOp)
 	      pop(stack(IndRight, HoleRight, TreeRight)),
 	      if null(IndRight) \/ less_than(IndOp, IndRight)
		 contract(IndLeft, stack(IndOp, term(Op, [TreeRight]), Stack)
	      else if ambiguous(IndOp, IndRight)
		      pull apart TreeRight, create two parse trees
	      else
	     	 error	% including bigger_than(IndLeft, IndOp)
 	   else
 	      error % This includes the case of bigger_than(IndLeft, IndOp),
	            % because it is only possible when SpecRight = 'FX'/'FY'.
	   	    % There is no ambiguous case for this one.


 	3. When Op is unary postfix: (it happens in T' -> f1 T' | f2 T' )

 	If empty(Stack) 
 	   push(stack(IndOp, Hole, term(Op, [Hole])),
	else
	   if null(IndRight)
	      error		% PrecRight can not be 'null'
	   else if bigger_than(IndOp, IndRight)
 	           if null(IndLeft) \/ less_than(IndLeft, IndOp)
		      contract(IndLeft, stack(IndOp, term(Op, [Hole]), Stack)
		   else if bigger_than(IndLeft, IndOp) \/
				ambiguous(IndLeft, IndOp)
 		           push(stack(IndOp, Hole, term(Op, [Hole])),
	  	   otherwise
		 	   error
	   else
 	      error % This includes the case of less_than(IndOp, IndRight),
		    % There is no ambiguous case


 	4. When Op is binary infix: (it happens in T' -> f5 T | f6 T | f7 T)

	If empty(Stack)
	   error
	else
	   pop(stack(IndRight, HoleRight, TreeRight)),
	   if null(IndRight) \/ less_than(IndOp, IndRight)
	      contract(IndLeft, stack(IndOp, Hole, term(Op, [TreeRight]), Stack)
	   else if ambiguous(IndOp, IndRight)
		   pull apart TreeRight and create two parse trees
	   else
	      error	% it's impossible that big_than(IndOp, IndRight) holds


 	5. When Op is e: (it happens in T' -> e)
	  create empty stack, and record Remains.
 */

/*------------------------------------------------------------------------------
 * All predicates in this file should be assume to be always succeed (unless
 * stated) and leave no choice points behind.
 *----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
 * term(+Tokens, -Return, +Language).
 * term(+Tokens, -Return, +SpecLeft, +PrecLeft, +Language):-
 *	Token:		one look ahead token
 *	Tokens:		tokens to be parsed
 *	Language:	language of the module
 *	Return:		the returned parse result.  It is a list of:
 *				Stack/Remains or error(A,B)/Remains
 *	  Stack:	a list of: 
 *			stack(Spec, Prec, Hole, ParseTree)
 * 			where Spec and Prec is the indicator of the major
 *			operator in the parse tree, Hole is an unfilled position
 *			in ParseTree.
 *	  Remains:	tokens left unparsed
 *	  ParseTree:	a structure of the form:
 *			term(Name_of_MajorFunctor, ListOfArgus)
 *			referring to the ground representation
 *	  Indicator:	Indicator of the major function in the term, consisting
 *			Spec and Prec. Spec can be a specifier and 'null'.
 *	SpecLeft:	specifier of the immediately left operator, can be
 *			'null' when there is no operator at the left.
 *	PrecLeft:	precedence of the immediately left operator, can be
 *			a positive integer or uninstantiated, indicating
 *			not applicable.
 */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * term/3
 */

% term([], [], _).  comment out to remove choice point, not certain about effect

term(Tokens, Return, Language):-
	term(Tokens, Return, null, _, Language).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * term/6 
 */

% this is the case for Stack=[] and Remains=[]
term([], [[]/[]], _, _, _).

term([Token|Tokens], Return, SpecLeft, PrecLeft, Language):-
   ( check_term_token(Token, SymbolList, Language)
     -> term_aux(SymbolList, Tokens, Return2, SpecLeft, PrecLeft, Language),
	( Return2 = []
	  -> length(Tokens, Position),
	     Return = [error('misuse of infix/postfix functions', [])/Position]
	  ;  Return = Return2
	)
     ;  length(Tokens, Position),
	Return = [error('undeclared or illegal symbol in term: "~w"', [Token])/Position]
   ).

/*------------------------------------------------------------------------------
 * term_aux(+Decl, +Tokens, -Return, +SpecLeft, +PrecLeft, +Language)
 *	for the format of Decl, see interface file.
 */

% The following two term_aux predicates cope with overloading.
term_aux([], _, [], _, _, _).

term_aux([Decl|SymbolList], Tokens, Return, SpecLeft, PrecLeft, Language):-
   term_aux(Decl, Tokens, Return2, SpecLeft, PrecLeft, Language),
   term_aux(SymbolList, Tokens, Return3, SpecLeft, PrecLeft, Language),
   append(Return3, Return2, Return).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

% This clause is for grammar T -> f1 T | f2 T
term_aux(prefix_function(Functor, Indicator), Tokens, Return, SpecLeft,
		PrecLeft, Language) :-
   Indicator =.. [SpecOp, PrecOp],
   term(Tokens, Return2, SpecOp, PrecOp, Language),
   term_aux_prefix(Return2, Return, Tokens, SpecLeft, PrecLeft, 
			Functor, SpecOp, PrecOp).

term_aux_prefix([], [], _, _, _, _, _, _).
term_aux_prefix([Stack/Remains|Return], NewReturn, Tokens, SpecLeft, PrecLeft,
		Functor, SpecOp, PrecOp):-
   ( Stack = error(_, _)
     -> NewReturn = [Stack/Remains|NewReturn2]
     ;  ( pop(stack(SpecRight, PrecRight, HoleRight, TreeRight), Stack, Stack2),
          nonvar(HoleRight)
          -> ( ( null(SpecLeft, PrecLeft);
                 less_than(SpecLeft, PrecLeft, SpecOp, PrecOp) 
               )
	       -> ( ( null(SpecRight, PrecRight);
	              less_than(SpecOp, PrecOp, SpecRight, PrecRight)
	            )
	            -> contract(Stack2, SpecLeft, PrecLeft, 
		     	        stack(SpecOp, PrecOp, no_hole,
				'MetaDefs.Term.F2'(Functor, [TreeRight])),
			        Stacks),
		       stack_to_return(Stacks, Tokens, Remains, NewReturn,
					NewReturn2)
	            ;  ( ambiguous(SpecOp, PrecOp, SpecRight, PrecRight)  
		         -> % assume PrecOp < PrecRight
			    copy_term(Stack2, NewStack2),
			    contract(Stack2, SpecLeft, PrecLeft, 
				     stack(SpecOp, PrecOp, no_hole,
				      'MetaDefs.Term.F2'(Functor, [TreeRight])),
				     Stacks2),
			    stack_to_return(Stacks2, Tokens, Remains,
						NewReturn3, NewReturn2),
			    % assume PrecOp > PrecRight
			    TreeRight = 'MetaDefs.Term.F2'(FuncRight,
								[Argu|Argus]),
			    contract(NewStack2, SpecLeft, PrecLeft, 
				     stack(SpecRight, PrecRight, no_hole,
				      'MetaDefs.Term.F2'(FuncRight,
				         ['MetaDefs.Term.F2'(Functor, [Argu])
					  |Argus])),
				     Stacks3),
			    stack_to_return(Stacks3, Tokens, Remains, NewReturn,
						NewReturn3)
	       		 ;  TreeRight = 'MetaDefs.Term.F2'(FuncRight, _),
			    length(Tokens, Position),
			    NewReturn = [ error('wrong precedence or associativity between operators: "~w" and "~w"', [Functor, FuncRight])/Position
					 | NewReturn2]
		       )
	          )
	       ;  length(Tokens, Position),
		  NewReturn = [ error('misuse of operator "~w" (wrong precedence or associativity)', [Functor])/Position
		    	       | NewReturn2]
	     )
          ;  length(Tokens, Position),
	     NewReturn = [ error('misuse of prefix function "~w"', [Functor])/Position| NewReturn2]
        )
   ),
   term_aux_prefix(Return, NewReturn2, Tokens, SpecLeft, PrecLeft, 
			Functor, SpecOp, PrecOp).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * error handling in terms of function_no_ind, list, (Term):
 *	if there is an valid parsing for this term, the errors encountered
 *	inside the term is discarded, otherwise they are returned.
 */

% The following procedures correspond to the grammar: T -> c T'

% function without indicator
% function_no_ind should take a list of function declarations. This is for
% identify function with their arities.
term_aux(function_no_ind(Functors), Tokens, Return, SpecLeft, PrecLeft,
		Language) :-
   ( Tokens = [ '(' |Tokens2]
     -> term_seq(Tokens2, Remains2, Term_seqs, Language, ErrorReturn),
		% there can be only one remains
	( Term_seqs = []
	  -> Return = ErrorReturn
	  ; ( Remains2 = [')'|Remains3]
	      -> Term_seqs = [H|_],
		 length(H, Arity), 
	         create_functions(Functors, Term_seqs, Arity, Terms, []),
		 ( Terms = []
		   -> Functors = ['MetaDefs.Name.F4'(_, GF, _, _)|_],
		      length(Remains3, Position),
		      Return = [error('undefined function "~w/~d"', [F, Arity])/Position],
		      gstring2string(GF, F)
		   ;  term_infinite(Remains3, Return, Terms, SpecLeft, PrecLeft,
				Language)
		 )
	      ;  length(Remains2, Position),
		 Return = [error('missing ")"', [])/Position]
	    )
	)
     ;  length(Tokens, Position),
	Return = [error('missing "("', [])/Position]
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Giving a list of normal function declarations and a list of term_seqs,
 * this returns the parse trees of the function. Arity of the function is
 * checked. 
 */

create_functions([], _, _, Terms, Terms).
create_functions(['MetaDefs.Name.F4'(Module, Symbol, 'MetaDefs.Function.C0',
			ArityD)|Functors], Term_seqs, Arity, Terms, Terms2) :-
	( ArityD == Arity
	  -> create_functions_aux(Term_seqs, 'MetaDefs.Name.F4'(Module,
			Symbol, 'MetaDefs.Function.C0', ArityD), Terms, Terms3)
	  ;  Terms = Terms3
	),
	create_functions(Functors, Term_seqs, Arity, Terms3, Terms2).


create_functions_aux([], _, Terms, Terms).
create_functions_aux([Term_seq|Term_seqs], Functor, 
		['MetaDefs.Term.F2'(Functor, Term_seq)|Terms], Terms2) :-
	create_functions_aux(Term_seqs, Functor, Terms, Terms2).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

% list
term_aux(list, Tokens, Return, SpecLeft, PrecLeft, Language) :-
   ( lookup_constructor('"List', 1, '"Lists', Language)
     -> list(Tokens, Remains, ListTerms, Language, ErrorReturn),
	( ListTerms = []
	  -> Return = ErrorReturn
	  ;  term_infinite(Remains, Return, ListTerms, SpecLeft, PrecLeft,
				Language)
	)
     ;  length(Tokens, Position),
        Return = [error('system module "Lists" not imported', [])/Position]
  ).

% set
term_aux(set, Tokens, Return, SpecLeft, PrecLeft, Language) :-
   ( lookup_constructor('"Set', 1, '"Sets', Language)
     -> set(Tokens, Remains, SetTerms, Language, ErrorReturn),
	( SetTerms = []
	  -> Return = ErrorReturn
	  ;  term_infinite(Remains, Return, SetTerms, SpecLeft, PrecLeft,
				Language)
	)
     ;  length(Tokens, Position),
        Return = [error('system module "Sets" not imported', [])/Position]
  ).

% (Term)
term_aux(bracket, Tokens, Return, SpecLeft, PrecLeft, Language) :-
	term(Tokens, Return2, null, _, Language),
	( select_return(Return2, [')'], Terms, Remains2, ErrorReturn)
	  -> ( Terms = []	% indicating the term cannot be parsed
	       -> ( select_return(Return2, null, Terms4, Remains4, _)
		    -> true
		    ;  write('Internal error No 1.'), nl, abort
		  ),
		  ( Terms4 = []
		    -> Return = ErrorReturn
		    ;  length(Remains4, Position),
		       Return = [error('")" expected', [])/Position|ErrorReturn]
		  )
	       ;  ( Remains2 = [')'|Remains3]
		    -> term_infinite(Remains3, Return, Terms, SpecLeft,
					PrecLeft, Language)
		    ;  length(Remains2, Position),
		       Return = [error('missing ")"', [])/Position]
		  )
	     )
	  ;  write('Internal error No 2.'), nl, abort
				% this should never happen
	).


% constant
term_aux(constant(ConstantTerms), Tokens, Return, SpecLeft, PrecLeft, 
		Language) :-
	term_infinite(Tokens, Return, ConstantTerms, SpecLeft, PrecLeft,
			Language).


% variable, number, float and string
term_aux(simple_object(Object), Tokens, Return, SpecLeft, PrecLeft, Language) :-
	term_infinite(Tokens, Return, [Object], SpecLeft, PrecLeft, Language).


% infix/postfix functions and predicates are missed out in T
term_aux(infix_function(_, _), Tokens,
		[error('unexpected infix function', [])/Position], _, _, _) :-
	length(Tokens, Position).
term_aux(postfix_function(_, _), Tokens,
		[error('unexpected postfix function', [])/Position], _, _, _) :-
	length(Tokens, Position).
term_aux(predicate(_), Tokens,
		[error('unexpected predicate', [])/Position], _, _, _) :-
	length(Tokens, Position).


/*------------------------------------------------------------------------------
 * term_infinite:
 *	LeftTerms is a list of terms of Term(8) on the immediate left side.
 */

term_infinite(Tokens, Return, LeftTerms, SpecLeft, PrecLeft, Language) :-
   term_prime(Tokens, Return2, SpecLeft, PrecLeft, Language),
   term_infinite_aux(Return2, Return, Tokens, LeftTerms, SpecLeft, PrecLeft).


term_infinite_aux([], [], _, _, _, _).
term_infinite_aux([Stack/Remains|Return], NewReturn, Tokens, LeftTerms,
			SpecLeft, PrecLeft) :-
   ( Stack = error(_, _)
     -> NewReturn = [Stack/Remains|NewReturn2]
     ;  ( pop(stack(SpecRight, PrecRight, _, TreeRight), Stack, _)
          -> ( null(SpecRight, PrecRight)
               -> length(Tokens, Position),
		  NewReturn = [error('operator expected', [])/Position|NewReturn2]
	       ;  ( ( null(SpecLeft, PrecLeft);
	              less_than(SpecLeft, PrecLeft, SpecRight, PrecRight);
		      ambiguous(SpecLeft, PrecLeft, SpecRight, PrecRight)
	            )
	            -> fill_holes(LeftTerms, Stack, NewStacks),
		       stack_to_return(NewStacks, Tokens, Remains, NewReturn,
					NewReturn2)
	            ;  ( bigger_than(SpecLeft, PrecLeft, SpecRight, PrecRight)
		         -> push_in_constants(LeftTerms, Stack, NewStacks2),
		            stack_to_return(NewStacks2, Tokens, Remains,
						NewReturn, NewReturn2)
		         ;  TreeRight = 'MetaDefs.Term.F2'(FuncRight, _),
			    length(Tokens, Position),
		            NewReturn = [error('misuse of operator "~w" (wrong precedence or associativity)', [FuncRight])/Position
					 |NewReturn2]
		       )
	         )
	     )
          ;  push_in_constants(LeftTerms, Stack, NewStacks3),
	     stack_to_return(NewStacks3, Tokens, Remains, NewReturn, NewReturn2)
			% equivalent to a push
        )
   ),
   term_infinite_aux(Return, NewReturn2, Tokens, LeftTerms, SpecLeft, PrecLeft).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

% This fills the HoleRight with one of LeftTerms.
fill_holes([LeftTerm], Stack, [Stack]) :-
	!,
	pop(stack(_, _, LeftTerm, _), Stack, _).
		% HoleRight is filled with LeftTerm straight away
fill_holes([LeftTerm|LeftTerms], Stack, [Stack2|NewStacks]):-
	copy_term(Stack, Stack2),
	pop(stack(_, _, LeftTerm, _), Stack2, _),
	fill_holes(LeftTerms, Stack, NewStacks).

% This pushs in a term of Term(8) into the stack.
push_in_constants([LeftTerm], Stack, [Stack2]) :-
	!,
	push(stack(null, _, no_hole, LeftTerm), Stack, Stack2).
push_in_constants([LeftTerm|LeftTerms], Stack, [Stack2|NewStacks]) :-
	copy_term(Stack, Stack3),
	push(stack(null, _, no_hole, LeftTerm), Stack3, Stack2),
	push_in_constants(LeftTerms, Stack, NewStacks).


/*------------------------------------------------------------------------------
 * check_term_token(+Token, -SymbolList, +Language)
 * It succeeds when Token is a valid in terms, and fails if Token is not
 * valid in Term.  
 * 	SymbolList is a list of elements in the form:
 *		Attribute/decl(Functor, Indicator),
 *		or null (not applicable)
 *	when Attribute is function_no_ind (or constant), the declaration part
 *	is a list of declarations, all function_no_ind (or constants) specified
 *	with that symbol.
 */

% This bit can be optimised by throwing away lists
check_term_token(little_name(Var), simple_object(VarRep), _) :-
   var_conversion(Var, 0, VarRep).

check_term_token('_'(Var), simple_object('MetaDefs.Var.F2'(GVar, Label)), _) :-
   string2Gstring(Var, GVar),
   underscore_label(Label).

check_term_token(string(String),
		simple_object('MetaDefs.Str.F1'(GString)), Language) :-
   lookup_base('"String', '"Strings', Language),
   string2Gstring(String, GString).

check_term_token(float(Float), simple_object('MetaDefs.Flo.F1'(Float)),
		Language) :-
   lookup_base('"Float', '"Floats', Language).

check_term_token(number(N), simple_object('MetaDefs.Num.F1'(N)), Language):-
   lookup_base('"Integer', '"Integers', Language).

check_term_token('[', list, _).
check_term_token('{', set, _).
check_term_token('(', bracket, _).
check_term_token(big_name(Name), SymbolList, Language) :-
   lookup_function_dict(big_name(Name), SymbolList, Language).
	% decide whether it's a constant, function_no_ind, prefix_function,
	%	infix_function, and postfix_function.
	%	Symbols of other kinds should not be included in SymbolList.
check_term_token(graphic_name(Name), SymbolList, Language) :-
   lookup_function_dict(graphic_name(Name), SymbolList, Language).


%------------------------------------------------------------------------------

var_conversion(Var, Label, VarRep) :-
   ( Var = 'v'
     -> VarRep = 'MetaDefs.Var.F1'(Label)
     ;  VarRep = 'MetaDefs.Var.F2'(GVar, Label),
	string2Gstring(Var, GVar)
   ).

/*------------------------------------------------------------------------------
 * This procedure assumes that every term in a term_seq should end at the same
 * place.  Can this be verified?
 * Error returns are removed from the sequence of terms.  If there is no valid
 * parse tree, term_seq is returned as an empty list.
 */

term_seq(Tokens, Remains, Term_seqs, Language, ErrorReturn) :-
   term_seq_aux(Tokens, Remains, Amb_term_seqs, Language, ErrorReturn2),
   cartesian_product(Amb_term_seqs, Term_seqs),
   ( Term_seqs = [],
     ErrorReturn2 = []
     -> length(Remains, Position),
	ErrorReturn = [error('"," or ")" expected', [])/Position]
     ;  ErrorReturn = ErrorReturn2
   ).


term_seq_aux(Tokens, Remains, Terms_seq, Language, ErrorReturn) :-
	term(Tokens, Return, null, _, Language),
	( select_return(Return, [',', ')'], Terms, Remains2, ErrorReturn2)
	  -> ( Terms = []	% indicating the term cannot be parsed
	       -> Terms_seq = [],
		  Remains = Remains2,
	          ( select_return(Return, null, Terms4, Remains4, _)
		    -> true
		    ;  write('Internal error No 3.'), nl, abort
		  ),
		  ( Terms4 = []
		    -> ErrorReturn = ErrorReturn2
		    ;  length(Remains4, Position),
		       ErrorReturn =
		        [error('")" or "," expected', [])/Position|ErrorReturn2]
		  )
	       ;  ( Remains2 = [')'|_]
		    -> Remains = Remains2,
		       ErrorReturn = [],
	               Terms_seq = [Terms]
		    ;  ( Remains2 = [','|Remains3]
			 -> term_seq_aux(Remains3, Remains, Terms_seq2,
						Language, ErrorReturn),
		            ( Terms_seq2 = []
			      -> Terms_seq = []
			      ;  Terms_seq = [Terms|Terms_seq2]
		            )
			 ;  Terms_seq = [],
			    length(Remains2, Position),
			    ErrorReturn = [error('incomplete term sequence or illegal symbol in term sequence', [])/Position]
		       )
		  )
	     )
	  ;  write('Internal error No 4.'), nl, abort
				% this should never happen
	).



/*------------------------------------------------------------------------------
 */

% This separates error returns and proper returns.  Returns not ended with
% a certain Token (e.g. ',') is also removed.
% NewRemains can be left uninstantiated when Terms is []

select_return([], _, [], _, []).
select_return([Stack/Remains|Rest], Terminators, Terms, NewRemains,
			ErrorReturn) :-
   ( Stack = error(_, _)
     -> ErrorReturn = [Stack/Remains|ErrorReturn2],
	Terms = Terms2
     ;  ErrorReturn = ErrorReturn2,
	select_valid_term(Terminators, Stack, Terms, Terms2,
		NewRemains, Remains)	% this should not fail, if it does so
					% it's an internal error.
   ),
   select_return(Rest, Terminators, Terms2, NewRemains, ErrorReturn2).

%------------------------------------------------------------------------------

select_valid_term(Terminators, Stack, Terms, Terms2, NewRemains,
	Remains) :-
   ( Stack = [stack(_, _, Hole, Term)], nonvar(Hole)
     -> select_valid_term_aux(Terminators, Term, Terms, Terms2,
		NewRemains, Remains)
     ;  Terms = Terms2             % ignore the error
   ).

% the case of accept all cases
select_valid_term_aux(null, Term, [Term|Terms], Terms, Remains, Remains).
% the case of accept only end_of_atom condition
select_valid_term_aux(end_of_atom, Term, Terms2, Terms, NewRemains, Remains) :-
   ( end_of_atom(Remains)
     -> Terms2 = [Term|Terms],
        NewRemains = Remains
     ;  Terms2 = Terms
   ).
% the same as null, but should not have been used
select_valid_term_aux([], Term, [Term|Terms], Terms, Remains, Remains).
% the case with a list of terminators
select_valid_term_aux([H|T], Term, Terms2, Terms, NewRemains, Remains) :-
   ( Remains = [Token|_]
     -> ( member_check(Token, [H|T])
	  -> Terms2 = [Term|Terms],	% when it ends at designated terminator
	     NewRemains = Remains	% accept it.
	  ;  Terms2 = Terms
	)
     ;  Terms2 = [Term|Terms],		% when it's the end of item, accept it.
	NewRemains = Remains
   ).

/*------------------------------------------------------------------------------
 * list(+Tokens, -Remains, +ListTerms, +Language, -ErrorReturn) :-
 *	ListTerms is a list of parse trees for the list term.
 *	ErrorReturn contains all the errors encountered in parsing the list.
 */

list(Tokens, Remains, ListTerms, Language, ErrorReturn) :-
	( Tokens = [ ']'|Remains]
	  -> ListTerms = ['MetaDefs.CTerm.F1'('MetaDefs.Name.F4'('"Lists',
				'"Nil', 'MetaDefs.Constant.C0', 0))]
	  ;  list_expr(Tokens, Remains2, ListTerms2, Language, ErrorReturn2),
	     ( ListTerms2 = []
	       -> ListTerms = [],
		  ErrorReturn = ErrorReturn2
	       ;  ( Remains2 = [']'|Remains3]
	            -> ListTerms = ListTerms2,
		       Remains = Remains3
	            ;  ListTerms = [],
		       length(Remains2, Position),
		       ErrorReturn = [error('missing "]"', [])/Position]
		  )
	     )
	).


list_expr(Tokens, Remains, ListTerms, Language, ErrorReturn) :- 
   term(Tokens, Return, null, _, Language),
   ( select_return(Return, [graphic_name('|'), ',', ']' ], Terms, Remains2,
			ErrorReturn2)
     -> ( Terms = []	% indicating the term cannot be parsed
	  -> ( select_return(Return, null, Terms4, Remains4, _)
	       -> true
	       ;  write('Internal error No. 5'), nl, abort
	     ),
	     ( Terms4 = []
	       -> ErrorReturn = ErrorReturn2
	       ;  length(Remains4, Position),
		  ErrorReturn = [error('"|", "," or "]" expected', [])/Position|
				 ErrorReturn2]
	     ),
	     ListTerms = []
	  ;  ( Remains2 = [Token|Remains3]
	       -> ( list_expr_aux(Token, Remains3, Remains, Terms, ListTerms,
					ErrorReturn, Language)
		    -> true
		    ;  ListTerms = [],
		       length(Remains2, Position),
		       ErrorReturn = [error('illegal symbol in list: "~w"',
					[Token])/Position]
		  )
	       ;  ListTerms = [],
		  length(Remains2, Position),
		  ErrorReturn = [error('incomplete list', [])/Position]
	     )
         )
      ;  write('Internal error No 6.'), nl, abort
			% this should never happen
   ).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * list_expr_aux(+Token, +Tokens, -Remains, -Terms, -TailTerms, -ErrorReturn,
 *			+Language)
 *	Token identifies the three cases in a list structure.
 *	Terms is a list of heads.
 *	TailTerms is a list of tails.
 */

list_expr_aux(',', Remains2, Remains, Terms, TailTerms, ErrorReturn, Language):-
   list_expr(Remains2, Remains3, TailTerms2, Language, ErrorReturn2),
   ( TailTerms2 = []
     -> TailTerms = [],
	ErrorReturn = ErrorReturn2
     ;  ErrorReturn = [],
	Remains = Remains3,
	create_lists(Terms, TailTerms2, TailTerms, [])
   ).

list_expr_aux(']', Remains2, [']'|Remains2], Terms, TailTerms, [], _):-
   create_lists(Terms, ['MetaDefs.CTerm.F1'('MetaDefs.Name.F4'('"Lists',
			'"Nil', 'MetaDefs.Constant.C0', 0))], TailTerms, []).

list_expr_aux(graphic_name('|'), Remains2, Remains, Terms, TailTerms,
		ErrorReturn, Language):-
   ( Remains2 = [Token|Remains3]
     -> ( (  Token = little_name(V), Label = 0
	   ; Token = '_'(V), underscore_label(Label)
	  )
	  -> ErrorReturn = [],
	     Remains = Remains3,
	     var_conversion(V, Label, VarRep),
	     create_lists(Terms, [VarRep], TailTerms, [])
	  ;  ( Token = '['
	       -> list(Remains3, Remains4, TailTerms2, Language, ErrorReturn2),
		  ( TailTerms2 = []
		    -> TailTerms = [],
		       ErrorReturn = ErrorReturn2
		    ;  ErrorReturn = [],
		       Remains = Remains4,
		       create_lists(Terms, TailTerms2, TailTerms, [])
		  )
	       ;  length(Remains2, Position),
		  ErrorReturn = [error('illegal symbol in list: "~w"', [Token])/Position],
		  TailTerms = []
	     )
        )
     ;  TailTerms = [],
	length(Remains3, Position),
	ErrorReturn = [error('incomplete list', [])/Position]
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Given a list of Terms and a list of TailTerms, create_lists create ambiguous
 * parse trees for the list.
 */

create_lists([], _, ListTerms, ListTerms).
create_lists([Term|Terms], TailTerms, ListTerms, ListTerms2) :-
	create_lists_aux(TailTerms, Term, ListTerms, ListTerms3),
	create_lists(Terms, TailTerms, ListTerms3, ListTerms2).


create_lists_aux([], _, ListTerms, ListTerms).
create_lists_aux([TailTerm|TailTerms], Term, 
			['MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Lists',
			     '"Cons', 'MetaDefs.Function.C0', 2),
			[Term, TailTerm])|ListTerms],
			ListTerms2) :-
	create_lists_aux(TailTerms, Term, ListTerms, ListTerms2).


/*------------------------------------------------------------------------------
 * set(+Tokens, -Remains, +SetTerms, +Language, -ErrorReturn) :-
 *	SetTerms is a set of parse trees for the set term.
 *	ErrorReturn contains all the errors encountered in parsing the set.
 */

set(Tokens, Remains, SetTerms, Language, ErrorReturn) :-
   ( Tokens = [ '}'(1000000000)|Remains]
     -> SetTerms = ['MetaDefs.CTerm.F1'('MetaDefs.Name.F4'('"Sets',
				'"Null', 'MetaDefs.Constant.C0', 0))]
     ;  set_expr(Tokens, Remains2, SetTerms2, Language, ErrorReturn2),
	( SetTerms2 = []
	  -> SetTerms = [],
	     ErrorReturn = ErrorReturn2
	  ;  ( Remains2 = ['}'(1000000000)|Remains3]
	       -> SetTerms = SetTerms2,
		  Remains = Remains3
	       ;  SetTerms = [],
		  length(Remains2, Position),
		  ErrorReturn = [error('missing "}"', [])/Position]
	     )
	)
   ).

%------------------------------------------------------------------------------

set_expr(Tokens, Remains, SetTerms, Language, ErrorReturn) :- 
   term(Tokens, Return, null, _, Language),
   ( select_return(Return, [graphic_name('|'), graphic_name(':'), ',',
			'}'(1000000000)], Terms, Remains2, ErrorReturn2)
     -> ( Terms = []	% indicating the term cannot be parsed
	  -> ( select_return(Return, null, Terms4, Remains4, _)
	       -> true
	       ;  write('Internal error No 7.'), nl, abort
	     ),
	     ( Terms4 = []
	       -> ErrorReturn = ErrorReturn2
	       ;  length(Remains4, Position),
		  ErrorReturn =
			[error('"|", ":", "," or "}" expected', [])/Position|
			 ErrorReturn2]
	     ),
	     SetTerms = []
	  ;  ( Remains2 = [Token|Remains3]
	       -> ( set_expr_aux(Token, Remains3, Remains, Terms, SetTerms,
					ErrorReturn, Language)
		    -> true
		    ;  SetTerms = [],
		       length(Remains2, Position),
		       ( Token = '}'(_)
			 -> ErrorReturn = [error('illegal symbol in set: "~w"',
						['}'])/Position]
		         ;  ErrorReturn = [error('illegal symbol in set: "~w"',
					[Token])/Position]
		       )
		  )
	       ;  SetTerms = [],
		  length(Remains2, Position),
		  ErrorReturn = [error('incomplete set', [])/Position]
	     )
         )
      ;  write('Internal error No 8.'), nl, abort
			% this should never happen
   ).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * set_expr_aux(+Token, +Tokens, -Remains, -Terms, -TailTerms, -ErrorReturn,
 *			+Language)
 *	Token identifies the three cases in a set structure.
 *	Terms is a set of heads.
 *	TailTerms is a set of tails.
 */


set_expr_aux(',', Tokens, Remains, Terms, TailTerms, ErrorReturn, Language):-
   set_expr(Tokens, Remains3, TailTerms2, Language, ErrorReturn2),
   ( TailTerms2 = []
     -> TailTerms = [],
	ErrorReturn = ErrorReturn2
     ;  ErrorReturn = [],
	Remains = Remains3,
	create_sets(Terms, TailTerms2, TailTerms, [])
   ).

set_expr_aux('}'(1000000000), Tokens, ['}'(1000000000)|Tokens], Terms,
	TailTerms, [], _):-
   create_sets(Terms, ['MetaDefs.CTerm.F1'('MetaDefs.Name.F4'('"Sets', '"Null',
	'MetaDefs.Constant.C0', 0))], TailTerms, []).

set_expr_aux(graphic_name(':'), Tokens, Remains, Terms, TailTerms,
		ErrorReturn, Language):-  !,	% note the cut
   formula(Tokens, Return, Language),
   ( select_return(Return, null, Formulae, Remains, ErrorReturn2)
     -> ( Formulae = []
          -> TailTerms = [],
	     ErrorReturn = ErrorReturn2
          ;  Terms = [Term|_],
	     Formulae = [Formula|_],
	     collect_variables(Term, [], TVars),
	     collect_variables(Formula, [], FVars),
	     ( subset(TVars, FVars)
	       -> ErrorReturn = [],
	          create_intensional_set([Terms, Formulae], TailTerms)
	       ;  TailTerms = [],
	          set_minus(TVars, FVars, Rest),
		  strip_variables(Rest, Rest2),
	          length(Tokens, Position),
	          ErrorReturn = [error('bound variables "~w" in the intensional set do not occur in the formula', [Rest2])/Position]
	     )
       )
     ; write('Internal error No 9.'), nl, abort
   ).

set_expr_aux(graphic_name('|'), Tokens, Remains, Terms, TailTerms,
		ErrorReturn, Language):-
   ( Tokens = [Token|Remains3]
     -> ( (  Token = little_name(V), Label = 0
	   ; Token = '_'(V), underscore_label(Label)
	  )
	  -> var_conversion(V, Label, VarRep),
	     ErrorReturn = [],
	     Remains = Remains3,
	     create_sets(Terms, [VarRep], TailTerms, [])
%	     create_sets(Terms,
%		     ['MetaDefs.CTerm.F1'('MetaDefs.Name.F4'('"Sets', '"Null',
%			 'MetaDefs.Constant.C0', 0))], SetTerms, []),
%	     cartesian_product([SetTerms, [VarRep]], TermSeq),
%	     create_functions(
%		['MetaDefs.Name.F4'('"Sets', '"+', 'MetaDefs.Function.C0', 2)],
%		TermSeq, 2, TailTerms, [])
%  the organisation here should be done in the compiler.
	  ;  ( Token = '{'
	       -> set(Remains3, Remains4, TailTerms2, Language, ErrorReturn2),
		  ( TailTerms2 = []
		    -> TailTerms = [],
		       ErrorReturn = ErrorReturn2
		    ;  ErrorReturn = [],
		       Remains = Remains4,
		       create_sets(Terms, TailTerms2, TailTerms, [])
		  )
	       ;  length(Tokens, Position),
		  ( Token = '}'(_)
		    -> ErrorReturn = [error('illegal symbol in set: "~w"',
						['}'])/Position]
		    ;  ErrorReturn = [error('illegal symbol in set: "~w"',
					[Token])/Position]
		  ),
		  TailTerms = []
	     )
        )
     ;  TailTerms = [],
	length(Remains3, Position),
	ErrorReturn = [error('incomplete set', [])/Position]
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Given a set of Terms and a set of TailTerms, create_sets create ambiguous
 * parse trees for the set.
 */

create_sets([], _, SetTerms, SetTerms).
create_sets([Term|Terms], TailTerms, SetTerms, SetTerms2) :-
	create_sets_aux(TailTerms, Term, SetTerms, SetTerms3),
	create_sets(Terms, TailTerms, SetTerms3, SetTerms2).


create_sets_aux([], _, SetTerms, SetTerms).
create_sets_aux([TailTerm|TailTerms], Term, 
			['MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Sets',
			     '"Inc', 'MetaDefs.Function.C0', 2),
			[Term, TailTerm])|SetTerms],
			SetTerms2) :-
	create_sets_aux(TailTerms, Term, SetTerms, SetTerms2).

%------------------------------------------------------------------------------

create_intensional_set(Terms, NewTerms) :-
   cartesian_product(Terms, Terms2),
   create_intensional_set_aux(Terms2, NewTerms).

create_intensional_set_aux([], []).
create_intensional_set_aux([[Term1, Term2]|TermSeqs],
	['MetaDefs.SuchThat.F2'(Term1, Term2)|NewTerms]) :-
   create_intensional_set_aux(TermSeqs, NewTerms).

/*------------------------------------------------------------------------------
 * term_prime(+Tokens, -Return, +SpecLeft, +PrecLeft, +Language).
 *	parameters are defined the same as term/6
 * term_prime recognises the termination of a term which are:
 *	empty, ')', ']', '}', ';', ',', '|'  predicate, connective, 
 */

% partly corresponding to the grammar T' -> e
term_prime([], [[]/[]], _, _, _).

% corresponding to the grammar T' -> f3 T'| f4 T'| f5 T | f6 T | f7 T | e
term_prime([Token|Tokens], Return, SpecLeft, PrecLeft, Language):-
   ( end_of_term(Token)
     -> Return = [ []/[Token|Tokens] ]
     ;  ( check_term_token(Token, SymbolList, Language)
	  -> term_prime_aux(SymbolList, Tokens, Return2, SpecLeft,
				PrecLeft, Language),
	     ( Return2 = []
	       -> length(Tokens, Position),
		  Return = [ error('operator expected', [])/Position]
	       ;  Return = Return2
	     )
	  ;  length(Tokens, Position),
	     Return = [error('undeclared or illegal symbol in term: "~w"', [Token])/Position]
	)
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

% Predicate is also a sign of end-of-term, delt in term_prime_aux
end_of_term(',') :- !.
end_of_term(';') :- !.
end_of_term(':') :- !.
end_of_term(']') :- !.
end_of_term(graphic_name(Name)) :- !,
   end_of_term(Name).
end_of_term(Token) :-
   end_of_atom(Token).

/*------------------------------------------------------------------------------
 */

% The following two predicate of term_prime_aux copes with overloading
term_prime_aux([], _, [], _, _, _).

term_prime_aux([Decl|SymbolList], Tokens, Return, SpecLeft, PrecLeft,
		Language) :-
	term_prime_aux(Decl, Tokens, Return2, SpecLeft, PrecLeft, Language),
	term_prime_aux(SymbolList, Tokens, Return3, SpecLeft, PrecLeft,
			Language),
	append(Return3, Return2, Return).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

% corresponding to the grammar T' -> f3 T'| f4 T'
term_prime_aux(postfix_function(Functor, Indicator), Tokens, Return, SpecLeft,
		PrecLeft, Language):-
   Indicator =.. [SpecOp, PrecOp],
   term_prime(Tokens, Return2, SpecOp, PrecOp, Language),
   term_prime_postfix(Return2, Return, Tokens, SpecLeft, PrecLeft, Functor,
			    SpecOp, PrecOp).

term_prime_postfix([], [], _, _, _, _, _, _).
term_prime_postfix([Stack/Remains|Return], NewReturn, Tokens, SpecLeft,
	PrecLeft, Functor, SpecOp, PrecOp):-
   ( Stack = error(_, _)
     -> NewReturn = [Stack/Remains|NewReturn2]
     ; ( pop(stack(SpecRight, PrecRight, _, TreeRight), Stack, _)
         -> ( null(SpecRight, PrecRight)
	      -> length(Tokens, Position),
		 NewReturn = [ error('error with unknown reason', [])/Position
			      | NewReturn2]
	      ;  ( bigger_than(SpecOp, PrecOp, SpecRight, PrecRight)
	           -> ( ( null(SpecLeft, PrecLeft);
	                  less_than(SpecLeft, PrecLeft, SpecOp, PrecOp) 
		        )
	       	        -> contract(Stack, SpecLeft, PrecLeft, 
		    	            stack(SpecOp, PrecOp, Hole,
					'MetaDefs.Term.F2'(Functor, [Hole])),
			            Stacks),
		           stack_to_return(Stacks, Tokens, Remains, NewReturn,
					NewReturn2)
	                ;  ( ( bigger_than(SpecLeft, PrecLeft, SpecOp, PrecOp);
		    	       ambiguous(SpecLeft, PrecLeft, SpecOp, PrecOp)
			     )
			     -> NewReturn = [Stack2/Remains|NewReturn2],
				push(stack(SpecOp, PrecOp, Hole,
				 	'MetaDefs.Term.F2'(Functor, [Hole])),
					Stack, Stack2)
			     ;  length(Tokens, Position),
				NewReturn = [ error('misuse of operator "~w" (wrong precedence or associativity', [Functor])/Position
					     | NewReturn2]
		           )
		      )
	           ;  TreeRight = 'MetaDefs.Term.F2'(FuncRight, _),
		      length(Tokens, Position),
		      NewReturn = [ error('wrong precedence or associativity between operators "~w" and "~w"', [Functor, FuncRight])/Position
				   | NewReturn2]
	         )
	    )
         ;  NewReturn = [ [stack(SpecOp, PrecOp, Hole,
			   'MetaDefs.Term.F2'(Functor, [Hole]))]/Remains
			 | NewReturn2 ]
       )
   ),
   term_prime_postfix(Return, NewReturn2, Tokens, SpecLeft, PrecLeft, 
			Functor, SpecOp, PrecOp).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

% corresponding to the grammar T' -> f5 T | f6 T | f7 T
term_prime_aux(infix_function(Functor, Indicator), Tokens, Return, SpecLeft,
		PrecLeft, Language):-
   Indicator =.. [SpecOp, PrecOp],
   term(Tokens, Return2, SpecOp, PrecOp, Language),
   term_prime_infix(Return2, Return, Tokens, SpecLeft, PrecLeft,
		       Functor, SpecOp, PrecOp).


term_prime_infix([], [], _, _, _, _, _, _).
term_prime_infix([Stack/Remains|Return], NewReturn, Tokens, SpecLeft, PrecLeft,
		Functor, SpecOp, PrecOp):-
   ( Stack = error(_, _)
     -> NewReturn = [Stack/Remains|NewReturn2]
     ; ( pop(stack(SpecRight, PrecRight, HoleRight, TreeRight), Stack, Stack2),
         nonvar(HoleRight)
         -> ( ( null(SpecRight, PrecRight);
	        less_than(SpecOp, PrecOp, SpecRight, PrecRight)
	      )
	      -> contract(Stack2, SpecLeft, PrecLeft, 
			    stack(SpecOp, PrecOp, Hole,
				'MetaDefs.Term.F2'(Functor, [Hole, TreeRight])),
		          Stacks),
		 stack_to_return(Stacks, Tokens, Remains, NewReturn, NewReturn2)
	      ;  ( ambiguous(SpecOp, PrecOp, SpecRight, PrecRight)
	           -> % assume PrecOp < PrecRight
		      copy_term(Stack2, NewStack2),
		      contract(Stack2, SpecLeft, PrecLeft, 
		               stack(SpecOp, PrecOp, Hole,
			        'MetaDefs.Term.F2'(Functor, [Hole, TreeRight])),
			       Stacks2),
		      stack_to_return(Stacks2, Tokens, Remains, NewReturn3,
					NewReturn2),
		      % assume PrecOp > PrecRight
		      TreeRight = 'MetaDefs.Term.F2'(FuncRight, [Argu|Argus]),
		      contract(NewStack2, SpecLeft, PrecLeft, 
			       stack(SpecRight, PrecRight, Hole2,
				'MetaDefs.Term.F2'(FuncRight,
				['MetaDefs.Term.F2'(Functor, [Hole2, Argu])
				  |Argus])),
			       Stacks3),
		      stack_to_return(Stacks3, Tokens, Remains, NewReturn,
					NewReturn3)
	           ;  TreeRight = 'MetaDefs.Term.F2'(FuncRight, _),
		      length(Tokens, Position),
		      NewReturn = [ error('wrong precedence or associativity between operators "~w" and "~w"', [Functor, FuncRight])/Position
				   | NewReturn2]
	         )
	    )
         ;  length(Tokens, Position),
	    NewReturn = [ error('misuse of infix function "~w"', [Functor])/Position
			 |NewReturn2]
       )
   ),
   term_prime_infix(Return, NewReturn2, Tokens, SpecLeft, PrecLeft, 
			Functor, SpecOp, PrecOp).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

% variable, list, string, number, constant, float, and prefix functions
% are missed out in T', because they are not legal in T'.
term_prime_aux(prefix_function(_, _), Tokens,
		[error('unexpected prefix function', [])/Position], _, _, _):-
   length(Tokens, Position).
term_prime_aux(function_no_ind(_), Tokens,
		[error('unexpected function of no indicator', [])/Position],
		_, _, _):-
   length(Tokens, Position).
term_prime_aux(constant(_), Tokens,
		[error('unexpected constant', [])/Position], _, _, _):-
   length(Tokens, Position).
term_prime_aux(simple_object(_), Tokens,
		[error('unexpected variable/string/integer/float', [])
		      /Position], _, _, _):-
   length(Tokens, Position).
term_prime_aux(list, Tokens,
		[error('unexpected list', [])/Position], _, _, _):-
   length(Tokens, Position).
term_prime_aux(set, Tokens,
		[error('unexpected set', [])/Position], _, _, _):-
   length(Tokens, Position).
term_prime_aux(bracket, Tokens,
		[error('unexpected "("', [])/Position], _, _, _):-
   length(Tokens, Position).
% predicate as a terminator for terms
term_prime_aux(predicate(Token), Tokens, [ []/[Token|Tokens] ], _, _, _).


/*------------------------------------------------------------------------------
 * Operator relations
 * bigger_than, less_than and ambiguous fails if one of the indicator
 * is not defined.
 */


% null(Specifier, Precedence)
null(null, _).


bigger_than(_, PrecLeft, _, PrecRight):-
	( var(PrecLeft);
	  var(PrecRight)
	), !, fail.
bigger_than(_, PrecLeft, _, PrecRight):- PrecLeft > PrecRight, !.
bigger_than('Syntax.YFX.F1', Prec, 'Syntax.YFX.F1', Prec):- !.
bigger_than('Syntax.YFX.F1', Prec, 'Syntax.YF.F1', Prec):- !.
bigger_than('Syntax.XFX.F1', Prec, 'Syntax.YFX.F1', Prec):- !.
bigger_than('Syntax.XFX.F1', Prec, 'Syntax.YF.F1', Prec):- !.
bigger_than('Syntax.FX.F1', Prec, 'Syntax.YFX.F1', Prec):- !.
bigger_than('Syntax.FX.F1', Prec, 'Syntax.YF.F1', Prec):- !.
bigger_than('Syntax.XF.F1', Prec, 'Syntax.YFX.F1', Prec):- !.
bigger_than('Syntax.XF.F1', Prec, 'Syntax.YF.F1', Prec):- !.
bigger_than('Syntax.YF.F1', Prec, 'Syntax.YFX.F1', Prec):- !.
bigger_than('Syntax.YF.F1', Prec, 'Syntax.YF.F1', Prec).


less_than(_, PrecLeft, _, PrecRight):-
	( var(PrecLeft);
	  var(PrecRight)
	), !, fail.
less_than(_, PrecLeft, _, PrecRight):- 		PrecLeft < PrecRight, !.
less_than('Syntax.XFY.F1', Prec, 'Syntax.XFX.F1', Prec):- !.
less_than('Syntax.XFY.F1', Prec, 'Syntax.XFY.F1', Prec):- !.
less_than('Syntax.XFY.F1', Prec, 'Syntax.XF.F1', Prec):- !.
less_than('Syntax.FY.F1', Prec, 'Syntax.XFX.F1', Prec):- !.
less_than('Syntax.FY.F1', Prec, 'Syntax.XFY.F1', Prec):- !.
less_than('Syntax.FY.F1', Prec, 'Syntax.XF.F1', Prec):- !.
less_than('Syntax.XFY.F1', Prec, 'Syntax.FX.F1', Prec):- !.
less_than('Syntax.XFY.F1', Prec, 'Syntax.FY.F1', Prec):- !.
less_than('Syntax.FY.F1', Prec, 'Syntax.FX.F1', Prec):- !.
less_than('Syntax.FY.F1', Prec, 'Syntax.FY.F1', Prec).


ambiguous('Syntax.XFY.F1', Prec, 'Syntax.YFX.F1', Prec):- !.
ambiguous('Syntax.XFY.F1', Prec, 'Syntax.YF.F1', Prec):- !.
ambiguous('Syntax.FY.F1', Prec, 'Syntax.YFX.F1', Prec):- !.
ambiguous('Syntax.FY.F1', Prec, 'Syntax.YF.F1', Prec).


/*------------------------------------------------------------------------------
 * Stack operations
 */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * push(+Element, +Stack, -NewStack)
 */

push(Element, Stack, [Element|Stack]).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * pop(-Element, +Stack, -NewStack) 
 * it fails if the stack is empty.
 */

pop(Element, [Element|Stack], Stack).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

stack_to_return([], _, _, NewReturn, NewReturn).
stack_to_return([Stack|Stacks], Tokens, Remains, NewReturn, NewReturn2) :-
	( Stack = error(A, B)
	  -> length(Tokens, Position),
	     NewReturn = [error(A, B)/Position|NewReturn3]
	  ;  NewReturn = [Stack/Remains|NewReturn3]
	),
	stack_to_return(Stacks, Tokens, Remains, NewReturn3, NewReturn2).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * contract(+Stack, +SpecLeft, +PrecLeft, +Element, -NewStack)
 * this predicate contracts the stack into normal form, as defined in the
 * algorithm
 * It assumes that the IndOp (in Element) can not be 'null'
 */

contract([], _, _, Element, [[Element]]).
contract([stack(SpecRight, PrecRight, HoleRight, TreeRight)|Stack2],
		SpecLeft, PrecLeft, 
		stack(SpecOp, PrecOp, HoleOp, TreeOp), NewStacks):-
   ( ( null(SpecLeft, PrecLeft);
       less_than(SpecLeft, PrecLeft, SpecOp, PrecOp)
     ),
     ( bigger_than(SpecOp, PrecOp, SpecRight, PrecRight);
       ambiguous(SpecOp, PrecOp, SpecRight, PrecRight)
     )
     -> ( ( null(SpecLeft, PrecLeft);
	    less_than(SpecLeft, PrecLeft, SpecRight, PrecRight)
	  )
	  -> HoleRight = TreeOp,
			% not sure if I need to test the HoleRight or not
	     contract(Stack2, SpecLeft, PrecLeft, 
		   stack(SpecRight, PrecRight, HoleOp, TreeRight), NewStacks)
	  ;  ( bigger_than(SpecLeft, PrecLeft, SpecRight, PrecRight)
	       -> NewStacks = [[stack(SpecOp, PrecOp, HoleOp, TreeOp),
	  		       	stack(SpecRight, PrecRight, HoleRight,TreeRight)
			       | Stack2]
			      ]
	       ;  ( ambiguous(SpecLeft, PrecLeft, SpecRight, PrecRight)
		    -> % the ambiguous case, create two parse trees
		       copy_term(quintet(HoleRight, TreeRight, HoleOp,
					TreeOp, Stack2),
				 quintet(NewHoleRight, NewTreeRight,
					NewHoleOp, NewTreeOp, NewStack2)),
			    % create a parse tree where PrecLeft < PrecRight
		       HoleRight = TreeOp,
			    % not sure if I need to test the HoleRight or not
			    % I thin  there is no need, because terms are
			    % formed only when there is enough information
			    % about the precedence.
		       NewStacks = [ [stack(SpecOp, PrecOp,NewHoleOp,NewTreeOp),
				      stack(SpecRight, PrecRight, NewHoleRight,
						NewTreeRight)
				      | NewStack2
				     ]
				    | NewStacks2],
		       contract(Stack2, SpecLeft, PrecLeft, 
				stack(SpecRight, PrecRight, HoleOp, TreeRight),
				NewStacks2)
		    ; NewStacks = [ error('misuse of operators/connectives (wrong precedence or associativity)', []) ]
		      % old fragment
		      % arg(1, TreeRight, FuncRight),
		      % NewStacks = [ error('misuse of operator "~w" (wrong precedence or associativity)', [FuncRight]) ]
		      % this is a strange error message
		  )
	     )
        )
     ;  NewStacks = [ [stack(SpecOp, PrecOp, HoleOp, TreeOp),
		       stack(SpecRight, PrecRight, HoleRight, TreeRight)
		       | Stack2]
		    ]
   ).

