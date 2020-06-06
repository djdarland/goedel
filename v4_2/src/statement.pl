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


/* File:	statement.pl
Subject:	parse the statements, including CFormula and NonCFormula
Author: 	Jiwei Wang
Date:		3 December 1991

		Changed to new grammar for IF-THEN-ELSE, at 5 June 1992.

================================================================================
*/

%% '$$module'('@(#)statement.pl 1.39 last updated 93/11/24 17:32:03 by jiwei').


:- dynamic underscore_counter/1.

/*------------------------------------------------------------------------------
 * statement(+Item, +ModuleName, -Formulae, -ErrorReturn, +Language)
 *	Formulae = [] is the error signal,
 *	ErrorReturn is not always instantiated
 *	ModuleName is of GString type
 */

statement(item(Tokens, _, _), ModuleName, Formulae, ErrorReturn, Language):-
   underscore_init,	% initialising underscore label counter.
   headatom(Tokens, Remains2, ModuleName, HeadAtoms, ErrorReturn2, Language),
   ( HeadAtoms = []
     -> ErrorReturn = ErrorReturn2,
	Formulae = []
     ;  ( Remains2 = []
	  -> create_formula('MetaDefs.<-''.F2',
				[HeadAtoms, ['MetaDefs.Empty.C0']], Formulae)
	  ;  ( Remains2 = [graphic_name('<-')|Remains3]
	       -> body(Remains3, CFormulae, ErrorReturn3, Language),
		  ( CFormulae = []
		    -> ErrorReturn = ErrorReturn3,
		       Formulae = []
		    ;  ErrorReturn = [],
		       create_formula('MetaDefs.<-''.F2', 
					[HeadAtoms, CFormulae], Formulae)
		  )
	       ;  length(Remains2, Position),
		  ErrorReturn = [error('"<-" expected in statement',[])/Position],
		  Formulae = []
	     )
	)
   ).

%------------------------------------------------------------------------------

underscore_counter(1).

underscore_init :-
   retractall(underscore_counter(_)),
   assert(underscore_counter(1)).

underscore_label(Label) :-
   retract(underscore_counter(Label)),
   Label2 is Label + 1,
   assert(underscore_counter(Label2)).

/*------------------------------------------------------------------------------
 * headatom(+Tokens, -Remains, +ModuleName, -Formulae, -ErrorReturn, +Language)
 * 	Formulae = [] indicates error
 *	ModuleName is of GString type
 */

headatom(Tokens, Remains, ModuleName, Formulae, ErrorReturn, Language):-
   atom(Tokens, Remains, Formulae2, ErrorReturn2, Language),
   select_headatom(Formulae2, Formulae, ModuleName),
   ( Formulae2 = [],
     Formulae = []
     -> ( ErrorReturn2 = []
	  -> length(Tokens, Position),
	     ErrorReturn = [error('illegal headatom', [])/Position]
	  ;  ErrorReturn = ErrorReturn2
	)
     ;  ( Formulae2 \== [],
	  Formulae = []
	  -> length(Remains, Position),
	     ErrorReturn = [error('illegal headatom (possibly attempting to define a predicate not declared in this module)', [])/Position]
	  ;  ErrorReturn = ErrorReturn2
	)
   ).

% still select_headatom does not filter out (Atom).  Hope this is tolerable.
select_headatom([], [], _).
select_headatom([Atom|Formulae], NewFormulae, ModuleName) :-
   ( ( Atom = 'MetaDefs.PAtom.F1'('MetaDefs.Name.F4'(ModuleName, _, _, _));
       Atom = 'MetaDefs.Atom.F2'('MetaDefs.Name.F4'(ModuleName, _,_,_), _)
     )
     -> NewFormulae = [Atom|NewFormulae2]
     ;  ( Atom = 'MetaDefs.Some.F2'([Variable|_], 'MetaDefs.Atom.F2'(
				'MetaDefs.Name.F4'(ModuleName,S,C,A), Terms)),
	  Variable = 'MetaDefs.Var.F2'(GVar, _),
	  name(GVar, [0'", 0'_|_])
	  -> NewFormulae = ['MetaDefs.Atom.F2'(
				'MetaDefs.Name.F4'(ModuleName, S, C, A), Terms)
			    | NewFormulae2]
			% remove quantifiers for underscores
          ;  NewFormulae = NewFormulae2
	)
   ),
   select_headatom(Formulae, NewFormulae2, ModuleName).
	
/*------------------------------------------------------------------------------
 The grammar for Body has to be treated differently from that in the Goedel
 Report.  In the parser, this part of grammar is treated as follows:

	CFormula(0) ->   <'{'> CFormula(f) <'}'[Label]>
		       | Formula(f)

	CFormula(2) -> CFormula(f) & CFormula(f1)
			f =< 1 and f =< 2
 */

/*------------------------------------------------------------------------------
 * body(+Tokens, -Formulae, -ErrorReturn, +Language)
 *	Formulae can be [], indicating a unit clause.
 *	ErrorReturn \= [] indicates errors.
 *	CFormulae are those in the scope of a bar_commit.
 * bar-commit takes 0 as the commit label which is not allowed in other
 * user defined commits.
 * one-solution (1000000000) is not completed yet.
 */

body([], [], [error('empty statement body', [])/0], _).

body([Token|Tokens], Formulae, ErrorReturn, Language) :-
   ( Token = graphic_name('|')
     -> cformula(Tokens, Remains2, Formulae2, ErrorReturn2, Language),
        ( Formulae2 = [],
	  ErrorReturn2 = []
	  -> ( Remains2 == []
	       -> ErrorReturn = [],
		  Formulae = ['MetaDefs.Commit.F2'(0, 'MetaDefs.PAtom.F1'(
					'MetaDefs.Name.F4'('"', '"True',
					'MetaDefs.Proposition.C0', 0)))]
	       ;  Formulae = [],
		  length(Remains2, Position),
		  Remains2 = [Symb|_],
		  ErrorReturn = [error('illegal symbol: "~w"', [Symb])/Position]
	     )
	  ;  ( Formulae2 = []
	       -> Formulae = [],
	          ErrorReturn = ErrorReturn2
	       ;  ( Remains2 == []
		    -> ErrorReturn = [],
		       create_formula('MetaDefs.&''.F2', 
				      [ ['MetaDefs.Commit.F2'(0,
					  'MetaDefs.PAtom.F1'(
					    'MetaDefs.Name.F4'('"', '"True',
					      'MetaDefs.Proposition.C0', 0)))],
				      Formulae2 ], Formulae)
	            ;  Formulae = [],
		       length(Remains2, Position),
		       Remains2 = [Symb|_],
		       ErrorReturn = [error('illegal symbol: "~w"', [Symb])/Position]
		  )
	     )
	)
     ;  cformula([Token|Tokens], Remains3, Formulae3, ErrorReturn3, Language),
	( Formulae3 = []
	  -> Formulae = [],
	     ( ErrorReturn3 = []
	       -> length([Token|Tokens], Position),
		  ErrorReturn = [error('formula expected', [])/Position]
	       ;  ErrorReturn = ErrorReturn3
	     )
	 ;  ( Remains3 = []
	      -> Formulae = Formulae3,
		 ErrorReturn = []
	      ;	 ( Remains3 = [graphic_name('|')|Remains4]
	           -> cformula(Remains4, Remains5, Formulae4, ErrorReturn4,
					Language),
		      ( Formulae4 = [],
		        ErrorReturn4 = []
		        -> ( Remains5 == []	% Remains5 can be var, don't use =
			     -> ErrorReturn = [],
			        create_formula('MetaDefs.Commit.F2',
						[[0], Formulae3], Formulae)
		             ;  Formulae = [],
			        length(Remains4, Position),
			        ErrorReturn = [error('formula expected',[])/Position]
		           )
		        ;  ( Formulae4 = []
			     -> Formulae = [],
			        ErrorReturn = ErrorReturn4
			     ;  ( Remains5 = []
			          -> create_formula('MetaDefs.Commit.F2',
						[[0], Formulae3], Formulae5),
			    	     create_formula('MetaDefs.&''.F2', 
					      [Formulae5, Formulae4], Formulae)
			          ;  Formulae = [],
				     length(Remains5, Position),
				     Remains5 = [Symb|_],
			             ErrorReturn = [error('illegal symbol: "~w"',
							[Symb])/Position]
			        ) 
		           )
		      )
	           ;  Formulae = [],
		      length(Remains3, Position),
		      Remains3 = [Symb|_],
		      ErrorReturn = [error('illegal symbol: "~w"', [Symb])/Position]
		 )
	    )
        )
   ).


/*------------------------------------------------------------------------------
 * cformula(+Tokens, -Remains, -Formulae, -ErrorReturn, +Language)
 * 	Formulae can be []
 * 	Remains should always be instantiated
 *	ErrorReturn \= []  as the error signal.
 */
	     
% This should be the only place with empty CFormula
cformula([], [], [], [], _).

cformula([Token|Tokens], Remains, Formulae, ErrorReturn, Language) :-
   cformula_aux(Token, Tokens, Remains2, Formulae2, ErrorReturn2, Language),
   ( Formulae2 = []
     -> ErrorReturn = ErrorReturn2,
	Formulae = []
     ;  ( end_of_cformula(Remains2)
	  -> Formulae = Formulae2,
	     Remains = Remains2,
	     ErrorReturn = []
	  ;  ( Remains2 = [graphic_name('&')|Remains3]
	       -> cformula(Remains3, Remains, Formulae4, ErrorReturn4,
					Language),
		  ( Formulae4 = []
		    -> ErrorReturn = ErrorReturn4,
		       Formulae = []
		    ;  ErrorReturn = [],
		       create_formula('MetaDefs.&''.F2', 
					    [Formulae2, Formulae4], Formulae)
		  )
	       ;  Formulae = [],
		  length(Remains2, Position),
		  Remains2 = [Symb|_],
		  ErrorReturn = [error('illegal symbol: "~w"', [Symb])/Position]
	     )
	)
   ).


end_of_cformula([]).
end_of_cformula([')'|_]) :- !.
end_of_cformula(['}'(_)|_]) :- !.
end_of_cformula([graphic_name('|')|_]).

/*------------------------------------------------------------------------------
 * cformula_aux(+Token, +Tokens, -Remains, -Formulae, -ErrorReturn, +Language)
 *	Formulae = [] is the error signal.
 * The algorithm for CFormula is first to parse a list of tokens as special
 * formula i.e. conditionals, commits.  If this fails, we parse it as Formula
 */

cformula_aux('{', Tokens, Remains, Formulae, ErrorReturn, Language) :-
   !,
   cformula(Tokens, Remains2, Formulae2, ErrorReturn2, Language),
   ( Formulae2 =[]
     -> term_aux(set, Tokens, Return, null, _, Language),
	( select_return(Return, null, SetTerms, _, ErrorReturn3)
	  -> true
	  ;  write('Internal error No 25.'), nl, abort 
        ),
	( SetTerms = []
	  -> Formulae = [],
	     ( ErrorReturn2 = [], ErrorReturn3 = []
	       -> length(Tokens, Position),
	          ErrorReturn = [error('formula expected', [])/Position]
	       ;  append(ErrorReturn2, ErrorReturn3, ErrorReturn)
	     )
	  ;  formula(['{'|Tokens], Return2, Language),
	     ( select_return(Return2, null, Formulae, Remains, ErrorReturn)
	       -> true
	       ;  write('Internal error No 26.'), nl, abort 
	     )
	)
     ;  ( Remains2 = ['}'(Label)|Remains]
	  -> ErrorReturn = [],
	     create_formula('MetaDefs.Commit.F2', [[Label], Formulae2],
					Formulae)
	  ;  Formulae = [],
	     length(Remains2, Position),
	     ErrorReturn = [error('missing "}"', [])/Position]
	)
   ).

cformula_aux(Token, Tokens, Remains, Formulae, ErrorReturn, Language) :-
   formula([Token|Tokens], Return, Language),
   ( select_return(Return, null, Formulae, Remains, ErrorReturn)
     -> true
     ;  write('Internal error No 27.'), nl, abort 
   ).


/*------------------------------------------------------------------------------
 * lookup_predicate_dict(+Token, -SymbolList, +Language)
 * This fails if Token is not declared in Language
 */

lookup_predicate_dict(Token, SymbolList, Language):-
   ( Token = big_name(Name);
     Token = graphic_name(Name)
   ),
   string2Gstring(Name, GName),
   'ParserPrograms.ParserSymbols.P3'(GName, Language, Candidates), !,
			% the cut here is essential
   candidates2predicates(Candidates, SymbolList2, IsTerm),
   collect_predicate_no_ind(SymbolList2, SymbolList3),
   collect_proposition(SymbolList3, SymbolList4),
   ( var(IsTerm)
     -> SymbolList = SymbolList4,
	SymbolList \== []
     ;  SymbolList = [term(Token)|SymbolList4]
				% Name may not be big_name but doesn't matter
   ).


candidates2predicates([], [], _).
candidates2predicates([Candidate|Candidates], SymbolList, IsTerm) :-
   ( Candidate = 'ParserPrograms.FCandidate.F2'(_, _)
     -> IsTerm = yes,
        SymbolList = SymbolList2
     ;  Candidate = 'ParserPrograms.PCandidate.F2'(Name, Ind),
        candidates2predicates_aux(Ind, Name, Symbol),
        SymbolList = [Symbol|SymbolList2]
   ),
   candidates2predicates(Candidates, SymbolList2, IsTerm).


candidates2predicates_aux('Syntax.NoPredInd.C0',
				'MetaDefs.Name.F4'(M, S, C, A), Symbol) :-
   ( A = 0
     -> Symbol = proposition('MetaDefs.PAtom.F1'('MetaDefs.Name.F4'(M, S, C, A)))
     ;  Symbol = predicate_no_ind('MetaDefs.Name.F4'(M, S, C, A))
		% note this is not confirmed with the interface
   ).
candidates2predicates_aux('Syntax.ZPZ.C0', Name, infix_predicate(Name)).
candidates2predicates_aux('Syntax.ZP.C0', Name, postfix_predicate(Name)).
candidates2predicates_aux('Syntax.PZ.C0', Name, prefix_predicate(Name)).

%------------------------------------------------------------------------------

collect_predicate_no_ind(SymbolList, SymbolList2):-
   collect_predicate_no_ind(SymbolList, SymbolList3, Names),
   ( Names = []
     -> SymbolList2 = SymbolList3
     ;  SymbolList2 = [predicate_no_ind(Names)|SymbolList3]
   ).

collect_predicate_no_ind([], [], []).
collect_predicate_no_ind([Symbol|SymbolList], SymbolList2, Names) :-
   ( Symbol = predicate_no_ind(Name)
     -> SymbolList2 = SymbolList3,
	Names = [Name|Names2]
     ;  SymbolList2 = [Symbol|SymbolList3],
        Names = Names2
   ),
   collect_predicate_no_ind(SymbolList, SymbolList3, Names2).

%------------------------------------------------------------------------------

collect_proposition(SymbolList, SymbolList2):-
   collect_proposition(SymbolList, SymbolList3, Names),
   ( Names = []
     -> SymbolList2 = SymbolList3
     ;  SymbolList2 = [proposition(Names)|SymbolList3]
   ).

collect_proposition([], [], []).
collect_proposition([Symbol|SymbolList], SymbolList2, Names) :-
   ( Symbol = proposition(Name)
     -> SymbolList2 = SymbolList3,
	Names = [Name|Names2]
     ;  SymbolList2 = [Symbol|SymbolList3],
        Names = Names2
   ),
   collect_proposition(SymbolList, SymbolList3, Names2).

/*------------------------------------------------------------------------------
 * lookup_function_dict(+Token, -SymbolList, +Language)
 * This fails if Token is not declared in Language
 */

lookup_function_dict(Token, SymbolList, Language):-
   ( Token = big_name(Name);
     Token = graphic_name(Name)
   ),
   string2Gstring(Name, GName),
   'ParserPrograms.ParserSymbols.P3'(GName, Language, Candidates), !,
   candidates2functions(Candidates, SymbolList2, IsPredicate),
   collect_function_no_ind(SymbolList2, SymbolList3),
   collect_constant(SymbolList3, SymbolList4),
   ( var(IsPredicate)
     -> SymbolList = SymbolList4,
	SymbolList \== []
     ;  SymbolList = [predicate(Token)|SymbolList4]
				% Name may not be big_name but doesn't matter
   ).

/* comment out as = and ~= are provided in SymbolTable
	( ( Name = '='; Name = '~=')
	  -> SymbolList = [predicate(graphic_name(Name))|SymbolList5]
	  ;  SymbolList = SymbolList5,
	     SymbolList \== []
	).
*/


candidates2functions([], [], _).
candidates2functions([Candidate|Candidates], SymbolList, IsPredicate) :-
   ( Candidate = 'ParserPrograms.PCandidate.F2'(_, _)
     -> IsPredicate = yes,
	SymbolList = SymbolList2
     ;  Candidate = 'ParserPrograms.FCandidate.F2'(Name, Ind),
	candidates2functions_aux(Ind, Name, Symbol),
        SymbolList = [Symbol|SymbolList2]
   ),
   candidates2functions(Candidates, SymbolList2, IsPredicate).
	

candidates2functions_aux('Syntax.NoFunctInd.C0',
				'MetaDefs.Name.F4'(M, S, C, A), Symbol) :-
   ( A = 0
     -> Symbol = constant('MetaDefs.CTerm.F1'('MetaDefs.Name.F4'(M, S, C, A)))
     ;  Symbol = function_no_ind('MetaDefs.Name.F4'(M, S, C, A))
		% the interface format is done in collect_function_no_ind
   ).
candidates2functions_aux('Syntax.XFX.F1'(Prec), Name,
			infix_function(Name, 'Syntax.XFX.F1'(Prec))).
candidates2functions_aux('Syntax.XFY.F1'(Prec), Name,
			infix_function(Name, 'Syntax.XFY.F1'(Prec))).
candidates2functions_aux('Syntax.YFX.F1'(Prec), Name,
			infix_function(Name, 'Syntax.YFX.F1'(Prec))).
candidates2functions_aux('Syntax.XF.F1'(Prec), Name,
			postfix_function(Name, 'Syntax.XF.F1'(Prec))).
candidates2functions_aux('Syntax.YF.F1'(Prec), Name,
			postfix_function(Name, 'Syntax.YF.F1'(Prec))).
candidates2functions_aux('Syntax.FX.F1'(Prec), Name,
			prefix_function(Name, 'Syntax.FX.F1'(Prec))).
candidates2functions_aux('Syntax.FY.F1'(Prec), Name,
			prefix_function(Name, 'Syntax.FY.F1'(Prec))).

%------------------------------------------------------------------------------

collect_function_no_ind(SymbolList, SymbolList2):-
   collect_function_no_ind(SymbolList, SymbolList3, Names),
   ( Names = []
     -> SymbolList2 = SymbolList3
     ;  SymbolList2 = [function_no_ind(Names)|SymbolList3]
   ).

collect_function_no_ind([], [], []).
collect_function_no_ind([Symbol|SymbolList], SymbolList2, Names) :-
   ( Symbol = function_no_ind(Name)
     -> SymbolList2 = SymbolList3,
	Names = [Name|Names2]
     ;  SymbolList2 = [Symbol|SymbolList3],
        Names = Names2
   ),
   collect_function_no_ind(SymbolList, SymbolList3, Names2).

%------------------------------------------------------------------------------

collect_constant(SymbolList, SymbolList2):-
	collect_constant(SymbolList, SymbolList3, Names),
	( Names = []
	  -> SymbolList2 = SymbolList3
	  ;  SymbolList2 = [constant(Names)|SymbolList3]
	).

collect_constant([], [], []).
collect_constant([Symbol|SymbolList], SymbolList2, Names) :-
   ( Symbol = constant(Name)
     -> SymbolList2 = SymbolList3,
	Names = [Name|Names2]
     ;  SymbolList2 = [Symbol|SymbolList3],
        Names = Names2
   ),
   collect_constant(SymbolList, SymbolList3, Names2).

/*------------------------------------------------------------------------------
 * lookup_base(+BaseName, +ModuleName, +SymbolTable)
 * 	this fails if such base of such module is not declared.
 */

lookup_base(BaseName, ModuleName, SymbolTable) :-
   'ParserPrograms.ParserTypeSymbols.P3'(BaseName, SymbolTable, Names), !,
   member('MetaDefs.Name.F4'(ModuleName, BaseName, 'MetaDefs.Base.C0', 0),
				   Names), !.

/*------------------------------------------------------------------------------
 * lookup_constructor(+ConstrName, +Arity, +ModuleName, +SymbolTable)
 * 	this fails if such constructor of such module is not declared.
 */

lookup_constructor(ConstrName, Arity, ModuleName, SymbolTable) :-
   'ParserPrograms.ParserTypeSymbols.P3'(ConstrName, SymbolTable, Names), !,
   member('MetaDefs.Name.F4'(ModuleName, ConstrName,
			'MetaDefs.Constructor.C0', Arity), Names), !.

