
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
File:		parser.pl
Subject:	the top level of the Goedel parser
Author: 	Jiwei Wang
Date:		11 September 1991

The whole parser consists of files:
	tokenizer.pl:	the tokenizer.
	parser.pl:	the top-level of the parser.
	statement.pl:	parsing statements.
	formula.pl:	parsing formula.
	term.pl:	parsing terms.
	Programs.pl:	the system module Program and the type-checker.
	
================================================================================
*/

'$$module'('@(#)parser.pl 1.66 last updated 94/01/24 13:00:34 by bowers
').

% for gfreeze
:- op(500, yfx, and).
:- op(400, yfx, or).

/*------------------------------------------------------------------------------
 * parse_program(+ModuleName, +Switch, -Program)
 * This is the top level entry point of the parser.  It takes a main-module
 * name and returns the program in ground representation.
 * Switch to indicate whether doing compile or make
 */

parse_program(ModuleName, Switch, Program):-
   module_as_items(ModuleName, ExpTokens, LocTokens),
   string2Gstring(ModuleName, GModuleName),
   identify_module_kind(ExpTokens, ModuleKind),
   identify_module_part(ExpTokens, ModulePartExp, ModulePartLoc),
   'ParserPrograms.QuickNewProgram.P3'(GModuleName, ModuleKind, EmptyProg),
   parse_module_decl(GModuleName, Switch, ExpTokens, ExpTokens2, LocTokens,
		LocTokens2, [ModuleName], _LoadedModule, EmptyProg, Prog2),
   'ParserPrograms.ExportSymbolTable.P3'(Prog2, GModuleName, ExpLang), 
   parse_language1(ExpTokens2, ExpTokens3, GModuleName, 'ProgDefs.Exported.C0',
			ExpLang, ExpLang2),
   parse_language2(ExpTokens3, ExpTokens4, GModuleName, 'ProgDefs.Exported.C0',
			ExpLang2, ExpLang3),
   'ParserPrograms.UpdateProgramLanguage.P4'(Prog2, GModuleName, ExpLang3, Prog3), 
   parse_delays_aux(ExpTokens4, ExpTokens5, GModuleName, ModulePartExp,
	                ExpLang3, Prog3, Prog4, export),
   'ParserPrograms.LocalSymbolTable.P3'(Prog4, GModuleName, LocLang), 
   parse_language1(LocTokens2, LocTokens3, GModuleName, 'ProgDefs.Hidden.C0',
			LocLang, LocLang2),
   parse_language2(LocTokens3, LocTokens4, GModuleName, 'ProgDefs.Hidden.C0',
			LocLang2, LocLang3),
   'ParserPrograms.UpdateProgramLanguage.P4'(Prog4, GModuleName, LocLang3, Prog5), 
   parse_delays_aux(LocTokens4, LocTokens5, GModuleName, ModulePartLoc,
	                LocLang3, Prog5, Prog6, local),
   parse_statements(GModuleName, ExpTokens5, LocTokens5, LocLang3,
			Prog6, Program).


/*------------------------------------------------------------------------------
 */

parse_module(ModuleName, Switch, Loaded, NewLoaded, OldProg, NewProg):-
   string2Gstring(ModuleName, GModuleName),
   ( ( member(ModuleName, Loaded); system_module_name(GModuleName) )
     -> NewProg = OldProg,
	NewLoaded = Loaded	% skip if the module is already parsed 
     ;  ( Switch = compile, lang_file_ok(ModuleName)
	  -> sappend(ModuleName, '.lng', LangFile),
	     open(LangFile, read, Stream),
	     my_format(user_error, 'Loading the language of module "~a" ...~n', [ModuleName]),
   	     read(Stream, version(_, ordinary)),
	     read(Stream, ModuleDef),
	     read(Stream, ModuleDescriptor),
	     close(Stream),
	     'ParserPrograms.QuickInsertModuleDef.P4'(OldProg, GModuleName,
				ModuleDef, Prog2),
	     'SharedPrograms.ProgramLanguage.P2'(Prog2, Lang),
	     'ParserPrograms.QuickInsertModuleDescriptor.P4'(Lang, GModuleName,
				ModuleDescriptor, Language),
	     'ParserPrograms.ReplaceProgramLanguage.P3'(Prog2, Language, Prog3),
	     ModuleDef = 'ProgDefs.ModDef.F4'(_, ImpExp, ImpLoc, Lifts),
	     append(ImpLoc, ImpExp, ImportedMods1),             
	     append(Lifts, ImportedMods1, ImportedMods),
	     gstringlist2stringlist(ImportedMods, ImportedMods2),
	     make_modules(ImportedMods, Prog3, Prog4),
	     parse_module_aux(ImportedMods2, Switch, [ModuleName|Loaded],
				NewLoaded, Prog4, NewProg)
	  ;  module_as_items(ModuleName, ExpTokens, LocTokens),
   	     identify_module_part(ExpTokens, ModulePartExp, ModulePartLoc),
             parse_module_decl(GModuleName, Switch, ExpTokens, ExpTokens2,
			LocTokens, LocTokens2, [ModuleName|Loaded], NewLoaded,
			OldProg, Prog2),
             'ParserPrograms.ExportSymbolTable.P3'(Prog2, GModuleName, ExpLang), 
	     parse_language1(ExpTokens2, ExpTokens3, GModuleName, 
	                'ProgDefs.Exported.C0',	ExpLang, ExpLang2),
	     parse_language2(ExpTokens3, ExpTokens4, GModuleName, 
	                'ProgDefs.Exported.C0',	ExpLang2, ExpLang3),
	     'ParserPrograms.UpdateProgramLanguage.P4'(Prog2, GModuleName,
			ExpLang3, Prog3), 
	     parse_delays_aux(ExpTokens4, ExpTokens5, GModuleName,
			ModulePartExp, ExpLang3, Prog3, Prog4, export),
	     'ParserPrograms.LocalSymbolTable.P3'(Prog4, GModuleName, LocLang), 
	     parse_language1(LocTokens2, LocTokens3, GModuleName, 
	                'ProgDefs.Hidden.C0', LocLang, LocLang2),
	     parse_language2(LocTokens3, LocTokens4, GModuleName, 
	               'ProgDefs.Hidden.C0', LocLang2, LocLang3),
	     'ParserPrograms.UpdateProgramLanguage.P4'(Prog4, GModuleName,
			LocLang3, Prog5), 
	     parse_delays_aux(LocTokens4, LocTokens5, GModuleName, 
                        ModulePartLoc, LocLang3, Prog5, Prog6, local),
	     parse_statements(GModuleName, ExpTokens5, LocTokens5,
			LocLang3, Prog6, NewProg)
	)
   ).


lang_file_ok(ModuleName) :-
   sappend(ModuleName, '.lng', LangFile),
   open(LangFile, read, Stream),
   file_version(V),
   ( read(Stream, version(V, ordinary))
     -> close(Stream)
     ;  close(Stream), fail
   ).


%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

make_modules([], Prog, Prog).
make_modules([GMod|Mods], Prog, NewProg) :-
   'ParserPrograms.QuickMakeModule.P3'(GMod, Prog, Prog2),
   make_modules(Mods, Prog2, NewProg).

/*------------------------------------------------------------------------------
 * parse_module_aux is the entry point for parsing IMPORT declarations.
 * This is also the place to do "make" and "compile" commands.  
 * Left for future versions.
 */

parse_module_aux([], _, Loaded, Loaded, Program, Program).
parse_module_aux([Mod|Mods], Switch, Loaded, NewLoaded, OldProg, NewProg):-
   parse_module(Mod, Switch, Loaded, Loaded2, OldProg, Prog2),
   parse_module_aux(Mods, Switch, Loaded2, NewLoaded, Prog2, NewProg).


/*==============================================================================
 * module_as_items(+ModuleName, -list(item), -list(item))
 * module_as_items(+ModuleName, -Exp_list, -Loc_list)
 */

module_as_items(ModuleName, ExpTokens, LocTokens):-
   module_part(ModuleName, '.exp', ExpTokens),
   module_part(ModuleName, '.loc', LocTokens).


/*------------------------------------------------------------------------------
 * module_part(+ModuleName, +String, -List(item))
 */

module_part(ModuleName, String, Module):-
   sappend(ModuleName, String, FileName),
   ( open(FileName, read, Stream)
     -> my_format(user_error, 'Reading file "~w" ...~n', [FileName]),
	get_one_module(Stream, Module),	% in tokenizer.pl
	close(Stream)
     ;  Module = []	% nonexisting file is NOT regarded as an error.
   ).

	
/*==============================================================================
 * parse_module_decl(+ModuleName, +Switch, +ExpTokens, +LocTokens, +OldProg,
 *			-NewProg)
 * takes care of the constraints for module and the imported modules
 * the module-name declarations are not removed from the tokens.
 */

parse_module_decl(GModuleName, Switch, ExpTokens, ExpTokens2, LocTokens,
		LocTokens2, Loaded, NewLoaded, OldProg, NewProg):-
   gstring2string(GModuleName, ModuleName),
   parse_import(GModuleName, Switch, ExpTokens, ExpTokens3, LocTokens,
		LocTokens3, Loaded, NewLoaded, OldProg, NewProg),
   ( system_module_name(GModuleName)
     -> my_format(user_error, 'Parsing system module "~w" ...~n', [ModuleName])
     ;  my_format(user_error, 'Parsing module "~w" ...~n', [ModuleName])
   ),
   check_module(GModuleName, ExpTokens3, ExpTokens2, LocTokens3, LocTokens2).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

check_module(GModuleName, ExpTokens, ExpTokens2, LocTokens, LocTokens2):-
   gstring2string(GModuleName, ModuleName),
   ( LocTokens == [], ExpTokens == []
     -> ExpTokens2 = [],
	LocTokens2 = [],
	print_out_error('Error: module "~w" missing', [ModuleName], null, 0, 0,
			[], null)
     ;  ( ExpTokens == []
          -> ExpTokens2 =[],
	     ( LocTokens = [item([big_name('MODULE'), big_name(LocName)], _, _)
			    | LocTokens2]
	       -> true
	       ;  LocTokens = [_|LocTokens2],
		  print_out_error('Error: illegal module declaration in the export part of module "~w" or export part of module "~w" missing',
				[ModuleName, ModuleName], null, 0, 0, [], null)
	     )
 	  ;  ( LocTokens == []
               -> LocTokens2 = [],
          	  ( ExpTokens = [item([big_name('CLOSED'), big_name(ExpName)],
				      _, _) | ExpTokens2]
                    -> true
          	    ;  ( ExpTokens = [item([big_name('EXPORT'),big_name(ExpName)
					   ], _, _) | ExpTokens2]
			 -> format(user_error, '~nWarning: local part of module "~w" missing.~n', [ModuleName])
                         ;  ExpTokens = [_|ExpTokens2],
		            print_out_error('Error: illegal module declaration in the export part of module "~w"',
					[ModuleName], null, 0, 0, [], null)
		       )
		  )
	       ;  ( ( ExpTokens = [item([big_name('EXPORT'), big_name(ExpName)],
					_, _) | ExpTokens2];
	              ExpTokens = [item([big_name('CLOSED'), big_name(ExpName)],
					 _, _) | ExpTokens2]
		    )
                    -> true
                    ;  ExpTokens = [_|ExpTokens2],
		       print_out_error('Error: illegal module declaration in the export part of module "~w"',
					[ModuleName], null, 0, 0, [], null)
		  ),
	          ( LocTokens = [item([big_name('LOCAL'), big_name(LocName)],
					_, _) | LocTokens2]
	            -> true
	            ;  LocTokens = [_|LocTokens2],
		       print_out_error('Error: illegal module declaration in the local part of module "~w"',
					[ModuleName], null, 0, 0, [], null)
	          )
	     )
	)
   ),
   % note LocName and ExpName may have not been instantiated.
   ( LocName = ModuleName
     -> true
     ;  format(user_error, '~nWarning: module name of the local part differs from the file name.~n', [])
   ),
   ( ExpName = ModuleName
     -> true
     ;  format(user_error, '~nWarning: module name of the export part differs from the file name.~n', [])
   ).

/*------------------------------------------------------------------------------
 * parse_import(+ModuleName, Switch, +ExpTokens, -ExpTokens2, +LocTokens,
 *		-LocTokens2, +Loaded, -NewLoaded, +OldProg, -NewProg)
 */

parse_import(ModuleName, Switch, ExpTokens, ExpTokens2, LocTokens, LocTokens2,
		Loaded, NewLoaded, OldProg, NewProg):-
   identify_module_part(ExpTokens, ModulePartExp, ModulePartLoc),
   extract_import_modules(ExpTokens, ExpTokens2, ML, ML_Sexp, export),
   sort(ML, ExpMods),
   insert_import_decl(ML_Sexp, ModuleName, ModulePartExp, OldProg, Prog2),
   extract_import_modules(LocTokens, LocTokens2, ML2, ML_Sloc, local),
   sort(ML2, LocMods),
   insert_import_decl(ML_Sloc, ModuleName, ModulePartLoc, Prog2, Prog3),
   append(LocMods, ExpMods, ML3),
   sort(ML3, Mods),
   parse_module_aux(Mods, Switch, Loaded, NewLoaded, Prog3, NewProg).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

insert_import_decl([], _, _, Prog, Prog).
insert_import_decl([ModList/Switch|Mods], ModuleName, ModulePart, OldProg,
		NewProg) :-
   insert_import_decl_aux(ModList, Switch, ModuleName, ModulePart, OldProg,
				Prog2),
   insert_import_decl(Mods, ModuleName, ModulePart, Prog2, NewProg).


insert_import_decl_aux([], _, _, _, Prog, Prog).
insert_import_decl_aux([Mod|ModList], Switch, ModuleName, ModulePart, OldProg,
				NewProg) :-
   string2Gstring(Mod, GMod),
   ( Switch = 'IMPORT'
     -> 'ParserPrograms.QuickInsertImport.P5'(OldProg, ModuleName, ModulePart,
     				GMod, Prog2)
     ;  'ParserPrograms.QuickInsertLift.P4'(OldProg, ModuleName, GMod, Prog2)
   ),
   insert_import_decl_aux(ModList, Switch, ModuleName, ModulePart, Prog2,
				NewProg).

%------------------------------------------------------------------------------

extract_import_modules([], [], [], [], _).
extract_import_modules([Item|Items], Rest, Mods, Mod_Switch, WhichPart):-
   ( (  Item = item([big_name('IMPORT')|Tokens], Ln1, Ln2), Switch = 'IMPORT'
      ; Item = item([big_name('LIFT')|Tokens], Ln1, Ln2), Switch = 'LIFT'
     )
     -> ( Switch = 'LIFT', WhichPart = export
	  -> print_out_error('Error: LIFT declaration in the export part of the module',
		  [], WhichPart, Ln1, Ln2, [], null),
	     extract_import_modules(Items, Rest, Mods, Mod_Switch, WhichPart)
          ;  user_name_seq(Tokens, Tokens2, Mods2, ErrorNames),
	     ( ErrorNames = []
	       -> true
	       ;  print_out_error('Error: illegal module name(s) in ~w statement: ~w',
			[Switch, ErrorNames], WhichPart, Ln1, Ln2, [], null)
	     ),
	     ( Tokens2 = []
	       -> true
	       ;  length(Tokens2, Position),
	          print_out_error('Error: illegal symbol in ~w statement', [Switch],
		     WhichPart, Ln1, Ln2, [big_name(Switch)|Tokens], Position)
	     ),
	     extract_import_modules(Items, Rest, Mods3, Mod_Switch2, WhichPart),
	     Mod_Switch = [Mods2/Switch|Mod_Switch2],
	     append(Mods3, Mods2, Mods)
	)
     ;  Rest = [Item|Rest2],
	extract_import_modules(Items, Rest2, Mods, Mod_Switch, WhichPart)
   ).

/*------------------------------------------------------------------------------
 * identify_module_kind does not report any error.
 */

identify_module_kind(ExpTokens, ModuleKind) :-
   ( ExpTokens == []
     -> ModuleKind = 'ProgDefs.ModuleKind.C0'
     ;  ( ExpTokens = [item([big_name('EXPORT')|_], _, _) | _]
	  -> ModuleKind = 'ProgDefs.NormalKind.C0'
	  ;  ModuleKind = 'ProgDefs.ClosedKind.C0'
	)
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

identify_module_part(ExpTokens, ModulePartExp, ModulePartLoc) :-
   ( ExpTokens = []
     -> ModulePartLoc = 'ProgDefs.Module.C0'
     ;  ( ExpTokens = [item([big_name('EXPORT')|_], _, _) | _]
	  -> ModulePartExp = 'ProgDefs.Export.C0'
	  ;  ModulePartExp = 'ProgDefs.Closed.C0'	% is this ok ?
	),
	ModulePartLoc = 'ProgDefs.Local.C0'
   ).


/*=============================================================================
 * parse_language1(+Items, -RestItems, +ModuleName, +Accessibility,
 *			+OldLang, -NewLang)
 * This parses the type declarations (constructor and base).
 * NOTE the accessibility of Opaque is ignored for the time being.
 */

parse_language1([], [], _, _, Lang, Lang).
parse_language1([Item|Items], RestItems, ModuleName, Accessibility,
		OldLang, NewLang):-
   ( Item = item([big_name(BigName)|Tokens], Ln1, Ln2),
     language_decl1(BigName, Tokens, Ln1, Ln2, ModuleName, Accessibility,
			OldLang, Lang2)
     -> % dot,
	parse_language1(Items, RestItems, ModuleName, Accessibility, Lang2,
			NewLang)
     ;  RestItems = [Item|RestItems2],
	parse_language1(Items, RestItems2, ModuleName, Accessibility, OldLang,
			NewLang)
   ).


/*------------------------------------------------------------------------------
 * Parse base declaration
 */

language_decl1('BASE', Tokens, Ln1, Ln2, ModuleName, Accessibility, OldLang,
		NewLang):-
   user_name_seq(Tokens, Tokens2, BaseNames, ErrorNames),
   ( ErrorNames = []
     -> true
     ;  accessibility2part(Accessibility, WhichPart),
        print_out_error('Error: illegal user name(s): ~w',
			[ErrorNames], WhichPart, Ln1, Ln2, [], null)
   ),
   ( Tokens2 = []
     -> insert_base_decl(BaseNames, ModuleName, Accessibility, Ln1, Ln2,
			OldLang, NewLang)
     ;  NewLang = OldLang,
	accessibility2part(Accessibility, WhichPart),
	length(Tokens2, Position),
        print_out_error('Error: illegal symbol in base declaration',
		[], WhichPart, Ln1, Ln2, [big_name('BASE')|Tokens], Position)
   ).


insert_base_decl([], _, _, _, _, Lang, Lang).
insert_base_decl([BaseName|BaseNames], ModuleName, Accessibility, Ln1, Ln2,
		OldLang, NewLang) :-
   string2Gstring(BaseName, GBaseName),
   'ParserPrograms.ParserTypeSymbols.P3'(GBaseName, OldLang, BNames), 
   ( member('MetaDefs.Name.F4'(ModuleName, GBaseName, 'MetaDefs.Base.C0',0),
		BNames)
     -> NewLang2 = OldLang,
   	accessibility2part(Accessibility, WhichPart),
	print_out_error('Error: base "~w" already declared',
			[BaseName], WhichPart, Ln1, Ln2, [], null)
     ;  'ParserPrograms.ParserTypeInsert.P5'(OldLang,
		'MetaDefs.Name.F4'(ModuleName, GBaseName, 'MetaDefs.Base.C0',0),
		Accessibility, 'ProgDefs.BaseDecl.C0', NewLang2)
   ),
   insert_base_decl(BaseNames, ModuleName, Accessibility, Ln1, Ln2,
		NewLang2, NewLang).
				

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

accessibility2part('ProgDefs.Exported.C0', export).
accessibility2part('ProgDefs.Hidden.C0', local).
     
/*------------------------------------------------------------------------------
 * Parse constructor declaration
 */

language_decl1('CONSTRUCTOR', Tokens, Ln1, Ln2, ModuleName, Accessibility,
		OldLang, NewLang):-
   constructor_decl(Tokens, [big_name('CONSTRUCTOR')|Tokens], Ln1, Ln2,
			ModuleName, Accessibility, OldLang, NewLang).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

constructor_decl(Tokens, WholeTokens, Ln1, Ln2, ModuleName, Accessibility,
			OldLang, NewLang):-
   ( Tokens = [UserName, graphic_name('/'), number(Arity)|Tokens2]
     -> ( user_name(UserName, ConstrName)
          -> ( positive_int(Arity)
               -> string2Gstring(ConstrName, GConstrName),
   		  'ParserPrograms.ParserTypeSymbols.P3'(GConstrName, OldLang, CCands),
   		  ( member('MetaDefs.Name.F4'(ModuleName, GConstrName,
					'MetaDefs.Constructor.C0', Arity),
				CCands)
     		    -> NewLang2 = OldLang,
   		       accessibility2part(Accessibility, WhichPart),
		       print_out_error('Error: constructor "~w/~d" already declared',
			  [ConstrName, Arity], WhichPart, Ln1, Ln2, [], null)
		    ;  'ParserPrograms.ParserTypeInsert.P5'(OldLang,
				'MetaDefs.Name.F4'(ModuleName, GConstrName,
					'MetaDefs.Constructor.C0', Arity),
			    Accessibility, 'ProgDefs.ConstructorDecl.F1'(Arity),
			    NewLang2)
		  )
               ;  NewLang2 = OldLang,
   		  accessibility2part(Accessibility, WhichPart),
		  print_out_error('Error: illegal arity for constructor "~w"',
				[ConstrName], WhichPart, Ln1, Ln2, [], null)
             )
     	  ;  NewLang = OldLang,
	     length(Tokens, Position),
   	     accessibility2part(Accessibility, WhichPart),
	     print_out_error('Error: illegal identifier in constructor declaration: "~w"',
			[UserName], WhichPart, Ln1, Ln2, WholeTokens, Position)
        ),
        ( Tokens2 = []
          -> NewLang = NewLang2
          ;  ( Tokens2 = [','|Tokens3]
	       -> constructor_decl(Tokens3, WholeTokens, Ln1, Ln2, ModuleName,
				Accessibility,
				NewLang2, NewLang)
	       ;  length(Tokens2, Position),
   	     	  accessibility2part(Accessibility, WhichPart),
		  print_out_error('Error: "," expected', [], WhichPart,
				Ln1, Ln2, WholeTokens, Position),
		  split_list(Tokens2, ',', _, Tokens3),
	          constructor_decl(Tokens3, WholeTokens, Ln1, Ln2, ModuleName,
				Accessibility,
				NewLang2, NewLang)

	     )
	)
     ;  OldLang = NewLang,
	length(Tokens, Position),
	print_out_error('Error: syntax error in constructor declaration',
			[], WhichPart, Ln1, Ln2, WholeTokens, Position)
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

positive_int(N) :-
   N > 0.

/*=============================================================================
 * parse_language2(+Tokens, -Tokens2, +ModuleName, +Accessibility,
 *			+OldLang, -NewLang)
 * This parses the rest of language declarations (constant, function,
 * proposition, predicate.
 */

parse_language2([], [], _, _, Lang, Lang).
parse_language2([Item|Items], Rest, ModuleName, Accessibility, OldLang,
			NewLang):-
   ( Item = item([big_name(BigName)|Tokens], Ln1, Ln2),
     language_decl2(BigName, Tokens, Ln1, Ln2, ModuleName, Accessibility,
			OldLang, Lang2)
     -> % dot,
	parse_language2(Items, Rest, ModuleName, Accessibility, Lang2,
			NewLang)
     ;  Rest = [Item|Rest2],
	parse_language2(Items, Rest2, ModuleName, Accessibility, OldLang,
			NewLang)
   ).

/*------------------------------------------------------------------------------
 * Parse constant declaration
 */

language_decl2('CONSTANT', Tokens, Ln1, Ln2, ModuleName, Accessibility, OldLang,
		NewLang):-
   constant_decl(Tokens, [big_name('CONSTANT')|Tokens], Ln1, Ln2, ModuleName,
		Accessibility, OldLang, NewLang).


constant_decl([], _, _, _, _, _, Lang, Lang) :- !.

constant_decl(Tokens, WholeTokens, Ln1, Ln2, ModuleName, Accessibility,
		OldLang, NewLang) :-
   accessibility2part(Accessibility, WhichPart),
   user_name_seq(Tokens, Tokens2, ConstNames, ErrorNames),
   ( ErrorNames = []
     -> true
     ;  ( ModuleName = '"Integers'
	  -> remove_number_constant(ErrorNames, ErrorNames2)
		% numbers as constants are not inserted into dictionary, but
		% simply removed from error message.
	  ;  ErrorNames = ErrorNames2
	),
	( ErrorNames2 = []
	  -> true
	  ;  print_out_error('Error: illegal constant(s): ~w',
			[ErrorNames2], WhichPart, Ln1, Ln2, [], null)
	)
   ),
   ( Tokens2 = [graphic_name(':')|Tokens3]
     -> declared_type(Tokens3, Tokens4, WholeTokens, Type, ModuleName, Ln1,
			Ln2, OldLang, WhichPart),
	( Type = null	% when there is an error in the type
	  -> split_list(Tokens4, ';', _, Tokens5),
	     constant_decl(Tokens5, WholeTokens, Ln1, Ln2, ModuleName,
				Accessibility, OldLang, NewLang)
	  ;  insert_constants(ConstNames, Type, ModuleName, Accessibility,
				Ln1, Ln2, OldLang, Lang2),
	     ( Tokens4 = []
	       -> NewLang = Lang2
	       ;  ( Tokens4 = [';'|Tokens5]
		    -> constant_decl(Tokens5, WholeTokens, Ln1, Ln2,
				ModuleName, Accessibility, Lang2, NewLang)
		    ;  length(Tokens4, Position),
		       print_out_error('Error: ";" or "." expected',
				[], WhichPart, Ln1, Ln2, WholeTokens, Position),
		       split_list(Tokens4, ';', _, Tokens5),
		       constant_decl(Tokens5, WholeTokens, Ln1, Ln2, ModuleName,
				      Accessibility, Lang2, NewLang)

		  )
	     )
	)
     ;  length(Tokens2, Position),
	print_out_error('Error: ":" expected', [], WhichPart, Ln1, Ln2,
				WholeTokens, Position),
	split_list(Tokens2, ';', _, Tokens3),
	constant_decl(Tokens3, WholeTokens, Ln1, Ln2, ModuleName, Accessibility,
			OldLang, NewLang)
   ).


insert_constants([], _, _, _, _, _, Lang, Lang).
insert_constants([Symbol|ConstNames], Type, ModuleName, Accessibility, Ln1, Ln2,
			OldLang, NewLang) :-
   string2Gstring(Symbol, GSymbol),
   'ParserPrograms.ParserSymbols.P3'(GSymbol, OldLang, Candidates), 
   ( member('ParserPrograms.FCandidate.F2'('MetaDefs.Name.F4'(ModuleName, GSymbol,
		'MetaDefs.Constant.C0', 0), _), Candidates)
     -> Lang2 = OldLang,
	accessibility2part(Accessibility, WhichPart),
	print_out_error('Error: constant "~w" already declared',
			[Symbol], WhichPart, Ln1, Ln2, [], null)
     ;  'ParserPrograms.ParserInsert.P5'(OldLang, 'MetaDefs.Name.F4'(ModuleName,
					GSymbol, 'MetaDefs.Constant.C0', 0),
				Accessibility,
				'ProgDefs.ConstantDecl.F1'(Type), Lang2)
   ),
   insert_constants(ConstNames, Type, ModuleName, Accessibility, Ln1, Ln2,
			Lang2, NewLang).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

remove_number_constant([], []).
remove_number_constant([number(_)|ErrorNames], ErrorNames2) :- !,
   remove_number_constant(ErrorNames, ErrorNames2).
remove_number_constant([Token|ErrorNames], [Token|ErrorNames2]) :-
   remove_number_constant(ErrorNames, ErrorNames2).


/*------------------------------------------------------------------------------
 * Related module conditions (base and constructor) are imposed on type.
 */

declared_type([Token|Tokens], RestTokens, WholeTokens, Type, ModuleName,
		Ln1, Ln2, Language, WhichPart) :-
   type(Token, Tokens, RestTokens, WholeTokens, Type, ModuleName, Ln1, Ln2,
		Language, WhichPart).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * type(+Token, +Tokens, -RestTokens, +WholeTokens, -Type, ?ModuleName,
 *	+Ln1, +Ln2, +Language, +WhichPart)
 * Type = 'null' indicating error.
 * ModuleName can be instantiated or uninstantiated. Instantiated ModuleName
 * impose the required module condition.
 */


type(little_name(Name), Tokens, Tokens, _, 'MetaDefs.Par.F2'(GName, 0), _,
		_, _, _, _) :-
   !,
   string2Gstring(Name, GName).

type(UserName, Tokens, RestTokens, WholeTokens, Type, ModuleName, Ln1, Ln2,
		Language, WhichPart) :-
   ( user_name(UserName, Name)
     -> string2Gstring(Name, GName),
        'ParserPrograms.ParserTypeSymbols.P3'(GName, Language, Candidates), 
	( Candidates = []
	  -> Type = null,
	     RestTokens = Tokens,
	     length(Tokens, Position),
	     print_out_error('Error: undeclared base or constructor: "~w"',
			[Name], WhichPart, Ln1, Ln2, WholeTokens, Position)
          ;  ( Tokens = ['('|Tokens2]
	       -> type_seq(Tokens2, RestTokens, WholeTokens, SubTypes, Error,
				Ln1, Ln2, Language, WhichPart),
	          ( nonvar(Error)
	            -> Type = null
	            ;  length(SubTypes, Arity),
		       ( ambiguous_checking(ModuleName, GName,
				  'MetaDefs.Constructor.C0', Arity, Ln1, Ln2,
				  Candidates, WhichPart)
		         -> Type = 'MetaDefs.Type.F2'(
			     		 'MetaDefs.Name.F4'(ModuleName, GName,
					      'MetaDefs.Constructor.C0', Arity),
				         SubTypes)
		         ;  Type = null
		       )
	          )
	       ;  RestTokens = Tokens,
	          ( ambiguous_checking(ModuleName, GName, 'MetaDefs.Base.C0', 0,
					Ln1, Ln2, Candidates, WhichPart)
	            -> Type = 'MetaDefs.BType.F1'('MetaDefs.Name.F4'(ModuleName,
						GName, 'MetaDefs.Base.C0', 0))
	            ;  Type = null
	          )
             )
	)
     ;  Type = null,
	RestTokens = Tokens,
	length(Tokens, Position),
	print_out_error('Error: illegal base or constructor identifier: "~w"',
			[UserName], WhichPart, Ln1, Ln2, WholeTokens, Position)
   ).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * This fails if there is an error.
 */

ambiguous_checking(ModuleName, Symbol, Category, Arity, Ln1, Ln2, Candidates,
		WhichPart) :-
   gstring2string(Symbol, S),
   ( var(ModuleName)
     -> ( delete_once(Candidates,
			'MetaDefs.Name.F4'(ModuleName, Symbol, Category, Arity),
		    	Candidates2)
	  -> ( member('MetaDefs.Name.F4'(ModuleName2, Symbol, Category, Arity),
			Candidates2)
	       -> 
	          gstring2string(ModuleName, M),
	          gstring2string(ModuleName2, M2),
	      	  print_out_error('Error: ambiguous base/constructor "~w" from modules "~w" and "~w"',
			[S, M, M2], WhichPart, Ln1, Ln2, [], null),
		  fail
	       ;  true
	     )
	  ;  print_out_error('Error: undeclared base or constructor: "~w"',
				[S], WhichPart, Ln1, Ln2, [], null),
	     fail
	)
     ;  ( member('MetaDefs.Name.F4'(ModuleName, Symbol, Category, Arity),
			Candidates)
	  -> true
	  ;  print_out_error('Error: attempt to define symbol(s) of non-local type "~w"',
				[S], WhichPart, Ln1, Ln2, [], null),
	     fail
	)
   ).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * type_seq(+Tokens, -RestTokens, +WholeTokens, -SubTypes, -Error, +Ln1, +Ln2,
 *		+Language, +WhichPart)
 * Error being instantiate denotes error
 */

type_seq([], [], _, [], _, _, _, _, _).

type_seq([Token|Tokens], RestTokens, WholeTokens, SubTypes, Error, Ln1, Ln2,
		Language, WhichPart) :-
   type(Token, Tokens, Tokens2, WholeTokens, Type, _, Ln1, Ln2, Language,
		WhichPart),
   ( Type = null
     -> Error = yes,
	RestTokens = Tokens2
     ;  ( Tokens2 = [')'|RestTokens]
          -> SubTypes = [Type]
	  ;  ( Tokens2 = [','|Tokens3]
	       -> type_seq(Tokens3, RestTokens, WholeTokens, SubTypes2, Error,
				Ln1, Ln2, Language, WhichPart),
	          ( nonvar(Error)
	            -> true
	            ;  SubTypes = [Type|SubTypes2]
	          )
	       ;  ( Tokens2 = [Illegal|RestTokens]
		    -> length(RestTokens, Position),
		       print_out_error('Error: illegal symbol in type: "~w"',
			[Illegal], WhichPart, Ln1, Ln2, WholeTokens, Position)
		    ;  RestTokens = Tokens2,
		       length(RestTokens, Position),
		       print_out_error('Error: ")" expected', [], WhichPart,
				Ln1, Ln2, WholeTokens, Position)
		  )
	     )
	)
   ).


/*------------------------------------------------------------------------------
 * Parse function declaration
 */

language_decl2('FUNCTION', Tokens, Ln1, Ln2, ModuleName, Accessibility, OldLang,
		NewLang):-
   function_decl(Tokens, [big_name('FUNCTION')|Tokens], Ln1, Ln2,
			ModuleName, Accessibility, OldLang, NewLang).


function_decl([], _, _, _, _, _, Lang, Lang) :- !.

function_decl(Tokens, WholeTokens, Ln1, Ln2, ModuleName, Accessibility,
		OldLang, NewLang):-
   accessibility2part(Accessibility, WhichPart),
   user_name_seq(Tokens, Tokens2, FuncNames, ErrorNames),
   ( ErrorNames = []
     -> true
     ;  print_out_error('Error: illegal function identifier(s): ~w',
			[ErrorNames], WhichPart, Ln1, Ln2, [], null)
   ),
   spec_and_type(Tokens2, Tokens3, WholeTokens, Spec, DomainTypes,
		RangeType, Error, Ln1, Ln2, ModuleName, OldLang, WhichPart),
   ( nonvar(Error)
     -> % there is an error in spec and type 
	split_list(Tokens3, ';', _, Tokens4),
	function_decl(Tokens4, WholeTokens, Ln1, Ln2, ModuleName,
				Accessibility, OldLang, NewLang)
     ;  ( transparency_check(DomainTypes, RangeType)
          -> length(DomainTypes, Arity),
	     insert_function_decl(FuncNames, 'ProgDefs.FunctionDecl.F4'(Arity,
						Spec, DomainTypes, RangeType),
		Arity, ModuleName, Accessibility, Ln1, Ln2, OldLang, Lang2),
	     ( Tokens3 =[]
	       -> NewLang = Lang2
	       ;  ( Tokens3 = [';'|Tokens5]
	            -> function_decl(Tokens5, WholeTokens, Ln1, Ln2, ModuleName,
					Accessibility, Lang2, NewLang)
	            ;  length(Tokens3, Position),
		       print_out_error('Error: ";" or "." expected', [],
				WhichPart, Ln1, Ln2, WholeTokens, Position),
		       split_list(Tokens3, ';', _, Tokens6),
		       function_decl(Tokens6, WholeTokens, Ln1, Ln2, ModuleName,
					Accessibility, Lang2, NewLang)
	          )
	     )
	  ;  print_out_error('Error: non-transparent function(s) ~w',
				[FuncNames], WhichPart, Ln1, Ln2, [], null),
	     split_list(Tokens3, ';', _, Tokens4),
	     function_decl(Tokens4, WholeTokens, Ln1, Ln2, ModuleName,
				Accessibility, OldLang, NewLang)
	)
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

insert_function_decl([], _, _, _, _, _, _, Lang, Lang).
insert_function_decl([FuncName|FuncNames], Decl, Arity, ModuleName,
			Accessibility, Ln1, Ln2, OldLang, NewLang) :-
   string2Gstring(FuncName, GFuncName),
   'ParserPrograms.ParserSymbols.P3'(GFuncName, OldLang, Candidates), 
   ( member('ParserPrograms.FCandidate.F2'('MetaDefs.Name.F4'(ModuleName, GFuncName,
		'MetaDefs.Function.C0', Arity), _),
		Candidates)
     -> Lang2 = OldLang,
   	accessibility2part(Accessibility, WhichPart),
	print_out_error('Error: function "~w/~d" already declared',
			[FuncName, Arity], WhichPart, Ln1, Ln2, [], null)
     ;  'ParserPrograms.ParserInsert.P5'(OldLang, 'MetaDefs.Name.F4'(ModuleName,
				GFuncName, 'MetaDefs.Function.C0', Arity),
				Accessibility, Decl, Lang2)
   ),
   insert_function_decl(FuncNames, Decl, Arity, ModuleName, Accessibility,
		Ln1, Ln2, Lang2, NewLang).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

spec_and_type([], [], _, _, _, _, yes, _, _, _, _, _).
spec_and_type([graphic_name(':'), Token|Tokens], RestTokens, WholeTokens, Spec,
	DomainTypes, RangeType, Error, Ln1, Ln2, ModuleName, Lang, WhichPart) :-
   !,
   ( function_spec(Token, Spec, Prec, Arity)
     -> ( Tokens = ['(', number(Prec), ')'|Tokens2]
	  -> ( positive_int(Prec)
	       -> function_type(Tokens2, RestTokens, WholeTokens, DomainTypes,
		     RangeType, Error, Ln1, Ln2, ModuleName, Lang, WhichPart),
		  ( length(DomainTypes, Arity)
		    -> true
	       	    ;  Error = yes,
		       length(RestTokens, Position),
		       print_out_error('Error: inappropriate arity for operator',
				[], WhichPart, Ln1, Ln2, WholeTokens, Position)
		  )
	       ;  Error = yes,
		  RestTokens = Tokens,
		  length(Tokens, Position),
		  print_out_error('Error: illegal precedence',
				[], WhichPart, Ln1, Ln2, WholeTokens, Position)
	     )
	  ;  Error = yes,
	     RestTokens = Tokens,
	     length(Tokens, Position),
	     print_out_error('Error: illegal function specification (function precedence expected)',
				[], WhichPart, Ln1, Ln2, WholeTokens, Position)
	)
     ;  Spec = 'Syntax.NoFunctInd.C0',
	function_type([graphic_name(':'), Token|Tokens], RestTokens,
			WholeTokens, DomainTypes, RangeType, Error, Ln1, Ln2,
			ModuleName, Lang, WhichPart)
   ).

spec_and_type(Tokens, Tokens, WholeTokens, _, _, _, yes, Ln1, Ln2, _, _,
		WhichPart) :-
   length(Tokens, Position),
   print_out_error('Error: ":" expected', [], WhichPart, Ln1, Ln2,
			WholeTokens, Position).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

function_spec(big_name(Name), Spec, Prec, Arity) :-
	function_spec_aux(Name, Spec, Prec, Arity).
function_spec(little_name(Name), Spec, Prec, Arity) :-
	function_spec_aux(Name, Spec, Prec, Arity).

function_spec_aux('Fx', 'Syntax.FX.F1'(Prec), Prec, 1).
function_spec_aux('Fy', 'Syntax.FY.F1'(Prec), Prec, 1).
function_spec_aux('xF', 'Syntax.XF.F1'(Prec), Prec, 1).
function_spec_aux('yF', 'Syntax.YF.F1'(Prec), Prec, 1).
function_spec_aux('xFx', 'Syntax.XFX.F1'(Prec), Prec, 2).
function_spec_aux('xFy', 'Syntax.XFY.F1'(Prec), Prec, 2).
function_spec_aux('yFx', 'Syntax.YFX.F1'(Prec), Prec, 2).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Error will be instantiated if there is an error
 */

function_type(Tokens, RestTokens, WholeTokens, DomainTypes, RangeType, Error,
		Ln1, Ln2, ModuleName, Language, WhichPart) :-
   ( Tokens = [graphic_name(':')|Tokens2]
     -> function_type_aux(Tokens2, RestTokens, WholeTokens, DomainTypes,
		RangeType, Error, Ln1, Ln2, ModuleName, Language, WhichPart)
     ;  Error = yes,
	RestTokens = Tokens,
	length(Tokens, Position),
	print_out_error('Error: ":" expected', [], WhichPart, Ln1, Ln2,
			WholeTokens, Position)
   ).


function_type_aux(Tokens, RestTokens, WholeTokens, DomainTypes, RangeType,
		Error, Ln1, Ln2, ModuleName, Language, WhichPart) :-
   star_type_seq(Tokens, Tokens2, WholeTokens, DomainTypes, Error, Ln1, Ln2,
		Language, WhichPart),
   ( var(Error)
     -> ( Tokens2 = [graphic_name('->')|Tokens3]
     	  -> declared_type(Tokens3, RestTokens, WholeTokens, RangeType,
			ModuleName, Ln1, Ln2, Language, WhichPart),
	     ( RangeType = null
	       -> Error = yes
	       ;  true
	     )
     	  ;  Error = yes,
	     RestTokens = Tokens2,
	     length(Tokens2, Position),
	     print_out_error('Error: "->" expected', [], WhichPart, Ln1, Ln2,
				WholeTokens, Position)
	)
     ;  RestTokens = Tokens2
   ).


star_type_seq([], [], _, [], _, _, _, _, _).

star_type_seq([Token|Tokens], RestTokens, WholeTokens, SubTypes, Error, Ln1,
		Ln2, Language, WhichPart):-
   type(Token, Tokens, Tokens2, WholeTokens, Type, _, Ln1, Ln2, Language,
		WhichPart),
   ( Type = null
     -> Error = yes,
	RestTokens = Tokens2
     ;  ( ( Tokens2 = [];		% for predicate decl
	    Tokens2 = [graphic_name('->')|_];	% for function decl
	    Tokens2 = [';'|_]	% for predicate decl
	  )
          -> SubTypes = [Type],
	     RestTokens = Tokens2
	  ;  ( Tokens2 = [graphic_name('*')|Tokens3]
	       -> star_type_seq(Tokens3, RestTokens, WholeTokens, SubTypes2,
				Error, Ln1, Ln2, Language, WhichPart),
	          ( nonvar(Error)
	            -> true
	            ;  SubTypes = [Type|SubTypes2]
	          )
	       ;  Error = yes,
		  ( Tokens2 = [Illegal|RestTokens]
		    -> length(RestTokens, Position),
		       print_out_error('Error: illegal symbol in type: "~w"',
			[Illegal], WhichPart, Ln1, Ln2, WholeTokens, Position)
		    ;  RestTokens = Tokens2,
		       length(RestTokens, Position),
		       print_out_error('Error: ")" expected', [], WhichPart,
				Ln1, Ln2, WholeTokens, Position)
		  )
	     )
	)
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * transparency_check(+DomainTypes, +RangeType)
 * it fails if the function declaration is not transparent.
 */

transparency_check(DomainTypes, RangeType) :-
   get_parameter(DomainTypes, DomainParas, []),
   get_parameter(RangeType, RangeParas, []),
   subset(DomainParas, RangeParas).

get_parameter([], Paras, Paras).
get_parameter([Type|Types], Paras, Paras2) :-
   get_parameter(Type, Paras, Paras3),
   get_parameter(Types, Paras3, Paras2).

get_parameter('MetaDefs.Par.F2'(String, Int),
		['MetaDefs.Par.F2'(String, Int)|Paras], Paras).
get_parameter('MetaDefs.BType.F1'(_), Paras, Paras).
get_parameter('MetaDefs.Type.F2'(_, Types), Paras, Paras2) :-
   get_parameter(Types, Paras, Paras2).


/*------------------------------------------------------------------------------
 * Parse proposition declaration
 */

language_decl2('PROPOSITION', Tokens, Ln1, Ln2, ModuleName, Accessibility,
		OldLang, NewLang):-
   accessibility2part(Accessibility, WhichPart),
   user_name_seq(Tokens, Tokens2, PropNames, ErrorNames),
   ( ErrorNames = []
     -> true
     ;  print_out_error('Error: illegal proposition identifier(s): ~w',
			[ErrorNames], WhichPart, Ln1, Ln2, [], null)
   ),
   ( Tokens2 = []
     -> insert_proposition_decl(PropNames, ModuleName, Accessibility, Ln1, Ln2,
			OldLang, NewLang)
     ;  length(Tokens2, Position),
        print_out_error('Error: illegal symbol', [], WhichPart, Ln1, Ln2,
			Tokens, Position)
   ).


insert_proposition_decl([], _, _, _, _, Lang, Lang).
insert_proposition_decl([PropName|PropNames], ModuleName, Accessibility,
			Ln1, Ln2, OldLang, NewLang) :-
   string2Gstring(PropName, GPropName),
   ( reserved_proposition_name(PropName)
     -> Lang2 = OldLang,
   	accessibility2part(Accessibility, WhichPart),
	print_out_error('Error: attempt to redefine reserved proposition "~w"',
		[PropName], WhichPart, Ln1, Ln2, [], null)
     ;  'ParserPrograms.ParserSymbols.P3'(GPropName, OldLang, Candidates), 
        ( member('ParserPrograms.PCandidate.F2'('MetaDefs.Name.F4'(ModuleName,
		GPropName, 'MetaDefs.Proposition.C0', 0), _), Candidates)
          -> Lang2 = OldLang,
   	     accessibility2part(Accessibility, WhichPart),
	     print_out_error('Error: proposition "~w" already declared',
			[PropName], WhichPart, Ln1, Ln2, [], null)
          ;  'ParserPrograms.ParserInsert.P5'(OldLang, 'MetaDefs.Name.F4'(ModuleName,
				GPropName, 'MetaDefs.Proposition.C0', 0),
			Accessibility, 'ProgDefs.PropositionDecl.C0', Lang2)
	)
   ),
   insert_proposition_decl(PropNames, ModuleName, Accessibility, Ln1, Ln2,
		Lang2, NewLang).


/*------------------------------------------------------------------------------
 * Parse predicate declaration
 * almost the same as the program for function declarations. 
 */

language_decl2('PREDICATE', Tokens, Ln1, Ln2, ModuleName, Accessibility,
		OldLang, NewLang):-
   predicate_decl(Tokens, [big_name('PREDICATE')|Tokens], Ln1, Ln2,
			ModuleName, Accessibility, OldLang, NewLang).


predicate_decl([], _, _, _, _, _, Lang, Lang) :- !.

predicate_decl(Tokens, WholeTokens, Ln1, Ln2, ModuleName, Accessibility,
		OldLang, NewLang):-
   accessibility2part(Accessibility, WhichPart),
   user_name_seq(Tokens, Tokens2, PredNames, ErrorNames),
   ( ErrorNames = []
     -> true
     ;  print_out_error('Error: illegal predicate identifier(s): ~w',
			[ErrorNames], WhichPart, Ln1, Ln2, [], null)
   ),
   ( Tokens2 = [graphic_name(':'), Token|Tokens3]
     -> ( predicate_spec(Token, Spec, Arity)
	  -> predicate_type(Tokens3, Tokens4, WholeTokens, DomainTypes, Arity,
				Error, Ln1, Ln2, OldLang, WhichPart)
	  ;  Spec = 'Syntax.NoPredInd.C0',
	     predicate_type([graphic_name(':'), Token|Tokens3], Tokens4,
				WholeTokens, DomainTypes, _, Error, Ln1, Ln2,
				OldLang, WhichPart)
	),
	( var(Error)
	  -> length(DomainTypes, Arity),
	     insert_predicate_decl(PredNames, 'ProgDefs.PredicateDecl.F3'(Arity,
						Spec, DomainTypes),
		    Arity, ModuleName, Accessibility, Ln1, Ln2, OldLang, Lang2),
	     ( Tokens4 =[]
	       -> NewLang = Lang2
	       ;  ( Tokens4 = [';'|Tokens5]
		    -> predicate_decl(Tokens5, WholeTokens, Ln1, Ln2,
				ModuleName, Accessibility, Lang2, NewLang)
		    ;  length(Tokens4, Position1), 
		       Position is Position1 - 1,
		       print_out_error('Error: illegal symbol', [], WhichPart,
				Ln1, Ln2, WholeTokens, Position),
		       split_list(Tokens4, ';', _, Tokens5),
		       predicate_decl(Tokens5, WholeTokens, Ln1, Ln2,
				ModuleName, Accessibility, OldLang, NewLang)
		  )
	     )
	  ;  split_list(Tokens4, ';', _, Tokens5),
	     predicate_decl(Tokens5, WholeTokens, Ln1, Ln2, ModuleName,
	    		 Accessibility, OldLang, NewLang)
	)
     ;  length(Tokens2, Position),
	print_out_error('Error: ":" expected', [], WhichPart, Ln1, Ln2,
			WholeTokens, Position),
	split_list(Tokens2, ';', _, Tokens3),
	predicate_decl(Tokens3, WholeTokens, Ln1, Ln2, ModuleName,
			Accessibility, OldLang, NewLang)
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

predicate_type(Tokens, RestTokens, WholeTokens, DomainTypes, Arity, Error,
		Ln1, Ln2, Language, WhichPart) :-
   ( Tokens = [graphic_name(':')|Tokens2]
     -> star_type_seq(Tokens2, RestTokens, WholeTokens, DomainTypes, Error,
			Ln1, Ln2, Language, WhichPart),
	( var(Error)
	  -> ( length(DomainTypes, Arity)
	       -> true
	       ;  Error = yes,
		  length(RestTokens, Position),
		  print_out_error('Error: inappropriate arity for operator',
			[], WhichPart, Ln1, Ln2, WholeTokens, Position)
	     )
	  ;  true
	)
     ;  Error = yes,
	RestTokens = Tokens,
	length(Tokens, Position),
	print_out_error('Error: ":" expected', [], WhichPart, Ln1, Ln2,
			WholeTokens, Position)
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

insert_predicate_decl([], _, _, _, _, _, _, Lang, Lang).
insert_predicate_decl([PredName|PredNames], Decl, Arity, ModuleName,
			Accessibility, Ln1, Ln2, OldLang, NewLang) :-
   string2Gstring(PredName, GPredName),
   ( reserved_predicate_name(PredName)
     -> Lang2 = OldLang,
   	accessibility2part(Accessibility, WhichPart),
	print_out_error('Error: attempt to redefine reserved predicate "~w"',
			     [PredName], WhichPart, Ln1, Ln2, [], null)
     ;  'ParserPrograms.ParserSymbols.P3'(GPredName, OldLang, Candidates), 
        ( member('ParserPrograms.PCandidate.F2'('MetaDefs.Name.F4'(ModuleName,
		GPredName, 'MetaDefs.Predicate.C0', Arity), _), Candidates)
          -> Lang2 = OldLang,
   	     accessibility2part(Accessibility, WhichPart),
	     print_out_error('Error: predicate "~w/~d" already declared',
			[PredName, Arity], WhichPart, Ln1, Ln2, [], null)
          ;  'ParserPrograms.ParserInsert.P5'(OldLang, 'MetaDefs.Name.F4'(ModuleName,
				  GPredName, 'MetaDefs.Predicate.C0', Arity),
				Accessibility, Decl, Lang2)
	)
   ),
   insert_predicate_decl(PredNames, Decl, Arity, ModuleName, Accessibility,
		Ln1, Ln2, Lang2, NewLang).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

predicate_spec(big_name(Name), Spec, Arity) :-
	predicate_spec_aux(Name, Spec, Arity).
predicate_spec(little_name(Name), Spec, Arity) :-
	predicate_spec_aux(Name, Spec, Arity).

predicate_spec_aux('Pz', 'Syntax.PZ.C0', 1).
predicate_spec_aux('zP', 'Syntax.ZP.C0', 1).
predicate_spec_aux('zPz', 'Syntax.ZPZ.C0', 2).


/*=============================================================================
 * user_name_seq(+Token_of_usernames, -UserNames, -ErrorNames).
 * this ends with empty, and :.
 */

user_name_seq([], [], [], []).
user_name_seq([Token|Tokens], Tokens2, UserNames, ErrorNames):-
   ( Token = ','
     -> user_name_seq(Tokens, Tokens2, UserNames, ErrorNames)
     ;  ( Token = graphic_name(':')
	  -> Tokens2 = [Token|Tokens],
	     UserNames = [],
	     ErrorNames = []
	  ;  ( user_name(Token, UserName)
	       -> UserNames = [UserName|UserNames2],
		  ErrorNames = ErrorNames2
	       ;  UserNames = UserNames2,
		  ErrorNames = [Token|ErrorNames2]
	     ),
	     user_name_seq(Tokens, Tokens2, UserNames2, ErrorNames2)
	)
   ).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * user_name fails if it's a reserved word and not a valid user name. 
 */

user_name(big_name(UserName), UserName) :-
   \+ reserved_word(UserName).

user_name(graphic_name(UserName), UserName) :-
   \+ reserved_word(UserName).


/*=============================================================================
 * parse_delays(+ModuleName, +ExpTokens, -ExpTokens2, +LocTokens, -LocTokens2,
 *		+ModulePartExp, +ModulePartLoc, +Language, +OldProg, -NewProg)
 * the module-name declarations are removed here
 */

parse_delays(ModuleName, ExpTokens, ExpTokens2, LocTokens, LocTokens2,
		ModulePartExp, ModulePartLoc, Language, OldProg, NewProg) :-
   parse_delays_aux(ExpTokens, ExpTokens2, ModuleName, ModulePartExp,
		Language, OldProg, Prog2, export),
   parse_delays_aux(LocTokens, LocTokens2, ModuleName, ModulePartLoc,
		Language, Prog2, NewProg, local).


parse_delays_aux([], [], _, _, _, Prog, Prog, _).
parse_delays_aux([Item|Items], RestItems, ModuleName, ModulePart, Language,
		OldProg, NewProg, WhichPart) :-
   ( Item = item([big_name('DELAY')|DelayPart], Ln1, Ln2)
     -> parse_delay_part(DelayPart, [big_name('DELAY')|DelayPart], ModuleName,
		ModulePart, Language, OldProg, Prog2, Ln1, Ln2, WhichPart),
	%dot,
	parse_delays_aux(Items, RestItems, ModuleName, ModulePart, Language,
			Prog2, NewProg, WhichPart)
     ;  RestItems = [Item|RestItems2],
	parse_delays_aux(Items, RestItems2, ModuleName, ModulePart, Language,
			OldProg, NewProg, WhichPart)
   ).


parse_delay_part([], _, _, _, _, Prog, Prog, _, _, _):- !.
parse_delay_part(Tokens, WholeTokens, ModuleName, ModulePart, Language,
		OldProg, NewProg, Ln1, Ln2, WhichPart):-
   underscore_init,     % initialising underscore label counter.
   headatom(Tokens, Tokens2, ModuleName, HeadAtoms, ErrorReturn, Language),
					%in statement.pl
   ( HeadAtoms = []
     -> print_error_return(ErrorReturn, WholeTokens, WhichPart, Ln1, Ln2),
	split_list(Tokens, ';', _, Tokens3),
	parse_delay_part(Tokens3, WholeTokens, ModuleName, ModulePart, Language,
			OldProg, NewProg, Ln1, Ln2, WhichPart)
     ;  'SharedPrograms.ProgramLanguage.P2'(OldProg, ProgLanguage),
	'ParserPrograms.ParserCheckFormula.P4'(HeadAtoms, ProgLanguage, HeadAtom,
		ErrorMessages), 
        ( ErrorMessages = []
	  -> get_predicate_arity(HeadAtom, PredicateSymbol, Arity),
	     ( Arity = 0
	       -> gstring2string(PredicateSymbol, Proposition),
		  print_out_error('Warning: proposition "~w" in delay declaration',
			[Proposition], WhichPart, Ln1, Ln2, [], null)
	       ;  true
	     ),
             ( Tokens2 = [big_name('UNTIL')|Tokens3]
               -> ( cond(Tokens3, Tokens4, Condition, WholeTokens, Error,
				Ln1, Ln2, WhichPart),
		    ( var(Error) 
		      -> ( delay_condition(OldProg, ModuleName, PredicateSymbol,
					Arity, HeadAtom)
			   -> collect_variables(HeadAtom, [], HeadVars),
			      collect_variables(Condition, [], CondVars),
			      set_minus(CondVars, HeadVars, BadVars),
			      strip_variables(BadVars, BadVars2),
			      ( BadVars2 = []
				-> true
				;  print_out_error('Warning: variables ~w in delay condition do not occur in the head-atom',
				     [BadVars2], WhichPart, Ln1, Ln2, [], null)
			      ),
			      'ParserPrograms.QuickInsertDelay.P7'(OldProg, ModuleName,
				PredicateSymbol, Arity, ModulePart,
				'ProgDefs.Delay.F2'(HeadAtom, Condition),
				Prog2)
			   ;  Prog2 = OldProg,
			      length(Tokens4, Position),
			      print_out_error('Error: headatom unifiable with previous delay declaration(s)', [], WhichPart, Ln1, Ln2, WholeTokens, Position)
			 )     
		      ;  Prog2 = OldProg
		    ),
		    ( Tokens4 = []
		      -> NewProg = Prog2
		      ;  ( Tokens4 = [';'|Tokens5]
			   -> parse_delay_part(Tokens5, WholeTokens, ModuleName,
				ModulePart, Language, Prog2, NewProg, Ln1, Ln2,
				WhichPart)
               		   ;  length(Tokens4, Position),
		  	      print_out_error('Error: missing ";" or "."',
				[], WhichPart, Ln1, Ln2, WholeTokens, Position),
		              split_list(Tokens4, ';', _, Tokens5),
		    	      parse_delay_part(Tokens5, WholeTokens, ModuleName,
				ModulePart, Language, Prog2, NewProg, Ln1, Ln2,
				WhichPart)
			 )
		    )
	          )
               ;  length(Tokens2, Position),
		  print_out_error('Error: "UNTIL" expected', [], WhichPart,
					Ln1, Ln2, WholeTokens, Position),
		  split_list(Tokens2, ';', _, Tokens3),
		  parse_delay_part(Tokens3, WholeTokens, ModuleName, ModulePart,
				Language, OldProg, NewProg, Ln1, Ln2, WhichPart)
	     )
	  ;  NewProg = OldProg,
	     print_type_error(ErrorMessages, WhichPart, Ln1, Ln2)
	)
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * This picks up delay conditions previously declared and checks whether the
 * new headatom unifies with them.  If so, it fails.
 */

delay_condition(Prog, ModuleName, PredicateSymbol, Arity, HeadAtom) :-
   ( 'SharedPrograms.SControlInProgram.P5'(Prog, ModuleName,
		'MetaDefs.Name.F4'(ModuleName, PredicateSymbol, _, Arity),
		HeadAtomList, _)
     -> delay_condition_aux(HeadAtomList, HeadAtom)
     ;  true
   ).


delay_condition_aux([], _).
delay_condition_aux([Atom|HeadAtomList], HeadAtom) :-
   ( 'SharedSyntax.SEmptyTermSubst.P1'(EmptySubst),
     'SharedSyntax.SUnifyAtoms.P4'(Atom, HeadAtom, EmptySubst, _)
     -> fail
     ;  delay_condition_aux(HeadAtomList, HeadAtom)
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

cond([], [], _, _, yes, _, _, _).

cond([Token|Tokens], RestTokens, Condition, WholeTokens, Error, Ln1, Ln2,
		WhichPart) :-
   cond1(Token, Tokens, RestTokens2, Cond1, WholeTokens, Error, Ln1, Ln2,
		WhichPart),
   ( var(Error)
     -> ( RestTokens2 = []
          -> Condition = Cond1,
             RestTokens = RestTokens2
          ;  RestTokens2 = [Token3|RestTokens3],
	     ( Token3 = graphic_name('&')
	       -> Condition = 'ProgDefs.And.F2'(Cond1, Cond2),
		  and_seq(RestTokens3, RestTokens, Cond2, WholeTokens, Error,
				Ln1, Ln2, WhichPart)
			% error will automatically returned
	       ;  ( Token3 = graphic_name('\/')
	            -> Condition = 'ProgDefs.Or.F2'(Cond1, Cond2),
		       or_seq(RestTokens3, RestTokens, Cond2, WholeTokens,
				Error, Ln1, Ln2, WhichPart)
	            ;  Condition = Cond1,
	     	       RestTokens = RestTokens2
	          )
	     )
	)
     ;  RestTokens = RestTokens2
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

cond1(big_name(ReservedWord), Tokens, RestTokens, Condition, WholeTokens,
		Error, Ln1, Ln2, WhichPart):-
   !,
   ( ReservedWord = 'TRUE'
     -> Condition = 'ProgDefs.TrueCond.C0',
	RestTokens = Tokens
     ;  ( Tokens = ['(', little_name(Var), ')' | RestTokens]
	  -> var_conversion(Var, 0, VarRep),
             ( ReservedWord = 'NONVAR'
	       -> Condition = 'ProgDefs.Nonvar.F1'(VarRep)
	       ;  ( ReservedWord = 'GROUND'
	            -> Condition = 'ProgDefs.Ground.F1'(VarRep)
	            ;  Error = yes,
		       length(Tokens, Position),
		       print_out_error('Error: illegal delay condition',
				[], WhichPart, Ln1, Ln2, WholeTokens, Position)
		  )
	     )
	  ;  Error = yes,
	     RestTokens = Tokens,
	     length(Tokens, Position),
	     print_out_error('Error: variable argument expected', [], WhichPart,
				Ln1, Ln2, WholeTokens, Position)
	)
   ).

cond1('(', Tokens, RestTokens, Condition, WholeTokens, Error, Ln1, Ln2,
		WhichPart) :-
   !,
   cond(Tokens, RestTokens2, Condition, WholeTokens, Error, Ln1, Ln2,
		WhichPart),
   ( nonvar(Error)
     -> RestTokens = RestTokens2
     ;  ( RestTokens2 = [')' | RestTokens]
          -> true
          ;  Error = yes,
	     RestTokens = RestTokens2,
	     length(RestTokens2, Position),
	     print_out_error('Error: ")" expected', [], WhichPart, Ln1, Ln2,
				WholeTokens, Position)
	)
   ).

% handling error cases
cond1(Token, Tokens, [Token|Tokens], _, WholeTokens, yes, Ln1, Ln2,
		WhichPart) :-
   length([Token|Tokens], Position),
   print_out_error('Error: unexpected symbols', [], WhichPart, Ln1, Ln2,
		WholeTokens, Position).
   

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

and_seq([], [], _, _, yes, _, _, _).

and_seq([Token|Tokens], RestTokens, Condition, WholeTokens, Error, Ln1, Ln2,
		WhichPart) :-
   cond1(Token, Tokens, RestTokens2, Cond1, WholeTokens, Error, Ln1, Ln2,
		WhichPart),
   ( var(Error)
     -> ( RestTokens2 = [graphic_name('&')|RestTokens3]
	  -> Condition = 'ProgDefs.And.F2'(Cond1, Cond2),
	     and_seq(RestTokens3, RestTokens, Cond2, WholeTokens, Error,
			Ln1, Ln2, WhichPart)
	  ;  Condition = Cond1,
	     RestTokens = RestTokens2
	)
     ;  RestTokens = RestTokens2
   ).

or_seq([], [], _, _, yes, _, _, _).

or_seq([Token|Tokens], RestTokens, Condition, WholeTokens, Error, Ln1, Ln2,
		WhichPart) :-
   cond1(Token, Tokens, RestTokens2, Cond1, WholeTokens, Error, Ln1, Ln2,
		WhichPart),
   ( var(Error)
     -> ( RestTokens2 = [graphic_name('\/')|RestTokens3]
	  -> Condition = 'ProgDefs.Or.F2'(Cond1, Cond2),
	     or_seq(RestTokens3, RestTokens, Cond2, WholeTokens, Error,
			Ln1, Ln2, WhichPart)
	  ;  Condition = Cond1,
	     RestTokens = RestTokens2
	)
     ;  RestTokens = RestTokens2
   ).


/*=============================================================================
 * parse_statements(+ModuleName, +ExpTokens, +LocTokens, +Language, +OldProg,
 *			-NewProg)
 */

parse_statements(ModuleName, ExpTokens, LocTokens, Language, OldProg, NewProg):-
   ( ExpTokens = []	% there should be no statement in export part
     -> true
     ;  assert(there_is_error),
	format(user_error, '~nError: there are illegal items in the export part of module "~w"~nThese items are:~n', [ModuleName]),
	print_error_items(ExpTokens)
   ),
   parse_statements_aux(LocTokens, ModuleName, Language, OldProg, NewProg).


parse_statements_aux([], _, _, Prog, Prog).
parse_statements_aux([Item|Items], ModuleName, Language, OldProg, NewProg):-
   statement(Item, ModuleName, Formulae, ErrorReturn, Language),
   Item = item(WholeTokens, Ln1, Ln2),	% for errors and other checking
   ( Formulae = []
     -> Prog2 = OldProg,
	print_error_return(ErrorReturn, WholeTokens, local, Ln1, Ln2)
     ;  'SharedPrograms.ProgramLanguage.P2'(OldProg, ProgLanguage),
        'ParserPrograms.ParserCheckStatement.P4'(Formulae, ProgLanguage, Statement,
			ErrorMessages),
	( ErrorMessages = []
	  -> get_predicate_arity(Statement, PredicateSymbol, Arity),
	     'ParserPrograms.QuickInsertStatement.P6'(OldProg, ModuleName,
			PredicateSymbol, Arity, Statement, Prog2),
	     singleton_variable_checking(Statement, PredicateSymbol, Arity,
				Ln1, Ln2),
	     quantified_variable_checking(Statement, Ln1, Ln2),
	     floundering_checking(Statement, Ln1, Ln2)
	  ;  Prog2 = OldProg,
	     print_type_error(ErrorMessages, local, Ln1, Ln2)
	)
   ),
   parse_statements_aux(Items, ModuleName, Language, Prog2, NewProg).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

get_predicate_arity('MetaDefs.<-''.F2'(Formula, _), PredicateSymbol, Arity):-
   get_predicate_arity(Formula, PredicateSymbol, Arity).
get_predicate_arity('MetaDefs.Atom.F2'(
			'MetaDefs.Name.F4'(_, PredicateSymbol, _, Arity), _),
		PredicateSymbol, Arity).
get_predicate_arity('MetaDefs.PAtom.F1'(
			'MetaDefs.Name.F4'(_, PredicateSymbol, _, Arity)),
		PredicateSymbol, Arity).

%------------------------------------------------------------------------------

my_format(Stream, Format, List) :-
   ( no_message
     -> true
     ;  format(Stream, Format, List)
   ).

/*=============================================================================
 * pretty_print_tokens(list(token))
 * no new-line is fed at the end unless there is a specific nl in the list.
 */

pretty_print_tokens([]).
pretty_print_tokens([H|T]):-
   pretty_print_token(H),
   pretty_print_tokens(T).

% nl for a new-line
pretty_print_token(nl):- 
   !,
   format(user_error, '~n', []).
pretty_print_token(big_name(A)):-
   !,
   format(user_error, ' ~w', [A]).
pretty_print_token(little_name(A)):-
   !,
   format(user_error, ' ~w', [A]).
pretty_print_token('_'(A)):-
   !,
   format(user_error, ' ~w', [A]).
pretty_print_token(string(A)):-
   !,
   format(user_error, ' "~w"', [A]).
pretty_print_token('}'(1000000000)):-
   !,
   format(user_error, ' }', []).
pretty_print_token('}'(A)):-
   !,
   format(user_error, ' }_~w', [A]).
pretty_print_token(number(A)):-
   !,
   format(user_error, ' ~w', [A]).
pretty_print_token(float(A)):-
   !,
   format(user_error, ' ~e', [A]).
pretty_print_token(graphic_name(A)):-
   !,
   format(user_error, ' ~w', [A]).
pretty_print_token(','):-
   !,
   format(user_error, ',', []).
pretty_print_token(A):-
   format(user_error, ' ~w', [A]).


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

print_error_items([]).
print_error_items([item(Tokens, Ln1, Ln2)|Items]) :-
   format(user_error, 'Item in lines ~d-~d : ', [Ln1, Ln2]),
   pretty_print_tokens(Tokens),
   format(user_error, '.~n', []),
   print_error_items(Items).


/*------------------------------------------------------------------------------
 */

print_error_return(ErrorReturn, Tokens, WhichPart, Ln1, Ln2) :-
   sort(ErrorReturn, ErrorReturn2),
   length(ErrorReturn2, Number_of_Errors),
   print_error_return(ErrorReturn2, Number_of_Errors, Tokens, WhichPart,
		Ln1, Ln2).


print_error_return([], _, _, _, _, _).
print_error_return([error(Format, Parameters)/Position|ErrorReturn],
	Number_of_Errors, Tokens, WhichPart, Ln1, Ln2) :-
   ( bad_message(Format, Parameters), Number_of_Errors > 1
     -> true
     ;  sappend('Error: ', Format, Format2),
        print_out_error(Format2, Parameters, WhichPart, Ln1, Ln2, Tokens,
			Position)
   ),
   print_error_return(ErrorReturn, Number_of_Errors, Tokens, WhichPart, Ln1, Ln2).

% this filters out complains about reservered words in term.
bad_message('undeclared or illegal symbol: "~w"', Parameters) :-
   token_to_pattern(Parameters, Parameters2),
   sort(Parameters2, Parameters3),
   intersection(Parameters3, ['ALL','ELSE','IF','SOME','THEN'], Set),
   Set \== [].


/*------------------------------------------------------------------------------
 * print_out_error(+Format, +Parameters, +WhichPart, +Ln1, +Ln2, +Tokens,
 *			+Position)
 *   Tokens can be [], indicating that the item is not going to be printed 
 *   Position can be null, indicating '*here*' is not printed, or donot_print
 *        which switch off the whole printing.
 *   If WhichPart is null, line numbers and lines are not printed
 */

print_out_error(_, _, _, _, _, _, donot_print) :- !.

print_out_error(Format, Parameters, WhichPart, Ln1, Ln2, Tokens, Position) :-
   name(Format, Chars),
   ( Chars = [0'E, 0'r, 0'r, 0'o, 0'r, 0': | Rest]
     -> assert(there_is_error),		% message to stop compilation
	format(user_error, '~nError:', [])
     ;  ( Chars = [0'W, 0'a, 0'r, 0'n, 0'i, 0'n, 0'g, 0': | Rest]
	  -> format(user_error, '~nWarning:', [])
	  ;  Rest = Chars
	)
   ),
   token_to_pattern(Parameters, Parameters2),
   ( ( WhichPart = null; Ln1 = 0, Ln2 = 0 )
     -> true
     ;  format(user_error, ' in lines ~d-~d', [Ln1, Ln2]),
	( WhichPart = local
          -> format(user_error, ' of local part,', [])
          ;  format(user_error, ' of export part,', [])
	)
   ),
   name(Format2, Rest),
   format(user_error, Format2, Parameters2),
   format(user_error, '.~n', []),
   ( Tokens = []
     -> true
     ;  length(Tokens, N),
        Counter is N - Position,
        format(user_error, '   ', []),
        print_position(Counter, Tokens)
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

print_position(null, Tokens) :-
   !,
   pretty_print_tokens(Tokens),
   format(user_error, '.~n', []).
print_position(0, Tokens) :-
   !,
   format(user_error, '~n**here**~n', []),
   pretty_print_tokens(Tokens),
   format(user_error, '.~n', []).
print_position(Counter, [Token|Tokens]) :-
   Counter2 is Counter - 1,
   pretty_print_token(Token),
   print_position(Counter2, Tokens).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

token_to_pattern([], []) :- !.
token_to_pattern([H|T], [H2|T2]) :- !,
   token_to_pattern(H, H2),
   token_to_pattern(T, T2).

token_to_pattern('MetaDefs.Var.F1'(_), 'v') :- !.
token_to_pattern('MetaDefs.Var.F2'(GSymbol, _), A) :- !,
   gstring2string(GSymbol, A).
token_to_pattern('MetaDefs.Name.F4'(_, GSymbol, _, _), A) :- !,
   gstring2string(GSymbol, A).
token_to_pattern(big_name(A), A) :- !.
token_to_pattern(little_name(A), A):- !.
token_to_pattern('_'(A), A):- !.
token_to_pattern(string(A), A):- !.
token_to_pattern(number(A), A):- !.
token_to_pattern(float(A), A):- !.
token_to_pattern(graphic_name(A), A):- !.
token_to_pattern(A, A).

/*------------------------------------------------------------------------------
 */


print_type_error(Msgs, Part, Ln1, Ln2) :-
   sort(Msgs, Msgs2),
   assert(there_is_error),
   ( Msgs2 = ['SharedPrograms.Ambiguity.C0'|_] ->
     print_ambiguity_error(Msgs2, Part, Ln1, Ln2)
   ; ( Part = null
       -> format(user_error, '~nType error in goal, possibly:~n', [])
       ;  format(user_error, '~nType error in lines ~d-~d of ~a part, possibly:~n'
                , [Ln1, Ln2, Part])
     ),
     print_type_error_aux(Msgs2)
   ).


print_type_error_aux([]).

print_type_error_aux([Msg|Msgs]) :-
   print_type_error_msg(Msg),
   print_type_error_aux(Msgs).

print_type_error_msg('SharedPrograms.TypeError.F3'(Term, Type, Context)) :-
   format(user_error, ' - type clash, ', []),
   print_type_error_term(Term),
   ( Term = 'MetaDefs.Num.F1'(_) ->
     format(user_error, ' cannot have type ', [])
   ; format(user_error, ' has type ', []),
     print_type_error_type(Type),
     format(user_error, ', does not match ', [])
   ),
   print_type_error_type(Context),
   nl(user_error).

print_type_error_msg('SharedPrograms.HeadError.F3'(Term, Type, Context)) :-
   format(user_error, ' - head type too specific, ', []),
   print_type_error_term(Term),
   format(user_error, ' has type ', []),
   print_type_error_type(Type),
   format(user_error, ', not type ', []),
   print_type_error_type(Context),
   nl(user_error).


print_ambiguity_error(_, Part, Ln1, Ln2) :-
   ( Part = null
     -> format(user_error, '~nError: ambiguity in the query.~n', [])
     ;  format(user_error, '~nError: ambiguity in lines ~d-~d of ~a part.~n'
      , [Ln1, Ln2, Part])
   ).


print_type_error_term('MetaDefs.Var.F1'(N)) :-
   ( N = 0 ->
     format(user_error, 'variable "~a"', ['v'])
   ; format(user_error, 'variable "~a_~d"', ['v', N])
   ).

print_type_error_term('MetaDefs.Var.F2'(S, N)) :-
   gstring2string(S, A), 
   ( N = 0 ->
     format(user_error, 'variable "~a"', [A])
   ; format(user_error, 'variable "~a_~d"', [A, N])
   ).

print_type_error_term('MetaDefs.CTerm.F1'('MetaDefs.Name.F4'(_, S, _, _))) :-
   gstring2string(S, A),
   format(user_error, 'constant "~a"', [A]).

print_type_error_term('MetaDefs.Term.F2'('MetaDefs.Name.F4'(_, S, _, N), _)) :-
   gstring2string(S, A),
   format(user_error, 'term with functor "~a/~d"', [A, N]).

print_type_error_term('MetaDefs.Str.F1'(S)) :-
   format(user_error, 'string ~a"', [S]).

print_type_error_term('MetaDefs.Int.F1'(N)) :-
   format(user_error, 'integer "~d"', [N]).

print_type_error_term('MetaDefs.Num.F1'(N)) :-
   format(user_error, 'number "~d"', [N]).


% ** More here **


print_type_error_type('MetaDefs.Par.F2'(S, N)) :-
   gstring2string(S, A),
   ( N = 0 ->
     format(user_error, '~a', [A])
   ; format(user_error, '~a_~d', [A, N])
   ).

print_type_error_type('MetaDefs.Par.F1'(N)) :-
   ( N = 0 ->
     format(user_error, 'p', [])
   ; format(user_error, 'p_~d', [N])
   ).

print_type_error_type('MetaDefs.BType.F1'('MetaDefs.Name.F4'(_, S, _, _))) :-
   gstring2string(S, A),
   format(user_error, '~a', [A]). 

print_type_error_type('MetaDefs.Type.F2'('MetaDefs.Name.F4'(_, S, _, _)
      , [Arg|Args])
      ) :-
   gstring2string(S, A),
   format(user_error, '~a(', [A]),
   print_type_error_type(Arg),
   print_type_error_type_list(Args),
   format(user_error, ')', []).

print_type_error_type_list([]).

print_type_error_type_list([Type|Types]) :-
   format(user_error, ',', []),
   print_type_error_type(Type),
   print_type_error_type_list(Types).

%==============================================================================
% A collection Goedel reserved words

system_module_name('"Integers').
system_module_name('"Rationals').
system_module_name('"Floats').
system_module_name('"Numbers').
system_module_name('"NumbersIO').

system_module_name('"Lists').
system_module_name('"Sets').
system_module_name('"Strings').
system_module_name('"Tables').

system_module_name('"Units').
system_module_name('"Flocks').
system_module_name('"FlocksIO').
system_module_name('"IO').

system_module_name('"Syntax').
system_module_name('"SharedSyntax').
system_module_name('"ExtraSyntax').
system_module_name('"Substs').
system_module_name('"MetaDefs').

system_module_name('"Programs').
system_module_name('"SharedPrograms').
system_module_name('"ProgDefs').
system_module_name('"ProgramCache').

system_module_name('"Scripts').
system_module_name('"ScriptsIO').
system_module_name('"ProgramsIO').
system_module_name('"Theories').
system_module_name('"TheoriesIO').
system_module_name('"AVLTrees').

%------------------------------------------------------------------------------

open_system_module('"Syntax').

%------------------------------------------------------------------------------

reserved_proposition_name('True').
reserved_proposition_name('False').

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

reserved_predicate_name('=').
reserved_predicate_name('~=').

%------------------------------------------------------------------------------

reserved_word('EXPORT').
reserved_word('LOCAL').
reserved_word('CLOSED').
reserved_word('MODULE').
reserved_word('THEORY').
reserved_word('IMPORT').
reserved_word('LIFT').
reserved_word('BASE').
reserved_word('CONSTRUCTOR').
reserved_word('CONSTANT').
reserved_word('FUNCTION').
reserved_word('PROPOSITION').
reserved_word('PREDICATE').
reserved_word('DELAY').
reserved_word('UNTIL').
reserved_word('GROUND').
reserved_word('NONVAR').
reserved_word('TRUE').
reserved_word('ALL').
reserved_word('SOME').
reserved_word('IF').
reserved_word('THEN').
reserved_word('ELSE').
reserved_word(':').
reserved_word('<-').
reserved_word('->').
reserved_word('<->').
reserved_word('&').
reserved_word('~').
reserved_word('\/').
reserved_word('|').

