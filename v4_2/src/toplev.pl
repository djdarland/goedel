
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


/*-----------------------------------------------------------------------------
 * The top level query evaluator for the Goedel system. Entry point top_loop/1.
 *
 * File: 	toplev.pl
 * Author:	Jiwei Wang
 * Date: 	Completely restored at 30 Jan. 1992
 *
==============================================================================*/


/*-----------------------------------------------------------------------------
 * Predicates for version control.
 */

'$$module'('@(#)toplev.pl 1.112 last updated 94/04/18 22:35:33 by jiwei
').

'$$module'(S) :- '$$init'(S).    % Special for init, which is not compiled.

goedel_info :-	\+ ('$$module'(S), write(S), fail).

%------------------------------------------------------------------------------

:- dynamic there_is_error/0.
:- dynamic no_message/0.
:- dynamic create_tracer/0.
:- dynamic trace_is_on/0.
:- dynamic checking_is_off/0.
:- dynamic spied/1.
:- dynamic cached_program/2.

%------------------------------------------------------------------------------
% operators for displaying Goedel formulas
% The precedences are determined by "=" and "+"

:- op(700, xfx, [=, '~=', <, >, =<, >=, 'In', 'Subset', 'StrictSubset']).
:- op(705, fx,  ['~ ']).
:- op(710, xfy, [' & ']).
:- op(720, xfy, [' \/ ']).
:- op(750, xf,  ['THEN ... ELSE ...']).
:- op(760, fx,  ['IF']).

:- op(200, xfy, [^]).
:- op(400, yfx, [*, /, 'Div', 'Mod', 'Rem', //]).
:- op(500, yfx, [+, -, '++']).
:- op(500, fx,  [-]).
:- op(600, yfx, [\]).


%------------------------------------------------------------------------------

% turn off SICStus system messages.
portray_message(informational, _).

/*===========================================================================*/
 
top_loop(ModuleName, Prog, SymbolTable) :-
   retractall(create_tracer),
   retractall(compile_script),
   retractall(no_message),
   next_command(ModuleName, Prog, SymbolTable).

/*-----------------------------------------------------------------------------
 */

next_command(ModuleName, Prog, SymbolTable) :-
   make_prompts(ModuleName, Prompt1, Prompt2),
   ask_to_terminator(Prompt1, Prompt2, Chars),
   process_command(Chars, SymbolTable, Prog, NewProg, ModuleName,
		NewModuleName, LanguageDif),
   !,
   ( LanguageDif = yes
     -> ( NewProg = null
	  -> NewSymbolTable = null
	  ;  string2Gstring(NewModuleName, GModuleName),
	     'ParserPrograms.GoalSymbolTable.P3'(NewProg, GModuleName, NewSymbolTable)
	)
     ;  NewSymbolTable = SymbolTable
   ),
   top_loop(NewModuleName, NewProg, NewSymbolTable).

/*-----------------------------------------------------------------------------
 */

process_command(String, SymbolTable, Prog, NewProg, ModuleName, NewModuleName,
	LanguageDif):-
   get_one_item_2nd_aux(String, _Remains, Tokens),
	% redundant characters ignored
   build_command(Tokens, SymbolTable, Prog, NewProg, ModuleName,
	NewModuleName, LanguageDif).

/*-----------------------------------------------------------------------------
 */
% ignore empty command
build_command([], _, Prog, Prog, ModuleName, ModuleName, no).

build_command([Token|Tokens], SymbolTable, Prog, NewProg, ModuleName,
		NewModuleName, LanguageDif) :-
   build_command(Token, Tokens, SymbolTable, Prog, NewProg, ModuleName,
		NewModuleName, LanguageDif).


build_command(';', Tokens, _, Prog, NewProg, ModuleName,
		NewModuleName, LanguageDif) :- !,
   ( Tokens = [little_name(Name)|Tokens2]
     -> ( build_command_aux(Name, Tokens2, Command)
	  -> command_execution(Command, Prog, NewProg, ModuleName,
			NewModuleName, LanguageDif)
          ;  NewModuleName = ModuleName,
	     NewProg = Prog,
	     LanguageDif = no,
	     format(user_error, 'Unknown command, type ;h. for help.~n', [])
        )
     ;  NewModuleName = ModuleName,
	NewProg = Prog,
	LanguageDif = no,
	format(user_error, 'Incorrect command.~n', [])
   ).

build_command(Token, Tokens, SymbolTable, Prog, Prog, ModuleName, ModuleName,
	no):- 
   ( Prog = null
     -> format(user_error, '~nUnexpected query, no program loaded.~n', [])
     ;  process_query([Token|Tokens], Prog, SymbolTable)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

command_execution(load_cmd(ModuleName), _, NewProg, _, NewModuleName, yes):-
   !,
   load_cmd(ModuleName, NewProg2),
   ( NewProg2 = null
     -> NewProg = null,
        NewModuleName = ''
     ;  NewProg = NewProg2,
        NewModuleName = ModuleName
   ).

command_execution(make_and_load_cmd(ModuleName), _, NewProg, _,
		NewModuleName, yes):-
   !,
   ( string2Gstring(ModuleName, GModuleName),
     system_module_name(GModuleName)
     -> load_cmd(ModuleName, NewProg),
        NewModuleName = ModuleName
     ;  make_cmd(ModuleName)
   ),
   ( there_is_error
     -> NewProg2 = null
     ;  load_cmd(ModuleName, NewProg2)
   ),
   ( NewProg2 = null
     -> NewProg = null,
        NewModuleName = ''
     ;  NewProg = NewProg2,
        NewModuleName = ModuleName
   ).

command_execution(type_cmd(Symbol), Prog, Prog, ModuleName, ModuleName, no) :-
   !,
   ( ModuleName = ''
     -> format(user_error, '~nNo program loaded.~n', [])
     ;  string2Gstring(Symbol, GSymbol),
        'ParserPrograms.DisplayPossibleTypes.P2'(Prog, GSymbol)
   ).

command_execution(save_cmd(File, Tokens), Prog, Prog, ModName, ModName, no) :-
   !,
   ( is_runtime_system
     -> format(user_error, '~n"save" command not available in the runtime Goedel.~n', [])
     ; save(File, State),
       contiuation(State, Tokens, Prog, ModName), !
   ).

command_execution(Command, Prog, Prog, ModuleName, ModuleName, no) :-
   call(Command).

%------------------------------------------------------------------------------

contiuation(0, _, _, _).

contiuation(1, Tokens, Prog, ModName) :-
   string2Gstring(ModName, GModuleName),
   'ParserPrograms.GoalSymbolTable.P3'(Prog, GModuleName, SymbolTable),
   process_query(Tokens, Prog, SymbolTable),
   unix(exit(0)).

%------------------------------------------------------------------------------

build_command_aux(h, _, help_cmd).
build_command_aux(help, _, help_cmd).

build_command_aux(q, _, quit_cmd).
build_command_aux(quit, _, quit_cmd).

build_command_aux(p, Tokens, prolog_cmd) :-
   ( Tokens = []
     -> true
     ;  show_usage(prolog)
   ).
build_command_aux(prolog, Tokens, prolog_cmd) :-
   ( Tokens = []
     -> true
     ;  show_usage(prolog)
   ).

build_command_aux(c, Tokens, Command) :- 
   ( Tokens = [big_name(Module)]
     -> Command = compile_cmd(Module)
     ;  Command = true, show_usage(c)
   ).
build_command_aux(compile, Tokens, Command) :-
   ( Tokens = [big_name(Module)]
     -> Command = compile_cmd(Module)
     ;  Command = true, show_usage(c)
   ).

build_command_aux(l, Tokens, Command) :-
   ( Tokens = [big_name(Module)]
     -> Command = load_cmd(Module)
     ;  Command = true, show_usage(l)
   ).
build_command_aux(load, Tokens, Command) :-
   ( Tokens = [big_name(Module)]
     -> Command = load_cmd(Module)
     ;  Command = true, show_usage(l)
   ).

build_command_aux(ml, Tokens, Command) :-
   ( Tokens = [big_name(Module)]
     -> Command = make_and_load_cmd(Module)
     ;  Command = true, show_usage(ml)
   ).

build_command_aux(cs, Tokens, Command) :-
   ( Tokens = [big_name(Module)]
     -> Command = check_syntax(Module)
     ;  Command = true, show_usage(cs)
   ).

build_command_aux(m, Tokens, Command) :-
   ( Tokens = [big_name(Module)]
     -> Command = make_cmd(Module)
     ;  Command = true, show_usage(m)
   ).
build_command_aux(make, Tokens, Command) :-
   ( Tokens = [big_name(Module)]
     -> Command = make_cmd(Module)
     ;  Command = true, show_usage(m)
   ).

build_command_aux(program_compile, Tokens, Command) :-
   ( Tokens = [big_name(Module)]
     -> Command = program_compile_cmd(Module)
     ;  Command = true, show_usage(pc)
   ).
build_command_aux(pc, Tokens, Command) :-
   ( Tokens = [big_name(Module)]
     -> Command = program_compile_cmd(Module)
     ;  Command = true, show_usage(pc)
   ).

build_command_aux(program_decompile, Tokens, Command) :-
   ( Tokens = [string(File)]
     -> Command = program_decompile_cmd(File)
     ;  Command = true, show_usage(pd)
   ).
build_command_aux(pd, Tokens, Command) :-
   ( Tokens = [string(File)]
     -> Command = program_decompile_cmd(File)
     ;  Command = true, show_usage(pd)
   ).

build_command_aux(script_compile, Tokens, Command) :-
   ( Tokens = [big_name(Module)]
     -> Command = script_compile_cmd(Module)
     ;  Command = true, show_usage(sc)
   ).
build_command_aux(sc, Tokens, Command) :-
   ( Tokens = [big_name(Module)]
     -> Command = script_compile_cmd(Module)
     ;  Command = true, show_usage(sc)
   ).

build_command_aux(save, Tokens, Command) :-
   ( Tokens = [string(File)|Tokens2]
     -> Command = save_cmd(File, Tokens2)
     ;  Command = true, show_usage(save)
   ).

build_command_aux(checking, Tokens, Command) :-
   ( Tokens = []
     -> Command = checking_on
     ;  Command = true, show_usage(checking)
   ).

build_command_aux(nochecking, Tokens, Command) :-
   ( Tokens = []
     -> Command = checking_off
     ;  Command = true, show_usage(nochecking)
   ).

build_command_aux(type, Tokens, Command) :-
   ( ( Tokens = [big_name(Symbol)]; Tokens = [graphic_name(Symbol)])
     -> Command = type_cmd(Symbol)
     ;  Command = true, show_usage(t)
   ).
build_command_aux(t, Tokens, Command) :-
   ( ( Tokens = [big_name(Symbol)]; Tokens = [graphic_name(Symbol)])
     -> Command = type_cmd(Symbol)
     ;  Command = true, show_usage(t)
   ).

build_command_aux(debug_compile, Tokens, Command) :-
   ( Tokens = [big_name(Module)]
     -> Command = debug_compile_cmd(Module)
     ;  Command = true, show_usage(dc)
   ).
build_command_aux(dc, Tokens, Command) :-
   ( Tokens = [big_name(Module)]
     -> Command = debug_compile_cmd(Module)
     ;  Command = true, show_usage(dc)
   ).

build_command_aux(flock_compile, Tokens, Command) :-
   ( Tokens = [string(F1), string(F2)]
     -> Command = flock_compile_cmd(F1, F2)
     ;  Command = true, show_usage(fc)
   ).
build_command_aux(fc, Tokens, Command) :-
   ( Tokens = [string(F1), string(F2)]
     -> Command = flock_compile_cmd(F1, F2)
     ;  Command = true, show_usage(fc)
   ).

build_command_aux(flock_decompile, Tokens, Command) :-
   ( Tokens = [string(F1), string(F2)]
     -> Command = flock_decompile_cmd(F1, F2)
     ;  Command = true, show_usage(fd)
   ).
build_command_aux(fd, Tokens, Command) :-
   ( Tokens = [string(F1), string(F2)]
     -> Command = flock_decompile_cmd(F1, F2)
     ;  Command = true, show_usage(fd)
   ).

build_command_aux(cp, Tokens, Command) :-
   ( Tokens = [string(F1), string(F2) ]
     -> Command = canonicalise_prolog_cmd(F1, F2)
     ;  Command = true, show_usage(cp)
   ).

build_command_aux(dp, Tokens, Command) :-
   ( Tokens = [string(F1), string(F2) ]
     -> Command = decanonicalise_prolog_cmd(F1, F2)
     ;  Command = true, show_usage(dp)
   ).

build_command_aux(trace, Tokens, Command) :-
   ( Tokens = []
     -> Command = trace_on
     ;  Command = true, show_usage(trace)
   ).

build_command_aux(notrace, Tokens, Command) :-
   ( Tokens = []
     -> Command = trace_off
     ;  Command = true, show_usage(notrace)
   ).

build_command_aux(spy, Tokens, Command) :-
   ( ( Tokens = [big_name(Predicate)];
       Tokens = [graphic_name(Predicate)]
     )
     -> Command = spyat(Predicate)
     ;  Command = true, show_usage(spy)
   ).

build_command_aux(nospy, Tokens, Command) :-
   ( ( Tokens = [big_name(Predicate)];
       Tokens = [graphic_name(Predicate)]
     )
     -> Command = nospyat(Predicate)
     ;  Command = true, show_usage(nospy)
   ).

build_command_aux(nospyall, Tokens, Command) :-
   ( Tokens = []
     -> Command = removespy
     ;  Command = true, show_usage(nospyall)
   ).

build_command_aux(cd, Tokens, Command) :-
   ( Tokens = [string(DirString)]
     -> Command = chdir_cmd(DirString)
     ;  ( Tokens = []
	  -> Command = chdir_cmd('~ ')
	  ;  Command = true, show_usage(cd)
	)
   ).

build_command_aux(ls, Tokens, Command) :-
   ( Tokens = [string(DirString)]
     -> Command = ls_cmd(DirString)
     ;  ( Tokens = []
	  -> Command = ls_cmd('')
	  ;  Command = true, show_usage(ls)
	)
   ).

build_command_aux(more, Tokens, Command) :-
   ( Tokens = [string(FileString)]
     -> Command = more_cmd(FileString)
     ;  Command = true, show_usage(more)
   ).

build_command_aux(pwd, Tokens, Command) :-
   ( Tokens = []
     -> Command = pwd_cmd
     ;  Command = true, show_usage(pwd)
   ).

build_command_aux(shell, Tokens, Command) :-
   ( Tokens = [string(String)]
     -> Command = unix_cmd(String)
     ;  ( Tokens = []
	  -> Command = unix_cmd('')
	  ;  Command = true, show_usage(unix)
	)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

show_usage(Abbrev) :- 
   usage(Abbrev, [U|_]), 
   format(user_error, 'Syntax error, use ~s~n', U).

%------------------------------------------------------------------------------
 
make_prompts(Module, Prompt1, Prompt2) :-
   name(Module, String1),
   append("[", String1, String2),
   append(String2, "] <- ", String3),
   name(Prompt1, String3),
   length(String3, N),
   rep(N, 32, String4),
   name(Prompt2, String4).

rep(0, _, []) :- !.
rep(N, X, [X|Xs]) :-
   N > 0, N1 is N-1, rep(N1, X, Xs).

%------------------------------------------------------------------------------
 
ask_to_terminator(Prompt1, Prompt2, Result) :-
   write(user_output, Prompt1), ttyflush,
   ttyget0(C0),
   find_first_non_blank(C0, Prompt1, C1),
   ( C1 = 0'"
     -> get_one_char(C1, Prompt2, C2, in_string)
     ;  get_one_char(C1, Prompt2, C2, not_in_string)
   ),
   read_to_terminator(C1, C2, Prompt2, Result, not_in_string).


read_to_terminator(0'., C2, _, [0'.], not_in_string) :- 
   blank_char(C2), !,
   myttyskip(C2, 10).

read_to_terminator(0'%, C2, _, [0'.], not_in_string) :- 
   !,
   myttyskip(C2, 10).

read_to_terminator(0'\, C2, Prompt, [0'\, C2|Cs], in_string) :-  !,
   get_one_char(C2, Prompt, C3, in_string),
   get_one_char(C3, Prompt, C4, in_string),
   read_to_terminator(C3, C4, Prompt, Cs, in_string).

read_to_terminator(0'", C2, Prompt, [0'"|Cs], in_string) :-  !,
   get_one_char(C2, Prompt, C3, not_in_string),
   read_to_terminator(C2, C3, Prompt, Cs, not_in_string).
   
read_to_terminator(0'", C2, Prompt, [0'"|Cs], not_in_string) :-  !,
   get_one_char(C2, Prompt, C3, in_string),
   read_to_terminator(C2, C3, Prompt, Cs, in_string).
   
read_to_terminator(C1, C2, Prompt, [C1|Cs], Switch) :-
   get_one_char(C2, Prompt, C3, Switch),
   read_to_terminator(C2, C3, Prompt, Cs, Switch).

%------------------------------------------------------------------------------
% skip until C 

myttyskip(FirstChar, C) :-
   ( FirstChar = C
     -> true
     ;  ttyget0(C2),
	myttyskip(C2, C)
   ).

%------------------------------------------------------------------------------

find_first_non_blank(10, Prompt, C) :- !,
   write(user_output, Prompt), ttyflush,
   ttyget0(C1),
   find_first_non_blank(C1, Prompt, C).

find_first_non_blank(32, Prompt, C) :- !,
   ttyget0(C1),
   find_first_non_blank(C1, Prompt, C).

find_first_non_blank(9, Prompt, C) :- !,
   ttyget0(C1),
   find_first_non_blank(C1, Prompt, C).

find_first_non_blank(C, _, C).

%------------------------------------------------------------------------------

get_one_char(0'., Prompt, C, Switch) :- !,
   ttyget0(C),
   ( blank_char(C), Switch = not_in_string
     -> true
     ;  ( C = 10
	  -> write(user_output, Prompt), ttyflush
	  ;  true
	)
   ).

get_one_char(_, Prompt, C, _) :-
   ttyget0(C),
   ( C = 10
     -> write(user_output, Prompt), ttyflush
     ;  true
   ).


blank_char(9).
blank_char(10).
blank_char(32).

%------------------------------------------------------------------------------

prolog_cmd :-
   ( read(P),
     call(P)
     -> format(user_output, 'Yes~n', [])
     ;  format(user_output, 'No~n', [])
   ). 

quit_cmd :-
   unix(exit(0)).

%------------------------------------------------------------------------------

canonicalise_prolog_cmd(F1, F2) :-
   ( open(F1, read, Stream1)
     -> ( open(F2, write, Stream2)
          -> (  cp_aux(Stream1, Stream2),
	        format(user_output, '~nCanonicalised "~a" is generated in "~a".~n', [F1, F2])
	      ; format(user_error, '~nError: syntax error in file "~a", command failed.~n', [F1])
	     ), !, 
             close(Stream2)
          ;  format(user_error, '~nError: cannot open file "~a".~n', [F2])
	),
        close(Stream1)
     ;  format(user_error, '~nError: cannot open file "~a".~n', [F1])
   ).

cp_aux(Stream1, Stream2) :-
   read(Stream1, Term),
   ( Term = end_of_file
     -> true
     ;  format(Stream2, "~k.~n", [Term]),
	cp_aux(Stream1, Stream2)
   ).

%------------------------------------------------------------------------------

decanonicalise_prolog_cmd(F1, F2) :-
   ( open(F1, read, Stream1)
     -> ( open(F2, write, Stream2)
	  -> (  dcp_aux(Stream1, Stream2),
	        format(user_output, '~nDecanonicalised "~a" is generated in "~a".~n', [F1, F2])
	      ; format(user_error, '~nError: syntax error in file "~a", command failed.~n', [F1])
	     ), !,
	     close(Stream2)
          ;  format(user_error, '~nError: cannot open file "~a".~n', [F2])
	),
   	close(Stream1)
     ;  format(user_error, '~nError: cannot open file "~a".~n', [F1])
   ).


dcp_aux(Stream1, Stream2) :-
   read(Stream1, Term),
   ( Term = end_of_file
     -> true
     ;  portray_clause(Stream2, Term),
	dcp_aux(Stream1, Stream2)
   ).

%------------------------------------------------------------------------------

load_cmd(ModuleName, Program):-
   string2Gstring(ModuleName, GModuleName),
   'ParserPrograms.QuickNewProgram.P3'(GModuleName, 'ProgDefs.NormalKind.C0', Prog),
   load_cmd_aux(GModuleName, [], _, Prog, Program), !.


load_cmd_aux(GModuleName, Loaded, NewLoaded, Prog, NewProg) :-
   ( member(GModuleName, Loaded)
     -> NewLoaded = Loaded,
	NewProg = Prog
     ;  ( system_module_name(GModuleName)
	  -> system_module(GModuleName, ModuleDef, ModuleDescriptor, _),
	     Signal = ordinary
	  ;  gstring2string(GModuleName, ModuleName),
	     sappend(ModuleName, '.lng', LangFile),
	     ( open(LangFile, read, Stream)
               -> format(user_output, 'Loading module "~a" ...~n', [ModuleName]),
		  ( file_version(V),
		    read(Stream, version(V, Signal))
		    -> read(Stream, ModuleDef),
                       read(Stream, ModuleDescriptor),
                       close(Stream),
		       ( (  is_runtime_system
			  ; is_eclipse		% defined in eclipse.pl
			 )
		         -> ( file_exist(ModuleName, '.pl')
		              -> (  my_load(ModuleName)
		                  ; FileError = yes, % in case my_load fails
			            format(user_error, '~nError: failed in loading file "~a.pl".~n', [ModuleName])
		                 ), !
		              ;  FileError = yes,
		  	         format(user_error, '~nError: cannot open file "~a.pl".~n', [ModuleName])
		            )
		         ;  ( file_exist(ModuleName, '.ql')
	                      -> load(ModuleName)
		              ;  FileError = yes,
		  	         format(user_error, '~nError: cannot open file "~a.ql".~n', [ModuleName])
		            )
	               )
		    ;  format(user_error,'Error: file "~a" is out of date.~n',
						[LangFile]),
                       close(Stream),
               	       FileError = yes
		  )
               ;  FileError = yes,
	          format(user_error, '~nError: cannot open file "~a".~n', [LangFile])
             )
	),
	( var(FileError)
	  -> ( Signal = script
               -> NewProg = 'ProgDefs.Program.F4'(GModuleName, ModuleDef,
	                        ModuleDescriptor, Empty),
                  'AVLTrees.AVLIsEmpty.P1'(Empty)  
               ;  'ParserPrograms.QuickInsertModuleDef.P4'(Prog, GModuleName,
				ModuleDef, Prog2),
	          'SharedPrograms.ProgramLanguage.P2'(Prog2, Lang),
	          'ParserPrograms.QuickInsertModuleDescriptor.P4'(Lang,
		           GModuleName,	ModuleDescriptor, Language),
	          'ParserPrograms.ReplaceProgramLanguage.P3'(Prog2, Language,
                           Prog3),
	          ModuleDef = 'ProgDefs.ModDef.F4'(_, ImpExp, ImpLoc, Lifts),
	          append(ImpLoc, ImpExp, ImportedMods1),
	          append(Lifts, ImportedMods1, ImportedMods),
	          load_cmd_aux2(ImportedMods, [GModuleName|Loaded], NewLoaded,
				Prog3, NewProg)
	     )
	  ;  NewProg = null
	)
   ).


load_cmd_aux2([], Loaded, Loaded, Prog, Prog).
load_cmd_aux2([GModuleName|ImportedMods], Loaded, NewLoaded, Prog, NewProg) :-
   load_cmd_aux(GModuleName, Loaded, Loaded2, Prog, Prog2),
   load_cmd_aux2(ImportedMods, Loaded2, NewLoaded, Prog2, NewProg).

/*-----------------------------------------------------------------------------
 * This is the routine for Program.NewProgram, very similar to load_cmd
 * Prolog code is not loaded
 */

load_system_module(ModuleName, Program):-
   string2Gstring(ModuleName, GModuleName),
   'SharedPrograms.EmptyLanguage.P1'(Language),
   'AVLTrees.AVLIsEmpty.P1'(EmptyAVL),
   Prog = 'ProgDefs.Program.F4'(GModuleName, EmptyAVL, Language, EmptyAVL),
   load_system_module_aux(GModuleName, [], _, Prog, Program), !.


load_system_module_aux(GModuleName, Loaded, NewLoaded, Prog, NewProg) :-
   ( member(GModuleName, Loaded)
     -> NewLoaded = Loaded,
	NewProg = Prog
     ;  system_module(GModuleName, ModuleDef, ModuleDescriptor, ModuleCode),
	'ParserPrograms.QuickInsertModuleDef.P4'(Prog, GModuleName, ModuleDef, Prog2),
	'SharedPrograms.ProgramLanguage.P2'(Prog2, Lang),
	'ParserPrograms.QuickInsertModuleDescriptor.P4'(Lang, GModuleName,
				ModuleDescriptor, Language),
	'ParserPrograms.ReplaceProgramLanguage.P3'(Prog2, Language, Prog3),
	'ParserPrograms.QuickInsertModuleCode.P4'(Prog3, GModuleName, ModuleCode,
				Prog4),
	ModuleDef = 'ProgDefs.ModDef.F4'(_, ImpExp, ImpLoc, Lifts),
	append(ImpLoc, ImpExp, ImportedMods1),
	append(Lifts, ImportedMods1, ImportedMods),
	load_system_module_aux2(ImportedMods, [GModuleName|Loaded], NewLoaded,
					Prog4, NewProg)
   ).


load_system_module_aux2([], Loaded, Loaded, Prog, Prog).
load_system_module_aux2([GMod|ImportedMods], Loaded, NewLoaded, Prog, NewProg):-
   load_system_module_aux(GMod, Loaded, Loaded2, Prog, Prog2),
   load_system_module_aux2(ImportedMods, Loaded2, NewLoaded, Prog2, NewProg).

%------------------------------------------------------------------------------

file_exist(Name, Postfix) :-
   sappend(Name, Postfix, File),
   sappend('test -f ', File, Command),
   unix(system(Command, 0)).

%------------------------------------------------------------------------------
% This is for the PC/DOS version of Goedel
% file_exist(Name, Postfix) :-
%   sappend(Name, Postfix, File),
%   open(File, read, Stream),
%   close(Stream).

%------------------------------------------------------------------------------

program_compile_cmd(ModuleName) :-
   string2Gstring(ModuleName, GModuleName),
   ( system_module_name(GModuleName) 
     -> load_system_module(ModuleName, Program),
        create_prm_file(ModuleName, Program)
     ;  retractall(there_is_error),
        parse_program(ModuleName, make, Program), !,
        ( there_is_error
          -> format(user_output, 'Ground representation of program "~a" NOT generated.~n', [ModuleName])
          ;  create_prm_file(ModuleName, Program)
	)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

create_prm_file(ModuleName, Program) :-
   sappend(ModuleName, '.prm', FileName),
   ( open(FileName, write, Stream)
     -> file_version(V),
	format(Stream, 'version(''~a'', prm).~n', [V]),   % inserting version number
	create_prm_file_aux(Program, Stream),
	close(Stream),
	format(user_output, 'Ground representation of program "~a" is in file "~a".~n', [ModuleName, FileName])
     ;  format(user_error, '~nError: cannot open file "~a".~n', [FileName]) 
   ).

create_prm_file_aux(Program, Stream) :-
   Program = 'ProgDefs.Program.F4'(MainModuleName, ModuleDefAVL, Language,
					   StatementsAndDelays),
   Language = 'ProgDefs.Language.F1'(ModuleDescriptorAVL),
   write_canonical(Stream, MainModuleName),
   format(Stream, '.~n', []),
   'AVLTrees.AVLToBinary.P2'(ModuleDefAVL, ModuleDefOBT),
   traverse_OBT(ModuleDefOBT, ModuleDescriptorAVL, StatementsAndDelays, Stream).

traverse_OBT('AVLTrees.Empty.C0', _, _, _).
traverse_OBT('AVLTrees.Tree.F4'(Left, ModuleName, Item, Right),
	ModuleDescriptorAVL, StatementsAndDelays, Stream) :-
   ( system_module_name(ModuleName)
     -> write_canonical(Stream, import(ModuleName)),
        format(Stream, '.~n', [])
     ;  write_canonical(Stream, module_name(ModuleName)),
        format(Stream, '.~n', []),
        write_canonical(Stream, Item),
        format(Stream, '.~n', []),
	'AVLTrees.AVLSearch.P3'(ModuleDescriptorAVL, ModuleName,
			ModuleDescriptor),
        write_canonical(Stream, ModuleDescriptor),
        format(Stream, '.~n', []),
	'AVLTrees.AVLSearch.P3'(StatementsAndDelays, ModuleName
           , 'ProgDefs.Code.F2'(_, Code)),
        write_canonical(Stream, Code),
        format(Stream, '.~n', [])
   ),
   traverse_OBT(Left, ModuleDescriptorAVL, StatementsAndDelays, Stream),
   traverse_OBT(Right, ModuleDescriptorAVL, StatementsAndDelays, Stream).
   
%------------------------------------------------------------------------------

program_decompile_cmd(GFileName) :-
   gstring2string(GFileName, File),
   postfix_handler(File, '.prm', FileName),
   ( open(FileName, read, Stream)
     -> readin_program(Stream, Program),
	close(Stream),
        'ParserPrograms.Decompile.P2'(Program, 'ParserPrograms.Noisy.C0')
     ;  format(user_error, '~nError: cannot open file "~a".~n', [FileName])
   ), !.

%------------------------------------------------------------------------------

script_compile_cmd(GModuleName) :-
   gstring2string(GModuleName, ModuleName),
   sappend(ModuleName, '.scr', FileName),
   ( open(FileName, read, Stream)
     -> 'ScriptsIO':readin_script(Stream, Script),
	close(Stream),
	compile_script(Script, ModuleName)
     ;  format(user_error, '~nError: cannot open file "~a".~n', [FileName])
   ), !.


%------------------------------------------------------------------------------

compile_cmd(ModuleName) :-
   retractall(there_is_error),
   parse_program(ModuleName, compile, Program), !,
   ( there_is_error
     -> true
     ;  compile_program(Program, ModuleName)
   ).

%------------------------------------------------------------------------------

check_syntax(ModuleName) :-
   retractall(there_is_error),
   parse_program(ModuleName, compile, _), !.

%------------------------------------------------------------------------------

make_cmd(ModuleName) :-
   retractall(there_is_error),
   parse_program(ModuleName, make, Program), !,
   ( there_is_error
     -> true
     ;  make_program(Program)
   ).

%------------------------------------------------------------------------------

debug_compile_cmd(ModuleName) :-
   retractall(there_is_error),
   assert(create_tracer),
   parse_program(ModuleName, compile, Program), !,
   ( there_is_error
     -> true
     ;  compile_program(Program, ModuleName)
   ),
   retractall(create_tracer).

%------------------------------------------------------------------------------

flock_compile_cmd(F1, F2) :-
   ( open(F1, read, Stream)
     -> read_file(Stream, Chars),
        close(Stream),
        (  'Units':token_identifiers(Chars, Tokens, []),
           flock_compile_cmd_aux(Tokens, FlockList),
           sappend(F2, '.flk', FileName),
           ( open(FileName, write, Stream2)
	     -> format(Stream2, '~q.~n', ['Flocks.Flock.F1'(FlockList)]),
	        format(user_output, '~nUnits in "~a" are compiled into flock "~a".~n', [F1, FileName]),
	        close(Stream2)
             ;  format(user_error, '~nError: cannot open file "~a".~n', [FileName])
	   )
	 ; format(user_error, '~nError: syntax error in file "~a", command failed.~n', [F1])
	), !
     ;  format(user_error, '~nError: cannot open file "~a".~n', [F1])
   ).


flock_compile_cmd_aux(Tokens, Flock) :-
   'Units':parse_unit(Tokens, Remains, Unit),
   ( Unit = null
     -> Flock = []
     ;  Flock = [Unit|Flock2],
        ( ( Remains = []; Remains = ['.'] )
          -> Flock2 = []
          ;  ( Remains = ['.'|Remains2]
               -> flock_compile_cmd_aux(Remains2, Flock2)
	       ;  fail			% syntax error
	     )
        )
   ).

read_file(Stream, Chars) :-
   get0(Stream, Char),
   ( Char = -1
     -> Chars = []
     ;  Chars = [Char|Chars2],
        read_file(Stream, Chars2)
   ).

%------------------------------------------------------------------------------

flock_decompile_cmd(F1, F2) :-
   postfix_handler(F1, '.flk', FF1),
   ( open(FF1, read, Stream)
     -> read(Stream, 'Flocks.Flock.F1'(List)),
	close(Stream),
        ( open(F2, write, Stream2)
	  -> (  flock_decompile_cmd_aux(List, Stream2),
	        format(user_output, '~nThe flock in "~a" is decompiled into units in "~a".~n', [FF1, F2])
	      ; format(user_error, '~nError: syntax error in file "~a", command failed.~n', [FF1])
	     ), !,
	     close(Stream2)
          ;  format(user_error, '~nError: cannot open file "~a".~n', [F2])
	)
     ;  format(user_error, '~nError: cannot open file "~a".~n', [FF1])
   ).

flock_decompile_cmd_aux([], _).
flock_decompile_cmd_aux([Unit|List], Stream) :-
   'Units':'Units.UnitToString.P2'(Unit, GString),
   gstring2string(GString, String),
   format(Stream, '~w.~n', [String]),
   flock_decompile_cmd_aux(List, Stream).

%------------------------------------------------------------------------------

postfix_handler(File, Postfix, Result) :-
   ( name(File, List),
     last_n_elements(List, 4, List2),
     name(Postfix, List2)
   ;
     sappend(File, Postfix, Result)
   ).

%------------------------------------------------------------------------------

trace_on :-
   format(user_output, 'Tracing on~n', []),
   assert(trace_is_on).

trace_off :-
   format(user_output, 'Tracing off~n', []),
   retractall(trace_is_on).

%------------------------------------------------------------------------------

checking_on :-
   format(user_output, 'All checking on~n', []),
   retractall(checking_is_off).

checking_off :-
   format(user_output, 'All checking off~n', []),
   assert(checking_is_off). 

%------------------------------------------------------------------------------

spyat(Predicate) :-
   format(user_output, 'Spy points set at all predicates with name "~w"~n',
		[Predicate]),
   assert(spied(Predicate)).

nospyat(Predicate) :-
   format(user_output, 'Spy points set at predicates with name "~w" removed~n',
		[Predicate]),
   retractall(spied(Predicate)).

removespy :-
   format(user_output, 'All spy points removed~n', []),
   retractall(spied(_)).

/*-----------------------------------------------------------------------------
 * help_cmd.
 */

help_cmd :-
   format(user_output, 'Goedel commands:~n~n', []),
   list_of_commands(L),
   help_cmd_aux(L),
   nl.

help_cmd_aux([]).
help_cmd_aux([H|T]) :-
   display_usage(H),
   help_cmd_aux(T).

list_of_commands([h, c, ml, l, m, dc, cs, pc, pd, sc, fc, fd, cp, dp, save,
	checking, nochecking, t, trace, notrace, spy, nospy, nospyall,
	cd, ls, pwd, more, shell, q]).

usage(h, [";help.", "(;h)", "This message"]).
usage(q, [";quit.", "(;q)", "Quit Goedel"]).
usage(ml, [";ml Module.", "", "Compile and load the module and its dependent modules"]).
usage(c, [";compile Module.", "(;c)", "Compile a Goedel module"]).
usage(l, [";load Module.", "(;l)", "Load compiled Goedel modules"]).
usage(m, [";make Module.", "(;m)", "Compile the module and its dependent modules"]).
usage(cs, [";cs Module.", "", "Check the syntax of a module"]).
usage(pc, [";pc Module.", "", "Program-compile a Goedel program"]).
usage(pd, [";pd ""File"".", "", "Program-decompile a Goedel program"]).
usage(sc, [";sc Module.", "", "Script-compile a Goedel script"]).
usage(fc, [";fc ""File"" ""Flock"".", "", "Flock-compile a flock to its representation"]).
usage(fd, [";fd ""Flock"" ""File"".", "", "Flock-decompile a flock representation to a flock"]).
usage(cp, [";cp ""File1"" ""File2"".", "", "Canonicalise Prolog program/terms"]).
usage(dp, [";dp ""File1"" ""File2"".", "", "Decanonicalise Prolog program/terms"]).
usage(dc, [";dc Module.", "", "Compile a Goedel program with the tracer on"]).
usage(t, [";type Symbol.", "(;t)", "Show the possible type of Symbol in the loaded module"]).
usage(save, [";save File Goal.", "", "Save the state into File to be restored to run Goal"]).
usage(checking, [";checking.", "", "Switch on various checkings (default)"]).
usage(nochecking, [";nochecking.", "", "Switch off all the checkings"]).
usage(trace, [";trace.", "", "Switch on the tracer"]).
usage(notrace, [";notrace.", "", "Switch off the tracer"]).
usage(spy, [";spy Predicate.", "", "Set a spy point at Predicate"]).
usage(nospy, [";nospy Predicate.", "", "Remove the spy point set at Predicate"]).
usage(nospyall, [";nospyall.", "", "Remove all spy points"]).
usage(prolog, [";prolog.", "(;p)", "Enter a Prolog query"]).
usage(cd, [";cd {""Directory""}.", "", "Change directory"]).
usage(ls, [";ls {""Directory""}.", "", "Directory listing"]).
usage(pwd, [";pwd.", "", "Display current directory"]).
usage(more, [";more ""File"".", "", "Display file"]).
usage(shell, [";shell {""Command""}.", "", "Enter shell or execute an operating system command"]).

display_usage(C) :-
   usage(C, Strings),
   format(user_output, '~20s~4s - ~s~n', Strings).

%------------------------------------------------------------------------------
 
pwd_cmd :-
   unix(system(pwd)).

%------------------------------------------------------------------------------

chdir_cmd(String) :-
   ( unix(cd(String))
     -> true
     ;  format(user_error, 'Fail to change to directory "~a".~n', [String])
   ).

%------------------------------------------------------------------------------

ls_cmd(String) :-
   sappend('ls ', String, UnixComm),
   ( unix(system(UnixComm))
     -> true
     ;  format(user_error, 'Fail to list directory ~a.~n', [String])
   ).

%------------------------------------------------------------------------------

more_cmd(String) :-
   sappend('more ', String, UnixComm),
   ( unix(system(UnixComm))
     -> true
     ;  format(user_error, 'Failed to display file ~a.~n', [String])
   ).

%------------------------------------------------------------------------------

unix_cmd(String) :-
   ( String = ''
     -> format(user_output, 'Entering shell, exit shell to get back to Goedel.~n', []),
        unix(system('set ignoreeof')),  % this doesn't really work, but
					% the problem disappears now.
        unix(shell)
     ;  ( unix(system(String))
          -> true
          ;  format(user_error, 'Failed to execute command ~a.~n', [String])
	)
   ).

%------------------------------------------------------------------------------

process_query(Tokens, Program, SymbolTable) :-
   colon_notation(Tokens, Remains, ColonVars, Error),
   ( var(Error)
     -> body(Remains, Formulae, ErrorReturn, SymbolTable),
        ( Formulae = []
     	  -> print_error_return(ErrorReturn, Tokens, null, 0, 0)
          ;  'SharedPrograms.ProgramLanguage.P2'(Program, ProgLanguage),
	     'ParserPrograms.ParserCheckFormula.P4'(Formulae, ProgLanguage,
	     				Goal, ErrorMessages),
             ( ErrorMessages = []
               -> floundering_checking_goal(Goal, Error),
	          ( Error = yes
		    -> true
		    ;  compile_goal(Goal, BuiltGoal, FreeVars, VarDict),
	               ( ColonVars = null
                         -> run_query(BuiltGoal, FreeVars, VarDict, Program)
	                 ;  set_minus(ColonVars, FreeVars, NotUsedVars),
		            ( NotUsedVars = []	 
	                      -> run_query(BuiltGoal,ColonVars,VarDict,Program)
		              ;  print_out_error('Error: variable(s) ~w before ":" do not occur in the goal',
					[NotUsedVars], null, 0, 0, [], null)
		            )
		       )
	          )
	       ;  print_type_error(ErrorMessages, null, 0, 0)
	     )
	)
     ;  true
   ).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * colon_notation(+Tokens, -Remains, -ColonVars, -Error)
 * when there is no colon notation ColonVars = null
 *	Error not instantiated shows no error
 */

colon_notation(Tokens, Remains, ColonVars, Error) :-
   split_list(Tokens, graphic_name(':'), VarTokens, Remains2),
   ( member('{', VarTokens)
     -> Remains = Tokens,
	ColonVars = null
     ;  ( Remains2 = []
     	  -> Remains = Tokens,
	     ColonVars = null
          ;  Remains = Remains2,
	     length(Remains, RestPosition),
	     var_seq(VarTokens, Tokens, RestPosition+1, ColonVars, Error)
	)
   ).

var_seq([], _, _, [], _).
var_seq([Token|VarTokens], WholeTokens, RestPosition, ColonVars, Error) :-
   var_seq(Token, VarTokens, WholeTokens, RestPosition, ColonVars, Error).

var_seq(little_name(VarName), VarTokens, WholeTokens, RestPosition,
		[ColonVar|ColonVars], Error) :- !,
   string2Gstring(VarName, GVarName),
   user:'SharedSyntax.SVariableName.P3'(ColonVar, GVarName, 0),
   ( VarTokens = []
     -> ColonVars = []
     ;  ( VarTokens = [','|VarTokens2]
	  -> var_seq(VarTokens2, WholeTokens, RestPosition, ColonVars, Error)
	  ;  Error = yes,
	     length(VarTokens, L),
	     Position is L + RestPosition,
	     print_out_error('Error: "," expected', [], null, 0, 0, WholeTokens,
				Position)
	)
   ).

var_seq('_'(_), VarTokens, WholeTokens, RestPosition, _, yes) :- !,
   length(VarTokens, L),
   Position is L + RestPosition,
   print_out_error('Error: underscore variable not allowed before ":"', [],
		null, 0, 0, WholeTokens, Position).

var_seq(Token, VarTokens, WholeTokens, RestPosition, _, yes) :- !,
   length(VarTokens, L),
   Position is L + RestPosition,
   print_out_error('Error: illegal symbol "~w" before ":"', [Token], null, 0, 0,
		WholeTokens, Position).

%------------------------------------------------------------------------------

run_query(Query, FreeVars, VarDict, Prog) :-
   retractall(tracer_counter(_)),
   assert(tracer_counter(0)),
   retractall(no_trace),
   retractall(skip_trace(_)),
   retractall(free_trace),
   retractall(start_trace),
   retractall(stop_trace),
   on_exception(catch_in_query,
	( call_residue(Query, Suspended),
	  ( Suspended == []
	    -> ( FreeVars == []
		 -> true
		 ;  display_answer(FreeVars, VarDict, Prog),
		    user_satisfied
	       ),
	       format(user_output, 'Yes~n', [])
	    ;  format(user_error, 'Floundered.  Unsolved goals are:~n', []),
	       process_unsolved_goals(Suspended),
	       raise_exception(catch_in_query)
          )
	), true),
   !.

run_query(_, _, _, _) :-
   format(user_output, 'No~n', []).

%------------------------------------------------------------------------------

process_unsolved_goals(Suspended):-
   deref_suspension_chain(Suspended, Goals),
   convert_formulas(Goals, GGoals),
   pv_to_gv(GGoals, dict([], 1), _, RGoals),
   print_unsolved_goals(RGoals).

%------------------------------------------------------------------------------
% the output of call_residue differs from its specification in the manual

deref_suspension_chain(Suspended, Goals) :-
   clean_suspension_chain(Suspended, VarPairs, ProperGoals),
   dereference_vars(VarPairs, ProperGoals, Goals).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

clean_suspension_chain([], [], []).
clean_suspension_chain([V-Goal|Rest], VarPairs, ProperGoals):-
   ( suspension_debris(Goal, V2)
     -> VarPairs = [V-V2|VarPairs2],
        ProperGoals = ProperGoals2
     ;  VarPairs = VarPairs2,                         
        ProperGoals = [V-Goal|ProperGoals2]
   ),
   clean_suspension_chain(Rest, VarPairs2, ProperGoals2).


suspension_debris(freeze_on_vlist(_, _, _, V), V).
suspension_debris(freeze_nonvar_and(_, _, V), V).
suspension_debris(evaluate_delay_aux(_, V), V).
suspension_debris(call(evaluate_delay_aux(_, V)), V) :- !.
suspension_debris(call(V=1), V).
suspension_debris(_:Goal, V) :-
   suspension_debris(Goal, V).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

dereference_vars([], Goals, Goals).
dereference_vars([V-V2|VarPairs], ProperGoals, Goals) :-
   replace_var_name(ProperGoals, V2, V, ProperGoals2), 
   dereference_vars(VarPairs, ProperGoals2, Goals).


replace_var_name([], _, _, []). 
replace_var_name([Var-Goal|Goals], V2, V, NewGoals) :-
   ( Var == V2
     -> NewGoals = [V-Goal|NewGoals2]
     ;  NewGoals = [Var-Goal|NewGoals2]
   ),
   replace_var_name(Goals, V2, V, NewGoals2).

%------------------------------------------------------------------------------


print_unsolved_goals([]).
print_unsolved_goals([V-Goal|Goals]) :-
   format(user_error, "Goal: ~w~nDelayed on: ~w~n~n", [Goal, V]),
   print_unsolved_goals(Goals).
   
convert_formulas([], []).
convert_formulas([V-Goal|Goals], [V-GGoal|GGoals]) :-
   convert_formula(Goal, GGoal),
   convert_formulas(Goals, GGoals).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

pv_to_gv(Var, dict(Dict, Count), dict(Dict2, Count2), RV) :-
   var(Var), !,
   ( search_var_dict(Dict, Var, RV)
     -> Dict2 = Dict,
        Count2 = Count
     ;  name(Count, L),
        name(RV, [0'v, 0'_|L]),
	Dict2 = [pair(Var, RV)|Dict],
	Count2 is Count + 1
   ).

pv_to_gv([], Dict, Dict, []):- !.
pv_to_gv([H|T], Dict, Dict2, [H2|T2]) :- !,
   pv_to_gv(H, Dict, Dict3, H2),
   pv_to_gv(T, Dict3, Dict2, T2).

pv_to_gv(Atom, Dict, Dict2, Atom2) :-
   Atom =.. [F|Argus],
   pv_to_gv(Argus, Dict, Dict2, Argus2),
   Atom2 =.. [F|Argus2].


search_var_dict([pair(V, RV)|_], Var, RV) :-
   V == Var, !.
search_var_dict([_|Dict], Var, RV) :-
   search_var_dict(Dict, Var, RV).

%------------------------------------------------------------------------------

convert_formula(Goal, GGoal) :-
   convert_to_goedel_formula(Goal, GFormula),
   remove_true(GFormula, GGoal).

   
remove_true(true ' & ' Goal, GGoal) :- !,
   remove_true(Goal, GGoal).
remove_true(Goal ' & ' true, GGoal) :- !,
   remove_true(Goal, GGoal).
remove_true(Goal ' \/ ' true, GGoal) :- !,
   remove_true(Goal, GGoal).
remove_true(true ' \/ ' Goal, GGoal) :- !,
   remove_true(Goal, GGoal).
remove_true('~ ' Goal, '~ ' GGoal) :- !,
   remove_true(Goal, GGoal).
remove_true('IF' Goal 'THEN ... ELSE ...', 'IF' GGoal 'THEN ... ELSE ...') :- !,
   remove_true(Goal, GGoal).

% when "true" is the whole formula, it slips through.
remove_true(Goal, Goal).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% quantifiers can not be recovered from the suspended goals
% it's not possible for cuts to appear in a suspended goal

convert_to_goedel_formula((Formula1, Formula2), (GFormula1 ' & ' GFormula2)) :-
   convert_to_goedel_formula(Formula1, GFormula1),
   convert_to_goedel_formula(Formula2, GFormula2).

convert_to_goedel_formula(if(Cond, _Then, _Else),
				('IF' GCond 'THEN ... ELSE ...')) :-
   convert_to_goedel_formula(Cond, GCond).

convert_to_goedel_formula((Formula1; Formula2), GFormula) :-
   convert_to_goedel_formula(Formula2, GFormula2),
   ( Formula1 = (Cond -> _Then)
     -> GFormula = ('IF' GCond 'THEN ... ELSE ...'),
        convert_to_goedel_formula(Cond, GCond)
     ;  GFormula = (GFormula1 ' \/ ' GFormula2),
        convert_to_goedel_formula(Formula1, GFormula1)
   ).

convert_to_goedel_formula(trace_call(_, _, _), true).
convert_to_goedel_formula(trace_exit(_, _, _), true).
convert_to_goedel_formula(trace_suspension(_, _, _, _), true).
convert_to_goedel_formula(trace_wakening(_, _, _), true).
convert_to_goedel_formula(fail, 'False').
convert_to_goedel_formula(true, true).

convert_to_goedel_formula(Term1 = Term2, GTerm1 = GTerm2) :-
   convert_arg_list([Term1, Term2], [GTerm1, GTerm2], 5).
   	% restricted to depth 5

convert_to_goedel_formula(Term1 >= Term2, GTerm1 >= GTerm2) :-
   convert_arg_list([Term1, Term2], [GTerm1, GTerm2], 5).
   	% restricted to depth 5

convert_to_goedel_formula(Term1 =< Term2, GTerm1 =< GTerm2) :-
   convert_arg_list([Term1, Term2], [GTerm1, GTerm2], 5).
   	% restricted to depth 5

convert_to_goedel_formula(Term1 < Term2, GTerm1 < GTerm2) :-
   convert_arg_list([Term1, Term2], [GTerm1, GTerm2], 5).
   	% restricted to depth 5

convert_to_goedel_formula(Term1 > Term2, GTerm1 > GTerm2) :-
   convert_arg_list([Term1, Term2], [GTerm1, GTerm2], 5).
   	% restricted to depth 5

convert_to_goedel_formula(dif(Term1, Term2), '~='(GTerm1, GTerm2)) :-
   convert_arg_list([Term1, Term2], [GTerm1, GTerm2], 5).
   	% restricted to depth 5

convert_to_goedel_formula(not_equal(_, _, Term1, Term2),
					'~='(GTerm1, GTerm2)) :-
   convert_arg_list([Term1, Term2], [GTerm1, GTerm2], 5).
   	% restricted to depth 5

convert_to_goedel_formula(not_equal(_, Term1, Term2), '~='(GTerm1, GTerm2)) :-
   convert_arg_list([Term1, Term2], [GTerm1, GTerm2], 5).
   	% restricted to depth 5

convert_to_goedel_formula(goedel_not(Formula), '~ '(GFormula)) :-
   convert_to_goedel_formula(Formula, GFormula).      
   
convert_to_goedel_formula(goedel_freeze(_, Formula), GFormula) :-
   convert_to_goedel_formula(Formula, GFormula).      

% for Sets
convert_to_goedel_formula(sort(Term1, Term2), '='(GTerm2, SetTerm)) :-
   convert_set_terms([Term1, Term2], [GTerm2, SetTerm]).

% for Sets
convert_to_goedel_formula(union(Term1, Term2, Term3),
				'='(GTerm3, +(GTerm1, GTerm2))) :-
   convert_set_terms([Term1, Term2, Term3], [GTerm1, GTerm2, GTerm3]).

% for Sets
convert_to_goedel_formula(difference(Term1, Term2, Term3),
				'='(GTerm3, '\'(GTerm1, GTerm2))) :-
   convert_set_terms([Term1, Term2, Term3], [GTerm1, GTerm2, GTerm3]). 

% for Sets
convert_to_goedel_formula(intersection(Term1, Term2, Term3),
				'='(GTerm3, *(GTerm1, GTerm2))) :-
   convert_set_terms([Term1, Term2, Term3], [GTerm1, GTerm2, GTerm3]). 


convert_to_goedel_formula(Module:Formula, GFormula) :-
   ( ( Module = user; Module = prolog)
     -> convert_to_goedel_formula(Formula, GFormula)
     ;  Formula =.. [Predicate|Argus],
	( constraint_goal_conversion(Predicate, Module, Op, Arity)
	  -> ( Op = 'SuchThat'		% to support intensional sets.
	       -> Argus = [T, W, X],
	          convert_arg_list([T], [GT], 5),
		  convert_to_goedel_formula(W, GW),
		  GFormula =.. [=, X, '{}'(GT:GW)]
               ;  convert_arg_list(Argus, GArgus, 5),               
                  ( Op = '='		% mainly to support "eval" in Rationals
	            -> GFormula =.. [=|GArgus]
	            ;  first_n_elements(GArgus, Arity, Front, [Rear]),
   				% there is only one arg left
	               Constraint =.. [Op|Front],     
                       GFormula =.. [=, Rear, Constraint]
	          )
	     )
          ;  convert_arg_list(Argus, GArgus, 5),	% restricted to depth 5
	     make_flat_name('MetaDefs.Name.F4'(GModule, GSymbol, _, _),
				Predicate, _, _),
	     gstring2string(GModule, M),
	     gstring2string(GSymbol, Symbol),
	     GPredicate =.. [Symbol|GArgus],
	     GFormula = M:GPredicate
   	)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

convert_set_terms(Terms, GTerms) :-
   convert_arg_list(Terms, GTerms2, 5),
   convert_set_terms_aux(GTerms2, GTerms).

convert_set_terms_aux([], []).
convert_set_terms_aux([H|T], [GH|GT]) :-
   ( var(H)
     -> GH = H
     ;  convert_set(H, GH, 5)   	% restricted to depth 5
   ),
   convert_set_terms_aux(T, GT).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% this is a special version created for displaying floundered goals
constraint_goal_conversion(negative, _, '-', 1).
constraint_goal_conversion(minus, _, '-', 2).
constraint_goal_conversion(plus, _, '+', 2).
constraint_goal_conversion(times, _, '*', 2).
constraint_goal_conversion(divides, 'Integers', 'Div', 2).
constraint_goal_conversion(divides, 'Rationals', '/', 2).
constraint_goal_conversion(power, _, '^', 2).
constraint_goal_conversion(mod, _, 'Mod', 2).
constraint_goal_conversion(rem, 'Integers', 'Rem', 2).
constraint_goal_conversion(sign, _, 'Sign', 1).
constraint_goal_conversion(maximum, _, 'Max', 2).
constraint_goal_conversion(minimum, _, 'Min', 2).
constraint_goal_conversion(absolute, _, 'Abs', 1).
constraint_goal_conversion(rational, 'Rationals', '//', 2).
constraint_goal_conversion(eval, 'Rationals', =, 2).

constraint_goal_conversion(normalise, 'Sets', 'Inc', 2).	% Wrong!
constraint_goal_conversion(union, 'Sets', '+', 2).          
constraint_goal_conversion(difference, 'Sets', '\', 2).          
constraint_goal_conversion(intersection, 'Sets', '*', 2).          
constraint_goal_conversion(set_of_aux, 'Sets', 'SuchThat', 3).

constraint_goal_conversion(concat, 'Strings', '++', 2).

%------------------------------------------------------------------------------
 
display_answer(FreeVars, VarDict, Prog) :-
   display_answer(FreeVars, VarDict, Prog, 1).

display_answer([Var|Vars], VarDict, Prog, VarNumber) :-
   member(Var/Value, VarDict), !,
   nl, display_binding(Var, Value, Prog, VarNumber, NewVarNumber),
   display_answer1(Vars, VarDict, Prog, NewVarNumber).

display_answer1([], _, _, _).

display_answer1([Var|Vars], VarDict, Prog, VarNumber) :-
   member(Var/Value, VarDict), !,
   format(user_output, ',~n', []),
   display_binding(Var, Value, Prog, VarNumber, NewVarNumber),
   display_answer1(Vars, VarDict, Prog, NewVarNumber).
 
%------------------------------------------------------------------------------

display_binding(Var, PVal, Program, VarNumber, NewVarNumber) :-
   Program = 'ProgDefs.Program.F4'(Module, _, _, _),
   pobj_to_ground(PVal, VarNumber, NewVarNumber, Ground),
   'SharedPrograms.TermToIntList.P4'(Program, Module, Var, VarCodes),
   'SharedPrograms.TermToIntList.P4'(Program, Module, Ground, ValCodes),
   write_codes(VarCodes), 
   write(user_output, ' = '),
   write_codes(ValCodes).

write_codes([]).
write_codes([C|Cs]) :-
   put(user_output, C),
   write_codes(Cs).

/*------------------------------------------------------------------------------
 * user_satisfied
 */

user_satisfied:-
   format(user_output, ' ? ', []), ttyflush,
   ttyget0(C),
   ( C = 0';
     -> ttyskip(10), fail
     ;  ( C = 10
	  -> true
   	  ;  ttyskip(10),
   	     format(user_output, '~n";" for more choices, otherwise <return>',
			[]),
	     user_satisfied
	)
   ).

%------------------------------------------------------------------------------
% Converts Goedel Prolog-object-code terms to Ground representation
%------------------------------------------------------------------------------

% Used for printing answer substitutions at the top level.

pobj_to_ground(Term, VarNo, NewVarNo, Ground) :-
   ( var(Term) -> 
     Term = '$$variable'(VarNo), 
     NewVarNo is VarNo + 1,
     Ground = 'MetaDefs.Var.F1'(VarNo)
   ; Term = '$$variable'(N) ->
     NewVarNo is VarNo,
     Ground = 'MetaDefs.Var.F1'(N)
   ; gstring(Term) ->
     Ground = 'MetaDefs.Str.F1'(Term),
     NewVarNo is VarNo
   ; integer(Term) ->
     Ground = 'MetaDefs.Int.F1'(Term),
     NewVarNo is VarNo
   ; Term = N // M ->
     Ground = 'MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Rationals', '"//'
        , 'MetaDefs.Function.C0', 2), ['MetaDefs.Int.F1'(N), 'MetaDefs.Int.F1'(M)]),
     NewVarNo is VarNo
   ; Term = 'ProgDefs.CachedProgram.F1'(String) ->
     Ground = 'MetaDefs.Prm.F1'(String),
     NewVarNo is VarNo
   ; Term = [] ->
     Ground = 'MetaDefs.CTerm.F1'('MetaDefs.Name.F4'('"Lists'
         , '"Nil', 'MetaDefs.Constant.C0', 0)),
     NewVarNo is VarNo   
   ; Term = [Head|Tail] ->
     Ground = 'MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Lists'
         , '"Cons', 'MetaDefs.Function.C0', 2), [GHead, GTail]),
     pobj_to_ground(Head, VarNo, VarNo1, GHead),
     pobj_to_ground(Tail, VarNo1, NewVarNo, GTail)
   ; Term = set(Set) ->
     ( Set = [] ->
       Ground = 'MetaDefs.CTerm.F1'('MetaDefs.Name.F4'('"Sets'
         , '"Null', 'MetaDefs.Constant.C0', 0)),
       NewVarNo is VarNo
     ; Set = [Head|Tail],
       Ground = 'MetaDefs.Term.F2'('MetaDefs.Name.F4'('"Sets'
         , '"Inc', 'MetaDefs.Function.C0', 2), [GHead, GTail]),
       pobj_to_ground(Head, VarNo, VarNo1, GHead),
       pobj_to_ground(set(Tail), VarNo1, NewVarNo, GTail)
     )
   ; Term =.. [Function|Args],
     string2Gstring(Function, FuncString),
     symbol_name(Name, FuncString),
     ( Args = [] ->
       Ground = 'MetaDefs.CTerm.F1'(Name),
       NewVarNo = VarNo
     ; Ground = 'MetaDefs.Term.F2'(Name, GArgs),
       list_pobj_to_ground(Args, VarNo, NewVarNo, GArgs)
     )
   ).


list_pobj_to_ground([], VarNo, VarNo, []).

list_pobj_to_ground([Term|Terms], VarNo, NewVarNo, [Ground|Grounds]) :-
   pobj_to_ground(Term, VarNo, VarNo1, Ground),
   list_pobj_to_ground(Terms, VarNo1, NewVarNo, Grounds).


gstring(String) :- atom(String), name(String, [0'"|_]).

%------------------------------------------------------------------------------
   
symbol_name(Name, String) :-
   ( var(Name) ->
     user:string_to_name(String, Name)
   ; user:name_to_string(Name, String)
   ).

% Strings are represented as Prolog atoms beginning with a
% '"' character (but not ending with one). Any '"' characters within the
% string are not doubled. This representation is chosen for efficiency and
% portability between SICStus and SEPIA Prologs.

name_to_string('MetaDefs.Name.F4'(Mod, Symb, Cat, Arity), String) :-
   combine_name_components(Mod, Symb, Cat, Arity, String).

name_to_string('MetaDefs.SimpleName.F1'(String), String).

combine_name_components(Mod, Symb, Cat, Arity, String) :-
   category_code(Cat, CatChar),
   integer_chars(Arity, ArityChars),
   name(Mod, [_|ModChars]),
   name(Symb, [_|SymbChars]),
   append(ModChars, [46|SymbChars], Chars1),    % 46 is ASCII '.'
   append(Chars1, [46, CatChar|ArityChars], StringChars),
   name(String, [34|StringChars]).              % 34 is ASCII '"'

%------------------------------------------------------------------------------
 
string_to_name(String, Name) :-
   ( split_name_string(String, Mod, Symb, Cat, Arity) ->
     Name = 'MetaDefs.Name.F4'(Mod, Symb, Cat, Arity)
   ; Name = 'MetaDefs.SimpleName.F1'(String)
   ).


split_name_string(String, Mod, Symb, Cat, Arity) :-
   name(String, [34|C1s]),
   chars_to_dot(C1s, ModChars, C2s),
   chars_to_dot(C2s, SymbChars, C3s),
   chars_to_dot(C3s, [CatChar|ArityChars], _),
   name(Mod, [34|ModChars]),
   name(Symb, [34|SymbChars]),
   category_char(Cat, CatChar),
   integer_chars(Arity, ArityChars).


chars_to_dot([], [], []).

chars_to_dot([C|Cs], Prefix, Rest) :-
   ( C = 46 ->
     Prefix = [], Rest = Cs
   ; Prefix = [C|Prefix1],
     chars_to_dot(Cs, Prefix1, Rest)
   ).


category_code('MetaDefs.Base.C0', 66).            % 'B'
category_code('MetaDefs.Constructor.C0', 83).     % 'S'
category_code('MetaDefs.Constant.C0', 67).        % 'C'
category_code('MetaDefs.Function.C0', 70).        % 'F'
category_code('MetaDefs.Predicate.C0', 80).       % 'P'
category_code('MetaDefs.Proposition.C0', 79).     % 'O'


integer_chars(Int, Chars) :-
   ( var(Int) -> 
     chars_to_int(Chars, 0, Int)
   ; int_to_chars(Int, Chars)
   ).


chars_to_int([], Int, Int).

chars_to_int([C|Cs], SoFar, Int) :-
   SoFar1 is 10 * SoFar + C - 48,               % 48 is ASCII 0
   chars_to_int(Cs, SoFar1, Int).


int_to_chars(Int, Chars) :-
   int_to_chars1(Int, Chars, []).

int_to_chars1(Int, Chars, CharsT) :-
   ( Int = 0 ->
     Chars = [0'0|CharsT]
   ; int_to_chars2(Int, Chars, CharsT)
   ).

int_to_chars2(Int, Chars, CharsT) :-
   ( Int = 0 ->
     Chars = CharsT
   ; C is 0'0 + Int mod 10,
     Int1 is Int // 10,
     int_to_chars2(Int1, Chars, [C|CharsT])
   ).

%------------------------------------------------------------------------------

readin_program(Stream, Program) :-
   ( user:file_version(V),
     read(Stream, version(V, prm)) 		% test the version number
     -> read(Stream, MainModuleName),
 	Program = 'ProgDefs.Program.F4'(MainModuleName, ModuleDefAVL, Language,
			StatementsAndDelays),
   	Language = 'ProgDefs.Language.F1'(ModuleDescriptorAVL),
   	user:'AVLTrees.AVLIsEmpty.P1'(EmptyAVL),
   	user:'SharedPrograms.EmptyLanguage.P1'(EmptyLang),
   	EmptyLang = 'ProgDefs.Language.F1'(EmptyMDAVL),
   	readin_program_aux(Stream, EmptyAVL, ModuleDefAVL, EmptyAVL,
			StatementsAndDelays, EmptyMDAVL, ModuleDescriptorAVL)
     ;  format(user_error, 'Error: ".prm" file is out of date.~n', []),
	fail
   ).

readin_program_aux(Stream, ModuleDefAVL, ModuleDefAVL2, StatementsAndDelays,
	StatementsAndDelays2, ModuleDescriptorAVL, ModuleDescriptorAVL2) :-
   read(Stream, Term),
   ( Term = end_of_file
     -> ModuleDefAVL = ModuleDefAVL2,
	StatementsAndDelays = StatementsAndDelays2,
	ModuleDescriptorAVL = ModuleDescriptorAVL2
     ;  ( Term = import(GModuleName)
          -> user:system_module(GModuleName, ModuleDef, ModuleDes, Codes),
	     user:'AVLTrees.AVLInsert.P4'(ModuleDefAVL, GModuleName,
				ModuleDef, ModuleDefAVL3),
	     user:'AVLTrees.AVLInsert.P4'(ModuleDescriptorAVL, GModuleName,
			ModuleDes, ModuleDescriptorAVL3),
	     user:'AVLTrees.AVLInsert.P4'(StatementsAndDelays, GModuleName,
			'ProgDefs.Code.F2'(0, Codes), StatementsAndDelays3)
          ;  Term = module_name(GModuleName),
	     read(Stream, ModuleDef),
	     user:'AVLTrees.AVLInsert.P4'(ModuleDefAVL, GModuleName, ModuleDef,
			     ModuleDefAVL3),
	     read(Stream, ModuleDes),
	     user:'AVLTrees.AVLInsert.P4'(ModuleDescriptorAVL, GModuleName,
		ModuleDes, ModuleDescriptorAVL3),
	     read(Stream, Codes),
             user:'SharedPrograms.NextModuleVersion.P2'(GModuleName, Version),
	     user:'AVLTrees.AVLInsert.P4'(StatementsAndDelays, GModuleName,
		'ProgDefs.Code.F2'(Version, Codes), StatementsAndDelays3)
        ),
        readin_program_aux(Stream, ModuleDefAVL3, ModuleDefAVL2,
		StatementsAndDelays3, StatementsAndDelays2,
		ModuleDescriptorAVL3, ModuleDescriptorAVL2)
   ).
