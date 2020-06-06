
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
File:           trace.pl
Subject:        supporting routines for the tracer.
Author:         Jiwei Wang
Date:           19 May 1992

Signals used in the tracer:
	skip_trace:	created in trace_call
			should be removed in trace_exit and trace_fail
			it causes no trace info be printed out

	no_trace:	created in trace_call
			should be reset in process_query
			it causes trace info not to be printed out

	free_trace:	created in trace_call
			should be reset in process_query
			it causes no questions is to be asked, but trace
			info will still be printed out

	start_trace:	created after a spy point is encountered
			removed after an "l" command is issued
			should be reset in process_query

	stop_trace:	used to switch off trace_is_on,
			created by "l" command, and removed by creating
			start_trace.

Specification of the Tracer:
	There are two modes of tracing, "trace" and "spy".  "trace" sets off
	from the query, while "spy" only triggers the tracing at specified
	predicates.  "spy Predicate" puts spy points on predicates of such
	name, regardless their arity and module.  "trace" and "spy" only
	works on debug-compiled modules.  It is therefore recommanded that
	correct modules should not be debug-compiled.  (However, "trace" also
	works on the query even no module in the program is debug-compiled.)

1. Commands in the questions of the tracer:
	c - continue:	continue execution without trace info.
	a - abort:	abort current query.
	l - leap:	leap to the next spy point.
	f - free:	let the tracer run free and print out all the trace info
	s - skip:	skip the trace info for executing current goal
	n - next:	next goal, the same as a CR
	h - help:	print this information

   (This differs from Prolog tracer at two commands, break and redo.)

2. What do you see in the trace information ?
	Terms declared in the local part of a closed module (e.g. terms of
	the ground represetation) are masked off because they are
	incomprehensible and substantially large.  However, terms declared
	in the local part of users modules will all show up.  Operators are
	printed out in normal form.

3. Problems.
	Constraints cannot be traced.  There is no indication when commits
	are switch off.  Variables will be renamed after being suspended.
	You'll see something funny when negation is involved. 


================================================================================
*/

'$$module'('@(#)tracer.pl 1.33 last updated 94/04/08 15:45:54 by jiwei
').

:- dynamic tracer_counter/1.

:- dynamic display_depth/1.

:- dynamic no_trace/0.
:- dynamic skip_trace/1.
:- dynamic free_trace/0.
:- dynamic start_trace/0.
:- dynamic stop_trace/0.

% default level 3.
display_depth(3).
default_display_depth(3).

%------------------------------------------------------------------------------

trace_call(Predicate, Args, N) :-
   get_tracer_count(N),
   ( ( trace_is_on, \+ stop_trace;
       start_trace;
       Predicate = _:P, spied(P), assert(start_trace), retractall(stop_trace)
		% builtin predicates = and ~= are not to be spied.
     )
     -> ( ( skip_trace(_); no_trace )
	  -> true
	  ;  tracer_display(Predicate, Args, 'call', N),
   	     ( ( free_trace
	         -> format(user_error, '~n', [])
	         ;  tracer_question(Predicate, Args, 'call', N, _)
	       );
               trace_fail(Predicate, Args, N)
	     )
	)
     ;  true
   ).

%------------------------------------------------------------------------------

trace_fail(Predicate, Args, N) :-
   ( skip_trace(N)
     -> retractall(skip_trace(_))
     ;  true
   ), 
   \+ no_trace, \+ skip_trace(_),
   tracer_display(Predicate, Args, 'fail', N),
   ( free_trace
     -> format(user_error, '~n', []),
	fail
     ;  tracer_question(Predicate, Args, 'fail', N, Redo),
	( Redo == redo    % allows reentry of the predicate in failure
	  -> tracer_display(Predicate, Args, 'call', N),
   	     ( ( free_trace
	         -> format(user_error, '~n', [])
	         ;  tracer_question(Predicate, Args, 'call', N, _)
	       );
               trace_fail(Predicate, Args, N)
	     )
	  ;  fail
	)
   ).

%------------------------------------------------------------------------------

trace_exit(Predicate, Args, N) :-
   ( skip_trace(N)
     -> retractall(skip_trace(N))
     ;  true
   ), 
   ( ( no_trace; skip_trace(_) )
     -> true
     ;  ( ( trace_is_on, \+ stop_trace; start_trace )
	  -> tracer_display(Predicate, Args, 'exit', N),
   	     ( ( free_trace
	         -> format(user_error, '~n', [])
	         ;  tracer_question(Predicate, Args, 'exit', N, _)
	       );
               trace_redo(Predicate, Args, N)
	     )
          ;  true
	)
   ).

%------------------------------------------------------------------------------

trace_redo(Predicate, Args, N) :-
   \+ no_trace, \+ skip_trace(_),
   tracer_display(Predicate, Args, 'redo', N),
   ( free_trace
     -> format(user_error, '~n', [])
     ;  tracer_question(Predicate, Args, 'redo', N, _)
   ),
   fail.

%------------------------------------------------------------------------------
% Signal is var, indicating there is no suspension

trace_suspension(Delay, Predicate, Args, Signal) :-
   ( ( trace_is_on, \+ stop_trace;
       start_trace;
       Predicate = _:P, spied(P), assert(start_trace), retractall(stop_trace)
		% builtin predicates = and ~= are not to be spied.
     )
     -> ( ( skip_trace(_); no_trace )
	  -> true
	  ;  evaluate_delay(Delay, X),
	     ( var(X)
	       -> get_tracer_count(Signal),
		  tracer_display(Predicate, Args, 'suspend', Signal),
		  nl(user_error)
	       ;  true
	     )
	)
     ;  true
   ).

%------------------------------------------------------------------------------

trace_wakening(Predicate, Args, Signal) :-
   ( var(Signal)
     -> true
     ;  ( ( trace_is_on, \+ stop_trace;
            start_trace;
            Predicate = _:P, spied(P), assert(start_trace),
			retractall(stop_trace)
		% builtin predicates = and ~= are not to be spied.
          )
          -> ( ( skip_trace(_); no_trace )
	       -> true
	       ;  tracer_display(Predicate, Args, 'awaken', Signal),
		  nl(user_error)
	     )
          ;  true
        )
   ).

%------------------------------------------------------------------------------

tracer_display(Predicate, Args, Entry, Count) :-
   extract_module_prefix(Predicate, M, P),
   convert_to_goedel_term(Args, GArgs),
   Pred =.. [P|GArgs],
   ( M = ''
     -> format(user_error, '   ~w ~w: ~w', [Count, Entry, Pred])
     ;  format(user_error, '   ~w ~w: ~w', [Count, Entry, M:Pred])
   ).

extract_module_prefix(M:P, M, P) :- !.
extract_module_prefix(P, '', P).

%------------------------------------------------------------------------------

tracer_question(Predicate, Args, Entry, N, Return) :-
   format(user_error, ' ? ', []),
   ttyflush,
   ttyget0(C),
   ( C = 10
     -> true
     ;  ttyskip(10)
   ),
   ( tracer_question_aux(C, N, Entry, Return)
     -> ( 48 < C, C < 58
          -> tracer_display(Predicate, Args, Entry, N),
	     tracer_question(Predicate, Args, Entry, N, Return)
          ;  ( C = 0'h
	       -> tracer_question(Predicate, Args, Entry, N, Return)
	       ;  true
	     )
	)
     ;  format(user_error, 'Incorrect command. Use "h" for help.', []),
	tracer_question(Predicate, Args, Entry, N, Return)
   ).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% for system designers only
tracer_question_aux(0'b, _, _, _) :- !,
   break.

tracer_question_aux(0'a, _, _, _) :- !,
   raise_exception(catch_in_query).

tracer_question_aux(0's, N, call, _) :- !,
   assert(skip_trace(N)).

tracer_question_aux(0'r, _, fail, redo) :- !.

tracer_question_aux(0'f, _, _, _) :- !,
   assert(free_trace).

tracer_question_aux(0'l, _, _, _) :- !,
   retractall(start_trace),
   assert(stop_trace).
   
tracer_question_aux(0'c, _, _, _) :- !,
   assert(no_trace).

tracer_question_aux(0'n, _, _, _) :- !.

tracer_question_aux(10, _, _, _) :- !.

tracer_question_aux(0'?, _, _, _) :- !,
   display_tracer_message.

tracer_question_aux(0'h, _, _, _) :- !,
   display_tracer_message.

tracer_question_aux(C, _, _, _) :-
   ( 48 < C, C < 58
     -> retractall(display_depth(_)),
	Level is C - 48,
	assert(display_depth(Level))
     ;  fail
   ).

%------------------------------------------------------------------------------

display_tracer_message :-
   format(user_error,
	'   a - abort:      abort current query.~n', []),
   format(user_error,
	'   c - continue:   continue execution without trace info.~n', []),
   format(user_error,
	'   f - free:       let the tracer run free and print out all the trace info.~n', []),
   format(user_error,
	'   h(?) - help:    print this message.~n', []),
   format(user_error,
	'   l - leap:       leap to the next spy point.~n', []),
   format(user_error,
	'   n - next:       next goal, the same as a carriage return.~n', []),
   format(user_error,
	'   r - redo:       reenter the failed goal, valid only in "fail" entries.~n', []),
   format(user_error,
	'   s - skip:       skip the trace info for executing the current goal,~n', []),
   format(user_error,
	'                        valid only in "call" entries.~n', []),
   format(user_error,
	'   1-9:            set term display depth in the tracer.~n', []).


%------------------------------------------------------------------------------

get_tracer_count(N) :-
   retract(tracer_counter(N)),
   M is N + 1,
   assert(tracer_counter(M)).

%------------------------------------------------------------------------------

convert_to_goedel_term(Term, Term) :-
   var(Term), !.

convert_to_goedel_term(Term, Term2) :-
   ( display_depth(N)
     -> true
     ;  default_display_depth(N)
   ),
   convert_arg_list(Term, Term2, N).	% it must be a list

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

convert_arg_list([], [], _).
convert_arg_list([H|T], [GH|GT], N) :-
   convert_to_goedel_term(H, GH, N),
   convert_arg_list(T, GT, N).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

convert_to_goedel_term(Term, Term, _) :-
   var(Term), !.

convert_to_goedel_term([], [], _) :- !.
convert_to_goedel_term([H|T], List, N) :- !,
   convert_list([H|T], List, N, N).

convert_to_goedel_term(_, '...', 0) :- !.

% convert_to_goedel_term('Scripts.Script.F5'(_,_,_,_,_), '<Script>', _) :- !.
% convert_to_goedel_term('ProgDefs.Program.F4'(_,_,_,_), '<Program>', _) :- !.
% convert_to_goedel_term('ProgDefs.Language.F1'(_), '<Language>', _) :- !.

convert_to_goedel_term(set(Term), GTerm, N) :- !,
   ( var(Term)
     -> convert_to_goedel_term(Term, GTerm, N)
     ;  convert_set(Term, GTerm, N)
   ).

convert_to_goedel_term(Term, Term, _) :-
   integer(Term), !.

convert_to_goedel_term(Term, GTerm, _) :-
   gstring(Term), !,
   'Strings.StringInts.P2'(Term, IntList),
   'SharedPrograms.ExpandString.P3'(IntList, XIntList, []),
   name(GTerm, XIntList).
% Note the representation is changed here.

convert_to_goedel_term(Term, GTerm, N) :-
   Term =.. [Func|Args],
   make_flat_name('MetaDefs.Name.F4'(_, GSymbol, _, _), Func, _, _), !,
   N2 is N - 1,
   convert_arg_list(Args, GArgs, N2),
   gstring2string(GSymbol, Symbol),
   GTerm =.. [Symbol|GArgs].

convert_to_goedel_term(Term, Term, _).
% including integer(Term), !.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% convert_set uses the curly bracket representation of SICStus, this
% may not be available in other Prolog systems.
convert_set(Var, Var, _) :-
   var(Var), !.
convert_set([], '{}', _).
convert_set([T|Terms], '{}'(GTerm), N) :-
   convert_set_aux([T|Terms], GTerm, N, N).

convert_set_aux(Var, Var, _, _) :- 
   var(Var), !.
convert_set_aux([_], '...', 0, _) :- !.
convert_set_aux([Term], GTerm, _, NN) :-
   !,
   convert_to_goedel_term(Term, GTerm, NN).
convert_set_aux([_|_], '...', 0, _) :- !.
convert_set_aux([Term|Terms], (GTerm ',' GTerm2), N, NN) :-
   convert_to_goedel_term(Term, GTerm, NN),
   N2 is N - 1,
   convert_set_aux(Terms, GTerm2, N2, NN).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

convert_list(List, List, _, _) :- 
   var(List), !.
convert_list([], [], _, _) :- !.
convert_list([_|_], ['...'], 0, _) :- !.
convert_list([H|T], [GH|GT], N, NN) :- !,
   convert_to_goedel_term(H, GH, NN),
   N2 is N - 1,
   convert_list(T, GT, N2, NN).

