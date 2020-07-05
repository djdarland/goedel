g:- multifile '$$module'/1.

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
File:       utilities.pl
Author:     Jiwei Wang
Date:       11 September 1991

Commonly used predicates. 
*/

%% '$$module'('@(#)utilities.pl 1.15 last updated 93/12/08 16:19:53 by jiwei').

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

				% split_list : +List * +Atom * -List * -List.
				% split_list divides list into two according the atom A.
				% The A is not in either of the split lists.

split_list([], _, [], []).
split_list([A|T], A, [], T):- !.
split_list([H|T], A, [H|R], S):-
	split_list(T, A, R, S).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/*
member(X,[X|_]).
member(X,[_|Y]):-
	member(X,Y).
*/

				% member_check doesn't allow backtracking
member_check(X,[X|_]) :- !.
member_check(X,[_|Y]) :- 
	member_check(X,Y).

/*
append([],Y,Y).
append([U|X],Y,[U|Z]):-
	append(X,Y,Z).
*/

reverse(X,Y) :-
	reverse3(X,[],Y).

reverse3([],Y,Y).
reverse3([X|U],Z,Y) :-
	reverse3(U,[X|Z],Y).


subset([], _).
subset([H|T], List) :-
	member(H, List),
	subset(T, List), !.


				% set_minus(S1, S2, S3),
				% S3 contains elements in S1 but not in S2 

set_minus([], _, []).
set_minus([H|T], S, S3) :-
	member(H, S), !,
	set_minus(T, S, S3).

set_minus([H|T], S, [H|S3]) :-
	set_minus(T, S, S3).


list2listOflist([], []).
list2listOflist([Term|Terms], [ [Term] | TermLists]) :-
	list2listOflist(Terms, TermLists).


first_n_elements(List, N, Front, Rear) :-
	length(List, N2),
	N2 >= N,
	first_n_elements_aux(N, List, Front, Rear).

first_n_elements_aux(0, Rear, [], Rear).
first_n_elements_aux(N, [H|T], [H|Front], Rear) :-
	N2 is N - 1,
	first_n_elements_aux(N2, T, Front, Rear).



last_n_elements(List, N, List2) :-
	length(List, N2),
	N2 >= N,
	last_n_elements_aux(List, N, List2).

last_n_elements_aux([], _, []).
last_n_elements_aux([H|T], N, List) :-
	( length([H|T], N)
	-> List = [H|T]
	;  last_n_elements_aux(T, N, List)
	).

/*------------------------------------------------------------------------------
* Assuming sets as ordered
*/

union([], Ys, Ys) :- !.

union([X|Xs], [], [X|Xs]) :- !.

union([X|Xs], [X|Ys], [X|Zs]) :- !,
	union(Xs, Ys, Zs).

union([X|Xs], [Y|Ys], [X|Zs]) :-
	X @< Y, !,
	union(Xs, [Y|Ys], Zs).

union([X|Xs], [Y|Ys], [Y|Zs]) :-
	X @> Y, !,
	union([X|Xs], Ys, Zs).

/*------------------------------------------------------------------------------
* Assuming sets as ordered
*/

difference([X|Xs], [], [X|Xs]) :- !.

difference([], _, []) :- !.

difference([X|Xs], [X|Ys], Zs) :- !,
	difference(Xs, Ys, Zs).

difference([X|Xs], [Y|Ys], [X|Zs]) :-
	X @< Y, !,
	difference(Xs, [Y|Ys], Zs).

difference([X|Xs], [Y|Ys], Zs) :-
	X @> Y, !,
	difference([X|Xs], Ys, Zs).

/*------------------------------------------------------------------------------
* Assuming sets as ordered
*/

intersection([], _, []) :- !.

intersection(_, [], []) :- !.

intersection([X|Xs], [X|Ys], [X|Zs]) :- !,
	intersection(Xs, Ys, Zs).

intersection([X|Xs], [Y|Ys], Zs) :-
	X @< Y, !,
	intersection(Xs, [Y|Ys], Zs).

intersection([X|Xs], [Y|Ys], Zs) :-
	X @> Y, !,
	intersection([X|Xs], Ys, Zs).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

				% sappend takes two atoms and returns an atom concatenated from the two
sappend(A, B, C):-
	name(A, A1),
	name(B, B1),
	append(A1, B1, C1),
	name(C, C1), !.


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* delete_once fails if X doesn't occur in the list
*/

delete_once([X|T], X, T) :- !.
delete_once([H|T], X, [H|T2]) :-
	delete_once(T, X, T2).

				%------------------------------------------------------------------------------

delete_all([], _, []).
delete_all([A|T], A, L):- !,
	delete_all(T, A, L).
delete_all([H|T], A,  [H|L]):-
	delete_all(T, A, L).

				%------------------------------------------------------------------------------

delete_last([], []).
delete_last([_], []):- !.
delete_last([H|T], [H|T2]):- 
	delete_last(T, T2).

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* cartesian_product mainly performs an Cartesian products of terms_seqs
* to produce parse trees for a sequence of terms.
*/


cartesian_product([], []) :-  !.
cartesian_product(Terms_seqs, Term_seqs) :-
	reverse(Terms_seqs, [Terms|Terms_seqs2]),
	init_product_aux(Terms, Init_product),
	product_aux(Terms_seqs2, Init_product, Term_seqs).


init_product_aux([], []).
init_product_aux([H|T], [[H]|Product]) :- 
	init_product_aux(T, Product).


product_aux([], Product, Product).
product_aux([H|T], Product, NewProduct) :-
	times_many(H, Product, Product2, []),
	product_aux(T, Product2, NewProduct).


				% times_many(One_list, Product_so_far, NewProduct, NewlyProducedTail).
times_many([], _, P, P).
times_many([H|T], Product, NewProduct, PTail):-
	times_one(Product, H, NewProduct, PTail2),
	times_many(T, Product, PTail2, PTail).

				% times_one(Product, One_element_of_a_list, NewProduct, NewlyProducedTail).

times_one([], _, L, L).
times_one([H|T], One, [[One|H]|L], LTail):-
	times_one(T, One, L, LTail).


/*------------------------------------------------------------------------------
*/

string2Gstring(String, GString) :-
	name(String, List),
	name(GString, [34|List]). % 34 is the ASCII code for "

gstring2string(GString, String) :-
	( name(GString, [34|List])
	-> name(String, List)
	;  GString = String	% in case it's not a Gstring
	).

gstringlist2stringlist([], []).
gstringlist2stringlist([GString|T], [String|T2]) :- 
	gstring2string(GString, String),
	gstringlist2stringlist(T, T2).

/*------------------------------------------------------------------------------
*/

var_list([]).
var_list([Var|Vars]):-
	var(Var),
	var_list(Vars).

call_residue(A, B) :-
	call_residue_vars(A, B).

%%% Below added by Dennis J Darland

djd_copy_char(Stream1, Stream2) :-
	%% Copy Stream1 to Stream2
	%% Added 6/7/2020 because cannot do with SICStus directly
	get_char(Stream1, C),
	(C \= end_of_file) -> (put_char(Stream2, C),
		     djd_copy_char(Stream1, Stream2))
	; true.

djd_cat_1(File1, File2, Work3) :-
	%% cat File1 to File2		%
	%% Added 6/7/2020 because cannot do with SICStus directly 
	(open(Work3, write, Stream3, [type(text)])
	-> open(File1, read, Stream1, [type(text)])
	-> (open(File2, read, Stream2, [type(text)])
	   -> (djd_copy_char(Stream1, Stream3),
		  djd_copy_char(Stream2, Stream3),
		  close(Stream1),
		  close(Stream2),
		  close(Stream3),
		  djd_cp(Work3, File2),
		  format(user_error,'~nError: Cat "~a" and "~a" giving file "~a".~n', [File1], [File2], [Work3]))
	   
	   ;  format(user_error,'~nError: cannot open file "~a".~n', [File2])
	   )
	;  format(user_error,'~nError: cannot open file "~a".~n', [File1])
	) 
	; format(user_error,'~nError: cannot open file "~a".~n', [Work3]).


djd_cp(File1, File2) :-
		%% Copy File1 to File2
		%% Added 6/7/2020 because cannot do with SICStus directly
		(open(File2, write, Stream2, [type(text)])
		-> (open(File1, read, Stream1, [type(text)])
		   -> (djd_copy_char(Stream1, Stream2),
			  close(Stream1),
			  close(Stream2))
		   ;  format(user_error,'~nError: cannot open file "~a".~n', [File1]))
			     , write(dennis_open_021)

		;  format(user_error,'~nError: cannot open file "~a".~n', [File2]))
			     , write(dennis_open_022)
.
		
		djd_cat_2(File1, File2, File3) :-
			%% Copy File1 and then File2 to File3
			%% Added 6/7/2020 because cannot do with SICStus directly
			open(File3, write, Stream3, [type(text)])
			-> (open(File1, read, Stream1, [type(text)])
			   -> (open(File2, read, Stream2, [type(text)])
			      -> (djd_copy_char(Stream1, Stream3),
				     djd_copy_char(Stream2, Stream3),
				     close(Stream1),
				     close(Stream2),
				     close(Stream3))
			      ;  format(user_error,'~nError: cannot open file "~a".~n', [File2]))
			   	     , write(dennis_open_023)

			   ;  format(user_error,'~nError: cannot open file "~a".~n', [File1]))
				     , write(dennis_open_024)

				     ;  format(user_error,'~nError: cannot open file "~a".~n', [File3])
				     	     , write(dennis_open_025)
.
			
