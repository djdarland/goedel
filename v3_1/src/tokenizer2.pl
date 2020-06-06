
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
File:		tokeniser2.pl
Subject:	tokenising Goedel programs.
Author:		Jiwei Wang
Date:		19 Jan. 1992

This version of tokenizer assumes that module is read in a list.

This tokenizer does not return line number and returns items in a list of
lists.

The good thing about new Goedel language definition is that tokenizing can be
done with one look-ahead char.
*/

'$$module'('@(#)tokenizer2.pl 1.12 last updated 93/11/24 17:34:36 by jiwei
').


/*------------------------------------------------------------------------------
 */

/* Comment out for to avoid redefine get_one_module

get_one_module(Stream, Module):-
   stream2chars(Stream, Chars),
   get_one_module_aux(Chars, Module).

get_one_module_aux([], []) :-  !.
get_one_module_aux(Chars, Module) :-
   get_one_item_2nd(Chars, Remains, Item),
   ( Item = end_of_file
     -> Module = []
     ;  Module = [Item|Module2],
	get_one_module_aux(Remains, Module2)
   ).


stream2chars(Str,Cs):-
   get0(Str, C),
   stream2chars_aux(Str, C, Cs).

stream2chars_aux(Str, C, Cs):-
   ( C = -1
     -> Cs = []
     ;  Cs = [C|Cs1],
	get0(Str, C1),
	stream2chars_aux(Str, C1, Cs1)
   ).

 */

/*------------------------------------------------------------------------------
 * get_one_item_2nd(+Chars, -Remains, -Item)
 *
 * get_one_item_2nd_aux(+Chars, -Remains, -Item)
 *
 *   Item:   list(token_2nd).
 *	     or end_of_file
 *
 * usage: get_one_item_2nd(+Chars, -Remains, -Item)
 *
 * get_one_item_2nd is determinate and alway succeeds.
 */

get_one_item_2nd(Chars, Remains, Item) :-
   get_one_token_2nd(Chars, Remains2, Token),
   ( Token == terminator
     -> format(user_error, '~nWarning: there is an empty item.~n', []),
        get_one_item_2nd(Remains2, Remains, Item)
     ;  ( Token == end_of_file
          -> Item = end_of_file,
	     Remains = Remains2
          ;  Item = [Token|Item2],
	     get_one_item_2nd_aux(Remains2, Remains, Item2)
        )
   ).
   

get_one_item_2nd_aux(Chars, Remains, Item):-
   get_one_token_2nd(Chars, Remains2, Token),
   ( Token == terminator
     -> Item = [],
	Remains = Remains2
     ;  ( Token == end_of_file
          -> format(user_error, '~nError: unexpected end_of_input.~n', []),
     	     Item = [],
	     Remains = Remains2
          ;  Item = [Token|Item2],
	     get_one_item_2nd_aux(Remains2, Remains, Item2)
	)
   ).

/*------------------------------------------------------------------------------
 */

get_one_token_2nd([], [], end_of_file).
get_one_token_2nd([Char|Chars], Remains, Token):-
   token_2nd(Char, Chars, Remains, Token).


/*------------------------------------------------------------------------------
 */

% Comments
token_2nd(0'%, Chars, Remains, Token) :- !,
   comment_2nd(Chars, Remains2),
   get_one_token_2nd(Remains2, Remains, Token).

/*------------------------------------------------------------------------------
 */

comment_2nd([], []).
comment_2nd([Char|Chars], Remains):-
   ( Char = 10
     -> Remains = Chars
     ;  comment_2nd(Chars, Remains)
   ).

/*------------------------------------------------------------------------------
 */

% Layout-char
token_2nd(32, Chars, Remains, Token) :- !,
   get_one_token_2nd(Chars, Remains, Token).
token_2nd(9, Chars, Remains, Token) :- !,
   get_one_token_2nd(Chars, Remains, Token).
token_2nd(10, Chars, Remains, Token) :- !,
   get_one_token_2nd(Chars, Remains, Token).

% String with error recovery
token_2nd(0'", Chars, Remains, string(String)) :- !,
   string_chars_2nd(Chars, Remains, SChars),
   name(String, SChars).

/*------------------------------------------------------------------------------
 * One " terminates string, two " escapes this, anything else just adds
 * characters to the string
 */

string_chars_2nd([], [], []) :-
   format(user_error, '~nError: incomplete string.~n', []).
		% does error recovery
string_chars_2nd([C|Chars], Remains, SChars) :-
   ( C = 0'"
     -> SChars = [],
	Remains = Chars
     ;  ( C = 0'\
	  -> ( Chars = [C2|Remains2]
	       -> ( C2 = 0'n               % \n
        	    -> SChars = [10|SChars2]
        	    ;  ( C2 = 0't          % \t
         		 -> SChars = [9|SChars2]
	                 ;  ( C2 = 0'b     % \b
          	              -> SChars = [8|SChars2]
                              ;  SChars = [C2|SChars2]
                            )
                       )
                  ),
	     	  string_chars_2nd(Remains2, Remains, SChars2)
	       ;  format(user_error, '~nError: incomplete string.~n', [])
	     )
          ;  SChars = [C|SChars2],
             string_chars_2nd(Chars, Remains, SChars2)
        )
   ).

/*------------------------------------------------------------------------------
 */

% left curly
token_2nd(0'{, Chars, Chars, '{') :- !.

% right curly (Label)
token_2nd(0'}, Chars, Remains, '}'(N)) :- !,
   commit_label_2nd(Chars, Remains, N).

commit_label_2nd([], [], 1000000000).
	% in fact this is an error
commit_label_2nd([C|Chars], Remains, N) :-
   ( C == 0'_
     -> number_chars_2nd(Chars, Remains, Cs),
	name(N, Cs)
     ;  N = 1000000000,
	Remains = [C|Chars]
   ).


% left round
token_2nd(0'(, Chars, Chars, '(') :- !.

% right round
token_2nd(0'), Chars, Chars, ')') :- !.

% left square
token_2nd(0'[, Chars, Chars, '[') :- !.

% right square
token_2nd(0'], Chars, Chars, ']') :- !.

% Comma
token_2nd(0',, Chars, Chars, ',') :- !.

% Underscore, the name is "_littlename"
token_2nd(0'_, Chars, Remains, '_'(N)) :- !,
   ( Chars = [C|Chars2], little_letter(C) 
     -> name_chars_2nd(Chars2, Remains, Cs),
        name(N, [0'_, C|Cs])
     ;  N = '_',
	Remains = Chars
   ).


% SemiColon
token_2nd(0';, Chars, Chars, ';') :- !.

% Terminator
token_2nd(0'., Chars, Chars, terminator) :- !.


% Big-name
token_2nd(C, Chars, Remains, big_name(N)) :- 
   0'A =< C, C =< 0'Z,
   !,
   name_chars_2nd(Chars, Remains, Cs),
   name(N, [C|Cs]).

% Little-name
token_2nd(C, Chars, Remains, little_name(N)) :- 
   0'a =< C, C =< 0'z,
   !,
   name_chars_2nd(Chars, Remains, Cs),
   name(N, [C|Cs]).

/*------------------------------------------------------------------------------
 */

% name_chars_2nd is written in this clumsy form in order to remove choice points
% hence improve efficiency
name_chars_2nd([], [], []).
	% an error case
name_chars_2nd([C|Chars], Remains, Cs) :-
   ( ( little_letter(C);
       number_char(C);
       big_letter(C);
       C = 0'_
     )
     -> Cs = [C|Cs2],
	name_chars_2nd(Chars, Remains, Cs2)
     ;  Cs = [],
	Remains = [C|Chars]
   ).

/*------------------------------------------------------------------------------
 */

% Graphic-name

token_2nd(C, Chars, Remains, graphic_name(N)) :- 
   graphic_char(C),
   !,
   graphic_chars_2nd(Chars, Remains, Cs),
   name(N, [C|Cs]).


graphic_chars_2nd([], [], []).
	% a case for error
graphic_chars_2nd([C|Chars], Remains, Cs) :-
   ( graphic_char(C)
     -> Cs = [C|Cs2],
	graphic_chars_2nd(Chars, Remains, Cs2)
     ;  Cs = [],
	Remains =[C|Chars]
   ).

/*------------------------------------------------------------------------------
 * Representation of numbers:
 *   Number:      number(integer) where the integer >= 0.
 *   Float:      float(float)
 */

% Number & Float
token_2nd(C, Chars, Remains, Token) :-
   0'0 =< C, C =< 0'9,
   !,
   number_chars_2nd(Chars, Remains1, Cs1),
   ( Remains1 = [0'.|Remains2]
     -> ( Remains2 = [C0|Remains3],
          number_char(C0)   % make sure it's not P(a) <- a = 3.
          -> number_chars_2nd(Remains3, Remains4, Cs2),
             ( Remains4 = [0'E|Remains5]
               -> ( Remains5 = [C2|Remains6],
                    signed_number(C2)
                    -> number_chars_2nd(Remains6, Remains, Cs3),
                       append(Cs2, [0'E, C2|Cs3], Cs4),
                       append(Cs1, [0'., C0|Cs4], Cs5),
                       name(Int2, [C|Cs5]),
                       Token = float(Int2)
                    ;  format(user_error, '~nError: wrong format in the float number.~n', []),
                  		% here is the error recovery job
                       append(Cs1, [0'., C0|Cs2], Cs7),
                       name(Int4, [C|Cs7]),
                       Token = float(Int4),
		       Remains = Remains5
                  )
               ;  append(Cs1, [0'., C0|Cs2], Cs6),
                  name(Int3, [C|Cs6]),
                  Token = float(Int3),
		  Remains = Remains4
             )
          ;  name(Int1, [C|Cs1]),
             Token = number(Int1),
             Remains = [0'.|Remains2]
        )
     ;  name(Int1, [C|Cs1]),
        Token = number(Int1),
        Remains = Remains1
   ).


number_chars_2nd([], [], []).
number_chars_2nd([C|Chars], Remains, Cs) :-
   ( number_char(C)
     -> Cs = [C|Cs2],
	number_chars_2nd(Chars, Remains, Cs2)
     ;  Cs = [],
	Remains = [C|Chars]
   ).

/*------------------------------------------------------------------------------
 */

% Cope with illegal characters
token_2nd(C, Chars, Remains, Token) :- 
   format(user_error, '~nError: illegal character with ASCII code "~q".~n',
		[C]),
   get_one_token_2nd(Chars, Remains, Token).

