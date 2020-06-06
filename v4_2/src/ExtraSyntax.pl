:- module('ExtraSyntax', []).

:- op(500, yfx, and).
:- op(400, yfx, or).

%------------------------------------------------------------------------------
% Prolog support for Syntax system module
%------------------------------------------------------------------------------
 
% %Z%%M% %I% last updated %E% %U% by %Q%

%------------------------------------------------------------------------------
% Bindings in variable typings
%------------------------------------------------------------------------------
 
% Avoid unnecessary choicepoints

'ExtraSyntax.EBindingInVarTyping.P3'(Subst, Param, Type) :-
   ( var(Param) ->
     user:'Lists.Member.P2'('MetaDefs.@.F2'(Param,Type), Subst)
   ; user:'Lists.MemberCheck.P2'('MetaDefs.@.F2'(Param,Type),Subst)
   ).

'~ExtraSyntax.EBindingInVarTyping.P3'(Subst, Param, Type) :-
   'ExtraSyntax.EBindingInVarTyping.P3'(Subst, Param, Type).


'ExtraSyntax.EDelBindingInVarTyping.P4'(Typing, Var, Type, NewTyping) :-
   ( var(Var) ->
     user:'Lists.Delete.P3'('MetaDefs.@.F2'(Var, Type), Typing, NewTyping)
   ; user:'Lists.DeleteFirst.P3'('MetaDefs.@.F2'(Var, Type), Typing, NewTyping)
   ).

'~ExtraSyntax.EDelBindingInVarTyping.P4'(Typing, Var, Type, NewTyping) :-
   'ExtraSyntax.DelBindingInVarTyping.P4'(Typing, Var, Type, NewTyping).

%------------------------------------------------------------------------------
% Pseudo-set maniplation predicates
%------------------------------------------------------------------------------

'ExtraSyntax.Order.P2'(X, Y) :-
   sort(X, Y).

'~ExtraSyntax.Order.P2'(X, Y) :-
   sort(X, Y).
 
'ExtraSyntax.Union.P3'(X, Y, Z) :-
   user:union(X, Y, Z).

'~ExtraSyntax.Union.P3'(X, Y, Z) :-
   user:union(X, Y, Z).

'ExtraSyntax.Difference.P3'(X, Y, Z) :-
   user:difference(X, Y, Z).

'~ExtraSyntax.Difference.P3'(X, Y, Z) :-
   user:difference(X, Y, Z).

