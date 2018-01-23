:- use_module(library(clpfd)).
:- use_module(library(lists)).

% Peter Bromley
% Semantics CLP Final


% get_gc_list(M, N, Out) generates a list in global_cardinality form
% by calculating the number of zeros and positive integers based on M and N
get_gc_list(M, N, Out) :-
  ( M = 0 -> Out = [0-N]
  ; Out = [M-1|T], N1 is N-1, M1 is M-1, get_gc_list(M1, N1, T)
  ).

% firstpos(L, K) gives K is the first positive integer of L
firstpos([H|T], K) :-
 (H #= 0) #\/ (H #= K),
 (H #= 0) #=> K1 #= K,
 firstpos(T, K1).
firstpos([], _).


% endviews(N, M, List, LV, RV) prunes a list with endview constraints LV
%  and RV, making sure the list is only unique positive integers
endviews(N, M, VarList, LV, RV) :-
  length(VarList, N),
  domain(VarList, 0, M),
  get_gc_list(M, N, GCList),
  global_cardinality(VarList, GCList),
  firstpos(VarList, LV),
  reverse(VarList, VarListR),
  firstpos(VarListR, RV).
