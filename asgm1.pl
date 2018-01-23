:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(between)).


split(0, L, [], L) :- !.
split(N, [H|T], [H|F1], B1) :-
  N > 0,
  N1 is N - 1,
  split(N1, T, F1, B1).

% chop(N, L, R): R is a list of nonempty lists of length <= N, such that the
% concatenation of the lists is R.
chop(N, L, R) :-
  length(L, Y),
  ( Y = 0 -> R = L
  ; N > Y -> R = [L]
  ).
chop(N, L, [F|R]) :-
  split(N, L, F, B),
  chop(N, B, R).




subgrids(In, Out) :-
  length(In, L),
  K is integer(sqrt(L)),
  chop(K, In, Div1),
  maplist(transpose, Div1, Div2),
  append(Div2, Div3),
  chop(K, Div3, Div4),
  maplist(append, Div4, Out).



my_is_set(L) :-
  \+ (append(_, [X|T], L), X \= 0, memberchk(X, T)).



consistent(Rows) :-
  transpose(Rows, Cols),
  subgrids(Rows, SubGrids),
  maplist(my_is_set, Rows),
  maplist(my_is_set, Cols),
  maplist(my_is_set, SubGrids).



replace_zero(X, 0, Out) :-
  between(1, X, Out).
replace_zero(_, X, X) :-
  X \= 0.
replace_zeros(L, O) :-
  length(L, X),
  maplist(replace_zero(X), L, O),
  my_is_set(O).


generate(Grid, O) :-
  maplist(replace_zeros, Grid, O).


sudoku0(Grid0, Grid) :-
  generate(Grid0, Grid),
  consistent(Grid).
