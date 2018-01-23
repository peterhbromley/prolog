:- use_module(library(clpfd)).
:- use_module(library(lists)).


separate_elements([], [], []).
separate_elements([g(X,Y,Z)|T], [g(X,Y,Z)|G], V) :-
  separate_elements(T, G, V).
separate_elements([v(X,Y,Z)|T], G, [v(X,Y,Z)|V]) :-
  separate_elements(T, G, V).

visnum([H|T], Max, K) :-
  (H #> Max) #<=> B,
  Max1 #= max(H, Max),
  K1 #= K-B,
  visnum(T, Max1, K1).
visnum([], _, 0).

visnum([H|T], K) :-
  K1 #= K-1,
  visnum(T, H, K1).


set_vars(Mx, N) :-
  length(Mx, N),
  maplist(same_length(Mx), Mx),
  append(Mx, AllVars),
  domain(AllVars, 1, N).


set_known_ints([], _).
set_known_ints([g(N, R, C)|T], Mx) :-
  nth1(R, Mx, Sublist),
  nth1(C, Sublist, X),
  X #= N,
  set_known_ints(T, Mx).



get_list(Dir, RC, Mx, L) :-
  ( Dir == s -> transpose(Mx, Mo), nth1(RC, Mo, L1), reverse(L1, L)
  ; Dir == n -> transpose(Mx, Mo), nth1(RC, Mo, L)
  ; Dir == e -> nth1(RC, Mx, L1), reverse(L1, L)
  ; Dir == w -> nth1(RC, Mx, L)
  ).


apply_visnum([], _).
apply_visnum([v(N, Dir, RC)|T], Mx) :-
  get_list(Dir, RC, Mx, L),
  visnum(L, N),
  apply_visnum(T, Mx).



split(0, L, [], L) :- !.
split(N, [H|T], [H|F1], B1) :-
  N > 0,
  N1 is N - 1,
  split(N1, T, F1, B1).


chop(N, L, R) :-
  length(L, Y),
  ( Y = 0 -> R = L
  ; N > Y -> R = [L]
  ).
chop(N, L, [F|R]) :-
  split(N, L, F, B),
  chop(N, B, R).


subgrids(In, K, Out) :-
  chop(K, In, Div1),
  maplist(transpose, Div1, Div2),
  append(Div2, Div3),
  chop(K, Div3, Div4),
  maplist(append, Div4, Out).

shave_all(X) :-
  fd_set(X, FD),
  fdset_to_list(FD, L),
  findall(X, member(X,L), Vs),
  list_to_fdset(Vs, FD1),
  X in_set FD1.

% Label the variables in Vars; after every Nth value assignment,
% shave the domain of variable X
labeling_with_shaving(X, N, Vars) :-
  bb_put(i, 0), % assign the value 0 to the blackboard variable i
  labeling([value(shave_during_labeling(X, N))],Vars).
% Auxiliary predicate, called by labeling in every iteration.
% X and N are given in the call to labeling, V is the next variable
shave_during_labeling(X, N, V, _Rest, _BB0, _BB) :-
  labeling([],[V]),
  bb_get(i,I), % read the value of blackboard variable i into I
  ( I<N -> I1 is I+1, bb_put(i, I1)
  ; shave_all(X), bb_put(i, 0)
  ).


skysudoku(ss(K, L), Mx) :-
  N is K*K,
  set_vars(Mx, N),
  separate_elements(L, G, V),
  set_known_ints(G, Mx),
  apply_visnum(V, Mx),
  maplist(all_distinct, Mx),
  transpose(Mx, Cols),
  maplist(all_distinct, Cols),
  subgrids(Mx, K, Sub),
  maplist(all_distinct, Sub),
  maplist(labeling_with_shaving(1, 5), Mx).
