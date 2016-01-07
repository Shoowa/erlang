-module(ch3ex).
-compile(export_all).

%% Exercise 3-1 A. Return sum of all integers between 1 and positive N.
sum(N) when N > 0 ->
  sum2(N,0).
sum2(Num, Sum) when Num > 0 ->
  sum2(Num-1, Sum+Num);
  sum2(0, Sum) -> Sum.

%% Exercise 3-1 B. Return sum of all integers between N & M.
sum(N, M) when N =< M ->
  sum3(N, M, 0).
sum3(Num, Max, Sum) when Num =< Max ->
  sum3(Num+1, Max, Sum+Num);
  sum3(_, _, Sum) -> Sum.

%% Exercise 3-2 A. Build a list containing N entries in proper order.
create(N) -> create_list(N, []).
create_list(Num, Accumulator) when Num > 0 ->
  create_list(Num-1, [Num|Accumulator]);
  create_list(0, Accumulator) -> Accumulator.

%% Exercise 3-2 B. Build a list containing N entries in reverse order.
reverse_create(N) -> rev_list(N, 1, []).
rev_list(Num, Sum, Accumulator) when Sum =< Num ->
  rev_list(Num, Sum+1, [Sum|Accumulator]);
  rev_list(_, _, Accumulator) -> Accumulator.

%% Exercise 3-3 A. Write a function that prints integers between 1 & N.

%% A uniparameter function interfaces for the user and the recursive biparameter function.
%% The default value of RECURSIVE_PRINT parameter-I is 1.
print(N) -> recursive_print(1, N).
recursive_print(I, Max) when I =< Max ->
  io:format("Number: ~w~n",[I]),
  recursive_print(I+1, Max);
%% When parameter-I surpasses the NMax value by one, print a newline. This acts as the "do-nothing" clause.
recursive_print(I, Max) when I > Max ->
  io:format("~n").

%% Exercise 3-3 B. Write a function that prints only even integers between 1 & N.
print_even(N) -> recursive_print_even(1, N).
recursive_print_even(I, Max) when I rem 2 =:= 0, I =< Max ->
  io:format("~w~n", [I]),
  recursive_print_even(I+1, Max);
recursive_print_even(I, Max) when I rem 2 =/= 0 ->
  recursive_print_even(I+1, Max);
recursive_print_even(I, Max) when I > Max ->
  io:format("~n").

%% Return a list of integers up to the N parameter.
filter(List, N) ->
  rec_filter(List, N, []).
rec_filter([H|T], N, Accumulator) when H =< N ->
  rec_filter(T, N, [H|Accumulator]);
  rec_filter([H|T], N, Accumulator) when H > N ->
    rec_filter(T, N, Accumulator);
    rec_filter([], _, Accumulator) ->
      reverse(Accumulator).

%% Return a reversed list.
reverse(List) ->
  rec_reverse(List, []).
rec_reverse([H|T], Accumulator) ->
  rec_reverse(T, [H|Accumulator]);
  rec_reverse([], Accumulator) ->
    Accumulator.

%% splitter - part of my initial solution to concatenating lists. Split the HEAD, but
%% in a separate function.
%split([H|T], Accumulator) ->
%  split(T, [H|Accumulator]);
%  split([], Accumulator) ->
%    Accumulator.

%% Join a list of lists. - also part of my initial solution to concatenating a list.
%concatenate(List) ->
%  reverse(rec_concat(List, [])).
%rec_concat([H|T], Accumulator) ->
%  rec_concat(T, split(H, Accumulator));
%  rec_concat([], Accumulator) ->
%    Accumulator.

concatenate([X|Xs]) ->
  concat(X, Xs, []).
concat([X|Xs], T, L) ->
  concat(Xs, T, [X|L]);
  concat([], [X|Xs], L) ->
    concat(X, Xs, L);
    concat([], [], L) ->
      reverse(L).

%% Stolen from stack exchange, and I stepped through it via the debugger. Ouch.
flatten(List) ->
  flatten(List, []).
flatten([H|T], Todo) ->
  flatten(H, [T|Todo]);
  flatten([], [H|Todo]) ->
    flatten(H, Todo);
    flatten([], []) ->
      [];
      flatten(E, Todo) ->
        [E|flatten(Todo, [])].
