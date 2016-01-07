-module(functions).
-compile(export_all).

% Find the head of a list.
head([H|_]) -> H.

% Find the second item of a list.
second([_,X|_]) -> X.

same(X,X) ->
  true;
same(_,_) ->
  false.

right_age(X) when X >= 16 andalso X =< 104 ->
  true;
right_age(_) ->
  false.

beach(Temperature) ->
  case Temperature of
    {celsius, N} when N >= 20, N =< 45 ->
      'favorable';
    {kelvin, N} when N >= 293, N =< 318 ->
      'scientifically favorable';
    {fahrenheit, N} when N >= 68, N =< 113 ->
      'favorable in the US';
    _ ->
      'avoid beach'
    end.

fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).

len([]) -> 0;
len([_|T]) -> 1 + len([T]).

tail_fac(N) -> tail_fac(N, 1).
tail_fac(0, Accumulator) -> Accumulator;
tail_fac(N, Accumulator) when N > 0 -> tail_fac(N-1, N*Accumulator).

duplicate(0,_) -> [];
duplicate(N,Term) when N > 0 -> [Term|duplicate(N-1,Term)].

tail_duplicate(N,Term) -> tail_duplicate(N,Term,[]).
tail_duplicate(0,_,List) -> List;
tail_duplicate(N,Term,List) when N > 0 -> tail_duplicate(N-1, Term, [Term|List]).
