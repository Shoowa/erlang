-module(chapterone).
-compile(export_all).

%average(List) -> sum(List) / len(List).
average(List) ->
  average_acc(List, 0, 0).
average_acc([H|T], Sum, Len) ->
  average_acc(T, Sum+H, Len+1);
  average_acc([], Sum, Len) -> Sum/Len.

sum([]) -> 0;
sum([Head|Tail]) -> Head + sum(Tail).

len([]) -> 0;
len([_|Tail]) -> 1 + len(Tail).

even([]) -> [];
even([Head|Tail]) when Head rem 2 == 0 -> [Head |even(Tail)];
even([_|Tail]) -> even(Tail).

member(_,[]) -> false;
member(X, [X|_]) -> true;
member(X, [_|T]) -> member(X, T).

sum2([],Sum) -> Sum;
sum2([H|T],Sum) -> sum2(T,H+Sum).

%bump([]) -> [];
%bump([Head|Tail]) -> [Head + 1|bump(Tail)].

bump(L) -> bump_acc(L, []).
bump_acc([H|T], Acc) -> bump_acc(T, [H+1| Acc]);
bump_acc([], Acc) -> Acc.
