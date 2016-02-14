-module(test).
-compile(export_all).

inside(X, Set) ->
  case lists:member(X, Set) of
    true -> yeah;
    false -> naw
  end.
