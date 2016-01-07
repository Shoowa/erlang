%% Exercise 3-4 A. Write a module "db.erl" that can create a database
%% and create, read, and delete elements in it.
%% The destroy/1 function will delete the database.
%% Don't use the module Lists.
%% Create your own recursive functions.
%% Hint: Use lists & tuples as main data structures.
%% When testing, remember Erlang variables are single-assignment.
-module(db).
-compile(export_all).

%% Create an empty list.
new() -> [].

%% Add a tuple to the head of the list.
write(Key, Element, Db) ->
  [{Key, Element}|Db].

%% Retrieve a city that is paired with the matching KEY, otherwise skip through the list.
read(Key, [{Key, Value}|_]) ->
  {ok, Value};
  read(Key, [{_,_}|T]) ->
    read(Key, T);
    read(_, []) ->
      {error, instance}.

%% Retrieve the names of any person located in a city matching the KEY.
match(Key, Database) -> rec_match(Key, Database, []).
rec_match(Key, [{Name, Key}|T], Accumulator) ->
  rec_match(Key, T, [Name|Accumulator]);
  rec_match(Key, [{_,_}|T], Accumulator) ->
    rec_match(Key, T, Accumulator);
    rec_match(_, [], Accumulator) ->
      Accumulator.

%% Match a name to the KEY and exclude it from the new list built with all other tuples.
delete(Key, Db) -> rec_delete(Key, Db, []).
rec_delete(Key, [{Key,_}|T], Accumulator) ->
  rec_delete(Key, T, Accumulator);
  rec_delete(Key, [H|T], Accumulator) ->
    rec_delete(Key, T, [H|Accumulator]);
    rec_delete(_, [], Accumulator) ->
      Accumulator.

%% This function is a stand-in to make a consistent interface. It won't do anything.
destroy(_) -> ok.
