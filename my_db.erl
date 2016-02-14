%% Exercise 5-1 from Erlang Programming.

-module(my_db).
-compile(export_all).

start() ->
  register(database, spawn(my_db, ignite, [])).

ignite() ->
  echo:start(),
  DB = db:new(),
  loop(DB).

loop(DB) ->
  receive
    {write, Key, Element} ->
      NewDB = db:write(Key, Element, DB),
      loop(NewDB);
    {delete, Key} ->
      NewDB = db:delete(Key, DB),
      loop(NewDB);
    {read, Key} ->
      Result = db:read(Key, DB),
      echo:print(Result),
      loop(DB);
    {match, Element} ->
      Result = db:match(Element, DB),
      echo:print(Result),
      loop(DB);
    stop ->
      true
  end.

stop() ->
  database ! stop.

write(Key, Element) ->
  database ! {write, Key, Element}.

delete(Key) ->
  database ! {delete, Key}.

read(Key) ->
  database ! {read, Key}.

match(Element) ->
  database ! {match, Element}.
