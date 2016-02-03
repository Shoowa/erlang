-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
  register(mutex, spawn(?MODULE, init, [])).

stop() ->
  mutex ! stop.

wait() ->
  mutex ! {wait, self()},
  receive
    ok ->
      ok
  end.

signal() ->
  mutex ! {signal, self()}, ok.

init() ->
  free().

free() ->
  receive
    {wait, PID} ->
      PID ! ok,
      busy(PID);
    stop ->
      terminate()
  end.

busy(PID) ->
  receive
    {signal, PID} ->
      free()
  end.

terminate() ->
  receive
    {wait, PID} ->
      exit(PID, kill),
      terminate()
  after
    0 -> ok
  end.
