-module(myring).
-compile(export_all).

start(Num) ->
  start_proc(Num, self()).

start_proc(0, PID) ->
  PID ! ok;

start_proc(Num, PID) ->
  NPID = spawn(?MODULE, start_proc, [Num-1, PID]),
  NPID ! ok,
  receive ok ->
    ok
  end.
