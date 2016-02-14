-module(log_handler).
-export([init/1, terminate/1, handle_event/2]).

init(File) ->
  {ok, FD} = file:open(File, write),
  FD.

terminate(FD) ->
  file:close(FD).

handle_event({Action, ID, Event}, FD) ->
  {MegaSec, Sec, MicroSec} = now(),
  Args = io:format(FD, "~w, ~w, ~w, ~w, ~w, ~p, ~n",
  [MegaSec, Sec, MicroSec, Action, ID, Event]),
  FD;
handle_event(_, FD) ->
  FD.
