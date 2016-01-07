-module(echo).
-export([go/0, loop/0]).

go() ->
  register(echo, spawn(echo, loop, [])),
  echo ! {self(), hello},
  receive
    {_PID, MSG} ->
      io:format("~w~n", [MSG])
    end.

loop() ->
  receive
    {From, MSG} ->
      From ! {self(), MSG},
      loop();
      stop ->
        true
      end.
