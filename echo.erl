-module(echo).
-compile(export_all).

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

start() ->
	register(printer, spawn(echo, printerFunction, [])).
	
printerFunction() ->
	receive
		{print, MSG} ->
			io:format("~w~n", [MSG]),
			printerFunction();
		stop ->
			true
	end.
	
print(Term) ->
	printer ! {print, Term}.
	
stop() ->
	printer ! stop.