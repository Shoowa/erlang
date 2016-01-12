-module(ring2).
-compile(export_all).

%% This ring can't terminate every process and can't remember each node.
%% Leftover messages spill onto the firstNode.	

nodeMaker() ->
	receive
		{info, M, N, MSG} when N > 0 ->
			io:format("~w reporting: ~w~n", [self(), MSG]),
			Child = spawn(ring2, nodeMaker, []),
			Child ! {info, M-1, N-1, MSG},
			nodeMaker();
		{info, M, N, MSG} when N =:= 0 ->
			io:format("~w reporting: ~w~n", [self(), MSG]),
			firstNode ! {complete, M-1, MSG},
			nodeMaker();
		{complete, M, MSG} when M > 0 ->
			io:format("Process ~w reporting the ~w transmission: ~w~n", [self(), M, MSG]),
			firstNode ! {complete, M-1, MSG},
			nodeMaker();
		{complete, M, MSG} when M =:= 0 ->
			io:format("no more messages of this type: ~w~n", [MSG]),
			true
	end.

start(M, N, MSG) ->
	register(firstNode, spawn(ring2, nodeMaker, [])),
	firstNode ! {info, M, N-1, MSG}.