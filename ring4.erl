-module(ring4).
-compile(export_all).

nodeMessenger() ->
	receive
		{info, N, NodeList, Max, M, Message} when (M > 0) and (N =< Max) ->
			io:format("I am ~w and N is ~w and this is the message: ~w~n", [self(), N, Message]),
			Next = lists:nth(N, NodeList),
			Next ! {info, N+1, NodeList, Max, M-1, Message},
			nodeMessenger();
		{info, N, NodeList, Max, M, Message} when (M > 0) and (N > Max) ->
			io:format("Last node: ~w, and the message: ~w~n", [self(), Message]),
			hd(NodeList) ! {info, 2, NodeList, Max, M-1, Message},
			nodeMessenger();
		{info, _, NodeList, Max, 0, _} ->
			io:format("~w begins termination procedure.~n", [self()]),
			hd(NodeList) ! {quit, 2, NodeList, Max},
			nodeMessenger();
		{quit, N, NodeList, Max} when N =< Max ->
			io:format("~w terminating~n", [self()]),
			Next = lists:nth(N, NodeList),
			Next ! {quit, N+1, NodeList, Max},
			true;
		{quit, N, _, Max} when N > Max ->
			io:format("~w terminating~n", [self()]),
			true
	end.

start(M, N, Message) ->
	process_builder(M, N, Message, []).

process_builder(M, N, Message, NodeList) when N > 0 ->
	PID = spawn(ring4, nodeMessenger, []),
	process_builder(M, N-1, Message, [PID|NodeList]);
process_builder(M, 0, Message, NodeList) ->
	Max = length(NodeList),
	hd(NodeList) ! {info, 2, NodeList, Max, M, Message}.