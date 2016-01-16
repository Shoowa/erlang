-module(ring3).
-compile(export_all).

nodeMessenger() ->
	receive
		{info, [Next|RemainingPIDs], NodeList, M, Message} when M > 0 ->
			io:format("I am ~w and this is the message: ~w~n", [self(), Message]),
			Next ! {info, RemainingPIDs, NodeList, M-1, Message},
			nodeMessenger();
		{info, [], NodeList, M, Message} when M > 0 ->
			io:format("Last node: ~w, and the message: ~w~n", [self(), Message]),
			hd(NodeList) ! {info, tl(NodeList), NodeList, M-1, Message},
			nodeMessenger();
		{info, _, [TopNode|RemainingPIDs], 0, _} ->
			TopNode ! {quit, RemainingPIDs},
			nodeMessenger();
		{quit, [Next|RemainingPIDs]} ->
			Next ! {quit, RemainingPIDs},
			true;
		{quit, []} ->
			true
	end.

start(M, N, Message) ->
	process_builder(M, N, Message, []).

process_builder(M, N, Message, NodeList) when N > 0 ->
	PID = spawn(ring3, nodeMessenger, []),
	process_builder(M, N-1, Message, [PID|NodeList]);
process_builder(M, 0, Message, NodeList) ->
	hd(NodeList) ! {info, tl(NodeList), NodeList, M, Message}.