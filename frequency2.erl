%% Exercise 5-2 from Erlang Programming by Cesarini.
%% A) Change this code to ensure only the client who requested a
%% frequency can de-allocate the same frequency. De-allocating a frequency that
%% actually hasn't been allocated shouldn't break the program.
%% HINT: use self() in the allocate & de-allocate functions.
%% B) Only stop the freq-server if no frequencies are allocated.
%% C) Limit a client to 3 frequencies.

-module(frequency2).
-export([start/0, stop/0, allocate/0, deallocate/1]).

%% Export INIT because START uses it, but list it separately,
%% because we don't want to call this code directly.  The functions grouped above
%% will be called directly by the client, but not INIT.
-export([init/0]).

%% Register the process ID with an atom called "frequency".
%% Using the same name for both the module and the registered process is
%% an accepted practice in Erlang.
start() ->
	register(frequency, spawn(frequency2, init, [])).

%% Assigns a tuple to a variable. A list and an empty list are inside the tuple.
init() ->
	Frequencies = {get_frequencies(), []},
	loop(Frequencies).

% Hard-coded list of frequencies
get_frequencies() ->
	[10, 11, 12, 13, 14, 15].

%% Client functions that hide all the dirty work.
stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

%% Hide message passing inside a function.
call(Message) ->
	frequency ! {request, self(), Message},
	receive
		{reply, Reply} -> Reply
	end.

%% The server's receive-evaluate loop.
loop(Frequencies) ->
	receive
		{request, PID, allocate} ->
			{NewFrequencies, Reply} = allocate(Frequencies, PID),
			reply(PID, Reply),
			loop(NewFrequencies);
		{request, PID, {deallocate, Freq}} ->
			NewFrequencies = deallocate(Frequencies, Freq, PID),
			reply(PID, ok),
			loop(NewFrequencies);
		{request, PID, stop} ->
			check_if_Allocated_is_empty(Frequencies, PID)
	end.

reply(PID, Reply) ->
	PID ! {reply, Reply}.

%% Internal help functions used to allocate and de-allocate the lists
allocate({[], Allocated}, _PID) ->
	{{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, PID) ->
	case does_PID_have_3_freqs(PID, Allocated, 3) of
		true -> { {[Freq|Free], Allocated}, {error, exceeded_limit} };
		false -> { {Free, [{Freq, PID}|Allocated]}, {ok, Freq} }
	end.

deallocate({Free, Allocated}, Freq, PID) ->
	case lists:member({Freq, PID}, Allocated) of
		true ->
			NewAllocated = lists:keydelete(Freq, 1, Allocated),
			{[Freq|Free], NewAllocated};
		false ->
			{Free, Allocated}
	end.

check_if_Allocated_is_empty({_, []}, PID) ->
	reply(PID, ok);
check_if_Allocated_is_empty({_, [_|_]}, PID) ->
	reply(PID, "No. At least one or more frequencies are still allocated.").

does_PID_have_3_freqs(PID, [{_, PID}|T], N) ->
	does_PID_have_3_freqs(PID, T, N-1);
does_PID_have_3_freqs(_, _, 0) ->
	true;
does_PID_have_3_freqs(_, [], _) ->
	false.
