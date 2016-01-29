-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).

%% Export INIT because START uses it, but list it separately,
%% because we don't want to call this code directly.  The functions grouped above
%% will be called directly by the client, but not INIT.
-export([init/0]).

%% Register the process ID with an atom called "frequency".
%% Using the same name for both the module and the registered process is
%% an accepted practice in Erlang.
start() -> register(frequency, spawn(frequency, init, [])).

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
			NewFrequencies = deallocate(Frequencies, Freq),
			reply(PID, ok),
			loop(NewFrequencies);
		{request, PID, stop} ->
			reply(PID, ok)
	end.

reply(PID, Reply) ->
	PID ! {reply, Reply}.

%% Internal help functions used to allocate and de-allocate the lists
allocate({[], Allocated}, _PID) ->
	{{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, PID) ->
	{{Free, [{Freq, PID}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
	NewAllocated = lists:keydelete(Freq, 1, Allocated),
	{[Freq|Free], NewAllocated}.
