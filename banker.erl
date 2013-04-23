-module(banker).
-export([start/1, status/0, attach/1, detach/0, request/1, release/1, loop/2, test/0]).

%% Client %%

start(Capital) ->
	Banker = spawn(banker, loop, [Capital, []]),
	register(bank, Banker),
	Banker.

attach(Limit) ->
	bank ! {self(), attach_, Limit},
	receive
		Any -> Any
	end.

detach() ->
	bank ! {self(), detach_},
	receive
		Any -> Any
	end.

status() ->
	bank ! {self(), status_},
	receive
		Any -> Any
	end.

test() ->
	hi.

request(NUnits) ->
	bank ! {self(), request_, NUnits},
	receive
		Any -> Any
	end.

release(NUnits) ->
	bank ! {self(), release_, NUnits},
	receive
		Any -> Any
	end.

%% Server %%

loop(Capital, Clients) ->
	receive
		{Pid, attach_, Limit} ->
			Result = s_attach(Clients, Pid, Limit),
			case Result of
				{ok, NewClients} 	-> Pid ! {ok}, loop(Capital, NewClients);
				Any					-> Pid ! Any, loop(Capital, Clients)
			end;
		{Pid, detach_} ->
			Result = s_detach(Clients, Pid),
			case Result of
				{ok, NewClients} 	-> Pid ! {ok}, loop(Capital, NewClients);
				Any					-> Pid ! Any, loop(Capital, Clients)
			end;
		% {Pid, request_, NUnits} ->
		% {Pid, release_, NUnits} ->
		{Pid, status_} -> Pid ! {ok, s_status(Capital, Clients)}, loop(Capital, Clients)
	end.

% Atach a new client to the bank.
s_attach(Clients, Pid, Limit) ->
	Result = h_attach([], Clients, Pid, Limit),
	case Result of
		{error, _Message}	-> Result;
		NewClients 			-> {ok, NewClients}
	end.
h_attach(NewClients, [], Pid, Limit) -> [ {Pid, Limit, 0} | NewClients ];
h_attach(_NewClients, [{Pid, _L, _Loan} | _T], Pid, _Limit) -> {error, "Already attached."};
h_attach(NewClients, [H | T], Pid, Limit) ->
	h_attach([H | NewClients], T, Pid, Limit).

% Detach a client from the bank.
s_detach(Clients, Pid) ->
	Result = h_detach([], Clients, Pid),
	case Result of
		{error, _Message} 	-> Result;
		NewClients			-> {ok, NewClients}
	end.
h_detach(_NewClients, [], _Pid) -> {error, "Client not attached."};
h_detach(NewClients, [{Pid, _Limit, _Loan} | Tail], Pid) -> NewClients ++ Tail;
h_detach(NewClients, [H | T], Pid) ->
	h_detach([H | NewClients], T, Pid).

% Return the capital, cash on hand, and number of clients of the bank.
s_status(Capital, []) ->
	{Capital, Capital, 0};
s_status(Capital, Clients) ->
	{Capital, Capital - lists:sum([Loan || {_Client, _Limit, Loan} <- Clients]), length(Clients)}.