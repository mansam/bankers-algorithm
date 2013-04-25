-module(banker).
-export([start/1, status/0, attach/1, detach/0, request/1, release/1, loop/3, sort_by_claim/1, check_safety/3]).

%% Client %%

start(Capital) ->
	Banker = spawn(banker, loop, [Capital, [], []]),
	register(bank, Banker),
	Banker.

attach(Limit) ->
	bank ! {self(), attach_, Limit},
	receive
		{ok} -> io:format("Client ~p attached successfully with limit ~p.~n", [self(), Limit]);
		Any -> io:format("~p~n", [Any])
	end.

detach() ->
	bank ! {self(), detach_},
	receive
		{ok} -> io:format("Client ~p detached.~n", [ self() ] );
		Any -> io:format("~p~n", [Any])
	end.

status() ->
	bank ! {self(), status_},
	receive
		Any -> io:format("~p~n", [Any])
	end.

request(NUnits) ->
	bank ! {self(), request_, NUnits},
	receive
		Any -> io:format("~p~n", [Any])
	end.

release(NUnits) ->
	bank ! {self(), release_, NUnits},
	receive
		Any -> io:format("~p~n", [Any])
	end.

%% Server %%

loop(Capital, Clients, Defered) ->
	receive
		{Pid, attach_, Limit} ->
			Result = s_attach(Capital, Clients, Pid, Limit),
			case Result of
				{ok, NewClients} 	-> Pid ! {ok}, loop(Capital, NewClients, Defered);
				Any					-> Pid ! Any, loop(Capital, Clients, Defered)
			end;
		{Pid, detach_} ->
			Result = s_detach(Clients, Pid),
			case Result of
				{ok, NewClients} 	-> Pid ! {ok}, loop(Capital, NewClients, Defered);
				Any					-> Pid ! Any, loop(Capital, Clients, Defered)
			end;
		{Pid, request_, NUnits} ->
			Result = s_request(Capital, Clients, Pid, NUnits),
			case Result of
				{ok, NewClients}	-> Pid ! {ok}, loop(Capital, NewClients, Defered);
				Any					-> Pid ! Any, loop(Capital, Clients, Defered)
			end;
		{Pid, release_, NUnits} ->
			Result = s_release(Capital, Clients, Pid, NUnits),
			case Result of
				{ok, NewClients}	-> Pid ! {ok}, loop(Capital, NewClients, Defered);
				Any					-> Pid ! Any, loop(Capital, Clients, Defered)
			end;
		{Pid, status_} -> Pid ! {ok, s_status(Capital, Clients)}, loop(Capital, Clients, Defered)
	end.

sort_by_claim([]) -> [];
sort_by_claim(Clients) ->
	Sorter = fun({ClientA, LimitA, LoanA}, {ClientB, LimitB, LoanB}) -> {LimitA-LoanA, ClientA} =< {LimitB-LoanB, ClientB} end,
	lists:sort(Sorter, Clients).

cash_on_hand(Capital, []) -> Capital;
cash_on_hand(Capital, Clients) ->
	Capital - lists:sum([Loan || {_Client, _Limit, Loan} <- Clients]).

check_safety(_Capital, [], _NUnits) -> safe;
check_safety(Capital, Clients, NUnits) ->
	[{_Pid, Limit, Loan} | T] = sort_by_claim(Clients),
	case (Limit - Loan) =< cash_on_hand(Capital, Clients) of
		true	-> check_safety(Capital, T, NUnits);
		false	-> unsafe
	end.

s_request(Capital, Clients, Pid, NUnits) ->
	Result = h_request(Capital, [], Clients, Pid, NUnits),
	case Result of 
		{error, _Message}			-> Result;
		NewClients 					-> {ok, NewClients}
	end.
h_request(_Capital, _NewClients, [], _Pid, _NUnits) -> {error, "Client not attached."};
h_request(Capital, NewClients, [{Pid, Limit, Loan} | Tail], Pid, NUnits) ->
	Result = check_safety(Capital, NewClients ++ [{Pid, Limit, Loan + NUnits}] ++ Tail, NUnits),
	case Result of
		safe 	-> NewClients ++ [{Pid, Limit, Loan + NUnits}] ++ Tail;
		unsafe	-> {error, "Unsafe to grant request."}
	end;
h_request(Capital, NewClients, [H | T], Pid, NUnits) ->
	h_request(Capital, [H | NewClients], T, Pid, NUnits).

s_release(Capital, Clients, Pid, NUnits) ->
	Result = h_release(Capital, [], Clients, Pid, NUnits),
	case Result of 
		{error, _Message}			-> Result;
		NewClients 					-> {ok, NewClients}
	end.
h_release(_Capital, _NewClients, [], _Pid, _NUnits) -> {error, "Client not attached."};
h_release(_Capital, NewClients, [{Pid, Limit, Loan} | Tail], Pid, NUnits) ->
	NewClients ++ [{Pid, Limit, Loan - NUnits}] ++ Tail;
h_release(Capital, NewClients, [H | T], Pid, NUnits) ->
	h_release(Capital, [H | NewClients], T, Pid, NUnits).

% Atach a new client to the bank.
s_attach(Capital, _Clients, _Pid, Limit) when Limit > Capital ->
	{"error", "Client is too demanding."};
s_attach(_Capital, Clients, Pid, Limit) ->
	Result = h_attach([], Clients, Pid, Limit),
	case Result of
		{error, _Message}	-> Result;
		NewClients 			-> {ok, NewClients}
	end.
h_attach(NewClients, [], Pid, Limit) ->
	[ {Pid, Limit, 0} | NewClients ];
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
	{Capital, cash_on_hand(Capital, Clients), length(Clients)}.