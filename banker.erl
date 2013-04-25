% Author: Sam Lucidi
% banker.erl
% SE441 Concurrency: Banker's Algorithm
-module(banker).
-export([attach/1, detach/0, release/1, request/1,
     start/1, status/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% Client API %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Spawn a banker process with a starting capital of Capital,
% and register it to the atom 'banker'
start(Capital) ->
    Banker = spawn(fun () -> loop(Capital, [], []) end),
    register(bank, Banker),
    Banker.

% Request to attach calling process to the bank with a spending limit.
attach(Limit) ->
    bank ! {self(), attach_, Limit},
    receive
      {ok} ->
      io:format("Client ~p attached successfully with "
            "limit ~p.~n",
            [self(), Limit]);
      Any -> io:format("~p~n", [Any])
    end.

% Request to detach calling process from the bank.
detach() ->
    bank ! {self(), detach_},
    receive
      {ok} -> io:format("Client ~p detached.~n", [self()]);
      Any -> io:format("~p~n", [Any])
    end.

% Request that the bank's status be sent to the calling process.
status() ->
    bank ! {self(), status_},
    receive Any -> io:format("~p~n", [Any]) end.

% Request to withdraw NUnits from the bank.
% Will block if it would be unsafe to grant the request.
request(NUnits) ->
    bank ! {self(), request_, NUnits},
    receive Any -> io:format("~p~n", [Any]) end.

% Release NUnits of funds back to the bank.
release(NUnits) ->
    bank ! {self(), release_, NUnits},
    receive Any -> io:format("~p~n", [Any]) end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% Server %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Main loop of execution for the bank server.
% Respond to messages from clients forever.
loop(Capital, Clients, Defered) ->
    receive
      {Pid, attach_, Limit} ->
          Result = s_attach(Capital, Clients, Pid, Limit),
          case Result of
            {ok, NewClients} ->
                Pid ! {ok}, loop(Capital, NewClients, Defered);
                Any -> Pid ! Any, loop(Capital, Clients, Defered)
          end;
      {Pid, detach_} ->
          Result = s_detach(Clients, Pid),
          case Result of
            {ok, NewClients} ->
                Pid ! {ok},
                {ResolvedDeferals, NewDefered} = check_defered(
                                                                Capital,
                                                                NewClients,
                                                                Defered),
                loop(Capital, ResolvedDeferals, NewDefered);
            Any -> Pid ! Any, loop(Capital, Clients, Defered)
          end;
      {Pid, request_, NUnits} ->
          Result = s_request(Capital, Clients, Pid, NUnits),
          case Result of
            {ok, NewClients} ->
                Pid ! {ok}, loop(Capital, NewClients, Defered);
                % don't send a message when defering.
            {defer, Message} ->
                io:format("~p~n", [Message]),
                loop(Capital, Clients, [{Pid, NUnits} | Defered]);
            Any -> Pid ! Any, loop(Capital, Clients, Defered)
          end;
      {Pid, release_, NUnits} ->
          Result = s_release(Capital, Clients, Pid, NUnits),
          case Result of
            {ok, NewClients} ->
                Pid ! {ok},
                {ResolvedDeferals, NewDefered} = check_defered(
                                                                Capital,
                                                                NewClients,
                                                                Defered),
                loop(Capital, ResolvedDeferals, NewDefered);
            Any -> Pid ! Any, loop(Capital, Clients, Defered)
          end;
      {Pid, status_} ->
          Pid ! {ok, s_status(Capital, Clients)},
          loop(Capital, Clients, Defered)
    end.

% Check whether any defered unsafe requests
% from clients are now safe to grant, and
% grant them if possible.
check_defered(Capital, Clients, Defered) ->
    h_check_defered(Capital, Clients, Defered, []).

h_check_defered(_Capital, Clients, [], NewDefered) ->
    {Clients, NewDefered};
h_check_defered(Capital, Clients, [{Pid, NUnits} | T], NewDefered) ->
    {Pid, Limit, Loan} = lists:keyfind(Pid, 1, Clients),
    Remaining = lists:keydelete(Pid, 1, Clients),
    Result = check_safety(Capital,
              Remaining ++ [{Pid, Limit, Loan + NUnits}], NUnits),
    case Result of
      safe ->
          Pid ! {ok},
          io:format("Banker: Granting delayed request for ~p to ~p.~n", [NUnits, Pid]),
          h_check_defered(Capital,
                  Remaining ++ [{Pid, Limit, Loan + NUnits}], T, NewDefered);
      unsafe ->
          h_check_defered(Capital, Clients, T, [{Pid, NUnits} | NewDefered])
    end.

% Sort a list of clients by their remaining claim
sort_by_claim([]) -> [];
sort_by_claim(Clients) ->
    Sorter = fun ({ClientA, LimitA, LoanA},
          {ClientB, LimitB, LoanB}) ->
             {LimitA - LoanA, ClientA} =< {LimitB - LoanB, ClientB}
         end,
    lists:sort(Sorter, Clients).

% Determine the bank's cash on hand.
cash_on_hand(Capital, []) -> Capital;
cash_on_hand(Capital, Clients) ->
    Capital -
      lists:sum([Loan || {_Client, _Limit, Loan} <- Clients]).

% Determine whether a loan can be granted safely.
check_safety(_Capital, [], _NUnits) -> safe;
check_safety(Capital, Clients, NUnits) ->
    [{_Pid, Limit, Loan} | T] = sort_by_claim(Clients),
    case Limit - Loan =< cash_on_hand(Capital, Clients) of
      true -> check_safety(Capital, T, NUnits);
      false -> unsafe
    end.

% request handler. Attempt to grant money to a client,
% blocking the client if it would be unsafe to grant the request.
s_request(Capital, Clients, Pid, NUnits) ->
    Result = h_request(Capital, [], Clients, Pid, NUnits),
    case Result of
      {error, _Message} -> Result;
      {defer, _Message} -> Result;
      NewClients -> {ok, NewClients}
    end.

% request handler helper function.
h_request(_Capital, _NewClients, [], _Pid, _NUnits) ->
    {error, "Banker: Client not attached."};
h_request(Capital, NewClients,
      [{Pid, Limit, Loan} | Tail], Pid, NUnits) ->
    Result = check_safety(Capital,
              NewClients ++ [{Pid, Limit, Loan + NUnits}] ++ Tail,
              NUnits),
    case Result of
      safe ->
      NewClients ++ [{Pid, Limit, Loan + NUnits}] ++ Tail;
      unsafe -> {defer, "Banker: Defering unsafe request."}
    end;
h_request(Capital, NewClients, [H | T], Pid, NUnits) ->
    h_request(Capital, [H | NewClients], T, Pid, NUnits).

% release handler. Attempt to release a held loan.
s_release(Capital, Clients, Pid, NUnits) ->
    Result = h_release(Capital, [], Clients, Pid, NUnits),
    case Result of
      {error, _Message} -> Result;
      NewClients -> {ok, NewClients}
    end.

% release handler helper function.
h_release(_Capital, _NewClients, [], _Pid, _NUnits) ->
    {error, "Banker: Client not attached."};
h_release(_Capital, NewClients,
      [{Pid, Limit, Loan} | Tail], Pid, NUnits) ->
    NewClients ++ [{Pid, Limit, Loan - NUnits}] ++ Tail;
h_release(Capital, NewClients, [H | T], Pid, NUnits) ->
    h_release(Capital, [H | NewClients], T, Pid, NUnits).

% attach handler. Attach a new client process to the bank as long
% as their requested limit is not greater than the bank's capital,
% as that would be unsafe.
s_attach(Capital, _Clients, _Pid, Limit)
    when Limit > Capital ->
    {"error", "Client is too demanding."};
s_attach(_Capital, Clients, Pid, Limit) ->
    Result = h_attach([], Clients, Pid, Limit),
    case Result of
      {error, _Message} -> Result;
      NewClients -> {ok, NewClients}
    end.

% attach handler helper function.
h_attach(NewClients, [], Pid, Limit) ->
    [{Pid, Limit, 0} | NewClients];
h_attach(_NewClients, [{Pid, _L, _Loan} | _T], Pid,
     _Limit) ->
    {error, "Banker: Already attached."};
h_attach(NewClients, [H | T], Pid, Limit) ->
    h_attach([H | NewClients], T, Pid, Limit).

% detach handler. Detach a client from the bank, returning
% that client's funds to the bank's cash on hand.
s_detach(Clients, Pid) ->
    Result = h_detach([], Clients, Pid),
    case Result of
      {error, _Message} -> Result;
      NewClients -> {ok, NewClients}
    end.

h_detach(_NewClients, [], _Pid) ->
    {error, "Banker: Client not attached."};
h_detach(NewClients, [{Pid, _Limit, _Loan} | Tail],
     Pid) ->
    NewClients ++ Tail;
h_detach(NewClients, [H | T], Pid) ->
    h_detach([H | NewClients], T, Pid).

% status handler. Return the capital, cash on hand, and number of clients of the bank.
s_status(Capital, []) -> {Capital, Capital, 0};
s_status(Capital, Clients) ->
    {Capital, cash_on_hand(Capital, Clients),
     length(Clients)}.
