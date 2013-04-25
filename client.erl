-module(client).
-export([start/2]).

start(Limit, N) ->
	banker:attach(Limit),
	io:format("Attached ~p ~p~n", [N, self()]),
	for(N, fun(_) -> banker:status() end),
	banker:detach(),
	io:format("Detached ~p ~p~n", [N, self()]).

for(N, Fun) ->
	for(N, 0, Fun).
for(_N, _N, _Fun) -> ok;
for(N, Count, Fun) ->
	Fun(Count),
	for(N, Count + 1, Fun).

do_some_banking(Limit, Loan).
	Choice = random:uniform(2),
	case Choice of
		1 -> banker:request(), do_some_banking(Limit, Loan);
		2 -> banker:release(), do_some_banking(Limit, Loan)
	end.