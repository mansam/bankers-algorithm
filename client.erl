-module(client).
-export([start/2]).

start(Limit, N) ->
	banker:attach(self, Limit),
	io:format("Attached ~p ~p~n", [N, self()]),
	receive
		Any -> io:format("~p", Any)
	end.