-module(client).

-export([start/2]).

% Spawn a new client process with a limit of Limit
% that will perform N banking activities before quitting.
start(Limit, N) ->
    spawn(fun () -> direct_start(Limit, N) end).

%
%
direct_start(Limit, N) ->
    banker:attach(Limit),
    io:format("Attached ~p ~p~n", [N, self()]),
    do_some_banking(Limit, 0, N),
    banker:detach(),
    io:format("Detached ~p ~p~n", [N, self()]).

%
%
%
do_some_banking(_Limit, _Loan, 0) -> {ok};
do_some_banking(Limit, 0, N) ->
    Request = random:uniform(Limit),
    io:format("~p[~p,~p] about to request ~p.~n",
          [self(), Limit, 0, Request]),
    banker:request(Request),
    do_some_banking(Limit, Request, N - 1);
do_some_banking(Limit, Limit, N) ->
    Release = random:uniform(Limit),
    io:format("~p[~p,~p] about to release ~p.~n",
          [self(), Limit, Limit, Release]),
    banker:release(Release),
    do_some_banking(Limit, Limit - Release, N - 1);
do_some_banking(Limit, Loan, N) ->
    Choice = random:uniform(2),
    case Choice of
      1 ->
          Request = random:uniform(Limit - Loan),
          io:format("~p[~p,~p] about to request ~p.~n",
                [self(), Limit, Loan, Request]),
          banker:request(Request),
          do_some_banking(Limit, Loan + Request, N - 1);
      2 ->
          Release = random:uniform(Loan),
          io:format("~p[~p,~p] about to release ~p.~n",
                [self(), Limit, Loan, Release]),
          banker:release(Release),
          do_some_banking(Limit, Loan - Release, N - 1)
    end.
