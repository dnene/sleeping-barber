-module(test).
-export([run_test/0]).

-define(CUSTOMERS, 20).

run_test() ->
    room:start(),
    barber:start(),
    send(1,?CUSTOMERS+1).

send(N,N) ->
    ok;
send(M,N) ->
    room:enter(M),
    timer:sleep(100 + random:uniform(99)),
    send(M+1,N).
