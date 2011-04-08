-module(scenario).
-export([run_scenario/0]).

-define(CUSTOMERS, 20).

run_scenario() ->
    send(1,?CUSTOMERS+1).

send(N,N) ->
    ok;
send(M,N) ->
    gen_server:cast(room, {customer,M}),
    io:format("Sending customer ~p~n",[M]),
    timer:sleep(100 + random:uniform(99)),
    send(M+1,N).
