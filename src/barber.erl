-module(barber).
-author('Dhananjay Nene').
-behaviour(gen_server).

-export([start/0, notify/0]).
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(ROOM_SIZE, 3).

start() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).
notify() ->
    gen_server:cast(?MODULE, notify).

init([]) ->
    {ok, idle}.

handle_cast(notify, idle) -> 
    flush_notifies(),
    process_pending_customers(),
    {noreply, idle}.

flush_notifies() ->
    receive
	notify -> 
	    flush_notifies()
    after 0 -> 
	    ok
    end.

process_pending_customers() ->
    case room:get_next() of
	empty -> {reply, idle};
	{customer, Id} ->
	    io:format("cutting hair of customer          ~p~n",[Id]),
	    timer:sleep(100 + random:uniform(399)),
	    process_pending_customers()
    end.


handle_call(next,_From, Queue) ->
    % Barber is now free. Send him the next customer 
    case queue:out(Queue) of 
	{empty, _} -> {reply, empty, Queue};
	{{value,Id},Remaining} -> {reply,{customer,Id},Remaining}
    end.			    

handle_info(_Message, State) ->
    % No info messages implemented. Ignore all
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.



