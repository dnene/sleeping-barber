-module(room).
-author('Dhananjay Nene').
-behaviour(gen_server).

-export([start_link/0, enter/1, get_next/0]).
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(ROOM_SIZE, 3).

start_link() ->
    gen_server:start_link(?MODULE, ?MODULE, [], []).
enter(CustomerId) ->
    gen_server:cast(?MODULE, {customer,CustomerId}).
get_next() ->
    gen_server:call(?MODULE, next).

init([]) ->
    io:format("Starting room~n"),
    {ok, queue:new()}.

handle_cast({customer,CustomerId}, Queue) ->    
    L = queue:len(Queue),
    if
	L >= ?ROOM_SIZE ->
	    io:format("turning away customer                ~p~n",
		     [CustomerId]),
	    %% error_logger:info_msg("turning away customer                ~p~n",
	    %% 	     [CustomerId]),
	    {noreply, Queue};
	true ->
	    io:format("seating customer in the waiting room ~p~n",
		     [CustomerId]),
	    barber:notify(),
	    {noreply, queue:in(CustomerId,Queue)}
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



