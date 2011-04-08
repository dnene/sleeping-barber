-module(barber).
-author('Dhananjay Nene').
-behaviour(gen_server).

-export([start_link/0, notify/0]).
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(ROOM_SIZE, 3).

start_link() ->
    gen_server:start_link(?MODULE, ?MODULE, [], []).
notify() ->
    gen_server:cast(?MODULE, notify).

init([]) ->
    io:format("starting barber~n"),
    {ok, idle}.

handle_cast(notify, idle) -> 
    % received a notification that a new customer is waiting
    % However our logic continues to process all waiting customer
    % until there are no more waiting customers. Thus a notification
    % is really necessary only when the barber is idle. The remaining
    % notifications can be safely flushed
    flush_notifies(),
    % process waiting customers until the queue is empty
    process_pending_customers(),
    % back to idle state
    {noreply, idle}.

flush_notifies() ->
    % scan mailbox
    receive
	notify -> 
	    % flush all other notify messages in the mailbox
	    flush_notifies();
	Message ->
	    % unexpected message received. Rather than
	    % letting it occupy the mailbox, just remove it
	    io:format(
	      "Unexpected message: ~p. Ignoring...~n",
	      [Message]),
	    flush_notifies()
    after 0 -> 
	    ok
    end.

process_pending_customers() ->
    % get next waiting customer
    case room:get_next() of
	% no waiting customers
	empty -> {reply, idle};
	% got next customer from the queue
	{customer, Id} ->
	    io:format("cutting hair of customer          ~p~n",[Id]),
	    % sleep for a random amount of time
	    timer:sleep(100 + random:uniform(399)),
	    % process any further waiting customers
	    process_pending_customers()
    end.


handle_call(_Message,_From, State) ->
    % There are no call messages being expected
    % If any received will be silently ignored
    {noreply, State}.

handle_info(_Message, State) ->
    % No info messages implemented. Silently ignore all
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.



