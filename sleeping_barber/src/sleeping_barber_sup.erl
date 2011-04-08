%%%-------------------------------------------------------------------
%%% File    : sleeping_barber_sup.erl
%%% Author  : Dhananjay Nene <dhananjay.nene@gmail.com>
%%% Description : Supervisor for sleeping barber
%%%
%%% Created :  3 Apr 2011 by Dhananjay Nene <dhananjay.nene@gmail.com>
%%%-------------------------------------------------------------------
-module(sleeping_barber_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    RoomSpec = 
	{'room',
	 {'room',start_link,[] },
	 permanent,2000,worker,['room']
	},
    BarberSpec = 
	{'barber',
	 {'barber',start_link,[] },
	 permanent,2000,worker,['barber']
	},
    {ok,{{one_for_one,0,1}, [RoomSpec,BarberSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
