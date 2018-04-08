%%%-------------------------------------------------------------------
%% @doc dogstatsc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dogstatsc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-include("dogstatsc.hrl").

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{
      strategy  => simple_one_for_one,
      intensity => 1000,
      period    => 3600
     },

    Host = application:get_env(dogstatsc, host, "localhost"),
    Port = application:get_env(dogstatsc, port, ?DOGSTATSD_PORT),

    Spec = #{
      id       => 'dogstatsc_conn',
      start    => {'dogstatsc_conn', start_link, [Host, Port]},
      restart  => permanent,
      shutdown => 2000,
      type     => worker,
      modules  => ['dogstatsc_conn']
     },

    {ok, {SupFlags, [Spec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
