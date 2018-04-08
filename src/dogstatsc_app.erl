%%%-------------------------------------------------------------------
%% @doc dogstatsc public API
%% @end
%%%-------------------------------------------------------------------

-module(dogstatsc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("dogstatsc.hrl").

%% API
start(_StartType, _StartArgs) ->
    Ret = dogstatsc_sup:start_link(),

    dogstatsc_pool:new(),

    ConnNum = application:get_env(dogstatsc, conn_num, ?DEFAULT_CONN_NUM),
    register_conns(ConnNum),

    Ret.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
register_conns(0) -> ok;
register_conns(ConnNum) ->
    % to use the remainder by dividing unixtime by worker num as a key
    NextNum = ConnNum - 1,
    supervisor:start_child(dogstatsc_sup, []),
    register_conns(NextNum).
