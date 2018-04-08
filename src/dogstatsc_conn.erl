%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2018, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2018-03-27 21:19:56.222689
%%%-------------------------------------------------------------------
-module(dogstatsc_conn).

-behaviour(gen_statem).

%% API
-export([start_link/2]).

%% gen_statem callbacks
-export([
         init/1,
         callback_mode/0,
         format_status/2,
         opened/3,
         closed/3,
         handle_event/4,
         terminate/3,
         code_change/4
        ]).

-define(SERVER, ?MODULE).

-define(UDP_OPTS, [binary]).

-record(state, {
    buffer = [] :: [binary()],
    sock = undefined :: inet:socket() | undefined,
    host = "localhost" :: string(),
    port = 0 :: inet:port_number()
}).

-type action() :: {{timeout, reopen}, non_neg_integer(), {reopen, integer()}}.
-type actions() :: [action()].
-type conn_state() :: #state{}.
-type request() :: term().

-type opened_event() :: cast.
-type closed_event() :: {timeout, open} | cast.

-type opened_request() :: {send, request()}.
-type closed_request() :: {send, request()} | open | {reopen, integer()}.

-type state() :: opened | closed.

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% Host: DogStatsD hostname
%% Port: DogStatsD port number
%%
%% @spec start_link(Host, Port) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Host, Port) ->
    gen_statem:start_link(?MODULE, [Host, Port], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Host, Port]) ->
    dogstatsc_pool:insert(self()),
    {ok, closed, #state{host = Host, port = Port}, [{next_event, cast, open}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.

callback_mode() -> state_functions.

%% @private
%%
%% @doc
%% @end
-spec opened(opened_event(), opened_request(), conn_state()) -> {next_state, state(), conn_state()} |
                                                                {next_state, state(), conn_state(), actions()}.
opened(cast, {send, Req}, State = #state{buffer = Buff, sock = Sock, host = Host, port = Port}) ->
    % io:format("request: ~p~n", [Req]),
    Data = dogstatsc_datagram:encode(Req),
    case send(Sock, Host, Port, lists:reverse(Buff), Data) of
        ok ->
            {next_state, opened, State#state{buffer = []}};
        {error, closed, NewBuff} ->
            error_logger:error_msg("send failed: closed"),
            {next_state, closed, State#state{buffer = [Data|NewBuff]}, reopen_actions(0)};
        {error, Reason, NewBuff} ->
            error_logger:error_msg("send failed: ~p", [Reason]),
            {next_state, closed, State#state{buffer = [Data|NewBuff]}}
    end.

%% @private
%%
%% @doc
%% @end
-spec closed(closed_event(), closed_request(), conn_state()) -> {next_state, state(), conn_state()} |
                                                                {next_state, state(), conn_state(), actions()}.
closed({timeout, open}, {reopen, N}, State) -> open_udp(N, State);
closed(cast, open, State) -> open_udp(0, State);
closed(cast, {send, Req}, State = #state{buffer = Buff}) ->
    Data = dogstatsc_datagram:encode(Req),
    {next_state, closed, State#state{buffer=[Data|Buff]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(_EventType, _EventContent, _StateName, State) ->
    NextStateName = the_next_state_name,
    {next_state, NextStateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, #state{sock = Sock}) ->
    dogstatsc_pool:delete(self()),
    gen_udp:close(Sock).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
open_udp(N, State) ->
    case gen_udp:open(0, ?UDP_OPTS) of
        {ok, Sock} ->
            {next_state, opened, State#state{sock = Sock}};
        {error, Reason} ->
            error_logger:error_msg("open_udp failed: ~p", [Reason]),
            {next_state, closed, State, reopen_actions(N+1)}
    end.

% send data and buffers.
send(Sock, Host, Port, [], Data) ->
    % io:format("data: ~p~n", [Data]),
    case gen_udp:send(Sock, Host, Port, Data) of
        ok -> ok;
        {error, Reason} -> {error, Reason, Data}
    end;
send(Sock, Host, Port, Buff = [H|Rest], Data) ->
    % io:format("buff: ~p~n", [Buff]),
    case gen_udp:send(Sock, Host, Port, H) of
        ok ->
            send(Sock, Host, Port, Rest, Data);
        {error, Reason} ->
            {error, Reason, Buff}
    end.

reopen_actions(N) ->
    [{{timeout, open}, backoff(N), {reopen, N}}].

backoff(1) -> 1000;
backoff(2) -> 1000;
backoff(3) -> 2000;
backoff(4) -> 3000;
backoff(5) -> 5000;
backoff(_N) -> 8000.
