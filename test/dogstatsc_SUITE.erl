%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2018, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2018-03-28 09:15:01.701243
%%%-------------------------------------------------------------------
-module(dogstatsc_SUITE).


%% API
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([
         t_send_events/1,
         t_send_metrics/1,
         t_send_service_check/1,
         t_restart_conn/1
        ]).

%-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").

-define(PROPTEST(M,F), true = proper:quickcheck(M:F())).

-define(MOCK_PORT, 24000).

all() ->
    [
     {group, test}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
     {test,
      [],
      [
       t_send_events,
       t_send_metrics,
       t_send_service_check,
       t_restart_conn
      ]}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    application:set_env(dogstatsc, port, 24000),
    application:set_env(dogstatsc, host, "127.0.0.1"),
    {ok, _} = application:ensure_all_started(dogstatsc),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(dogstatsc),
    ok.


%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
group(_Groupname) ->
    [].

init_per_group(_Groupname, Config) ->
    Config.

end_per_group(_Groupname, _Config) ->
    ok.


%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(_TestCase, Config) ->
    {ok, Mock} = dogstatsd_mock:start_link(?MOCK_PORT, self()),
    [{dogstatsd, Mock},{from, self()}|Config].

end_per_testcase(_TestCase, _Config) ->
    ok = dogstatsd_mock:stop(),
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================
t_send_events(Config) ->
    ok = dogstatsc:send_events("test.events", "text", #{}),

    receive
        {ok, _Res} -> ok
    after
        1000 ->
            ct:fail(timeout)
    end,

    Config.

t_send_metrics(Config) ->
    ok = dogstatsc:send_metrics("test.metrics", c, 0.1, #{}),

    receive
        {ok, _Res} -> ok
    after
        1000 ->
            ct:fail(timeout)
    end,

    Config.

t_send_service_check(Config) ->
    ok = dogstatsc:send_service_check("test.sc", status_ok, #{}),

    receive
        {ok, _Res} -> ok
    after
        1000 ->
            ct:fail(timeout)
    end,

    Config.

t_restart_conn(Config) ->
    TestReason =
    [
     normal,
     kill,
     something
    ],

    TestFn =
    fun(Reason) ->
            PreWorkers = get_workers(),
            [exit(W, Reason) || W <- PreWorkers],
            timer:sleep(50),
            PreWorkers =/= get_workers()
    end,

    [TestFn(R) || R<-TestReason],

    Config.


get_workers() ->
    [{workers, Workers}] = ets:lookup(dogstatsc_conn, workers),
    Workers.
