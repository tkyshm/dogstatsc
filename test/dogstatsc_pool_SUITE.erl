%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2018, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2018-04-08 23:34:10.542221
%%%-------------------------------------------------------------------
-module(dogstatsc_pool_SUITE).


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
         t_pool/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
     {group, test}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
     {test, [], [
                 t_pool
                ]}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
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
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================
t_pool(Config) ->
    dogstatsc_pool:new(),
    ?assertEqual(true, dogstatsc_pool:insert(self())),
    ?assertEqual(true, dogstatsc_pool:insert(self())),

    true = dogstatsc_pool:insert(self()),
    ?assertEqual(self(), dogstatsc_pool:get_conn()),

    Config.
