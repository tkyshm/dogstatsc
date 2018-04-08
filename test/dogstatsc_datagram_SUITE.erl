%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2018, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2018-03-29 09:10:59.078828
%%%-------------------------------------------------------------------
-module(dogstatsc_datagram_SUITE).

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
         t_encode_metrics/1,
         t_encode_events/1,
         t_encode_sc/1
        ]).

% -include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROPTEST(M,F), true = proper:quickcheck(M:F())).

all() ->
    [
     {group, test}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
     {test, [], [
                 t_encode_metrics,
                 t_encode_events,
                 t_encode_sc
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
t_encode_metrics(Config) ->
    % correct patterns
    Propers =
    [
     % count
     {"test.count", c, 1.0, #{}, <<"test.count:1.0|c|@1.0">>},
     % guage
     {"test.guage", g, 2.0, #{}, <<"test.guage:2.0|g|@1.0">>},
     % timer
     {"test.timer", ms, 3.0, #{}, <<"test.timer:3.0|ms|@1.0">>},
     % histogram
     {"test.histgram", h, 4.0, #{}, <<"test.histgram:4.0|h|@1.0">>},
     % full options
     {"test.fullopts", ms, 4.0, #{rate => 0.8, tags => [{key, val}]}, <<"test.fullopts:4.0|ms|@0.8|#key:val">>}
    ],

    TestFn =
    fun({Tag, Type, Val, Opts, Expect}) ->
            Req = dogstatsc_datagram:new_metrics(Tag, Type, Val, Opts),
            ?assertEqual(Expect, dogstatsc_datagram:encode(Req))
    end,

    [TestFn(T) || T <- Propers],

    Config.

t_encode_events(Config) ->
    Propers =
    [
     % No options
     {
      "no.options",
      "somt event",
      #{
        timestamp        => 1523061849
       },
      <<"_e{10,10}:no.options|somt event|d:1523061849|p:normal|t:info">>
     },
     % full options
     {
      "no.options",
      "somt event",
      #{
        timestamp        => 1523061849,
        hostname         => "localhost",
        aggregation_key  => "fullopts",
        priority         => low,
        source_type_name => "some_type",
        alert_type       => warning,
        tags             => [{tag1, val1}, {tag2, val2}, {tag3, val3}]
       },
      <<"_e{10,10}:no.options|somt event|d:1523061849|h:localhost|k:fullopts|p:low|s:some_type|t:warning|#tag1:val1,tag2:val2,tag3:val3">>
     }
    ],

    TestFn =
    fun({Tag, Event, Opts, Expect}) ->
            Req2 = dogstatsc_datagram:new_events(Tag, Event, Opts),
            ?assertEqual(Expect, dogstatsc_datagram:encode(Req2))
    end,

    [TestFn(T) || T <- Propers],

    Config.

t_encode_sc(Config) ->
    Propers =
    [
     % ok
     {
      "test.ok",
      status_ok,
      #{timestamp => 1523067311},
      <<"_sc|test.ok|0|d:1523067311">>
     },
     % warn
     {
      "test.warn",
      status_warn,
      #{timestamp => 1523067311},
      <<"_sc|test.warn|1|d:1523067311">>
     },
     % critical
     {
      "test.crit",
      status_crit,
      #{timestamp => 1523067311},
      <<"_sc|test.crit|2|d:1523067311">>
     },
     % unknown
     {
      "test.unknown",
      status_unknown,
      #{timestamp => 1523067311},
      <<"_sc|test.unknown|3|d:1523067311">>
     },
     % not exists status type
     {
      "test.notype",
      notype,
      #{timestamp => 1523067311},
      <<"_sc|test.notype|3|d:1523067311">>
     },
     % full options
     {
      "test.fullopts",
      status_ok,
      #{timestamp => 1523067311,
        hostname => "example.jp",
        tags => [{key, val}],
        message => "hello world"},
      <<"_sc|test.fullopts|0|d:1523067311|h:example.jp|#key:val|m:hello world">>
     }
    ],

    TestFn =
    fun({Tag, Status, Opts, Expect}) ->
            Req3 = dogstatsc_datagram:new_service_check(Tag, Status, Opts),
            ?assertEqual(Expect, dogstatsc_datagram:encode(Req3))
    end,

    [TestFn(T) || T <- Propers],

    Config.
