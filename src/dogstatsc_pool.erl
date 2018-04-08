-module(dogstatsc_pool).

-export([new/0,
         get_conn/0,
         insert/1,
         delete/1]).

-define(POOL_KEY, workers).

-spec new() -> ets:tid() | atom().
new() -> ets:new(dogstatsc_conn, [named_table, public, set]).

-spec get_conn() -> pid().
get_conn() ->
    [{workers, Workers}] = ets:lookup(dogstatsc_conn, ?POOL_KEY),
    Index = os:system_time(nanosecond) rem length(Workers),
    lists:nth(Index+1, Workers).

-spec insert(Pid :: pid()) -> boolean().
insert(Pid) ->
    Workers =
    case ets:lookup(dogstatsc_conn, ?POOL_KEY) of
        [{workers, Wrks}] -> Wrks;
        [] -> []
    end,
    ets:insert(dogstatsc_conn, {?POOL_KEY, [Pid|Workers]}).

-spec delete(Pid :: pid()) -> boolean().
delete(Pid) ->
    [{workers, Workers}] = ets:lookup(dogstatsc_conn, ?POOL_KEY),
    ets:insert(dogstatsc_conn, {?POOL_KEY, lists:delete(Pid, Workers)}).
