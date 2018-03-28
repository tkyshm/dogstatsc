-module(dogstatsc_datagram).

-export([new_metrics/3,
         new_events/3,
         new_service_check/3]).

%% @doc
%% https://docs.datadoghq.com/developers/dogstatsd/#datagram-format
%% Datagram Format
%%
%% Metrics
%%
%% metric.name:value|type|@sample_rate|#tag1:value,tag2
%%
%% metric.name — a string with no colons, bars, or @ characters. See the metric naming policy.
%% value — an integer or float.
%% type — c for counter, g for gauge, ms for timer, h for histogram, s for set.
%% sample rate (optional) — a float between 0 and 1, inclusive. Only works with counter, histogram, and timer metrics. Default is 1 (i.e. sample 100% of the time).
%% tags (optional) — a comma separated list of tags. Use colons for key/value tags, i.e. env:prod. The key device is reserved; Datadog drops a user-added tag like device:foobar.
%%
%% @end
-record(metrics, {
    metric = undefined :: binary() | undefined,
    value = undefined :: float() | undefined,
    type = c :: c | g | ms | h | s,
    rate = 1.0 :: float(),
    tags = undefined :: binary() | undefined
}).

%% Events
%%
%% https://docs.datadoghq.com/developers/dogstatsd/#events-1
%% _e{title.length,text.length}:title|text|d:timestamp|h:hostname|p:priority|t:alert_type|#tag1,tag2
%%
%% _e - The datagram must begin with _e
%% title — Event title.
%% text — Event text. Insert line breaks with an escaped slash (\\n)
%% |d:timestamp (optional) — Add a timestamp to the event. Default is the current Unix epoch timestamp.
%% |h:hostname (optional) - Add a hostname to the event. No default.
%% |k:aggregation_key (optional) — Add an aggregation key to group the event with others that have the same key. No default.
%% |p:priority (optional) — Set to ‘normal’ or ‘low’. Default ‘normal’.
%% |s:source_type_name (optional) - Add a source type to the event. No default.
%% |t:alert_type (optional) — Set to ‘error’, ‘warning’, ‘info’ or ‘success’. Default ‘info’.
%% |#tag1:value1,tag2,tag3:value3... (optional)— The colon in tags is part of the tag list string and has no parsing purpose like for the other parameters. No default.
%%
-record(events, {
    title = undefined :: binary() | undefined,
    text = undefined :: binary() | undefined,
    timestamp = 0 :: non_neg_integer(),
    hostname = undefined,
    aggregation_key = undefined,
    priority = normal :: normal | low,
    source_type_name = undefined,
    alert_type = info :: info | warning | error | success,
    tags = <<>> :: binary()
}).

%% Service Check
%%
%% _sc|name|status|d:timestamp|h:hostname|#tag1:value1,tag2,tag3:value3,...|m:service_check_message
%%
%% _sc — the datagram must begin with _sc
%% name — Service check name.
%% status — Integer corresponding to the check status (OK = 0, WARNING = 1, CRITICAL = 2, UNKNOWN = 3).
%% d:timestamp (optional) — Add a timestamp to the check. Default is the current Unix epoch timestamp.
%% h:hostname (optional) — Add a hostname to the event. No default.
%% #tag1:value1,tag2,tag3:value3,... (optional) — The colon in tags is part of the tag list string and has no parsing purpose like for the other parameters.No default.
%% m:service_check_message (optional) — Add a message describing the current state of the service check. This field MUST be positioned last among the metadata fields. No default.
-record(service_check, {
    name = undefined :: binary() | undefined,
    status = 0 :: 0 | 1 | 2 | 3, % 0=OK, 1=warning, 2=critical, 3=unknown
    timestamp = 0 :: non_neg_integer(),
    hostname = undefined :: binary(),
    tags = <<>> :: binary(),
    message = <<>> :: binary()
}).


-export_type([metrics/0,
              events/0,
              service_check/0]).

-opaque metrics() :: #metrics{}.
-opaque events() :: #events{}.
-opaque service_check() :: #service_check{}.

%% @doc
%% @end
-spec new_metrics(Name :: binary(), Params :: map(), Opts :: map()) -> metrics() | {error, any()}.
new_metrics(Name, Params, Opts) ->
    % TODO: paramsをパース
    #metrics{
       metric = Name,
       value = maps:get(value, Params),
       type = maps:get(type, Params),
       rate = maps:get(rate, Opts, 1.0),
       tags = maps:get(tags, Opts, undefined)}.

%% @doc
%% @end
-spec new_events(Name :: binary(), Params :: map(), Opts :: map()) -> events() | {error, any()}.
new_events(Name, Params, Opts) ->
    #events{
       title = Name,
       text = maps:get(text, Params),
       timestamp = maps:get(timestamp, Opts, os:system_time(seconds)),
       hostname = maps:get(hostname, Opts, undefined),
       aggregation_key = maps:get(aggregation_key, Opts, undefined),
       priority = maps:get(priority, Opts, normal),
       source_type_name = map:get(source_type_name, Opts, undefined),
       alert_type = map:get(alert_type, Opts, info),
       tags = maps:get(tags, Opts, <<>>)}.

%% @doc
%% @end
-spec new_service_check(Name :: binary(), Params :: map(), Opts :: map()) -> service_check() | {error, any()}.
new_service_check(Name, Params, Opts) ->
    #service_check{
       name = Name,
       status = maps:get(status, Params),
       timestamp = maps:get(type, Opts, os:system_time(seconds)),
       tags = maps:get(rate, Opts, <<>>),
       message = maps:get(tags, Opts, <<>>)}.
