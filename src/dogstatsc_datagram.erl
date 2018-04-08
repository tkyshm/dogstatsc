%%% @doc
%%%
%%% A DogStatsD datagram module.
%%%
%%% @end
%%% @reference
%%% See <a href="https://docs.datadoghq.com/developers/dogstatsd/#datagram-format"> Datagram  Format</a>
-module(dogstatsc_datagram).

-export([new_metrics/4,
         new_events/3,
         new_service_check/3,
         encode/1]).

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
-record(metrics, {
    name = undefined :: string() | undefined,
    value = undefined :: float() | undefined,
    type = c :: metric_type(),
    rate = undefined :: float() | undefined,
    tags = undefined :: tags() | undefined
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
    title = undefined :: string() | undefined,
    text = undefined :: string() | undefined,
    timestamp = 0 :: non_neg_integer(),
    hostname = undefined :: string() | undefined,
    aggregation_key = undefined :: string() | undefined,
    priority = normal :: priority(),
    source_type_name = undefined :: string() | undefined,
    alert_type = info :: alert_type(),
    tags = undefined :: tags() | undefined
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
    name = undefined :: string() | undefined,
    status = status_unknown :: status(),
    timestamp = 0 :: non_neg_integer(),
    hostname = undefined :: string() | undefined,
    tags = undefined :: tags() | undefined,
    message = undefined :: string() | undefined
}).


-export_type([metrics/0,
              events/0,
              service_check/0,
              request/0,
              status/0,
              metric_type/0]).

-type status() :: status_ok | status_warn | status_crit | status_unknown.
-type metric_type() :: c | g | ms | h | s.
-type alert_type() :: info | warning | error | success.
-type priority() :: normal | low.
-type raw_request() :: binary().
-type tags() :: [{atom()|string()|binary(), string()}].
-type request() :: metrics() | events() | service_check().

-opaque metrics() :: #metrics{}.
-opaque events() :: #events{}.
-opaque service_check() :: #service_check{}.

%% @doc
%% new a metrics request record
%% @end
-spec new_metrics(Name :: string(), Type :: metric_type(), Value :: float(), Opts :: map()) -> metrics().
new_metrics(Name, Type, Value, Opts) ->
    #metrics{
       name = Name,
       value = Value,
       type = Type,
       rate = maps:get(rate, Opts, 1.0),
       tags = maps:get(tags, Opts, undefined)}.

%% @doc
%% new a events request record
%% @end
-spec new_events(Name :: string(), Text :: string(), Opts :: map()) -> events().
new_events(Name, Text, Opts) ->
    #events{
       title = Name,
       text = Text,
       timestamp = maps:get(timestamp, Opts, os:system_time(seconds)),
       hostname = maps:get(hostname, Opts, undefined),
       aggregation_key = maps:get(aggregation_key, Opts, undefined),
       priority = maps:get(priority, Opts, normal),
       source_type_name = maps:get(source_type_name, Opts, undefined),
       alert_type = maps:get(alert_type, Opts, info),
       tags = maps:get(tags, Opts, undefined)}.

%% @doc
%% new a service check request record.
%% @end
-spec new_service_check(Name :: string(), Status :: status(), Opts :: map()) -> service_check().
new_service_check(Name, Status, Opts) ->
    #service_check{
       name = Name,
       status = validate_status(Status),
       hostname = maps:get(hostname, Opts, undefined),
       timestamp = maps:get(timestamp, Opts, os:system_time(seconds)),
       tags = maps:get(tags, Opts, undefined),
       message = maps:get(message, Opts, undefined)}.

%% @doc
%% encode request for DogStatsD
%% @end
-spec encode(request()) -> raw_request().
encode(Metric = #metrics{}) ->
    % metric.name:value|type|@sample_rate|#tag1:value,tag2
    encode_metric(Metric);
encode(Events = #events{}) ->
    encode_events(Events);
encode(SC = #service_check{}) ->
    encode_service_check(SC).

%% @doc
%% @private
%% encode metric
%% @end
encode_metric(#metrics{name = undefined}) -> throw(name_is_undefined);
encode_metric(#metrics{value = undefined}) -> throw(value_is_undefined);
encode_metric(#metrics{name = Name, value = Val, type = Typ, rate = undefined, tags = undefined}) ->
    list_to_binary(io_lib:format("~ts:~p|~p", [Name, Val, Typ]));
encode_metric(#metrics{name = Name, value = Val, type = Typ, rate = Rate, tags = undefined}) ->
    list_to_binary(io_lib:format("~ts:~p|~p|@~p", [Name, Val, Typ, Rate]));
encode_metric(#metrics{name = Name, value = Val, type = Typ, rate = undefined, tags = Tags}) ->
    Str = io_lib:format("~ts:~p|~p|@1.0~ts", [Name, Val, Typ, to_tags(Tags)]),
    list_to_binary(Str);
encode_metric(#metrics{name = Name, value = Val, type = Typ, rate = Rate, tags = Tags}) ->
    Str = io_lib:format("~ts:~p|~p|@~p~ts", [Name, Val, Typ, Rate, to_tags(Tags)]),
    list_to_binary(Str).

%% @doc
%% @private
%% encode events
%% @end
encode_events(#events{title = undefined}) -> throw(name_is_undefined);
encode_events(#events{text = undefined}) -> throw(value_is_undefined);
encode_events(#events{title = Title,
                      text = Txt,
                      timestamp = TS,
                      hostname = Hostname,
                      aggregation_key = AggrKey,
                      priority = Priority,
                      source_type_name = SrcTyp,
                      alert_type = AlertTyp,
                      tags = Tags}) ->

    TxtLen = length(Txt),
    TitleLen = length(Title),

    HeadList = lists:reverse(["_e{", TitleLen, ",", TxtLen, "}:", Title, "|", Txt, "|d:", TS]),

    WithHost =
    case Hostname of
        undefined ->
            HeadList;
        Hostname ->
            [Hostname, "|h:" | HeadList]
    end,

    WithAggrKey =
    case AggrKey of
        undefined ->
            WithHost;
        AggrKey ->
            [AggrKey, "|k:" | WithHost]
    end,

    WithPriority = [Priority, "|p:"| WithAggrKey],

    WithSrcType =
    case SrcTyp of
        undefined ->
            WithPriority;
        SrcTyp ->
            [SrcTyp, "|s:" | WithPriority]
    end,

    WithAlert = [AlertTyp, "|t:" | WithSrcType],

    WithTags =
    case Tags of
        undefined ->
            WithAlert;
        Tags ->
            [to_tags(Tags) | WithAlert]
    end,

    IOList = lists:reverse(WithTags),

    list_to_binary(lists:concat(IOList)).


%% @doc
%% @private
%% encode service_check
%% @end
encode_service_check(#service_check{name = undefined}) -> throw(name_is_undefined);
encode_service_check(#service_check{name = Name, status = Status, timestamp = TS, hostname = Hostname, tags = Tags, message = Msg}) ->
    EncodedStatus =
    case Status of
        status_ok -> 0;
        status_warn -> 1;
        status_crit -> 2;
        status_unknown -> 3
    end,

    HeadList = lists:reverse(["_sc|", Name, "|", EncodedStatus, "|d:", TS]),

    WithHost =
    case Hostname of
        undefined ->
            HeadList;
        Hostname ->
            [Hostname, "|h:" | HeadList]
    end,

    WithTags =
    case Tags of
        undefined ->
            WithHost;
        Tags ->
            [to_tags(Tags) | WithHost]
    end,

    WithMsg =
    case Msg of
        undefined ->
            WithTags;
        Msg ->
            [Msg, "|m:"|WithTags]
    end,

    IOList = lists:reverse(WithMsg),

    list_to_binary(lists:concat(IOList)).

%% @doc
%% @private
%% validate service check status
%% @end
validate_status(status_ok) -> status_ok;
validate_status(status_warn) -> status_warn;
validate_status(status_crit) -> status_crit;
validate_status(_) -> status_unknown.

to_tags(Tags) ->
    lists:concat(to_iolist_tags(lists:reverse(Tags), [])).

to_iolist_tags([{Key, Value}], Acc) ->
    ["|#", Key, ":", Value|Acc];
to_iolist_tags([{Key, Value}|Tags], Acc) ->
    to_iolist_tags(Tags, [",", Key, ":", Value|Acc]).
