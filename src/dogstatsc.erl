%%% --------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2018, tkyshm
%%% @doc
%%% dogstatsc is public client API module to send several reqeusts for DogStatsD.
%%% @end
%%% Created : 2018-03-28 09:15:01.701243
%%% --------------------------------------
-module(dogstatsc).

-export([send_metrics/3,
         send_metrics/4,
         send_events/2,
         send_events/3,
         send_service_check/2,
         send_service_check/3]).

-include("dogstatsc.hrl").

%% @doc
%% send metrics to DogStatsD.
%% @end
-spec send_metrics(Name :: string(), Typ :: dogstatsc_datagram:metric_type(), Val :: float()) -> ok.
send_metrics(Name, Type, Value) ->
    send_metrics(Name, Type, Value, #{}).

%% @doc
%% send metrics to DogStatsD with options.
%%
%% <pre>
%% Opts = #{
%%   rate = float() (0 to 1)
%%   tags = dogstatsc_datagra:tags() | undefined
%% }.
%% </pre>
%%
%% @see dogstatsc_datagram:metric_type()
%% @see dogstatsc_datagram:tags()
%%
%% @end
-spec send_metrics(Name :: string(), Typ :: dogstatsc_datagram:metric_type(), Val :: float(), Opts :: map()) -> ok.
send_metrics(Name, Typ, Val, Opts) ->
    Req = dogstatsc_datagram:new_metrics(Name, Typ, Val, Opts),
    gen_statem:cast(dogstatsc_pool:get_conn(), {send, Req}).

%% @doc
%% send events to DogStatsD.
%% @end
-spec send_events(Name :: string(), Text :: string()) -> ok.
send_events(Name, Text) ->
    send_events(Name, Text, #{}).

%% @doc
%% send events to DogStatsD with options.
%%
%% <pre>
%% Opts = #{
%%   timestamp = non_neg_integer() (epoch time)
%%   hostname = string()
%%   aggregation_key = string()
%%   priority = dogstatsc_datagram:priority()
%%   source_type_name = string()
%%   alert_type = dogstatsc_datagram:alert_type() (default: info)
%%   tags = dogstatsc_datagra:tags() | undefined
%% }
%% </pre>
%%
%% @see dogstatsc_datagram:priority()
%% @see dogstatsc_datagram:alert_type()
%% @see dogstatsc_datagram:tags()
%%
%% @end
-spec send_events(Name :: string(), Text :: string(), Opts :: map()) -> ok.
send_events(Name, Text, Opts) ->
    Req = dogstatsc_datagram:new_events(Name, Text, Opts),
    gen_statem:cast(dogstatsc_pool:get_conn(), {send, Req}).

%% @doc
%% send service check to DogStatsD.
%% @end
-spec send_service_check(Name :: string(), SC :: dogstatsc_datagra:service_check()) -> ok.
send_service_check(Name, SC) ->
    send_service_check(Name, SC, #{}).

%% @doc
%% send service check to DogStatsD with option.
%%
%% <pre>
%% Opts = #{
%%   timestamp = non_neg_integer() epoch time (sec)
%%   hostname = string()
%%   message = string() | undefined
%%   tags = dogstatsc_datagra:tags() | undefined
%% }
%% </pre>
%%
%% @see dogstatsc_datagram:tags()
%%
%% @end
-spec send_service_check(Name :: string(), SC :: dogstatsc_datagra:status(), Opts :: map()) -> ok.
send_service_check(Name, SC, Opts) ->
    Req = dogstatsc_datagram:new_service_check(Name, SC, Opts),
    gen_statem:cast(dogstatsc_pool:get_conn(), {send, Req}).
