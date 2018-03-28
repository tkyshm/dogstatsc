%%% --------------------------------------
%%% @doc
%%% @end
%%% --------------------------------------
-module(dogstatsc).

-export([send_metrics/1,
         send_events/1,
         send_service_check/1]).

%% @doc
%% send
%% @end
-spec send_metrics(Metrics :: dogstatsc_datagra:metrics()) -> ok.
send_metrics(_Metrics) ->
    % TODO: 送信する
    ok.

%% @doc
%% send
%% @end
-spec send_events(Events :: dogstatsc_datagra:events()) -> ok.
send_events(_Events) ->
    ok.

%% @doc
%% send
%% @end
-spec send_service_check(SC :: dogstatsc_datagra:service_check()) -> ok.
send_service_check(_SC) ->
    ok.
