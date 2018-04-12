[![hex.pm version](https://img.shields.io/hexpm/v/ltsv.svg)](https://hex.pm/packages/dogstatsc)
[![Build Status](https://travis-ci.org/tkyshm/dogstatsc.svg?branch=master)](https://travis-ci.org/tkyshm/dogstatsc)

# [dogstatsc](https://hex.pm/packages/dogstatsc)

A [DogStatsD](https://docs.datadoghq.com/developers/dogstatsd/) client OTP application.

## Usage

- dogstatsc API
```console
% start dogstatsc application
> application:ensure_all_started(dogstatsc).
{ok, [dogstatc]}

% metrics
> dogstatsc:send_metrics("name.metrics.cpu", c, 1.0).
ok

% events
> dogstatsc:send_events("name.events", "event name text").
ok

% service check
> dogstatsc:send_service_check("name.sc.crit", status_crit).
ok
```

## Config

var      | description
-------- | -------------------------------------
port     | dogstatsd port (default: 8125)
host     | dogstatsd port (default: localhost)
conn_num | number of udp client workers (default: 2)

- sample config
```erlang
[
  {dogstatsc, [{port, 8125}, {host, 'localhost'}, {conn_num, 2}]}
].
```
