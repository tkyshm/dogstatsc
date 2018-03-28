.PHONY: elvis test bench

elvis:
	elvis --config elvis.config

bench:
	rebar3 ct -v --dir test --suite dogstatsc_SUITE --group bench

dialyzer:
	rebar3 dialyzer

test: elvis dialyzer
	rebar3 ct -v --dir test --suite dogstatsc_SUITE --group test
