.PHONY: elvis test bench

elvis:
	elvis --config elvis.config

dialyzer:
	rebar3 dialyzer

test: elvis dialyzer
	rebar3 ct -v --dir test
