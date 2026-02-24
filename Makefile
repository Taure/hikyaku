.PHONY: compile test fmt check

compile:
	rebar3 compile

test:
	rebar3 eunit

fmt:
	rebar3 fmt

check:
	rebar3 fmt --check
	rebar3 xref
	rebar3 dialyzer
