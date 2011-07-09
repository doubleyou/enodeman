all:
	rebar compile

first:
	rebar get-deps compile

up:
	rebar update-deps
