all:
	./rebar compile skip_deps=true

full:
	./rebar compile

first:
	./rebar get-deps compile

up:
	./rebar update-deps

.PHONY:tags
tags:
	ctags -R

.PHONY:clean
clean:
	rebar clean skip_deps=true
