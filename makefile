default:
	rebar clean compile eunit

t:
	rebar compile
	rebar eunit

c:
	erl -pa ebin -q -noshell -eval 'elua_crash:c()'


sync:
	erl -pa ebin -q -noshell -eval 'elua_block:sync()'


async:
	erl -pa ebin -q -noshell -eval 'elua_block:async()'


.PHONY: t
