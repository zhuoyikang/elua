default:
	rebar clean compile eunit

t:
	rebar compile
	rebar eunit


# sync:
# 	erl -pa ebin -q  -eval 'elua_block:sync()'

# async:
# 	erl -pa ebin -q  -eval 'elua_block:async()'

# nothing:
# 	erl -pa ebin -q  -eval 'elua_block:nothing()'


.PHONY: t
