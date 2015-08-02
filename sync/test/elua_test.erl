-module(elua_test).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    ?assertEqual(0, elua:count()),
    {ok, Res} = elua:create(90),
    ?assertEqual(1, elua:count()),
    ?assertEqual(90, elua:read(Res)),
    Fun = fun() ->
        {ok, Res2} = elua:create(18),
        ?assertEqual(2, elua:count()),
        ?assertEqual(18, elua:read(Res2))
    end,
    Fun(),
    erlang:garbage_collect(),
    ?assertEqual(90, elua:read(Res)),
    ?assertEqual(1, elua:count()).
