-module(elua_test).

-include_lib("eunit/include/eunit.hrl").

%% basic_test() ->
%%   ?assertEqual(0, elua:count()),
%%   {ok, Res} = elua:create(90),
%%   ?assertEqual(1, elua:count()),
%%   ?assertEqual(90, elua:read(Res)),
%%   Fun = fun() ->
%%             {ok, Res2} = elua:create(18),
%%             ?assertEqual(2, elua:count()),
%%             ?assertEqual(18, elua:read(Res2))
%%         end,
%%   Fun(),
%%   erlang:garbage_collect(),
%%   ?assertEqual(90, elua:read(Res)),
%%   ?assertEqual(1, elua:count()).

luafile_test() ->
  P = elua:luafile("t1.lua"),
  ?assertEqual(true, filelib:is_file(P)).

%% a example from https://github.com/raycmorgan/erl-lua
erl_lua_test()->
  T1FilePath = elua:luafile("t1.lua"),
  {ok, L} = elua:new_state(T1FilePath),
  ok = elua:getglobal(L,"print"),
  ok = elua:pushstring(L, "Hello from Lua!"),
  ok = elua:call(L, 1, 0).
