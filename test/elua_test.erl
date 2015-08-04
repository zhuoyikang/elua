-module(elua_test).

-include_lib("eunit/include/eunit.hrl").

-define(APPNAME,elua).
-define(LIBNAME,elua).
-define(LUANAME,lua).


luafile(FileName) ->
  case code:priv_dir(?APPNAME) of
    {error,bad_name} ->
      case filelib:is_dir(filename:join(["..",?LUANAME])) of
        true ->
          filename:join(["..",?LUANAME,FileName]);
        _ ->
          filename:join([?LUANAME,FileName])
      end;
    Dir ->
      filename:join([Dir,"../",?LUANAME,FileName])
  end.


%% test can find lua file or not?
luafile_test() ->
  P=luafile("lua_file_exist.lua"),
  ?assertEqual(true,filelib:is_file(P)).

%% luad file base test
lua_base_test()->
  LuaPath=luafile("lua_file_exist.lua"),
  {ok,L}=elua:newstate(),
  ok=elua:dofile(L,LuaPath),
  ok=elua:getglobal(L,"print"),
  ok=elua:pushstring(L,"Hello from Lua!"),
  ok=elua:call(L,1,0).


%% load lua file.
lua_dofile_test()->
  LuaPath=luafile("do_file_good.lua"),
  Lua1Path=luafile("do_file_bad.lua"),
  Lua2Path=luafile("do_file_bad2.lua"),
  {ok,L}=elua:newstate(),
  ok=elua:dofile(L,LuaPath),
  {error,_}=elua:dofile(L,Lua1Path),
  {error,_}=elua:dofile(L,Lua2Path),
  ok.

%% check stack over flow
lua_dofile_n_test() ->
  {ok,L}=elua:newstate(),
  LuaPath=luafile("do_file_good.lua"),
  Lua1Path=luafile("do_file_bad.lua"),
  Lua2Path=luafile("do_file_bad2.lua"),
  lists:foreach(fun(_X) ->
                    ok=elua:dofile(L,LuaPath),
                    {error,_}=elua:dofile(L,Lua1Path),
                    {error,_}=elua:dofile(L,Lua2Path)
                end,
                lists:seq(1,10000)),
  ok.

%% gen call test
gen_call_test()->
  LuaPath=luafile("gen_call.lua"),

  {ok,L}=elua:newstate(),

  %% load the t1.lua
  ok=elua:dofile(L,LuaPath),

  %% int
  {ok,[23]}=elua:gencall(L,"test_int","i:i",[23]),
  {ok,[5]}=elua:gencall(L,"test_int2","ii:i",[2,3]),
  {ok,[9]}=elua:gencall(L,"test_int3","iii:i",[2,3,4]),
  {ok,[14]}=elua:gencall(L,"test_int4","iiii:i",[2,3,4,5]),
  {ok,[2,3]}=elua:gencall(L,"test_int2_ret2","ii:ii",[2,3]),
  {ok,[2,3,4]}=elua:gencall(L,"test_int3_ret3","iii:iii",[2,3,4]),

  %% int no ret
  {ok,[]}=elua:gencall(L,"test_int3_ret3","i:",[2]),
  {ok,[23]}=elua:gencall(L,"test_int_input0_ret1",":i",[]),

  %% string
  {ok,["good"]}=elua:gencall(L,"test_string1","s:s",["good"]),
  {ok,["ab"]}=elua:gencall(L,"test_string2","ss:s",["a","b"]),
  {ok,["a","b"]}=elua:gencall(L,"test_string2_ret2","ss:ss",["a","b"]),

  %% string no ret
  {ok,[]}=elua:gencall(L,"test_string_no_ret","s:",["good"]),

  %% int string
  {ok,["aa"]}=elua:gencall(L,"test_int_string","is:s",[1,"a"]),
  {ok,["nice"]}=elua:gencall(L,"test_string_string0_ret1",":s",[]),

  %% make sure bad params call do not crash the erlang vm.
  ok.


gen_bad_call_test()->
  LuaPath=luafile("gen_call.lua"),

  {ok,L}=elua:newstate(),

  %% load the t1.lua
  ok=elua:dofile(L,LuaPath),

  %% int
  ?assertEqual({error, "fmt and list not match"}, elua:gencall(L,"test_int","s:i",[23])),
  ?assertEqual({error, "fmt and list not match"}, elua:gencall(L,"test_int2","is:i",[2,3])),
  ?assertEqual({ok,["9"]}, elua:gencall(L,"test_int3","iii:s",[2,3,4])),
  ?assertEqual({error,"fmt and list not match"}, elua:gencall(L,"test_int4","iisi:s",[2,3,4,5])),
  ?assertEqual({ok,[2,3]}, elua:gencall(L,"test_int2_ret2","ii:ii",[2,3])),
  ?assertEqual({error, "fmt and list not match"}, elua:gencall(L,"test_int3_ret3","isi:isi",[2,3,4])),

  %% int no ret
  ?assertEqual({ok,[]}, elua:gencall(L,"test_int3_ret3",":",[2])),
  ?assertEqual({error,"fmt and list not match"}, elua:gencall(L,"test_int_input0_ret1","f:i",[])),

  %% string
  ?assertEqual({error,"fmt and list not match"}, elua:gencall(L,"test_string1","i:s",["good"])),
  {error,_} = elua:gencall(L,"test_string2","s:i",["a","b"]),
  ?assertEqual({error,"fmt and list not match"}, elua:gencall(L,"test_string2_ret2","sis:ss",["a","b"])),


  %% string no ret
  ?assertEqual({error,"fmt and list not match"}, elua:gencall(L,"test_string_no_ret","i:",["good"])),

  %% int string
  ?assertEqual({error,"fmt and list not match"}, elua:gencall(L,"test_int_string","si:s",[1,"a"])),
  ?assertEqual({error, "fmt wrong"}, elua:gencall(L,"test_string_string0_ret1",":f",[])),

  ok.


%% test whether stack overflow
gen_call_times_test() ->
  LuaPath=luafile("gen_call.lua"),
  {ok,L}=elua:newstate(),
  ok=elua:dofile(L,LuaPath),
  lists:foreach(fun(_X) ->
                    Ret=elua:gencall(L,"test_int3_ret3","iii:iii",[2,3,4]),
                    ?assertEqual(Ret,{ok,[2,3,4]})
                end,
                lists:seq(1,1000000)),
  ok.


binary_test() ->
  LuaPath=luafile("pack_test.lua"),
  {ok,L}=elua:newstate(),
  ok=elua:dofile(L,LuaPath),
  ok.
