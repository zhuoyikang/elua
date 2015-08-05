-module(elua_block).
-compile([export_all]).


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


lock(Fun) ->
  io:format("Starting heartbeat.~n", []),
  {ok, _} = timer:apply_interval(500, ?MODULE, heart, []),
  timer:sleep(2000),
  io:format("Locking the VM~n", []),
  Count = erlang:system_info(schedulers),
  lists:foreach(
    fun(_) -> spawn(fun() -> Fun() end) end,
    lists:seq(1, Count)
   ),
  timer:sleep(3000),
  io:format(" finis~n").

heart() ->
  io:format("Tick~n", []).


%% 同步调用
sync() ->
  Fun=fun() ->
          io:format("x~n"),
          LuaPath=luafile("block.lua"),
          {ok,L}=elua:newstate(),
          ok=elua:dofile_sync(L,LuaPath)
      end,
  lock(Fun),
  ok.

%% 异步调用
async() ->
  Fun=fun() ->
          LuaPath=luafile("block.lua"),
          {ok,L}=elua:newstate(),
          catch elua:dofile_async(L,LuaPath)
      end,
  lock(Fun),
  ok.

nothing() ->
  Fun=fun() ->
          ok
      end,
  lock(Fun),
  ok.
