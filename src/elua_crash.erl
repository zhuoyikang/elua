-module(elua_crash).
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


c() ->
  LuaPath=luafile("block.lua"),
  N = 10,
  lists:foreach(fun(X) ->
                    spawn(fun() ->
                              io:format("fork ~p~n", [X]),
                              {ok,L}=elua:newstate_sync(),
                              catch elua:dofile_async(L,LuaPath),
                              io:format("end ~p~n", [X])
                          end)
                end,
                lists:seq(1,N)).
