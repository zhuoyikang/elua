-module(elua_block).
-compile([export_all]).


-define(APPNAME,elua).
-define(LIBNAME,elua).
-define(LUANAME,lua).

current_utc() ->
  {MegaSecs, Secs, _} = erlang:now(),
  MegaSecs * 1000000 + Secs.



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
  Count = erlang:system_info(schedulers),
  lock(Fun,Count).


statsc(TimeArray) ->
  {Min,Max,Sum} = lists:foldl(fun(T,{Max,Min,Sum}) ->
                                  Min2 = case  T < Min of
                                           true -> T;
                                           _ -> Min
                                         end,
                                  Max2 = case  T > Max of
                                           true -> T;
                                           _ -> Max
                                         end,
                                  Sum2= Sum+T,
                                  {Max2,Min2,Sum2}
                              end, {0,100000000,0}, TimeArray),
  {Min,Max,Sum/length(TimeArray)}.


lock(Fun,Count) ->
  io:format("Starting heartbeat.~n", []),
  {ok, _} = timer:apply_interval(500, ?MODULE, heart, []),
  timer:sleep(2000),
  Pid = self(),
  io:format("Locking the VM~n", []),
  Begin = current_utc(),
  lists:foreach(
    fun(_) -> spawn(fun() ->
                        {Time,_} = timer:tc(Fun),
                        Pid! {response, Time / 1000}
                    end) end,
    lists:seq(1, Count)
   ),
  Times = lists:map(fun(_) ->
                        receive
                          {response, Time} -> Time
                        end
                    end, lists:seq(1,Count)),
  io:format("~p", [Times]),
  io:format("finis ~p ~p~n", [current_utc() - Begin, statsc(Times)]).

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
          elua:dofile_async(L,LuaPath)
      end,
  lock(Fun),
  ok.

nothing() ->
  Fun=fun() ->
          ok
      end,
  lock(Fun),
  ok.


r400ms_a(N) ->
  Fun=fun() ->
          LuaPath=luafile("block_400ms.lua"),
          {ok,L}=elua:newstate(),
          elua:dofile_async(L,LuaPath)
      end,
  lock(Fun,N).


r400ms_s(N) ->
  Fun=fun() ->
          LuaPath=luafile("block_400ms.lua"),
          {ok,L}=elua:newstate(),
          catch elua:dofile_sync(L,LuaPath)
      end,
  lock(Fun,N).
