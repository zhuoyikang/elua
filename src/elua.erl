-module(elua).
-on_load(init/0).

-export([
         newstate_sync/0,
         newstate_async/0,
         newstate_async_nif/2,

         dofile_sync/2,
         dofile_async/2,
         dofile_async_nif/4,

         gencall_sync/4,
         gencall_async/4,
         gencall_async_nif/6,

         gencast/4
        ]).

-define(APPNAME,elua).
-define(LIBNAME,elua).
-define(LUANAME,lua).


receive_answer(Ref, Timeout) ->
  Start = os:timestamp(),
  receive
    {elua, Ref, Resp} ->
      Resp;
    {elua, _, _}=StaleAnswer ->
      error_logger:warning_msg("Esqlite3: Ignoring stale answer ~p~n", [StaleAnswer]),
      PassedMics = timer:now_diff(os:timestamp(), Start) div 1000,
      NewTimeout = case Timeout - PassedMics of
                     Passed when Passed < 0 -> 0;
                     TO -> TO
                   end,
      receive_answer(Ref, NewTimeout)
  after Timeout ->
      throw({error, timeout, Ref})
  end.

newstate_async() ->
  Ref = make_ref(),
  ok = newstate_async_nif(Ref, self()),
  receive_answer(Ref, 100000).


dofile_async(L,FilePath) ->
  Ref = make_ref(),
  ok = dofile_async_nif(L,Ref,self(),FilePath),
  receive_answer(Ref, 100000).


gencall_async(L,Func,Format,InArgs) ->
  Ref = make_ref(),
  ok = gencall_async_nif(L,Ref,self(),Func,Format,InArgs),
  receive_answer(Ref, 100000).

init() ->
  SoName=case code:priv_dir(?APPNAME) of
           {error,bad_name} ->
             case filelib:is_dir(filename:join(["..",priv])) of
               true ->
                 filename:join(["..",priv,?LIBNAME]);
               _ ->
                 filename:join([priv,?LIBNAME])
             end;
           Dir ->
             filename:join(Dir,?LIBNAME)
         end,
  erlang:load_nif(SoName,0).

not_loaded(Line) ->
  exit({not_loaded,[{module,?MODULE},{line,Line}]}).


%% some lua api
newstate_sync() ->
  not_loaded(?LINE).

newstate_async_nif(_Ref,_Dest) ->
  not_loaded(?LINE).

dofile_sync(_L,_FilePath) ->
  not_loaded(?LINE).
dofile_async_nif(_L,_Ref,_Dest,_FilePath) ->
  not_loaded(?LINE).

gencall_sync(_L,_Func,_Format,_InArgs) ->
  not_loaded(?LINE).
gencall_async_nif(_L,_Ref,_Dest,_Func,_Format,_InArgs) ->
  not_loaded(?LINE).

%% general cast call lua function and ignore the result
gencast(_L,_Func,_Format,_InArgs) ->
  not_loaded(?LINE).
