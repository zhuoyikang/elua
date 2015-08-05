-module(elua).
-on_load(init/0).

-export([
         newstate/0,
         dofile/2,
         dofile_nif/4,
         gencall/4,
         gencall_nif/6,
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

dofile(L,FilePath) ->
  Ref = make_ref(),
  ok = dofile_nif(L,Ref,self(),FilePath),
  receive_answer(Ref, 3000).

gencall(L,Func,Format,InArgs) ->
  Ref = make_ref(),
  ok = gencall_nif(L,Ref,self(),Func,Format,InArgs),
  receive_answer(Ref, 3000).

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
newstate() ->
  not_loaded(?LINE).

%% @doc xx
%%
%% Sends an asynchronous dofile command over the connection and returns
%% ok immediately. When the database is opened
%%
%%  @spec dofile_nif(lua_state(), reference(), pid(), string()) -> ok | {error, message()}
dofile_nif(_L,_Ref,_Dest,_FilePath) ->
  not_loaded(?LINE).

%% general call call lua function and get the result
%% format:  isb:i
%% input args is [int string binary]
%% output args is [int]
gencall_nif(_L,_Ref,_Dest,_Func,_Format,_InArgs) ->
  not_loaded(?LINE).

%% general cast call lua function and ignore the result
gencast(_L,_Func,_Format,_InArgs) ->
  not_loaded(?LINE).
