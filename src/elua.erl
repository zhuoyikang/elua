-module(elua).
-on_load(init/0).

-export([
         luafile/1,
         newstate/0,
         dofile/2,
         getglobal/2,
         pushstring/2,
         call/3,
         gencall/4,
         gencast/4
        ]).

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
      filename:join(Dir,?LUANAME,FileName)
  end.

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
dofile(_L,_FilePath) ->
  not_loaded(?LINE).
getglobal(_L,_Key) ->
  not_loaded(?LINE).
pushstring(_L,_String) ->
  not_loaded(?LINE).
call(_L,_InArgs,_OutArgs) ->
  not_loaded(?LINE).

%% general call call lua function and get the result
%% format:  isb:i
%% input args is [int string binary]
%% output args is [int]
gencall(_L,_Func,_Format,_InArgs) ->
  not_loaded(?LINE).

%% general cast call lua function and ignore the result
gencast(_L,_Func,_Format,_InArgs) ->
  not_loaded(?LINE).
