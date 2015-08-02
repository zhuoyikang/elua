-module(elua).
-on_load(init/0).

-export([
         luafile/1,
         new_state/1,
         getglobal/2,
         pushstring/2,
         call/3
        ]).

-define(APPNAME, elua).
-define(LIBNAME, elua).
-define(LUANAME, lua).

luafile(FileName) ->
  case code:priv_dir(?APPNAME) of
    {error, bad_name} ->
      case filelib:is_dir(filename:join(["..", ?LUANAME])) of
        true ->
          filename:join(["..", ?LUANAME, FileName]);
        _ ->
          filename:join([?LUANAME, FileName])
      end;
    Dir ->
      filename:join(Dir, ?LUANAME, FileName)
  end.

init() ->
  SoName = case code:priv_dir(?APPNAME) of
             {error, bad_name} ->
               case filelib:is_dir(filename:join(["..", priv])) of
                 true ->
                   filename:join(["..", priv, ?LIBNAME]);
                 _ ->
                   filename:join([priv, ?LIBNAME])
               end;
             Dir ->
               filename:join(Dir, ?LIBNAME)
           end,
  erlang:load_nif(SoName, 0).

not_loaded(Line) ->
  exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

new_state(_File) ->
  not_loaded(?LINE).

getglobal(_L, _Key) ->
  not_loaded(?LINE).

pushstring(_L,_String) ->
  not_loaded(?LINE).

call(_L, _InArgs, _OutArgs) ->
  not_loaded(?LINE).
