#include "erl_nif.h"
#include "lua/src/lua.h"
#include "lua/src/lauxlib.h"
#include "lua/src/lualib.h"
#include <stdio.h>

ErlNifResourceType* RES_SYNC;
ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_not_enough_memory;
ERL_NIF_TERM atom_do_file_error;

#define STACK_STRING_BUFF 255

typedef struct
{
    int count;
} Tracker;

typedef struct
{
    int id;
    lua_State *L;
} elua_t;

void
free_res(ErlNifEnv* env, void* obj)
{
    printf("free res 0x%x\n" ,(unsigned int)obj);
    elua_t *res = (elua_t *)obj;
    lua_close(res->L);
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    const char* mod = "elua";
    const char* name = "sync";
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    Tracker* tracker;

    RES_SYNC = enif_open_resource_type(env, mod, name, free_res, flags, NULL);
    if(RES_SYNC == NULL) return -1;

    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_not_enough_memory = enif_make_atom(env, "not_enough_memory");
    atom_do_file_error =enif_make_atom(env, "atom_do_file_error");

    tracker = (Tracker*) enif_alloc(sizeof(Tracker));
    tracker->count = 0;
    *priv = (void*) tracker;

    return 0;
}

static ERL_NIF_TERM
sync_new_state(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    lua_State *L;
    elua_t *res;
    char buff_str[STACK_STRING_BUFF];

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(enif_get_string(env, argv[0], buff_str, STACK_STRING_BUFF, ERL_NIF_LATIN1)<=0)
    {
        return enif_make_badarg(env);
    }

    L = luaL_newstate();
    if(L==NULL){
        return enif_make_tuple2(env, atom_error, atom_not_enough_memory);
    }

    luaL_openlibs(L);

    if(luaL_dofile(L, buff_str) !=0) {
        lua_close(L);
        return enif_make_tuple2(env, atom_error, atom_do_file_error);
    }

    res = enif_alloc_resource(RES_SYNC, sizeof(elua_t));
    if(res == NULL) return enif_make_badarg(env);

    printf("alloc res 0x%x\n" ,(unsigned int)res);

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->L = L;

    return enif_make_tuple2(env, atom_ok, ret);
};


static ERL_NIF_TERM
sync_getglobal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    elua_t *res;
    char buff_str[STACK_STRING_BUFF];

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    // first arg: ref
    if(!enif_get_resource(env, argv[0], RES_SYNC, (void**) &res))
    {
	return enif_make_badarg(env);
    }

    // second arg: atom of table name
    if(enif_get_string(env, argv[1], buff_str, STACK_STRING_BUFF, ERL_NIF_LATIN1)<=0)
    {
        return enif_make_badarg(env);
    }

    // call
    lua_getglobal(res->L, buff_str);

    return atom_ok;
};


static ERL_NIF_TERM
sync_pushstring(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    elua_t *res;
    char buff_str[STACK_STRING_BUFF];

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    // first arg: ref
    if(!enif_get_resource(env, argv[0], RES_SYNC, (void**) &res))
    {
	return enif_make_badarg(env);
    }

    // second arg: atom of table name
    if(enif_get_string(env, argv[1], buff_str, STACK_STRING_BUFF, ERL_NIF_LATIN1)<=0)
    {
        return enif_make_badarg(env);
    }

    lua_pushstring(res->L, buff_str);
    return atom_ok;
};


static ERL_NIF_TERM
sync_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    elua_t *res;
    unsigned int in_args, out_args;

    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    // first arg: ref
    if(!enif_get_resource(env, argv[0], RES_SYNC, (void**) &res))
    {
	return enif_make_badarg(env);
    }

    if(!enif_get_uint(env, argv[1], &in_args))
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint(env, argv[2], &out_args))
    {
        return enif_make_badarg(env);
    }

    lua_call(res->L, in_args, out_args);

    return atom_ok;
};


static ErlNifFunc nif_funcs[] = {
    {"new_state", 1, sync_new_state},
    {"getglobal", 2, sync_getglobal},
    {"pushstring", 2, sync_pushstring},
    {"call", 3, sync_call}
};

ERL_NIF_INIT(elua, nif_funcs, &load, NULL, NULL, NULL);
