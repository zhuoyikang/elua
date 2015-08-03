#include "erl_nif.h"
#include "lua/src/lua.h"
#include "lua/src/lauxlib.h"
#include "lua/src/lualib.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>

ErlNifResourceType* RES_SYNC;
ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;

#define STACK_STRING_BUFF 255

#define STR_NOT_ENOUGHT_MEMORY "not_enough_memory"
#define STR_DO_FILE_ERROR "do_file_error"

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
    // printf("free res 0x%x\n" ,(unsigned int)obj);
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

    tracker = (Tracker*) enif_alloc(sizeof(Tracker));
    tracker->count = 0;
    *priv = (void*) tracker;

    return 0;
}

static ERL_NIF_TERM
sync_newstate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    lua_State *L;
    elua_t *res;

    L = luaL_newstate();
    if(L==NULL){
        return enif_make_tuple2(env, atom_error,enif_make_string(env, STR_NOT_ENOUGHT_MEMORY, ERL_NIF_LATIN1));
    }

    luaL_openlibs(L);

    res = enif_alloc_resource(RES_SYNC, sizeof(elua_t));
    if(res == NULL) return enif_make_badarg(env);

    // printf("alloc res 0x%x\n" ,(unsigned int)res);

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->L = L;

    return enif_make_tuple2(env, atom_ok, ret);
};

static ERL_NIF_TERM
sync_dofile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

    // second arg: file_path
    if(enif_get_string(env, argv[1], buff_str, STACK_STRING_BUFF, ERL_NIF_LATIN1)<=0)
    {
        return enif_make_badarg(env);
    }

    if(luaL_dofile(res->L, buff_str) !=LUA_OK) {
        const char *error = lua_tostring(res->L, -1);
        ERL_NIF_TERM error_term = enif_make_string(env, error, ERL_NIF_LATIN1);
        lua_pop(res->L,1);
        return enif_make_tuple2(env, atom_error, error_term);
    }

    return atom_ok;
}

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

#define FMT_AND_LIST_NO_MATCH  "fmt and list not match"
#define FMT_AND_RET_NO_MATCH  "fmt and ret not match"
#define FMT_WRONG  "fmt wrong"

static ERL_NIF_TERM
sync_gencall(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    elua_t *res;
    char buff_str[STACK_STRING_BUFF];
    char buff_fmt[STACK_STRING_BUFF];
    char buff_fun[STACK_STRING_BUFF/2];
    unsigned input_len=0;
    unsigned output_len=0;

    if(argc != 4)
    {
        return enif_make_badarg(env);
    }

    // first arg: ref
    if(!enif_get_resource(env, argv[0], RES_SYNC, (void**) &res))
    {
	return enif_make_badarg(env);
    }

    // second arg : function name
    if(enif_get_string(env, argv[1], buff_fun, STACK_STRING_BUFF/2, ERL_NIF_LATIN1)<=0)
    {
        return enif_make_badarg(env);
    }

    // third arg: format
    if(enif_get_string(env, argv[2], buff_fmt, STACK_STRING_BUFF, ERL_NIF_LATIN1)<=0)
    {
        return enif_make_badarg(env);
    }

    // fourth arg: list of input args
    if(!enif_is_list(env, argv[3]))
    {
        return enif_make_badarg(env);
    }

    input_len = strchr(buff_fmt, ':') - buff_fmt;
    output_len = strlen(buff_fmt) - input_len -1;

    // printf("input args %d output args %d fun %s\n", input_len, output_len, buff_fun);
    ERL_NIF_TERM head,tail,list;
    list=argv[3];

    int i=0, status = 0, ret;
    ERL_NIF_TERM return_list = enif_make_list(env, 0);
    lua_getglobal(res->L, buff_fun);
    const char *error;

    while(buff_fmt[i]!='\0') {
        // printf("i:%d %c\n", i, buff_fmt[i]);

        if(status==0 && buff_fmt[i]!=':') {
            ret = enif_get_list_cell(env, list, &head, &tail);
            if(!ret) {
                error = FMT_AND_LIST_NO_MATCH;
                goto error;
            }
            list=tail;
        }

        switch(buff_fmt[i]) {
        case ':' :
            status=1;
            if(lua_pcall(res->L, input_len, output_len,0) != LUA_OK) {
                error = lua_tostring(res->L, -1);
                lua_pop(res->L,1);
                return enif_make_tuple2(env, atom_error, enif_make_string(env, error, ERL_NIF_LATIN1));
            }
            //output_len = - 1;
            break;
        case 'i':
            if( status == 0) {
                int input_int;
                ret = enif_get_int(env, head, &input_int);
                // printf("input %d\n", input_int);
                if(!ret) {
                    error = FMT_AND_LIST_NO_MATCH;
                    goto error;
                }

                lua_pushinteger(res->L, input_int);
            } else if ( status==1 ){
                int isnum;
                int n = lua_tointegerx(res->L, -1, &isnum);
                if(!isnum){
                    error = FMT_AND_LIST_NO_MATCH;
                    goto error;
                }
                // printf("output %d %d\n", output_len, n);
                return_list = enif_make_list_cell(env, enif_make_int(env, n), return_list);
                lua_pop(res->L,1);
                output_len--;
            }
            break;
        case 's':
            if( status == 0) {
                ret = enif_get_string(env, head, buff_str, STACK_STRING_BUFF, ERL_NIF_LATIN1);
                if(ret<=0) {
                    error = FMT_AND_LIST_NO_MATCH;
                    goto error;
                }
                lua_pushstring(res->L, buff_str);

            } else if ( status==1 ) {
                const char *s = lua_tostring(res->L, -1);
                if (s==NULL) {
                    error = FMT_AND_RET_NO_MATCH;
                    goto error;
                }
                // printf("output %d %s\n", output_len, s);
                return_list = enif_make_list_cell(env, enif_make_string(env, s, ERL_NIF_LATIN1), return_list);
                lua_pop(res->L,1);
                output_len--;
            }
            break;
        /* case 'd': */
        /*     break; */
        /* case 'b': */
        /*     break; */
        default:
            error = FMT_WRONG;
            goto error;
            break;
        }

        i++;
    }
    return enif_make_tuple2(env, atom_ok, return_list);

 error:
    // printf("in error \n");
    // @fix clean the heap var.
    // before call, pop the call
    if(status ==0 ) {
        lua_pop(res->L, 1);
    }
    // after call ,pop the left ret.
    else if(output_len>0) {
        lua_pop(res->L, output_len);
        // printf("would not be here %d \n", output_len);
    }

    return enif_make_tuple2(env, atom_error, enif_make_string(env, error, ERL_NIF_LATIN1));
}

static ERL_NIF_TERM
sync_gencast(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return atom_ok;
}

static ErlNifFunc nif_funcs[] = {
    {"newstate", 0, sync_newstate},
    {"dofile", 2, sync_dofile},
    {"getglobal", 2, sync_getglobal},
    {"pushstring", 2, sync_pushstring},
    {"call", 3, sync_call},
    {"gencall", 4, sync_gencall},
    {"gencast", 4, sync_gencast},
};

ERL_NIF_INIT(elua, nif_funcs, &load, NULL, NULL, NULL);
