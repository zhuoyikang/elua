#include "erl_nif.h"
#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
// #include "epack.h"
#include "equeue.h"

ErlNifResourceType* RES_SYNC;
ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_elua;
ERL_NIF_TERM atom_error;

#define STACK_STRING_BUFF 255

#define STR_NOT_ENOUGHT_MEMORY "not_enough_memory"
#define STR_DO_FILE_ERROR "do_file_error"

#define WORKER_NO 7



typedef struct
{
    int id;
    lua_State *L;
} elua_t;


typedef enum
{
    msg_unknown,
    msg_dofile,
    msg_gencall,
    msg_stop
}msg_type;

typedef struct
{
    //lua_State *L;
    elua_t *res;

    msg_type type;

    ErlNifEnv *env;
    ERL_NIF_TERM ref;
    ErlNifPid pid;

    ERL_NIF_TERM arg1;
    ERL_NIF_TERM arg2;
    ERL_NIF_TERM arg3;

}msg_t;

static msg_t *msg_create() {
    ErlNifEnv *env;
    msg_t *msg;

    env = enif_alloc_env();
    if(env == NULL) {
        return NULL;
    }

    msg = (msg_t *) enif_alloc(sizeof(msg_t));
    if(msg == NULL) {
        enif_free_env(env);
        return NULL;
    }

    msg->env =  env;
    msg->type = msg_unknown;
    msg->ref = 0;

    return msg;
}

void msg_destroy(void *obj)
{
    msg_t *msg = (msg_t *) obj;

    if(msg->env != NULL){
        // fix
        enif_free_env(msg->env);
    }
    enif_free(msg);
}

typedef struct
{
    ErlNifTid tid;
    ErlNifThreadOpts* opts;

    struct queue_t *q;
    int alive;
    int id;
} worker_t;


typedef struct
{
    int count;
    worker_t workers[WORKER_NO];
} Tracker;


static ERL_NIF_TERM
make_answer(msg_t *msg, ERL_NIF_TERM answer)
{
    return enif_make_tuple3(msg->env, atom_elua, msg->ref, answer);
}


static ERL_NIF_TERM
make_error_tuple(ErlNifEnv *env, const char *reason)
{
    return enif_make_tuple2(env, atom_error, enif_make_string(env, reason, ERL_NIF_LATIN1));
}


static ERL_NIF_TERM
dofile(ErlNifEnv *env, lua_State *L, const ERL_NIF_TERM arg);
static ERL_NIF_TERM
gencall(ErlNifEnv *env, lua_State *L,
        const ERL_NIF_TERM arg1,
        const ERL_NIF_TERM arg2,
        const ERL_NIF_TERM arg3);


static ERL_NIF_TERM
evaluate_msg(msg_t *msg, worker_t *w)
{
    switch(msg->type) {
    case msg_dofile:
        return dofile(msg->env, msg->res->L, msg->arg1);
    case msg_gencall:
        return gencall(msg->env, msg->res->L, msg->arg1, msg->arg2, msg->arg3);
    default:
        return make_error_tuple(msg->env, "invalid_command");
    }
}

static void *
worker_run(void *arg)
{
    worker_t *w = (worker_t *) arg;
    msg_t *msg;
    int continue_running = 1;
    ERL_NIF_TERM answer;

    w->alive = 1;
    while(continue_running) {
        msg = queue_pop(w->q);

        if(msg->type == msg_stop) {
            continue_running = 0;
        }
        else {
            answer = make_answer(msg, evaluate_msg(msg, w));
            // printf("%d receive\n", w->id);
            enif_send(NULL, &(msg->pid), msg->env, answer);
        }
        enif_release_resource(msg->res);
        msg_destroy(msg);
    }

    w->alive = 0;
    return NULL;
}

int worker_init(worker_t *worker,int id)
{
    struct queue_t *q;
    ErlNifThreadOpts* opts;

    q = queue_create();
    if(q == NULL ) {
        goto queue_error;
    }

    worker->q = q;
    worker->id =id;
    opts = enif_thread_opts_create("lua_thread_opts");

    if(opts == NULL) {
        goto opts_error;
    }

    worker->opts = opts;
    if(enif_thread_create("lua_thread",
                          &worker->tid,
                          worker_run,
                          worker,
                          worker->opts) != 0) {
        goto create_error;
    }

    return 0;

 create_error:
    enif_thread_opts_destroy(opts);
 opts_error:
    queue_destroy(q);
 queue_error:
    return -1;
}

void woker_destory(worker_t *w)
{
    msg_t *msg = msg_create();
    msg->type = msg_stop;
    queue_push(w->q, msg);

    enif_thread_join(w->tid, NULL);
    enif_thread_opts_destroy(w->opts);

    queue_destroy(w->q);
}

int tracker_init(Tracker *t)
{
    int i;
    for(i=0; i< WORKER_NO; i++) {
        if(worker_init(&t->workers[i], i) <0 ) {
            goto error;
        }
    }

    return 0;

 error:
    while(i>0) {
        --i;
        woker_destory(&t->workers[i]);
    }
    return -1;
}



void
free_res(ErlNifEnv* env, void* obj)
{
    // printf("free res 0x%x\n" ,(unsigned int)obj);
    elua_t *res = (elua_t *)obj;
    lua_close(res->L);
}

unsigned int worker_hash(lua_State *L)
{
    unsigned int idx = (unsigned int)L;
    return idx % WORKER_NO;
}

static ERL_NIF_TERM
push_command(ErlNifEnv *env, elua_t *res, msg_t *msg)
{
    Tracker *tracker = (Tracker*) enif_priv_data(env);
    int hash_idx=worker_hash(res->L);
    assert(hash_idx>=0 && hash_idx< WORKER_NO);
    worker_t *w = &tracker->workers[hash_idx];
    enif_keep_resource(res);

    if(!queue_push(w->q, msg)){
        enif_release_resource(res);
        return make_error_tuple(env, "command_push_failed");
    }


    // printf("%d send\n", w->id);
    return atom_ok;
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
    atom_elua = enif_make_atom(env, "elua");
    atom_error = enif_make_atom(env, "error");

    tracker = (Tracker*) enif_alloc(sizeof(Tracker));
    if(tracker_init(tracker) < 0 ) {
        enif_free(tracker);
        return -1;
    }
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
    // luaopen_pack(L);

    res = enif_alloc_resource(RES_SYNC, sizeof(elua_t));
    if(res == NULL) return enif_make_badarg(env);

    // printf("alloc res 0x%x\n" ,(unsigned int)res);

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->L = L;

    return enif_make_tuple2(env, atom_ok, ret);
};



static ERL_NIF_TERM
dofile(ErlNifEnv *env, lua_State *L, const ERL_NIF_TERM arg)
{
    char buff_str[STACK_STRING_BUFF];
    int size = enif_get_string(env, arg, buff_str, STACK_STRING_BUFF, ERL_NIF_LATIN1);
    if(size <= 0) {
        return make_error_tuple(env, "invalid_filename");
    }

    if(luaL_dofile(L, buff_str) !=LUA_OK) {
        const char *error = lua_tostring(L, -1);
        ERL_NIF_TERM error_tuple = make_error_tuple(env, error);
        lua_pop(L,1);
        return error_tuple;
    }

    // printf("do file well\n");
    return atom_ok;
}

static ERL_NIF_TERM
elua_dofile_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    elua_t *res;

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    // first arg: res
    if(!enif_get_resource(env, argv[0], RES_SYNC, (void**) &res)) {
        return enif_make_badarg(env);
    }

    return dofile(env,res->L, argv[1]);
}


static ERL_NIF_TERM
elua_dofile_async(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    elua_t *res;
    msg_t *msg;
    ErlNifPid pid;

    if(argc != 4) {
        return enif_make_badarg(env);
    }

    // first arg: res
    if(!enif_get_resource(env, argv[0], RES_SYNC, (void**) &res)) {
        return enif_make_badarg(env);
    }

    // ref
    if(!enif_is_ref(env, argv[1])){
        return make_error_tuple(env, "invalid_ref");
    }

    // dest pid
    if(!enif_get_local_pid(env, argv[2], &pid)) {
        return make_error_tuple(env, "invalid_pid");
    }

    msg = msg_create();
    if(!msg) {
        return make_error_tuple(env, "command_create_failed");
    }

    msg->type = msg_dofile;
    msg->ref = enif_make_copy(msg->env, argv[1]);
    msg->pid = pid;
    msg->arg1 = enif_make_copy(msg->env, argv[3]);
    msg->res = res;

    // printf("push command\n");
    return push_command(env, res, msg);
}

#define FMT_AND_LIST_NO_MATCH  "fmt and list not match"
#define FMT_AND_RET_NO_MATCH  "fmt and ret not match"
#define FMT_WRONG  "fmt wrong"


static ERL_NIF_TERM
gencall(ErlNifEnv *env, lua_State *L,
        const ERL_NIF_TERM arg_func,
        const ERL_NIF_TERM arg_fmt,
        const ERL_NIF_TERM arg_list)
{
    char buff_str[STACK_STRING_BUFF];
    char buff_fmt[STACK_STRING_BUFF];
    char buff_fun[STACK_STRING_BUFF/2];
    unsigned input_len=0;
    unsigned output_len=0;

    if(enif_get_string(env, arg_func, buff_fun, STACK_STRING_BUFF/2, ERL_NIF_LATIN1)<=0){
        return enif_make_badarg(env);
    }

    if(enif_get_string(env, arg_fmt, buff_fmt, STACK_STRING_BUFF, ERL_NIF_LATIN1)<=0){
        return enif_make_badarg(env);
    }

    if(!enif_is_list(env, arg_list)){
        return enif_make_badarg(env);
    }

    input_len = strchr(buff_fmt, ':') - buff_fmt;
    output_len = strlen(buff_fmt) - input_len -1;

    // printf("input args %d output args %d fun %s\n", input_len, output_len, buff_fun);
    ERL_NIF_TERM head,tail,list;
    list=arg_list;

    int i=0, status = 0, ret;
    ERL_NIF_TERM return_list = enif_make_list(env, 0);
    lua_getglobal(L, buff_fun);
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
            if(lua_pcall(L, input_len, output_len,0) != LUA_OK) {
                error = lua_tostring(L, -1);
                lua_pop(L,1);
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

                lua_pushinteger(L, input_int);
            } else if ( status==1 ){
                int isnum;
                int n = lua_tointegerx(L, -1, &isnum);
                if(!isnum){
                    error = FMT_AND_LIST_NO_MATCH;
                    goto error;
                }
                // printf("output %d %d\n", output_len, n);
                return_list = enif_make_list_cell(env, enif_make_int(env, n), return_list);
                lua_pop(L,1);
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
                lua_pushstring(L, buff_str);

            } else if ( status==1 ) {
                const char *s = lua_tostring(L, -1);
                if (s==NULL) {
                    error = FMT_AND_RET_NO_MATCH;
                    goto error;
                }
                // printf("output %d %s\n", output_len, s);
                return_list = enif_make_list_cell(env, enif_make_string(env, s, ERL_NIF_LATIN1), return_list);
                lua_pop(L,1);
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
        lua_pop(L, 1);
    }
    else if(output_len>0) {
        lua_pop(L, output_len);
    }

    return make_error_tuple(env, error);
}

static ERL_NIF_TERM
elua_gencall_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    elua_t *res;

    // first arg: ref
    if(!enif_get_resource(env, argv[0], RES_SYNC, (void**) &res)) {
        return enif_make_badarg(env);
    }

    if(!enif_is_list(env, argv[3])) {
        return enif_make_badarg(env);
    }

    return gencall(env, res->L, argv[1], argv[2], argv[3]);
}
static ERL_NIF_TERM
elua_gencall_async(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    elua_t *res;
    msg_t *msg;
    ErlNifPid pid;

    if(argc != 6) {
        return enif_make_badarg(env);
    }

    // first arg: ref
    if(!enif_get_resource(env, argv[0], RES_SYNC, (void**) &res)) {
        return enif_make_badarg(env);
    }

    // ref
    if(!enif_is_ref(env, argv[1])) {
        return make_error_tuple(env, "invalid_ref");
    }

    // dest pid
    if(!enif_get_local_pid(env, argv[2], &pid)) {
        return make_error_tuple(env, "invalid_pid");
    }

    // fourth arg: list of input args
    if(!enif_is_list(env, argv[5])) {
        return enif_make_badarg(env);
    }

    msg = msg_create();
    if(!msg) {
        return make_error_tuple(env, "command_create_failed");
    }

    msg->type = msg_gencall;
    msg->ref = enif_make_copy(msg->env, argv[1]);
    msg->pid = pid;
    msg->arg1 = enif_make_copy(msg->env, argv[3]);
    msg->arg2 = enif_make_copy(msg->env, argv[4]);
    msg->arg3 = enif_make_copy(msg->env, argv[5]);
    msg->res = res;

    return push_command(env, res, msg);
}

static ERL_NIF_TERM
sync_gencast(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return atom_ok;
}

static ErlNifFunc nif_funcs[] = {
    {"newstate", 0, sync_newstate},

    {"dofile_sync", 2, elua_dofile_sync},
    {"dofile_async_nif", 4, elua_dofile_async},

    {"gencall_sync", 4, elua_gencall_sync},
    {"gencall_async_nif", 6, elua_gencall_async},

    {"gencast", 4, sync_gencast}
};

ERL_NIF_INIT(elua, nif_funcs, &load, NULL, NULL, NULL);
