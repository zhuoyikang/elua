#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>

#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <limits.h>


#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"


int luaopen_emt(lua_State *L);



static int mt_sleep(lua_State *L)
{
    lua_Number interval = luaL_checknumber(L, 1);
    lua_Number units = luaL_optnumber(L, 2, 1);
    usleep(1000000 * interval / units);
    return 0;
}

static const luaL_Reg mt_lib[] = {
    {"sleep", mt_sleep},
    {0,0}
};


int luaopen_emt(lua_State *L)
{

    luaL_openlib(L, "emt", mt_lib, 0);
    return 1;
}
