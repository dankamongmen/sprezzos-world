#define LUA_LIB
#include <lua.h>
#include <lauxlib.h>

static int get_greeting(lua_State* L)
{
    lua_pushstring(L, "hello from Lua");
    return 1;
}

static const luaL_Reg funcs[] = {
    { "get_greeting", get_greeting },
    { NULL, NULL }
};

int luaopen_foo(lua_State* L)
{
    luaL_register(L, "foo", funcs);
    return 1;
}
