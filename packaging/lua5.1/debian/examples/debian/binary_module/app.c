#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

int luaopen_foo(lua_State* L);

int main(void)
{
    int result;
    lua_State* L = lua_open();
    luaL_openlibs(L);
    result = luaL_dostring(L, "require 'foo'");
    if (result != 0) return 1;
    result = luaL_dostring(L, "print(foo.get_greeting())");
    lua_close(L);
    return (result != 0);
}
