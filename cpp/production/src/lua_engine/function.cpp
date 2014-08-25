#include <lua_engine/function.h>
#include <ts_assert.h>
#include <lua_engine/lua_exceptions.h>

namespace lua
{
	void handle_call_error(lua_State* machine, int error_code)
	{
		switch (error_code)
		{
		case LUA_OK:
			break;
		case LUA_ERRRUN:
			throw std::runtime_error(lua_tostring(machine, -1));
			break;
		case LUA_ERRMEM:
			throw lua::allocation_failure();
			break;
		case LUA_ERRGCMM:
			throw std::runtime_error(lua_tostring(machine, -1));
			break;
		case LUA_ERRERR:
			throw std::exception(lua_tostring(machine, -1));
			break;
		default:
			TS_ASSERT(false, "Not supported error code!");
		}
	}
}