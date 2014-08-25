#include <exception>

#include <ts_assert.h>
#include <lua_engine/lua_engine.h>
#include <lua_engine/lua_exceptions.h>

namespace
{
	void handle_lua_loading_error_code(lua_State* machine, int error_code)
	{
		switch (error_code)
		{
		case LUA_OK:
			break;
		case LUA_YIELD:
		case LUA_ERRRUN:
			throw std::runtime_error(lua_tostring(machine, -1));
			break;
		case LUA_ERRSYNTAX:
			throw std::logic_error(lua_tostring(machine, -1));
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

namespace lua
{
	engine::engine() : machine(luaL_newstate())
	{}

	void engine::load(const std::string& code)
	{
		handle_lua_loading_error_code(machine.get(), luaL_dostring(machine.get(), code.c_str()));
	}

	void engine::load_file(const boost::filesystem::path& filename)
	{
		handle_lua_loading_error_code(machine.get(), luaL_dofile(machine.get(), filename.string().c_str()));
	}
}