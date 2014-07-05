#include <type_specializations.h>
#include <variable.h>

namespace lua
{
	void push_all(lua_State* machine)
	{}
}
/*
namespace lua
{
	template<>
	bool type_matches<int>(lua_State* machine, int position)
	{
		return lua_isnumber(machine, position) == LUA_TRUE;
	}

	template<>
	int pop<int>(lua_State* machine, int position)
	{
		int has_conversion_succeeded = 0;
		int result = static_cast<int>(lua_tonumberx(machine, position, &has_conversion_succeeded));

		if (has_conversion_succeeded == LUA_FALSE)
			throw std::bad_cast();

		return result;
	}

	template<>
	void push<int>(lua_State* machine, const int& value)
	{
		lua_pushinteger(machine, value);
	}
}

namespace lua
{
	template<>
	bool type_matches<std::string>(lua_State* machine, int position)
	{
		return lua_isstring(machine, position) == LUA_TRUE;
	}

	template<>
	std::string pop<std::string>(lua_State* machine, int position)
	{
		return std::string(lua_tostring(machine, position));
	}

	template<>
	void push<std::string>(lua_State* machine, const std::string& value)
	{
		lua_pushstring(machine, value.c_str());
	}
}*/