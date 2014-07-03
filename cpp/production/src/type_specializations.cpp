#include <variable.h>

namespace lua
{
	template<>
	bool type_matches<int>(lua_State* machine, int position)
	{
		return lua_isnumber(machine, position) == 1;
	}

	template<>
	int take<int>(lua_State* machine, int position)
	{
		return static_cast<int>(lua_tonumber(machine, position));
	}

	template<>
	void push<int>(lua_State* machine, const int& value)
	{
		lua_pushinteger(machine, value);
	}

	template<>
	bool type_matches<std::string>(lua_State* machine, int position)
	{
		return lua_isstring(machine, position) == 1;
	}

	template<>
	std::string take<std::string>(lua_State* machine, int position)
	{
		return std::string(lua_tostring(machine, position));
	}

	template<>
	void push<std::string>(lua_State* machine, const std::string& value)
	{
		lua_pushstring(machine, value.c_str());
	}
}