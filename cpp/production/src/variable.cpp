#include <variable.h>

namespace lua
{
	template<>
	bool type<int>(lua_State* machine, int position)
	{
		return lua_isnumber(machine, position) == 1;
	}

	template<>
	int take<int>(lua_State* machine, int position)
	{
		return static_cast<int>(lua_tonumber(machine, position));
	}
}