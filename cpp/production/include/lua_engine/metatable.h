#ifndef LUA_METATABLE_mdowmdiowqd98234jf4u3hfsdjkjsaklxjcwqoidj9823f89745hfrjkdfjsojwqe09dj32uhdewkjdwjdewjdwej
#define LUA_METATABLE_mdowmdiowqd98234jf4u3hfsdjkjsaklxjcwqoidj9823f89745hfrjkdfjsojwqe09dj32uhdewkjdwjdewjdwej

#include <lua_engine/table.h>

namespace lua
{
	class metatable : public lua::table
	{
	public:
		metatable()
		{}	    
	};

	template <typename T>
	void initialize_metatable(lua::metatable&)
	{}

	template <typename T>
	lua::metatable make_metatable()
	{
		lua::metatable tab;
		lua::initialize_metatable<T>(tab);
		return tab;
	}
}

#endif