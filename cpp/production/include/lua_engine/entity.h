#ifndef LUA_ENTITY_mfewoifm8934jg5u4gjfdvmlksmdcksmwdi23d9843jfsdflksdjlfkwejrdo23jrfi34jfe
#define LUA_ENTITY_mfewoifm8934jg5u4gjfdvmlksmdcksmwdi23d9843jfsdflksdjlfkwejrdo23jrfi34jfe

#include <lua_engine/basic_lua_helpers.h>

#include <string>
#include <vector>

namespace lua
{
	class entity
	{
	public:
		entity(const std::string& name);
		virtual ~entity();
		
		std::string first_name() const;
		std::string name() const;
		lua::type type(lua_State* machine) const;
		void fetch_on_stack(lua_State* machine) const;

		std::vector<std::string> path;
	private:
	};
}

#endif