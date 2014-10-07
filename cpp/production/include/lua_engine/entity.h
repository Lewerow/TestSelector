#ifndef LUA_ENTITY_mfewoifm8934jg5u4gjfdvmlksmdcksmwdi23d9843jfsdflksdjlfkwejrdo23jrfi34jfe
#define LUA_ENTITY_mfewoifm8934jg5u4gjfdvmlksmdcksmwdi23d9843jfsdflksdjlfkwejrdo23jrfi34jfe

#include <string>

struct lua_State;

namespace lua
{
	class entity
	{
	public:
		entity(const std::string& name);
		virtual ~entity();

		virtual void insert_into(lua_State* machine) const = 0;

		const std::string& name() const;

	private:
		std::string name_;
	};
}

#endif