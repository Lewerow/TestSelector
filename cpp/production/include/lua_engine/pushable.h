#ifndef LUA_OBJECT_H_dweqidj9483f9843fgdscksdmcoweci9j49gfj349fk34dfksadmlkdsnvjnjireocpwekdoiwejfiog
#define LUA_OBJECT_H_dweqidj9483f9843fgdscksdmcoweci9j49gfj349fk34dfksadmlkdsnvjnjireocpwekdoiwejfiog

#include <lua_engine/type_specializations.h>

struct lua_State;

namespace lua
{
	class pushable
	{
	public:
		virtual ~pushable(){}
		virtual void push(lua_State*) const = 0;
	};
}

#endif