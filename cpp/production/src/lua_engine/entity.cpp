#include <lua_engine/entity.h>

namespace lua
{
	entity::entity(const std::string& name) : name_(name)
	{}

	entity::~entity()
	{}

	const std::string& entity::name() const
	{
		return name_;
	}
}