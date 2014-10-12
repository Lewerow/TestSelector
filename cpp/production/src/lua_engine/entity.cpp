#include <lua_engine/entity.h>

#include <numeric>

#include <boost/algorithm/string/split.hpp>

namespace
{

	std::vector<std::string> make_path(const std::string& new_name)
	{
		std::vector<std::string> path;
		boost::split(path, new_name, [](char c){return (c == '.'); });
		return path;
	}

}

namespace lua
{
	entity::entity(const std::string& name) : path(make_path(name))
	{}

	entity::~entity()
	{}

	lua::type entity::type(lua_State* machine) const
	{
    	helpers::scoped::no_stack_size_change_verifier verifier(machine);
	    helpers::scoped::pop_at_most_n popper(machine, path.size());
		fetch_on_stack(machine);
		return lua::type(lua_type(machine, -1));
	}

	void entity::insert_into(lua_State* machine) const
	{}
		
	void entity::fetch_on_stack(lua_State* machine) const
	{
		lua_getglobal(machine, path[0].c_str());
		for (std::size_t i = 1; i < path.size(); ++i)
		{
			if (lua_istable(machine, -1) != LUA_TRUE)
				throw std::runtime_error("Unexpected type where table expected");

			lua_getfield(machine, -1, path[i].c_str());
		}
	}

	std::string entity::name() const
	{
		if (path.empty())
			return "";

		return std::accumulate(path.begin() + 1, path.end(), *path.begin(), [](const std::string& path, const std::string& piece){return path + "." + piece; });
	}
}
