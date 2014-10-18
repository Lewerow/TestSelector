#include <lua_engine/table.h>

namespace
{
	void make_field(lua_State* machine, const lua::pushable& e, const std::string& field_name)
	{
		{
			lua::helpers::scoped::exact_stack_size_change<1> singleNewElement(machine);
			e.push(machine);
		}
		
		lua_setfield(machine, -2, field_name.c_str());
	}
}

namespace lua
{
	table::table()
	{}

	void table::push_fields(lua_State* machine) const
	{
		for (auto f : fields)
			make_field(machine, *f.second, f.first);
	}

	void table::remove_field(const std::string& name)
	{
		fields.erase(name);
	}

	void table::push(lua_State* machine) const
	{
		lua_createtable(machine, sequential_size(), associative_size());
		push_fields(machine);
	}

	std::size_t table::sequential_size() const
	{
		return 0; // sequences are not supported yet
	}

	std::size_t table::associative_size() const
	{
		return fields.size();
	}

	std::size_t table::total_size() const
	{
		return sequential_size() + associative_size();
	}
}