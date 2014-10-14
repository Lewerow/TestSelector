#include <lua_engine/table.h>

namespace
{
	void make_field(lua_State* machine, const lua::entity& e, const std::string& field_name)
	{
		e.insert_into(machine);
		lua_setfield(machine, -2, field_name.c_str());
	}
}

namespace lua
{
	void table::push_fields(lua_State* machine) const
	{
		for (auto& f : fields)
			make_field(machine, *f, f->name());
	}
}