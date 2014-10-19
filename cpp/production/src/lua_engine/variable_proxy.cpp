#include <lua_engine/variable_proxy.h>
#include <lua_engine/configuration.h>

namespace lua
{
	reference_holder::reference_holder(lua_State* machine, lua::reference_type ref) : machine_(machine), ref_value(ref)
	{}

	reference_holder::~reference_holder()
	{
		lua::helpers::acquire_table(machine(), lua::configuration::proxy_register_name);
		luaL_unref(machine(), -1, ref_value);
		lua_pop(machine(), 1);
	}

	lua::reference_type reference_holder::operator*()
	{
		return ref_value;
	}

	lua_State* reference_holder::machine()
	{
		return machine_;
	}

	variable_proxy::variable_proxy(lua_State* machine, lua::type ltype, lua::reference_type ref) : type_(ltype), ref_holder(std::make_shared<lua::reference_holder>(machine, ref))
	{}

	lua::type variable_proxy::type() const
	{
		return type_;
	}

	void variable_proxy::push(lua_State* machine) const
	{
		TS_ASSERT(machine == ref_holder->machine(), "Reference can be pushed only to parent lua engine");
		lua::helpers::acquire_table(machine, lua::configuration::proxy_register_name);
		lua_rawgeti(machine, *(*ref_holder), -1);
		lua_remove(machine, lua_gettop(machine) - 1);
	}
}