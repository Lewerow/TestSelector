#ifndef VARIABLE_PROXY_H_miodfw39485gjmklfdoiwedci9ewjf9834jgwokdlkwenmfierjhnfgirf23rf
#define VARIABLE_PROXY_H_miodfw39485gjmklfdoiwedci9ewjf9834jgwokdlkwenmfierjhnfgirf23rf

#include <lua_engine/configuration.h>

#include <lua_engine/variable.h>

#include <memory>

namespace lua
{
	class reference_holder
	{
	public:
		reference_holder(lua_State* machine, lua::reference_type ref);
		~reference_holder();

		lua::reference_type operator*();
		lua_State* machine();

	private:
		lua_State* machine_;
		lua::reference_type ref_value;
	};

	class variable_proxy : public lua::pushable
	{
	public:
		variable_proxy(lua_State* machine, lua::type ltype, lua::reference_type);

		lua::type type() const;

		virtual void push(lua_State* machine) const;

	private:
		lua::type type_;
		std::shared_ptr<lua::reference_holder> ref_holder;
	};
}

namespace lua
{

	template <>
	struct lua_stack_helper<lua::variable_proxy>
	{
		static bool type_matches(lua_State*, int)
		{
			return true;
		}

		static lua::variable_proxy pop(lua_State* machine)
		{
			lua::helpers::scoped::exact_stack_size_change<-1> verifier(machine);
			lua::helpers::scoped::pop_n popper(machine, 1);

			lua::helpers::acquire_table(machine, lua::configuration::proxy_register_name);
			lua_insert(machine, lua_gettop(machine) - 1);
			lua::type type = lua::type(lua_type(machine, -1));
			return lua::variable_proxy(machine, type, luaL_ref(machine, -2));
		}

		static void push(lua_State* machine, const lua::variable_proxy& v)
		{
			v.push(machine);
		}
	};
}

#endif