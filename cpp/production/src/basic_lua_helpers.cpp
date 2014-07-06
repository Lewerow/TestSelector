#include <lua.hpp>

#include <ts_assert.h>
#include <basic_lua_helpers.h>
#include <type_specializations.h>

namespace lua
{
	namespace helpers
	{
		namespace scoped
		{
			pop_n::pop_n(lua_State* lua_machine, int n_) : machine(lua_machine), n(n_)
			{}

			pop_n::~pop_n()
			{
				lua_pop(machine, n);
			}
		}

		namespace
		{
			void lazy_table_initialization(lua_State* machine, const std::string& table_name, void (*initializer)(lua_State*, const std::string&))
			{
				if (!lua_istable(machine, -1))
				{
					if (lua_isnil(machine, -1))
						initializer(machine, table_name);
					else
						throw std::runtime_error("Unexpected type where table or nil expected");
				}
			}

			void table_as_field_initialization(lua_State* machine, const std::string& table_name)
			{
				lua_pop(machine, 1);
				push(machine, table_name);
				lua_newtable(machine);
				lua_settable(machine, -3);
				lua_getfield(machine, -1, table_name.c_str());
			}

			void global_table_initialization(lua_State* machine, const std::string& table_name)
			{
				lua_pop(machine, 1);
				lua_newtable(machine);
				lua_setglobal(machine, table_name.c_str());
				lua_getglobal(machine, table_name.c_str());
			}
		}

		void get_or_create_global_table(lua_State* machine, const std::string& table_name)
		{
			scoped::exact_stack_size_change<1> stack_verifier(machine);
			lua_getglobal(machine, table_name.c_str());
			lazy_table_initialization(machine, table_name, global_table_initialization);
		}

		void get_or_create_table_as_field(lua_State* machine, const std::string& table_name)
		{
			scoped::exact_stack_size_change<1> stack_verifier(machine);
			lua_getfield(machine, -1, table_name.c_str());
			lazy_table_initialization(machine, table_name, table_as_field_initialization);
		}
	}
}