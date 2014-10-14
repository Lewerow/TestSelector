#include <lua.hpp>

#include <boost/lexical_cast.hpp>

#include <ts_assert.h>

#include <lua_engine/basic_lua_helpers.h>
#include <lua_engine/type_specializations.h>
#include <lua_engine/entity.h>

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
        
            pop_at_most_n::pop_at_most_n(lua_State* lua_machine, int n_) : pop_n(lua_machine, n_), start_top(lua_gettop(lua_machine))
            {}

            pop_at_most_n::~pop_at_most_n()
            {
                int new_elements_on_stack_count = lua_gettop(machine) - start_top;
                n = (n > new_elements_on_stack_count) ? new_elements_on_stack_count : n;                

                TS_ASSERT(n >= 0, "More elements already taken from the stack than required to pop!");
            }
		}

		namespace
		{
			void table_initialization(lua_State* machine)
			{
				lua_pop(machine, 1);
				lua_newtable(machine);
			}

			int nop(lua_State*)
			{
				return 0;
			}

			void initialize_default(lua_State* machine, lua::type t)
			{
				switch (t)
				{
				case lua::lboolean:
					lua_pushboolean(machine, false);
					break;
				case lua::llightuserdata:
					lua_pushlightuserdata(machine, nullptr);
					break;
				case lua::lfunction:
					lua_pushcfunction(machine, nop);
					break;
				case lua::lnil:
					lua_pushnil(machine);
					break;
				case lua::lnumber:
					lua_pushnumber(machine, 0);
					break;
				case lua::lstring:
					lua_pushstring(machine, "");
					break;
				case lua::ltable:
					lua_newtable(machine);
					break;
				case lua::lthread:
					lua_newthread(machine);
					break;
				}
			}

			void lazy_initialize(lua_State* machine, const std::string& field_name, lua::type expected_type)
			{
				lua::type entity_type = lua::type(lua_type(machine, -1));
				if (entity_type != expected_type)
				{
					if (entity_type == lua::lnil)
					{
						lua_pop(machine, 1);
						initialize_default(machine, expected_type);
					}
					else
						throw std::runtime_error("Unexpected type (" + boost::lexical_cast<std::string>(entity_type)+") where " + boost::lexical_cast<std::string>(expected_type)+" or nil expected");
				}
			}
		}

		void acquire_table(lua_State* machine, const std::string& table_name)
		{
			scoped::exact_stack_size_change<1> stack_verifier(machine);
			lua_getglobal(machine, table_name.c_str());
			lazy_initialize(machine, table_name, lua::ltable);
		}

		void acquire_field(lua_State* machine, const std::string& field_name, lua::type expected_type)
		{
			scoped::exact_stack_size_change<1> stack_verifier(machine);
			lua_getfield(machine, -1, field_name.c_str());
			lazy_initialize(machine, field_name, expected_type);
		}
	}
}
