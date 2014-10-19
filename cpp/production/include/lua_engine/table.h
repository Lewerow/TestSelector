#ifndef TABLE_dmwoqidm98fj948w3j5ungjfdnkvncmowecksmclksmdckwemofcugvbtrbh45gu8943rf329ir0932ir3r
#define TABLE_dmwoqidm98fj948w3j5ungjfdnkvncmowecksmclksmdckwemofcugvbtrbh45gu8943rf329ir0932ir3r

#include <lua_engine/variable_proxy.h>
#include <lua_engine/configuration.h>

#include <memory>
#include <map>
#include <string>

#include <boost/lexical_cast.hpp>

namespace lua
{
	class table : public lua::pushable
	{
	public:
		table();

		template <typename T>
		void add_field(const std::string& name, T&& value)
		{
			fields[name] = std::make_shared<lua::variable<T> >(name, value);
		}

		void remove_field(const std::string& name);

		std::size_t sequential_size() const;
		std::size_t associative_size() const;
		std::size_t total_size() const;

		void push(lua_State* machine) const;



	private:
		void push_fields(lua_State* machine) const;

		std::map<std::string, std::shared_ptr<lua::pushable> > fields;
	};
}

namespace lua
{
	template <>
	struct lua_stack_helper<lua::table>
	{
		static bool type_matches(lua_State* machine, int position)
		{
			return lua_istable(machine, position) == LUA_TRUE;
		}

		static lua::table pop(lua_State* machine)
		{
			lua::helpers::scoped::exact_stack_size_change<-1> verifier(machine);

			lua::table tab;
			lua_pushnil(machine);
			while (lua_next(machine, -2) != LUA_FALSE)
			{
				std::string table_index;
				if (lua_isstring(machine, -2))
					table_index = lua_tostring(machine, -2);
				else
					table_index = boost::lexical_cast<std::string>(lua_tointeger(machine, -2));

				tab.add_field(table_index, lua_stack_helper<lua::variable_proxy>::pop(machine));
			}

			lua_pop(machine, 1);

			return tab;
		}

		static void push(lua_State* machine, const lua::table& t)
		{
			t.push(machine);
		}
	};
}

#endif