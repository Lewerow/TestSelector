#ifndef TABLE_dmwoqidm98fj948w3j5ungjfdnkvncmowecksmclksmdckwemofcugvbtrbh45gu8943rf329ir0932ir3r
#define TABLE_dmwoqidm98fj948w3j5ungjfdnkvncmowecksmclksmdckwemofcugvbtrbh45gu8943rf329ir0932ir3r

#include <lua_engine/variable.h>

#include <memory>
#include <map>

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
			return lua_isuserdata(machine, position) == LUA_TRUE || lua_islightuserdata(machine, position) == LUA_TRUE;
		}

		static lua::table pop(lua_State* machine)
		{
//			T* result = reinterpret_cast<T*>(lua_touserdata(machine, -1));

			lua_pop(machine, 1);
//			if (result == NULL)
				throw std::bad_cast();

//			return result;
		}

		static void push(lua_State* machine, const lua::table& t)
		{
			t.push(machine);
		}
	};
}

#endif