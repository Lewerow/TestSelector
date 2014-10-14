#ifndef TABLE_dmwoqidm98fj948w3j5ungjfdnkvncmowecksmclksmdckwemofcugvbtrbh45gu8943rf329ir0932ir3r
#define TABLE_dmwoqidm98fj948w3j5ungjfdnkvncmowecksmclksmdckwemofcugvbtrbh45gu8943rf329ir0932ir3r

#include <lua_engine/variable.h>

#include <memory>
#include <vector>

namespace lua
{
	class table : public lua::variable<lua::table&>
	{
	public:
		table(const std::string& name) : lua::variable<lua::table&>(name, *this)
		{}

		template <typename T>
		void add_field(const std::string& name, T&& value)
		{
			fields.push_back(std::make_unique<lua::variable<T> >(name, value));
		}

		void push_fields(lua_State* machine) const;

	private:
		std::vector<std::unique_ptr<lua::entity> > fields;
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
			lua_newtable(machine);
			t.push_fields(machine);
		}
	};
}

#endif