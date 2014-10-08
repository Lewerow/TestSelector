#ifndef BASIC_LUA_HELPERS_H_MOiewmkcvi9erjg98u54fgui4rjnedsmofk3q2d9jfkgnvewopfkopfkiregvurejkdfdsjflkdsjf
#define BASIC_LUA_HELPERS_H_MOiewmkcvi9erjg98u54fgui4rjnedsmofk3q2d9jfkgnvewopfkopfkiregvurejkdfdsjflkdsjf

#include <lua.hpp>

#include <ts_assert.h>

struct lua_State;

const int LUA_TRUE = 1;
const int LUA_FALSE = 0;

namespace lua
{
	enum type
	{
		lnil = LUA_TNIL,
		lboolean = LUA_TBOOLEAN,
		llight_userdata = LUA_TLIGHTUSERDATA,
		lnumber = LUA_TNUMBER,
		lstring = LUA_TSTRING,
		ltable = LUA_TTABLE,
		lfunction = LUA_TFUNCTION,
		luserdata = LUA_TUSERDATA,
		lthread = LUA_TTHREAD
	};

	enum status_code
	{
		ok = LUA_OK,
		yield = LUA_YIELD,
		runtime_error = LUA_ERRRUN,
		syntax_error = LUA_ERRSYNTAX,
		memory_error = LUA_ERRMEM,
		garbage_collector_error = LUA_ERRGCMM,
		error_during_error_handling = LUA_ERRERR
	};

	typedef int status_code_wrapper; // required for compiling cfunctions

	namespace helpers
	{
		namespace scoped
		{
			template <int expected_change>
			class exact_stack_size_change
			{
			public:
				exact_stack_size_change(lua_State* lua_machine) : machine(lua_machine), top(lua_gettop(machine))
				{}

				~exact_stack_size_change()
				{
					int new_top = lua_gettop(machine);
					TS_ASSERT((top + expected_change == new_top), "Top of Lua stack has changed more than expected");
				}

			private:
				lua_State* machine;
				int top;
			};

			typedef exact_stack_size_change<0> no_stack_size_change_verifier;

			class pop_n
			{
			public:
				pop_n(lua_State* lua_machine, int n_);
				~pop_n();

			private:
				lua_State* machine;
				int n;
			};
		}

		void get_or_create_global_table(lua_State* machine, const std::string& table_name);
		void get_or_create_table_as_field(lua_State* machine, const std::string& table_name);
	}
}

#endif
