#ifndef LUA_FUNCTION_H_MIOEWmdo49fj034fmnrejkdfvmclxdckqw90rjfriecklwdfoewfwefwef
#define LUA_FUNCTION_H_MIOEWmdo49fj034fmnrejkdfvmclxdckqw90rjfriecklwdfoewfwefwef

#include <string>

#include <lua.hpp>

#include <result_count.h>

#include <lua_engine/type_specializations.h>

namespace lua
{
	void handle_call_error(lua_State* machine, int error_code);

	class function
	{
	public:
		function(const std::string& name, lua_State* machine) : name_(name), machine_(machine)
		{}

		const std::string& name() const
		{
			return name_;
		}

		template <typename T, typename... arg_types>
		T call(arg_types&&... args)
		{
			lua_getglobal(machine(), name().c_str());
			push_all(machine(), std::forward<arg_types>(args)...);
			handle_call_error(machine(), lua_pcall(machine(), (sizeof...(args)), result_count<T>::value, 0));

			return pop<T>(machine());
		}

	private:

		lua_State* machine()
		{
			return machine_;
		}

		std::string name_;
		lua_State* machine_;
	};
}

#endif