#ifndef LUA_FUNCTION_H_MIOEWmdo49fj034fmnrejkdfvmclxdckqw90rjfriecklwdfoewfwefwef
#define LUA_FUNCTION_H_MIOEWmdo49fj034fmnrejkdfvmclxdckqw90rjfriecklwdfoewfwefwef

#include <string>

#include <lua.hpp>

#include <result_count.h>
#include <type_specializations.h>

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

		template <typename T, typename... Args>
		T call(Args... args)
		{
			lua_getglobal(machine(), name().c_str());
			push_all(machine(), args...);
			handle_call_error(machine(), lua_pcall(machine(), (sizeof...(args)), result_count<T>::value, 0));

			return take<T>(machine(), -1);
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