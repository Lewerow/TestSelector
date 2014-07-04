#ifndef LUA_ENGINE_H_dqoiwdji9dj84fneruidkxld09qw3dfj849fureifnvdokkfodwjfwefwe
#define LUA_ENGINE_H_dqoiwdji9dj84fneruidkxld09qw3dfj849fureifnvdokkfodwjfwefwe

#include <memory>
#include <string>

#include <lua.hpp>

#include <lua_state_default_deleter.h>
#include <variable.h>
#include <function.h>

namespace lua
{
	class engine
	{
	public:
		engine();

		void load(const std::string& code);
		void load_file(const std::string& filename);

		template <typename T>
		variable<T> get_variable(const std::string& varname)
		{
			return variable<T>(varname).get_value_from(machine.get());
		}

		template <typename T>
		T get(const std::string& varname)
		{
			return variable<T>(varname).get_value_from(machine.get()).value();
		}

		template <typename T>
		void push(const variable<T>& var)
		{
			return var.insert_into(machine.get());
		}

		template <typename T, typename... Args>
		T call(const std::string& name, Args... args)
		{
			return function(name, machine.get()).call<T>(args...);
		}
		
		template <typename T, typename... Args>
		variable<T> call_variable(const std::string& name, Args... args)
		{
			return variable<T>("", call(name, args))
		}
		
	private:
		std::unique_ptr<lua_State> machine;
	};
}

#endif