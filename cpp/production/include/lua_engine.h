#ifndef LUA_ENGINE_H_dqoiwdji9dj84fneruidkxld09qw3dfj849fureifnvdokkfodwjfwefwe
#define LUA_ENGINE_H_dqoiwdji9dj84fneruidkxld09qw3dfj849fureifnvdokkfodwjfwefwe

#include <memory>
#include <string>

#include <lua.hpp>

#include <lua_state_default_deleter.h>
#include <variable.h>

namespace lua
{
	class engine
	{
	public:
		engine();

		void load(const std::string& code);
		void load_file(const std::string& filename);

		template <typename T>
		T get(const std::string& varname)
		{
			return variable<T>(varname).get(machine.get()).value();
		}

/*		template <typename T>
		T push(const std::string& varname, const T& value)
		{
			return variable::push<T>(machine.get(), varname, value);
		}
		*/
	private:
		std::unique_ptr<lua_State> machine;
	};
}

#endif