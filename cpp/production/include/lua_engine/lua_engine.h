#ifndef LUA_ENGINE_H_dqoiwdji9dj84fneruidkxld09qw3dfj849fureifnvdokkfodwjfwefwe
#define LUA_ENGINE_H_dqoiwdji9dj84fneruidkxld09qw3dfj849fureifnvdokkfodwjfwefwe

#include <memory>
#include <string>

#include <boost/filesystem/path.hpp>

#include <lua.hpp>

#include <lua_engine/basic_lua_helpers.h>
#include <lua_engine/lua_state_default_deleter.h>
#include <lua_engine/variable.h>
#include <lua_engine/function.h>
#include <lua_engine/cfunction.h>

namespace lua
{
	class engine
	{
	public:
		engine();

        template <typename signature>
        void load(const lua::cfunction<signature>& cfunc)
		{
			push(lua::make_variable(cfunc.name(), cfunc));
        }

		void load(const std::string& code);
		void load_file(const boost::filesystem::path& filename);
		
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
			return variable<T>("", call(name, args...));
		}

		// will be provided later, if needed
		engine(const engine&) = delete;
		engine& operator=(const engine&) = delete;
		
	private:
		std::unique_ptr<lua_State> machine;
	};
}

#endif
