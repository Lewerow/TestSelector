#ifndef LUA_ENGINE_H_dqoiwdji9dj84fneruidkxld09qw3dfj849fureifnvdokkfodwjfwefwe
#define LUA_ENGINE_H_dqoiwdji9dj84fneruidkxld09qw3dfj849fureifnvdokkfodwjfwefwe

#include <lua_engine/basic_lua_helpers.h>
#include <lua_engine/lua_state_default_deleter.h>
#include <lua_engine/variable.h>
#include <lua_engine/function.h>
#include <lua_engine/cfunction.h>
#include <lua_engine/table.h>

#include <memory>
#include <string>

#include <boost/filesystem/path.hpp>

namespace lua
{
	class engine
	{
	public:
		engine();

		template <typename T>
		void load(const lua::variable<T>& var)
		{
			helpers::scoped::no_stack_size_change_verifier verifier(machine.get());
			var.insert_into(machine.get());
			lua_setglobal(machine.get(), var.first_name().c_str());
		}

		void load(const std::string& code);
		void load_file(const boost::filesystem::path& filename);
		
		lua::type typeof(const std::string& varname);

		template <typename T>
		T get(const std::string& varname)
		{
			return variable<T>(varname).get_value_from(machine.get()).value();
		}

		template <typename T, typename... arg_types>
		T call(const std::string& name, arg_types&&... args)
		{
			return function(name, machine.get()).call<T>(std::forward<arg_types>(args)...);
		}
		// will be provided later, if needed
		engine(const engine&) = delete;
		engine& operator=(const engine&) = delete;
		
	private:
		std::unique_ptr<lua_State> machine;
	};
}

#endif
