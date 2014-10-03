#ifndef cfunction_jfieowfjewfj984jferjfiejfjewofijewoifjewfewfejfo
#define cfunction_jfieowfjewfj984jferjfiejfjewofijewoifjewfewfejfo

#include <string>
#include <functional>

#include <boost/function_types/result_type.hpp>

#include <lua_engine/variable.h>
#include <lua_engine/type_specializations.h>

struct lua_State;

namespace lua
{
	class engine;

    template <typename result_type, typename... arg_types>
    class cfunction
    {
    public:
		cfunction(const std::string& new_name, const std::function<result_type(arg_types...)>& new_f) : name_(new_name), f(new_f)
        {}
        
		void insert_into(engine& machine) const
        {
			machine.push(lua::make_variable(name(), *this));
        }

        const std::string& name() const
        {
            return name_;
        }

		const std::function<result_type(arg_types...)>& functor() const
        {
            return f;
        }

		static int finalizer(lua_State* machine)
		{
			cfunction<result_type, arg_types...>* f = *reinterpret_cast<cfunction<result_type, arg_types...>**>(lua_touserdata(machine, 1));
			f->~cfunction<result_type, arg_types...>();
			return 0;
		}

    private:
		std::string name_;
		std::function<result_type(arg_types...)> f;
    };
	
	 template <typename result_type, typename... arg_types>
     cfunction<result_type, arg_types...> make_cfunction(const std::string& name, const std::function<result_type(arg_types...)>& f)	 {
		return cfunction<result_type, arg_types...>(name, f);	 }
}

namespace lua
{
	template<typename result_type, typename... arg_types>
	struct lua_stack_helper<cfunction<result_type, arg_types...> >
	{
		static bool type_matches(lua_State* machine, int position)
		{
			return lua_isuserdata(machine, position) == LUA_TRUE || lua_iscfunction(machine, position) == LUA_TRUE;
		}

		static cfunction<result_type, arg_types...>& pop(lua_State* machine)
		{
			if (!lua_isuserdata(machine, -1))
				throw std::bad_cast();

			cfunction<result_type, arg_types...>& f = **reinterpret_cast<cfunction<result_type, arg_types...>**>(lua_touserdata(machine, 1));
			return f;
		}

		static void push(lua_State* machine, const cfunction<result_type, arg_types...>& value)
		{
			void* address = lua_newuserdata(machine, sizeof(value));
			new (address)cfunction<result_type, arg_types...>(value);
			
			luaL_newmetatable(machine, (value.name() + "_finalizer").c_str());
			lua_pushstring(machine, "__gc");
			lua_pushcfunction(machine, (&cfunction<result_type, arg_types...>::finalizer));
			lua_settable(machine, -3);
			lua_setmetatable(machine, -2);
		}
	};
}

#endif
