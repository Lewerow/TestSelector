#ifndef cfunction_jfieowfjewfj984jferjfiejfjewofijewoifjewfewfejfo
#define cfunction_jfieowfjewfj984jferjfiejfjewofijewoifjewfewfejfo

#include <string>
#include <functional>
#include <type_traits>

#include <boost/function_types/result_type.hpp>
#include <boost/function_types/parameter_types.hpp>

#include <lua_engine/basic_lua_helpers.h>
#include <lua_engine/variable.h>
#include <lua_engine/type_specializations.h>

struct lua_State;

namespace lua
{

    template <typename return_type, typename... arg_types>
    static return_type autocall(lua_State* machine, const std::function<return_type(arg_types...)>& f)
    {
        return f(pop<arg_types>(machine)...);
    }

    template <typename signature>
    class cfunction
    {
        typedef cfunction<signature> this_type;
        typedef typename std::function<signature>::result_type result_type;
    public:
    	cfunction(const std::string& new_name, const std::function<signature>& new_f) : name_(new_name), f(new_f)
        {}

        const std::string& name() const
        {
            return name_;
        }

		const std::function<signature>& functor() const
        {
            return f;
        }

		static int finalizer(lua_State* machine)
		{
			//if it ever becomes needed, it's here already
            return 0;
		}

		static int caller(lua_State* machine)
		{
            this_type* f = reinterpret_cast<this_type*>(lua_touserdata(machine, lua_upvalueindex(1)));
			push(machine, autocall(machine, f->functor()));
	    	return lua::result_count<result_type>::value;
		}

    private:
		std::string name_;
		std::function<signature> f;
    };
	
    template <typename result_type, typename... arg_types>
	lua::cfunction<result_type(arg_types...)> make_cfunction(const std::string& name, result_type (*f)(arg_types...))
    {
		 return lua::cfunction<result_type(arg_types...)>(name, f);
    }

    template <typename result_type, typename class_type, typename... arg_types>
    lua::cfunction<result_type(class_type*, arg_types...) > make_cfunction(const std::string& name, result_type (class_type::* f)(arg_types...))
    {
        return lua::cfunction<result_type(class_type*, arg_types...)>(name, std::mem_fn(f));
    }
}

namespace lua
{
	template<typename signature>
	struct lua_stack_helper<lua::cfunction<signature> >
	{
		static bool type_matches(lua_State* machine, int position)
		{
			return lua_isuserdata(machine, position) == LUA_TRUE || lua_iscfunction(machine, position) == LUA_TRUE;
		}

		static lua::cfunction<signature>& pop(lua_State* machine)
		{
			if (!lua_isuserdata(machine, -1))
				throw std::bad_cast();

			lua::cfunction<signature>& f = **reinterpret_cast<lua::cfunction<signature>**>(lua_touserdata(machine, 1));
			return f;
		}

		static void push(lua_State* machine, const lua::cfunction<signature>& value)
		{
			void* address = lua_newuserdata(machine, sizeof(value));
			new (address)lua::cfunction<signature>(value);
			
			luaL_newmetatable(machine, (value.name() + "_finalizer").c_str());
			
			lua_pushstring(machine, "__gc");
			lua_pushcfunction(machine, (&lua::cfunction<signature>::finalizer));
			lua_settable(machine, -3);

			lua_pushstring(machine, "__call");
            lua_pushlightuserdata(machine, address);
			lua_pushcclosure(machine, (&lua::cfunction<signature>::caller), 1);
			lua_settable(machine, -3);

			lua_setmetatable(machine, -2);
		}
	};
}

#endif
