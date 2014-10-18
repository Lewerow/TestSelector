#ifndef cfunction_jfieowfjewfj984jferjfiejfjewofijewoifjewfewfejfo
#define cfunction_jfieowfjewfj984jferjfiejfjewofijewoifjewfewfejfo

#include <string>
#include <functional>
#include <type_traits>

#include <boost/function_types/result_type.hpp>
#include <boost/function_types/parameter_types.hpp>
#include <boost/lexical_cast.hpp>

#include <lua_engine/basic_lua_helpers.h>
#include <lua_engine/pushable.h>
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
	class cfunction : public lua::pushable
    {
		typedef cfunction<signature> this_type;
		typedef typename std::function<signature>::result_type result_type;
    public:
		cfunction(std::function<signature>&& f_) : f(f_)
        {}

		static int finalizer(lua_State* machine)
		{
			// need to explicitly call destructor of object, as Lua manages only memory allocation
			this_type* f = reinterpret_cast<this_type*>(lua_touserdata(machine, lua_upvalueindex(1)));
			f->~cfunction();
			return 0;
		}

		static int caller(lua_State* machine)
		{
			this_type* f = reinterpret_cast<this_type*>(lua_touserdata(machine, lua_upvalueindex(1)));
			auto result = autocall(machine, f->functor());
			lua::push(machine, result);

			return lua::result_count<result_type>::value;
		}

		const std::function<signature>& functor() const
		{
			return f;
		}

		void push(lua_State* machine) const
		{
			void* address = lua_newuserdata(machine, sizeof(*this));
			new (address)lua::cfunction<signature>(*this);

			lua::helpers::create_metatable(machine, "cfunction");

			lua_pushstring(machine, "__gc");
			lua_pushlightuserdata(machine, address);
			lua_pushcclosure(machine, (&lua::cfunction<signature>::finalizer), 1);
			lua_settable(machine, -3);

			lua_pushstring(machine, "__call");
			lua_pushlightuserdata(machine, address);
			lua_pushcclosure(machine, (&lua::cfunction<signature>::caller), 1);
			lua_settable(machine, -3);

			lua_setmetatable(machine, -2);
		}

	private:
		std::function<signature> f;
    };

	template <typename result_type, typename... arg_types>
	lua::cfunction<result_type(arg_types...)> make_cfunction(result_type(*f)(arg_types...))
	{
		return lua::cfunction<result_type(arg_types...)>(f);
	}

	template <typename result_type, typename class_type, typename... arg_types>
	lua::cfunction<result_type(class_type*, arg_types...) > make_cfunction(result_type(class_type::* f)(arg_types...))
	{
		return lua::cfunction<result_type(class_type*, arg_types...)>(std::mem_fn(f));
	}

	template <typename result_type, typename... arg_types, typename functor_type>
	lua::cfunction<result_type(arg_types...)> make_cfunction(functor_type&& f)
	{
		return lua::cfunction<result_type(arg_types...)>(std::function<result_type(arg_types...)>(std::forward<functor_type>(f)));
	}

	template <typename result_type, typename... arg_types>
	lua::variable<lua::cfunction<result_type(arg_types...)> > make_cfunction_variable(const std::string& name, result_type(*f)(arg_types...))
	{
		return lua::make_variable(name, lua::make_cfunction(f));
	}

	template <typename result_type, typename class_type, typename... arg_types>
	lua::variable<lua::cfunction<result_type(class_type*, arg_types...) > > make_cfunction_variable(const std::string& name, result_type(class_type::* f)(arg_types...))
	{
		return lua::make_variable(name, lua::make_cfunction(f));
	}

	template <typename result_type, typename... arg_types, typename functor_type>
	lua::variable<lua::cfunction<result_type(arg_types...)> > make_cfunction_variable(const std::string& name, functor_type&& f)
	{
		return lua::make_variable(name, lua::make_cfunction<result_type, arg_types...>(std::forward<functor_type>(f)));
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
			value.push(machine);
		}
	};
}

#endif
