#ifndef TYPE_SPECIALIZATIONS_H_dnweqdoiuwne9fu348rfk903idfmuiregj9203rkdmferjingvfdwe
#define TYPE_SPECIALIZATIONS_H_dnweqdoiuwne9fu348rfk903idfmuiregj9203rkdmferjingvfdwe

#include <boost/type_traits/is_array.hpp>

#include <lua.hpp>

#include <lua_engine/basic_lua_helpers.h>

namespace lua
{
	// general
    template <typename T>
	struct lua_stack_helper
	{
		static_assert(!boost::is_array<T>::value, "Arrays are not supported to be marshalled in/out of Lua");

		static bool type_matches(lua_State* machine, int position);
		static T pop(lua_State* machine);
		static void push(lua_State* machine, const T& value);
    };

	template <typename T>
	bool type_matches(lua_State* machine, int position)
	{
		return lua_stack_helper<T>::type_matches(machine, position);
	}

	template <typename T>
	T pop(lua_State* machine)
	{
		return lua_stack_helper<T>::pop(machine);
	}

	template <typename T>
	void push(lua_State* machine, const T& val)
	{
		return lua_stack_helper<T>::push(machine, val);
	}
	
    template<typename... varargs>
	struct all_pusher
	{
		static void push(lua_State*, varargs...)
		{
			static_assert(sizeof...(varargs) == 0, "Only for recursion end");
		}
	};

	template<typename... varargs>
	void push_all(lua_State* machine, varargs... args)
	{
		all_pusher<varargs...>::push(machine, args...);
	}

	template<typename vararg, typename... varargs>
	struct all_pusher<vararg, varargs...>
	{
		static void push(lua_State* machine, vararg arg, varargs... args)
		{
			lua::push<vararg>(machine, arg);
			push_all(machine, args...);
		}
	};
}

#include <typeinfo>
namespace lua
{
    template <typename T>
    struct lua_stack_helper<T*>
    {
        static bool type_matches(lua_State* machine, int position)
        {
            return lua_isuserdata(machine, position) == LUA_TRUE || lua_islightuserdata(machine, position) == LUA_TRUE;
        }

        static T* pop(lua_State* machine)
        {
			T* result = reinterpret_cast<T*>(lua_touserdata(machine, -1));

			lua_pop(machine, 1);
			if (result == NULL)
				throw std::bad_cast();

			return result;
        }

        static void push(lua_State* machine, T* t)
        {
            lua_pushlightuserdata(machine, t);
        } 
    };
}

namespace lua
{
	template<>
	struct lua_stack_helper<int>
	{
		static bool type_matches(lua_State* machine, int position)
		{
			return lua_isnumber(machine, position) == LUA_TRUE;
		}

		static int pop(lua_State* machine)
		{
			int has_conversion_succeeded = 0;
			int result = static_cast<int>(lua_tonumberx(machine, -1, &has_conversion_succeeded));

			lua_pop(machine, 1);
			if (has_conversion_succeeded == LUA_FALSE)
				throw std::bad_cast();

			return result;
		}

		static void push(lua_State* machine, int value)
		{
			lua_pushinteger(machine, value);
		}
	};
}

namespace lua
{
	template<>
	struct lua_stack_helper<const char*>
	{
		static bool type_matches(lua_State* machine, int position)
		{
			return lua_isstring(machine, position) == LUA_TRUE;
		}

		static const char* pop(lua_State* machine)
		{
			const char* popped = lua_tostring(machine, -1);
			lua_pop(machine, 1); 

			return popped;
		}

		static void push(lua_State* machine, const char* value)
		{
			lua_pushstring(machine, value);
		}
	};
}

#include <string>
namespace lua
{    
	template<>
	struct lua_stack_helper<std::string>
	{
		static bool type_matches(lua_State* machine, int position)
		{
			return lua_stack_helper<const char*>::type_matches(machine, position);
		}

		static std::string pop(lua_State* machine)
		{
			return std::string(lua_stack_helper<const char*>::pop(machine));
		}

		static void push(lua_State* machine, const std::string& value)
		{
			lua_stack_helper<const char*>::push(machine, value.c_str());
		}
	};
}


#include <boost/optional.hpp>
namespace lua
{
	template <typename T>
	struct lua_stack_helper<boost::optional<T> >
	{
		static bool type_matches(lua_State* machine, int position)
		{
			return (lua_isnil(machine, position) == LUA_TRUE) || lua::type_matches<T>(machine, position);
		}

		static boost::optional<T> pop(lua_State* machine)
		{
			if (lua_isnil(machine, -1) == LUA_TRUE)
			{
				lua_pop(machine, 1);
				return boost::none;
			}
			
			else
				return boost::optional<T>(lua::pop<T>(machine));
		}

		static void push(lua_State* machine, const boost::optional<T>& value)
		{
			if (value.is_initialized())
				push(machine, value.get());
			else
				lua_pushnil(machine);
		}
	};
}

#endif
