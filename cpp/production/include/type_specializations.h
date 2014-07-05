#ifndef TYPE_SPECIALIZATIONS_H_dnweqdoiuwne9fu348rfk903idfmuiregj9203rkdmferjingvfdwe
#define TYPE_SPECIALIZATIONS_H_dnweqdoiuwne9fu348rfk903idfmuiregj9203rkdmferjingvfdwe

#include <lua.hpp>

const int LUA_TRUE = 1;
const int LUA_FALSE = 0;


namespace lua
{
	// general
    template <typename T>
	struct lua_stack_helper
	{
		static bool type_matches(lua_State* machine, int position);
		static T pop(lua_State* machine, int position);
		static void push(lua_State* machine, const T& value);
	};

	template <typename T>
	bool type_matches(lua_State* machine, int position)
	{
		return lua_stack_helper<T>::type_matches(machine, position);
	}

	template <typename T>
	T pop(lua_State* machine, int position)
	{
		return lua_stack_helper<T>::pop(machine, position);
	}

	template <typename T>
	void push(lua_State* machine, const T& val)
	{
		return lua_stack_helper<T>::push(machine, val);
	}

	void push_all(lua_State* machine);

	template<typename Arg, typename... Args>
	void push_all(lua_State* machine, Arg arg, Args... args)
	{
		push<Arg>(machine, arg);
		push_all(machine, args...);
	}
}

#include <typeinfo>

namespace lua
{
	template<>
	struct lua_stack_helper<int>
	{
		static bool type_matches(lua_State* machine, int position)
		{
			return lua_isnumber(machine, position) == LUA_TRUE;
		}

		static int pop(lua_State* machine, int position)
		{
			int has_conversion_succeeded = 0;
			int result = static_cast<int>(lua_tonumberx(machine, position, &has_conversion_succeeded));

			if (has_conversion_succeeded == LUA_FALSE)
				throw std::bad_cast();

			return result;
		}

		static void push(lua_State* machine, const int& value)
		{
			lua_pushinteger(machine, value);
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
			return lua_isstring(machine, position) == LUA_TRUE;
		}

		static std::string pop(lua_State* machine, int position)
		{
			return std::string(lua_tostring(machine, position));
		}

		static void push(lua_State* machine, const std::string& value)
		{
			lua_pushstring(machine, value.c_str());
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

		static boost::optional<T> pop(lua_State* machine, int position)
		{
			if (lua_isnil(machine, position) == LUA_TRUE)
				return boost::none;
			else
				return boost::optional<T>(lua::pop<T>(machine, position));
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