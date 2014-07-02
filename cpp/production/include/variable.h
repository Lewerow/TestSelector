#ifndef VARIABLE_H_mciewomcreuifgvnjmk094w3rfj45ungjkrfmdflweiru89fh3jdkfwmld90238hnejfdkwqd
#define VARIABLE_H_mciewomcreuifgvnjmk094w3rfj45ungjkrfmdflweiru89fh3jdkfwmld90238hnejfdkwqd

#include <string>
#include <exception>
#include <type_traits>
#include <boost/optional.hpp>

#include <lua.hpp>

#include <ts_assert.h>

namespace lua
{
	template <typename T>
	class variable
	{
	public:

		variable(const std::string& varname, T val) : name_(varname), value_(val)
		{}

		variable(const std::string& varname) : name_(varname), value_(boost::none)
		{}

		variable& get(lua_State* machine)
		{
			TS_ASSERT(!value_.is_initialized(), "Cannot re-get variable");

			lua_getglobal(machine, name().c_str());
			if (type<T>(machine, -1))
				value_ = take<T>(machine, -1);
			else
				throw std::runtime_error("Requested variable with wrong type: " + name());

			return *this;
		}

		const T& value() const
		{
			TS_ASSERT(value_.is_initialized(), "Cannot get value of a non-initialized variable");
			return value_.get();
		}

		const std::string& name() const
		{
			return name_;
		}

	private:
		std::string name_;
		boost::optional<T> value_;
	};
	
	template <typename T>
	bool type(lua_State* machine, int position);
	template<typename T>
	T take(lua_State* machine, int position);

	template<>
	bool type<int>(lua_State* machine, int position);
	template<>
	int take<int>(lua_State* machine, int position);
}

#endif