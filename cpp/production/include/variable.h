#ifndef VARIABLE_H_mciewomcreuifgvnjmk094w3rfj45ungjkrfmdflweiru89fh3jdkfwmld90238hnejfdkwqd
#define VARIABLE_H_mciewomcreuifgvnjmk094w3rfj45ungjkrfmdflweiru89fh3jdkfwmld90238hnejfdkwqd

#include <string>
#include <exception>
#include <type_traits>
#include <boost/optional.hpp>

#include <lua.hpp>

#include <ts_assert.h>
#include <type_specializations.h>

namespace lua
{
	template <typename T>
	class variable
	{
	public:

		variable(const std::string& varname, T val) : name_{varname}, value_{val}
		{}

		variable(const std::string& varname) : name_(varname), value_(boost::none)
		{}

		variable& get_value_from(lua_State* machine)
		{
			TS_ASSERT(!value_.is_initialized(), "Cannot re-get variable");

			lua_getglobal(machine, name().c_str());
			if (type_matches<T>(machine, -1))
				value_ = take<T>(machine, -1);
			else
				throw std::runtime_error("Requested variable with wrong type: " + name());

			return *this;
		}

		void insert_into(lua_State* machine) const
		{
			push(machine, value());
			lua_setglobal(machine, name().c_str());
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

		variable(const variable<T>& rhs) : name_(rhs.name()), value_(rhs.value())
		{}

		variable<T>& operator= (const variable<T>& rhs)
		{
			*this = variable<T>(rhs);
			return *this;
		}

	private:
		std::string name_;
		boost::optional<T> value_;
	};

	template <typename T>
	variable<T> make_variable(const std::string& name, const T& val)
	{
		return variable<T>(name, val);
	}
}

#endif