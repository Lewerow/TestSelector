#ifndef VARIABLE_H_mciewomcreuifgvnjmk094w3rfj45ungjkrfmdflweiru89fh3jdkfwmld90238hnejfdkwqd
#define VARIABLE_H_mciewomcreuifgvnjmk094w3rfj45ungjkrfmdflweiru89fh3jdkfwmld90238hnejfdkwqd

#include <string>
#include <vector>
#include <exception>
#include <type_traits>
#include <numeric>

#include <boost/optional.hpp>
#include <lua.hpp>

#include <ts_assert.h>

#include <lua_engine/type_specializations.h>
#include <lua_engine/basic_lua_helpers.h>
#include <lua_engine/entity.h>

namespace lua
{
	template <typename T>
	class variable : public lua::entity
	{
	public:

		variable(const std::string& varname, T value) : entity(varname), val(value)
		{}

		variable(const std::string& varname) : entity(varname), val(boost::none)
		{}

		variable& get_value_from(lua_State* machine)
		{
			helpers::scoped::no_stack_size_change_verifier verifier(machine);
			helpers::scoped::pop_n popper(machine, (path.size() > 0) ? path.size() - 1 : 0);

			TS_ASSERT(!val.is_initialized(), "Cannot re-get variable");
			TS_ASSERT(!path.empty(), "Cannot get variable without name!");

			fetch_on_stack(machine);
			if (type_matches<T>(machine, -1))
				val = pop<T>(machine);
			else
			{
				lua_pop(machine, 1);
                std::string error_text = "Requested variable with wrong type: " + name();
				throw std::runtime_error(error_text);
			}

			return *this;
		}

		virtual void insert_into(lua_State* machine) const
		{
			helpers::scoped::no_stack_size_change_verifier verifier(machine);

			if(path.size() > 1)
			{
				helpers::scoped::pop_n popper(machine, path.size() - 1);
				helpers::get_or_create_global_table(machine, path[0]);
				
				for (std::size_t i = 1; i < path.size() - 1; ++i)
					helpers::get_or_create_table_as_field(machine, path[i]);

				push(machine, path.back().c_str());
				push(machine, value());
				lua_settable(machine, -3);
			}
			else
			{
				push(machine, value());
				lua_setglobal(machine, name().c_str());
			}
		}

		const T& value() const
		{
			TS_ASSERT(val.is_initialized(), "Cannot get value of a non-initialized variable");
			return val.get();
		}

		variable(const variable<T>& rhs) : entity(rhs.name()), val(rhs.value())
		{}

		variable<T>& operator= (const variable<T>& rhs)
		{
			*this = variable<T>(rhs);
			return *this;
		}

	private:
		boost::optional<T> val;
	};

	template <typename T>
	variable<T> make_variable(const std::string& name, const T& val)
	{
		return variable<T>(name, val);
	}

	template <typename T>
	void push(lua_State* machine, const lua::variable<T>& var)
	{
		push(machine, var.value());
	}

}

#endif
