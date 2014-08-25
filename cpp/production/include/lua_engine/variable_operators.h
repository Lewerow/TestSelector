#ifndef VARIABLE_OPERATORS_KPOKFIOEWMFnrewiufg48rijfdnmvcqewd983h2rfuefjkfew
#define VARIABLE_OPERATORS_KPOKFIOEWMFnrewiufg48rijfdnmvcqewd983h2rfuefjkfew

#include <ostream>

#include <lua_engine/variable.h>

namespace lua
{
	template <typename T>
	bool operator==(const lua::variable<T>& lhs, const variable<T>& rhs)
	{
		return lhs.name() == rhs.name() && lhs.value() == rhs.value();
	}

	template <typename T>
	std::ostream& operator<<(std::ostream& str, const variable<T>& var)
	{
		str << var.name() << ":" << var.value();
		return str;
	}
}

#endif