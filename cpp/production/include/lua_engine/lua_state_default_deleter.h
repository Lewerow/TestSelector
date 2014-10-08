#ifndef LUA_STATE_DEFAULT_DELETER_MSOdMAwidm39u8fmnedkcmslkdmiowefniuwenfdf
#define LUA_STATE_DEFAULT_DELETER_MSOdMAwidm39u8fmnedkcmslkdmiowefniuwenfdf

#include <memory>

struct lua_State;

namespace std
{
	template <>
	struct default_delete<lua_State>
	{
		void operator()(lua_State* state)
		{
			lua_close(state);
		}
	};
}

#endif