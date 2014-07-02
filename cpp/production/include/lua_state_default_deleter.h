#ifndef LUA_STATE_DEFAULT_DELETER_MSOdMAwidm39u8fmnedkcmslkdmiowefniuwenfdf
#define LUA_STATE_DEFAULT_DELETER_MSOdMAwidm39u8fmnedkcmslkdmiowefniuwenfdf

namespace std
{
	template <>
	struct default_delete<struct lua_State>
	{
		void operator()(struct lua_State* state)
		{
			lua_close(state);
		}
	};
}

#endif