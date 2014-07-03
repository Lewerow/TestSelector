#ifndef TYPE_SPECIALIZATIONS_H_dnweqdoiuwne9fu348rfk903idfmuiregj9203rkdmferjingvfdwe
#define TYPE_SPECIALIZATIONS_H_dnweqdoiuwne9fu348rfk903idfmuiregj9203rkdmferjingvfdwe

struct lua_State;

namespace lua
{
	// general
	template <typename T>
	bool type_matches(lua_State* machine, int position);
	template <typename T>
	T take(lua_State* machine, int position);
	template <typename T>
	void push(lua_State* machine, const T& val);
	

	// int
	template<>
	bool type_matches<int>(lua_State* machine, int position);
	template<>
	int take<int>(lua_State* machine, int position);
	template<>
	void push<int>(lua_State* machine, const int& value);

	// std::string
	template<>
	bool type_matches<std::string>(lua_State* machine, int position);
	template<>
	std::string take<std::string>(lua_State* machine, int position);
	template<>
	void push<std::string>(lua_State* machine, const std::string& value);
}

#endif