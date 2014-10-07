#ifndef TABLE_dmwoqidm98fj948w3j5ungjfdnkvncmowecksmclksmdckwemofcugvbtrbh45gu8943rf329ir0932ir3r
#define TABLE_dmwoqidm98fj948w3j5ungjfdnkvncmowecksmclksmdckwemofcugvbtrbh45gu8943rf329ir0932ir3r

#include <memory>
#include <vector>

#include <lua_engine/variable.h>

namespace lua
{
	class table : public lua::entity
	{
	public:
		table(const std::string& name) : entity(name)
		{}

		void insert_into(lua_State* machine) const;

	private:
		std::vector<std::unique_ptr<lua::entity> > fields;
	};
}

#endif