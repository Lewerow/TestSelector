#include <boost/test/auto_unit_test.hpp>
#include <lua_engine.h>

#include <variable_operators.h>

struct lua_fixture
{
	lua::engine engine;
};

BOOST_AUTO_TEST_SUITE(libraries_sanity)
BOOST_FIXTURE_TEST_SUITE(Lua, lua_fixture)

BOOST_AUTO_TEST_CASE(throws_at_non_existing_variable)
{
	engine.load("x = 5");
	BOOST_CHECK_THROW(engine.get<int>("y"), std::runtime_error);
}

BOOST_AUTO_TEST_CASE(variable_loading_from_string)
{
	engine.load("x = 5");
	int x = engine.get<int>("x");
	BOOST_CHECK_EQUAL(x, 5);
}

BOOST_AUTO_TEST_CASE(variable_roundtrip)
{
	engine.push(lua::make_variable("x", 5));
	int x = engine.get<int>("x");
	BOOST_CHECK_EQUAL(x, 5);
}

BOOST_AUTO_TEST_CASE(variable_roundtrip_for_strings)
{
	engine.push(lua::make_variable<std::string>("x", "alamakota"));
	std::string x = engine.get<std::string>("x");
	BOOST_CHECK_EQUAL(x, "alamakota");
}

BOOST_AUTO_TEST_CASE(variable_is_same_after_roudtrip)
{
	auto var = lua::make_variable("x", 4);
	engine.push(var);
	BOOST_CHECK_EQUAL(var, engine.get_variable<int>("x"));
}

BOOST_AUTO_TEST_SUITE_END()
BOOST_AUTO_TEST_SUITE_END()