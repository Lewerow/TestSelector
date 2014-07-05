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

BOOST_AUTO_TEST_CASE(optional_roundtrip)
{
	engine.push(lua::make_variable("x", boost::optional<std::string>()));
	BOOST_CHECK(!engine.get<boost::optional<int> >("x").is_initialized());
	BOOST_CHECK(!engine.get<boost::optional<std::string> >("y").is_initialized());

	engine.push(lua::make_variable("y", 5));
	auto var = engine.get<boost::optional<int> >("y");
	BOOST_CHECK(var.is_initialized());
	BOOST_CHECK_EQUAL(var.get(), 5); 
}

BOOST_AUTO_TEST_CASE(configuration_file_can_be_read)
{
	engine.load_file("../../tests/files/basic_configuration.lua");
	BOOST_CHECK_EQUAL(engine.get<int>("x"), 5);
	BOOST_CHECK_EQUAL(engine.get<std::string>("y"), "kot");
}

BOOST_AUTO_TEST_CASE(invalid_data_throws)
{
	BOOST_CHECK_THROW(engine.load("x = 2'sad'"), std::runtime_error);
}

BOOST_AUTO_TEST_CASE(add_function_can_be_called)
{
	engine.load("function add(a,b) return a+b end");
	auto sum = engine.call<int>("add", 2, 3);
	BOOST_CHECK_EQUAL(sum, 5);
}

BOOST_AUTO_TEST_CASE(add_function_can_be_called_with_more_args)
{
	engine.load("function add(a,b) return a+b end");
	auto sum = engine.call<int>("add", 2, 3, 5, 6);
	BOOST_CHECK_EQUAL(sum, 5);
}

BOOST_AUTO_TEST_CASE(data_is_convertible_between_types)
{
	engine.load("function add(a,b) return a+b end");
	auto sum = engine.call<std::string>("add", 2, 5);
	BOOST_CHECK_EQUAL("7", sum);
}

BOOST_AUTO_TEST_CASE(order_in_call_is_the_same)
{
	engine.load("function add_special(a,b) return 2*a+b end");
	auto sum = engine.call<std::string>("add_special", 2, 5);
	BOOST_CHECK_EQUAL("9", sum);
}

BOOST_AUTO_TEST_CASE(throws_on_non_existing_function_call)
{
	engine.load("function add_special(a,b) return 2*a+b end");
	BOOST_CHECK_THROW(engine.call<std::string>("add", 2, 5), std::runtime_error);
}

BOOST_AUTO_TEST_SUITE_END()
BOOST_AUTO_TEST_SUITE_END()