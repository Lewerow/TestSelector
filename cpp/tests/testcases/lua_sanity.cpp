#include <boost/test/auto_unit_test.hpp>

#include <lua_engine/engine.h>
#include <lua_engine/variable_operators.h>
#include <lua_engine/metatable.h>

struct lua_fixture
{
	lua::engine engine;
};

struct S
{
    int f(int a)
    {
        return 2*a + c;
    }

    int c;
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
	engine.load(lua::make_variable("x", 5));
	int x = engine.get<int>("x");
	BOOST_CHECK_EQUAL(x, 5);
}

BOOST_AUTO_TEST_CASE(pointers_can_be_loaded)
{
    std::unique_ptr<int> a(new int(600));
	engine.load(lua::make_variable("a", a.get()));
    BOOST_CHECK_EQUAL(lua::llightuserdata, engine.typeof("a"));
    BOOST_CHECK_EQUAL(*a, *engine.get<int*>("a"));
}

BOOST_AUTO_TEST_CASE(variable_roundtrip_for_strings)
{
	engine.load(lua::make_variable<std::string>("x", "alamakota"));
	std::string x = engine.get<std::string>("x");
	BOOST_CHECK_EQUAL(x, "alamakota");
}

BOOST_AUTO_TEST_CASE(variable_is_same_after_roudtrip)
{
	auto var = lua::make_variable("x", 4);
	engine.load(var);
	BOOST_CHECK_EQUAL(var.value(), engine.get<int>("x"));
}

BOOST_AUTO_TEST_CASE(variable_type_can_be_converted_to_string)
{
    auto var = lua::make_variable("x", 4);
	engine.load(var);
    BOOST_CHECK_EQUAL(std::string("4"), engine.get<std::string>("x"));
}

BOOST_AUTO_TEST_CASE(variable_type_can_be_converted_to_int)
{
	auto var = lua::make_variable("x", std::string("333"));
	engine.load(var);
	BOOST_CHECK_EQUAL(333, engine.get<int>("x"));
}

BOOST_AUTO_TEST_CASE(types_are_preserved_in_lua)
{
	engine.load(lua::make_variable("x", 333));
	BOOST_CHECK_EQUAL(lua::lnumber, engine.typeof("x"));
	BOOST_CHECK_EQUAL(333, engine.get<int>("x"));

	engine.load(lua::make_variable<std::string>("y", "333"));
	BOOST_CHECK_EQUAL(lua::lstring, engine.typeof("y"));
	BOOST_CHECK_EQUAL("333", engine.get<std::string>("y"));

	BOOST_CHECK_EQUAL(engine.get<int>("x"), engine.get<int>("y"));
}

BOOST_AUTO_TEST_CASE(optional_roundtrip)
{
	engine.load(lua::make_variable("x", boost::optional<std::string>()));
	BOOST_CHECK(!engine.get<boost::optional<int> >("x").is_initialized());
	BOOST_CHECK(!engine.get<boost::optional<std::string> >("y").is_initialized());

	engine.load(lua::make_variable("y", 5));
	auto var = engine.get<boost::optional<int> >("y");
	BOOST_CHECK(var.is_initialized());
	BOOST_CHECK_EQUAL(var.get(), 5); 
}

BOOST_AUTO_TEST_CASE(configuration_file_can_be_read)
{
	engine.load_file("../../tests/files/fake_file_for_lua_engine_checks.lua");
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

BOOST_AUTO_TEST_CASE(function_without_arguments_can_be_called)
{
	engine.load("function add_special() return 20 end");
	auto result = engine.call<std::string>("add_special");
	BOOST_CHECK_EQUAL("20", result);
}

int helper_cfunction(void)
{
    return 2;
}

BOOST_AUTO_TEST_CASE(cfunctions_can_be_called)
{
    auto f = lua::make_cfunction_variable("f", helper_cfunction);
    engine.load(f);
    BOOST_CHECK_EQUAL(helper_cfunction(), engine.call<int>("f"));
}

int helper_cfunction_with_args(int a, int b)
{
    return a + b;
}

BOOST_AUTO_TEST_CASE(cfunction_with_arguments_can_be_called)
{
    auto f = lua::make_cfunction_variable("sum", helper_cfunction_with_args);
    engine.load(f);
    BOOST_CHECK_EQUAL(helper_cfunction_with_args(2, 5), engine.call<int>("sum", 2, 5));
}

BOOST_AUTO_TEST_CASE(multiple_cfunctions_may_be_registered_with_same_name_last_one_stands)
{
    auto f = lua::make_cfunction_variable("sum", helper_cfunction);
    auto f2 = lua::make_cfunction_variable("sum", helper_cfunction_with_args);

    engine.load(f);
    engine.load(f2);

    BOOST_CHECK_EQUAL(2, engine.call<int>("sum", 2, 0));
}

BOOST_AUTO_TEST_CASE(member_functions_can_be_called_from_lua)
{
    S s;
    s.c = 10;
    auto f = lua::make_cfunction_variable("f", &S::f);
    s.c = 50;
    engine.load(f);
	engine.load(lua::make_variable("s", &s));
    engine.load("function g() return f(s, 10) end");    

    BOOST_CHECK_EQUAL(s.f(10), engine.call<int>("g"));
   
   s.c = 100;
   BOOST_CHECK_EQUAL(s.f(106), engine.call<int>("f", &s, 106));
}

BOOST_AUTO_TEST_CASE(member_functions_can_be_called_from_cpp)
{
    S s;
    s.c = 10;
    auto f = lua::make_cfunction_variable("f", &S::f);
    s.c = 50;
    engine.load(f);
    
   BOOST_CHECK_EQUAL(s.f(10), engine.call<int>("f", &s, 10));
   
   s.c = 100;
   BOOST_CHECK_EQUAL(s.f(106), engine.call<int>("f", &s, 106));
}

BOOST_AUTO_TEST_CASE(lambda_can_be_used_as_cfunction_but_require_explicit_template_parameters)
{
	auto g = [](int i) -> int{return 2 * i; };
	auto f = lua::make_cfunction_variable<int, int>("f", g);
	engine.load(f);

	BOOST_CHECK_EQUAL(g(100), engine.call<int>("f", 100));
	BOOST_CHECK_EQUAL(g(1000), engine.call<int>("f", 1000));
}

BOOST_AUTO_TEST_CASE(bound_functions_can_be_used_as_cfunctions_but_require_explicit_template_parameters)
{
	S s;
	s.c = 100;

	auto f = lua::make_cfunction_variable<int, int>("f", std::bind(&S::f, &s, std::placeholders::_1));
    engine.load(f);
    BOOST_CHECK_EQUAL(lua::luserdata, engine.typeof("f"));
	BOOST_CHECK_EQUAL(s.f(100), engine.call<int>("f", 100));
	
	s.c = 1000;
	BOOST_CHECK_EQUAL(s.f(100), engine.call<int>("f", 100));
}


BOOST_AUTO_TEST_CASE(throws_on_non_existing_function_call)
{
	engine.load("function add_special(a,b) return 2*a+b end");
    BOOST_CHECK_EQUAL(lua::lfunction, engine.typeof("add_special"));
	BOOST_CHECK_THROW(engine.call<std::string>("add", 2, 5), std::runtime_error);
}

BOOST_AUTO_TEST_CASE(tables_can_be_queried_as_variables)
{
	engine.load("x = {} x.x = 2");
	BOOST_CHECK_EQUAL(engine.get<int>("x.x"), 2);
	
	engine.load("y = {} y.y = {} y.y.y = {} y.y.y.y = 45");
	BOOST_CHECK_EQUAL(engine.get<std::string>("y.y.y.y"), "45");
}

BOOST_AUTO_TEST_CASE(table_variable_roudtrip_with_multiple_access)
{
	engine.load(lua::make_variable("x.x", 5));
	auto x = engine.get<int>("x.x");
	BOOST_CHECK_EQUAL(x, 5);

	engine.load(lua::make_variable<std::string>("y.y.y.y", "a45"));
	auto y = engine.get<std::string>("y.y.y.y");
	BOOST_CHECK_EQUAL(y, "a45");
    BOOST_CHECK_EQUAL(lua::ltable, engine.typeof("y"));
    BOOST_CHECK_EQUAL(lua::ltable, engine.typeof("y.y"));
    BOOST_CHECK_EQUAL(lua::ltable, engine.typeof("y.y.y"));
    BOOST_CHECK_EQUAL(lua::lstring, engine.typeof("y.y.y.y"));

	auto xx = engine.get<int>("x.x");
    BOOST_CHECK_EQUAL(lua::ltable, engine.typeof("x"));
	BOOST_CHECK_EQUAL(xx, 5);

	auto yy = engine.get<std::string>("y.y.y.y");
	BOOST_CHECK_EQUAL(yy, "a45");

	engine.load(lua::make_variable<int>("x.y.z", 19));
	BOOST_CHECK_EQUAL(lua::ltable, engine.typeof("x"));
	BOOST_CHECK_EQUAL(lua::ltable, engine.typeof("x.y"));
	BOOST_CHECK_EQUAL(lua::lnumber, engine.typeof("x.y.z"));
	BOOST_CHECK_EQUAL(19, engine.get<int>("x.y.z"));
}

BOOST_AUTO_TEST_CASE(tables_can_be_created)
{
	lua::table tab;
	engine.load(lua::make_variable("x", tab));

    BOOST_CHECK_EQUAL(lua::ltable, engine.typeof("x"));
}

BOOST_AUTO_TEST_CASE(multiple_values_can_be_inserted_to_table_independently)
{
	lua::table tab;
	engine.load(lua::make_variable("x", tab));

	lua::variable<int> a("x.a", 2);
	engine.load(a);

	lua::variable<std::string> b("x.b", "b");
	engine.load(b);

	BOOST_CHECK_EQUAL(lua::ltable, engine.typeof("x"));
	BOOST_CHECK_EQUAL(lua::lnumber, engine.typeof("x.a"));
	BOOST_CHECK_EQUAL(2, engine.get<int>("x.a"));
	BOOST_CHECK_EQUAL(lua::lstring, engine.typeof("x.b"));
	BOOST_CHECK_EQUAL("b", engine.get<std::string>("x.b"));
}

BOOST_AUTO_TEST_CASE(values_are_inserted_together_with_their_table)
{
	lua::table tab;
	tab.add_field("a", 2);
	tab.add_field("b", std::string("b"));

	auto v = lua::make_variable("x.v", 22);

	engine.load(lua::make_variable("x", tab));
	engine.load(v);

	BOOST_CHECK_EQUAL(lua::ltable, engine.typeof("x"));
	BOOST_CHECK_EQUAL(lua::lnumber, engine.typeof("x.a"));
	BOOST_CHECK_EQUAL(2, engine.get<int>("x.a"));
	BOOST_CHECK_EQUAL(lua::lstring, engine.typeof("x.b"));
	BOOST_CHECK_EQUAL("b", engine.get<std::string>("x.b"));
	BOOST_CHECK_EQUAL(22, engine.get<int>("x.v"));
}

BOOST_AUTO_TEST_CASE(table_fields_are_removable)
{
	lua::table tab;
	tab.add_field("a", 2);
	tab.add_field("b", std::string("b"));
	tab.remove_field("b");

	engine.load(lua::make_variable("x", tab));

	BOOST_CHECK_EQUAL(lua::ltable, engine.typeof("x"));
	BOOST_CHECK_EQUAL(lua::lnumber, engine.typeof("x.a"));
	BOOST_CHECK_EQUAL(2, engine.get<int>("x.a"));
	BOOST_CHECK_EQUAL(lua::lnil, engine.typeof("x.b"));
}

BOOST_AUTO_TEST_CASE(last_inserted_table_stands)
{
	lua::table a;
	a.add_field("a", 2);
	a.add_field("b", 5);

	lua::table b;
	b.add_field("a", 10);
	b.add_field("c", 100);

	engine.load(lua::make_variable("a", a));
	engine.load(lua::make_variable("a", b));

	BOOST_CHECK_EQUAL(lua::ltable, engine.typeof("a"));
	BOOST_CHECK_EQUAL(lua::lnil, engine.typeof("a.b"));
	BOOST_CHECK_EQUAL(10, engine.get<int>("a.a"));
	BOOST_CHECK_EQUAL(100, engine.get<int>("a.c"));
}

BOOST_AUTO_TEST_CASE(default_metatable_is_empty)
{
	struct A
	{};

	lua::metatable a = lua::make_metatable<A>();
	/*
	engine.load(lua::make_variable("a", a));

	BOOST_CHECK_EQUAL(lua::ltable, engine.typeof("a"));
	BOOST_CHECK_EQUAL(0, engine.get<lua::table>("a").total_size());*/
}

BOOST_AUTO_TEST_SUITE_END()
BOOST_AUTO_TEST_SUITE_END()
