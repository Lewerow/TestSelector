#include <boost/test/auto_unit_test.hpp>
#include <turtle/mock.hpp>

#include <factory.h>

namespace
{
	struct some_type
	{};

	struct some_type_creator : creator<some_type>
	{};
}

struct factory_fixture
{
	factory<some_type> x;
};

BOOST_AUTO_TEST_SUITE(helpers)
BOOST_AUTO_TEST_SUITE(factories)
BOOST_FIXTURE_TEST_SUITE(unique_owner, factory_fixture)

BOOST_AUTO_TEST_CASE(objects_can_be_registered_to_factories)
{
	BOOST_CHECK(x.atomic_register_creator("some_type", some_type_creator()));
	BOOST_CHECK(x.atomic_register_creator("other_type", some_type_creator()));
}

BOOST_AUTO_TEST_CASE(same_object_cannot_be_registered_twice_without_unregistering)
{
	BOOST_CHECK(x.atomic_register_creator("some_type", some_type_creator()));
	BOOST_CHECK(!x.atomic_register_creator("some_type", some_type_creator()));

	BOOST_CHECK(x.atomic_unregister_creator("some_type"));
	BOOST_CHECK(!x.atomic_unregister_creator("some_type"));
	BOOST_CHECK(x.atomic_register_creator("some_type", some_type_creator()));
}

BOOST_AUTO_TEST_CASE(products_have_proper_types)
{
	BOOST_CHECK(x.atomic_register_creator("some_type", some_type_creator()));
}

BOOST_AUTO_TEST_CASE(by_default_unique_ownership_is_returned)
{
	BOOST_CHECK(x.atomic_register_creator("some_type", some_type_creator()));
	std::unique_ptr<some_type> ptr = x.produce("some_type");
	BOOST_CHECK(dynamic_cast<some_type*>(ptr.get()) != nullptr);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(shared_owner)
BOOST_AUTO_TEST_CASE(works_well)
{
	factory<some_type, policies::shared_ownership> shared_factory;
	BOOST_CHECK(shared_factory.atomic_register_creator("some_type", some_type_creator()));
	auto ptr = shared_factory.produce("some_type");
	BOOST_CHECK(dynamic_cast<some_type*>(ptr.get()) != nullptr);
}


BOOST_AUTO_TEST_SUITE_END()
BOOST_AUTO_TEST_SUITE_END()
BOOST_AUTO_TEST_SUITE_END()