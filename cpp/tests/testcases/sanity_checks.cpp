#include <boost/test/auto_unit_test.hpp>

BOOST_AUTO_TEST_SUITE(sanity_checks)

BOOST_AUTO_TEST_CASE(asserts_work)
{
	BOOST_CHECK_EQUAL(true, !false);
}

BOOST_AUTO_TEST_SUITE_END()