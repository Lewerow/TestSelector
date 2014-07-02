#include <boost/test/auto_unit_test.hpp>
#include <test_runner.h>

BOOST_AUTO_TEST_SUITE(test_runner_tests)

BOOST_AUTO_TEST_CASE(test_runner_shall_run_all_tests)
{
	coverage_generator::test_runner runner;
}

BOOST_AUTO_TEST_SUITE_END()