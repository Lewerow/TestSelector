#include <boost/test/auto_unit_test.hpp>
#include <test_runner.h>
#include <testcase_chooser.h>

#include <vector>
#include <string>
#include <regex>

BOOST_AUTO_TEST_SUITE(test_runner_tests)

BOOST_AUTO_TEST_CASE(testcase_chooser_chooses_testcases_matching_filter)
{
	coverage_generator::testcase_chooser chooser ({ "first_test", "second_test", "third_test", "xxx", "", "xxx_test" });
    chooser.filter(std::regex(".*_test"));

	auto chosen = chooser.names();
	auto expected = { "first_test", "second_test", "third_test", "xxx_test" };
	BOOST_CHECK_EQUAL_COLLECTIONS(chosen.begin(), chosen.end(), expected.begin(), expected.end());
}


BOOST_AUTO_TEST_CASE(test_runner_shall_run_all_tests)
{
	coverage_generator::test_runner runner;
}

BOOST_AUTO_TEST_SUITE_END()