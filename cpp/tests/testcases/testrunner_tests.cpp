#include <boost/test/auto_unit_test.hpp>

#include <vector>
#include <string>
#include <regex>

#include <configurator.h>
#include <test_execution_strategy.h>
#include <coverage_gathering_strategy.h>
#include <test_runner.h>
#include <testcase_chooser.h>


BOOST_AUTO_TEST_SUITE(test_runner_tests)

BOOST_AUTO_TEST_CASE(testcase_chooser_chooses_testcases_matching_filter)
{
	coverage_generator::testcase_chooser chooser ({ "first_test", "second_test", "third_test", "xxx", "", "xxx_test" });
    chooser.filter(std::regex(".*_test"));

	auto check_testcases = [&chooser](const std::vector<std::string>& expected) {
		auto chosen = chooser.names();
		BOOST_CHECK_EQUAL_COLLECTIONS(chosen.begin(), chosen.end(), expected.begin(), expected.end());
	};

    check_testcases({ "first_test", "second_test", "third_test", "xxx_test" });

	chooser.filter(std::regex("xxx.*"));
	check_testcases({ "xxx_test" });
}


BOOST_AUTO_TEST_CASE(test_runner_shall_run_all_tests)
{
	std::shared_ptr<coverage_generator::configurator> configurator(std::make_shared<coverage_generator::configurator>(boost::filesystem::path("../../tests/files/basic_configuration.lua")));
	std::unique_ptr<coverage_generator::coverage_gathering_strategy> gathering_strategy(std::make_unique<coverage_generator::coverage_gathering_strategy>(configurator));
	std::unique_ptr<coverage_generator::test_execution_strategy> execution_strategy(std::make_unique<coverage_generator::test_execution_strategy>(configurator));
	coverage_generator::test_runner runner;
}

BOOST_AUTO_TEST_SUITE_END()