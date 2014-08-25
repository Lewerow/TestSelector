#include <boost/test/auto_unit_test.hpp>

#include <vector>
#include <string>
#include <regex>

#include <configurator.h>
#include <test_execution_strategy.h>
#include <coverage_gathering_strategy.h>
#include <test_runner.h>
#include <testcase_chooser.h>
#include <coverage_database.h>
#include <coverage_generator.h>


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

struct fixture
{/*
	configurator conf("../../tests/files/basic_configuration.lua");
	coverage::database cdb;
	test_runner tr(conf);
	coverage::generator cg(cdb);

	tr.attach_testcase_observer(&cg);
	tr.run_tests();
	tr.detach_testcase_observer(&cg);
	*/
};

BOOST_AUTO_TEST_CASE(asserts_at_detaching_not_attached)
{

}

BOOST_AUTO_TEST_CASE(typical_use_case)
{
	configurator conf("../../tests/files/basic_configuration.lua");
	coverage::database cdb;
	test_runner tr(conf);
	coverage::generator cg(cdb);

	tr.attach_testcase_observer(&cg);
	tr.run_tests();
	tr.detach_testcase_observer(&cg);
}

BOOST_AUTO_TEST_SUITE_END()