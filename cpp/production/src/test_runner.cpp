#include <test_runner.h>
#include <ts_assert.h>

test_runner::test_runner(const configurator&)
{}

void test_runner::run_tests()
{}

void test_runner::attach_testcase_observer(testcase_observer* observer)
{
	observers.push_back(observer);
}

void test_runner::detach_testcase_observer(testcase_observer* observer)
{
	auto location = std::find(observers.begin(), observers.end(), observer);

	TS_ASSERT(location != observers.end(), "Cannot detach not attached observer");

	observers.erase(location);
}