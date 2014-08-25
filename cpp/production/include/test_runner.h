#ifndef TEST_RUNNER_H_MOImf432rtf89urifmdcllx0q2eo239rfjrfdwfwefewfwef
#define TEST_RUNNER_H_MOImf432rtf89urifmdcllx0q2eo239rfjrfdwfwefewfwef

#include <vector>

#include <configurator.h>
#include <testcase_observer.h>

class test_runner
{
public:
	test_runner(const configurator&);

	void attach_testcase_observer(testcase_observer* observer);
	void detach_testcase_observer(testcase_observer* observer);

	void run_tests();

private:
	std::vector<testcase_observer*> observers;
};

#endif