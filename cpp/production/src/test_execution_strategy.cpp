#include <test_execution_strategy.h>

namespace coverage_generator
{
	test_execution_strategy::test_execution_strategy(const std::shared_ptr<configurator>& configuration) : config(configuration)
	{
	}


	test_execution_strategy::~test_execution_strategy()
	{
	}
}