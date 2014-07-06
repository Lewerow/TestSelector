#include <coverage_gathering_strategy.h>

namespace coverage_generator
{
	coverage_gathering_strategy::coverage_gathering_strategy(const std::shared_ptr<configurator>& configuration) : config(configuration)
	{
	}


	coverage_gathering_strategy::~coverage_gathering_strategy()
	{
	}
}