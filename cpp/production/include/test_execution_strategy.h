#ifndef TEST_EXECUTION_STRATEGY_Mionmdweuiondfiuerwfgn94fj324890fjdcvndkslfmweiofjuefgi4f4trf4fedxcsd
#define TEST_EXECUTION_STRATEGY_Mionmdweuiondfiuerwfgn94fj324890fjdcvndkslfmweiofjuefgi4f4trf4fedxcsd

#include <configurator.h>

namespace coverage_generator
{
	class test_execution_strategy
	{
	public:
		test_execution_strategy(const std::shared_ptr<configurator>& configuration);
		virtual ~test_execution_strategy();

	private:
		std::shared_ptr<configurator> config;
	};
}

#endif