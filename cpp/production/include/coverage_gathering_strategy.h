#ifndef COVERAGE_GATHERING_STRATEGY_H_ncfiojf934u5ugvcnkmewmdcijweduf23h49d8j23duihwiefhndsjkffihejriufghreiughuiewjfwqdjojoksadmcijsnciernvuib
#define COVERAGE_GATHERING_STRATEGY_H_ncfiojf934u5ugvcnkmewmdcijweduf23h49d8j23duihwiefhndsjkffihejriufghreiughuiewjfwqdjojoksadmcijsnciernvuib

#include <configurator.h>

namespace coverage_generator
{
	class coverage_gathering_strategy
	{
	public:
		coverage_gathering_strategy(const std::shared_ptr<configurator>& configuration);
		virtual ~coverage_gathering_strategy();

	private:
		std::shared_ptr<configurator> config;
	};
}

#endif
