#include <configurator.h>

namespace coverage_generator
{
	configurator::configurator(const boost::filesystem::path& configuration_file)
	{
		configuration_engine.load_file(configuration_file);
	}


	configurator::~configurator()
	{
	}
}