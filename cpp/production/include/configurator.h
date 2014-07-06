#ifndef CONFIGURATOR_H_Dwoiqdjmw984379ufnwejdmsklaxslpsqk09qjkd98j21jer9dfjiugfnvfkdvfvfd
#define CONFIGURATOR_H_Dwoiqdjmw984379ufnwejdmsklaxslpsqk09qjkd98j21jer9dfjiugfnvfkdvfvfd

#include <boost/filesystem/path.hpp>

#include <lua_engine.h>

namespace coverage_generator
{
	class configurator
	{
	public:
		configurator(const boost::filesystem::path& configuration_file);
		~configurator();

	private:
		lua::engine configuration_engine;
	};
}

#endif