#ifndef RESULT_CONVERTER_FACTORY_H_fioewrfj490fjuivckmeodfkiwefnrigfqdqwd90j3jd093d93d093jfrnkjsdnckjdnkvnfdjvnfdvnfv
#define RESULT_CONVERTER_FACTORY_H_fioewrfj490fjuivckmeodfkiwefnrigfqdqwd90j3jd093d93d093jfrnkjsdnckjdnkvnfdjvnfdvnfv

#include <string>
#include <memory>

#include <result_converter.h>

namespace coverage_generator
{
	class result_converter_factory
	{
		result_converter_factory();

		std::unique_ptr<result_converter> make_result_converter(const std::string& name);
	};
}

#endif