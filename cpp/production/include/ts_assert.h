#ifndef TS_ASSERT_H_miocewm98fj34985j4ugnrfjdmcd903dk382jf4ufierjfdslkq90d823jfujrfvdmkldmc
#define TS_ASSERT_H_miocewm98fj34985j4ugnrfjdmcd903dk382jf4ufierjfdslkq90d823jfujrfvdmkldmc

#include <string>

struct assertion_failed
{
	assertion_failed(const std::string& msg) : what(msg)
	{}

	std::string what;
};

#define TS_ASSERT(x, msg) while(!(x)){throw assertion_failed(msg);}

#endif