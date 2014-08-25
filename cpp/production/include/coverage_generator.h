#ifndef COVERAGE_GENERATOR_H_fmweoif409fergnjergtj43098tfg43griejg4ejferjgi4jg9jg9gj99jijgjowefkoiefkef
#define COVERAGE_GENERATOR_H_fmweoif409fergnjergtj43098tfg43griejg4ejferjgi4jg9jg9gj99jijgjowefkoiefkef

#include <coverage_database.h>
#include <testcase_observer.h>

namespace coverage
{
	class generator : public testcase_observer
	{
	public:
		generator(database& db);
	};
}

#endif