#include <algorithm>

#include <testcase_chooser.h>

namespace coverage_generator
{
	testcase_chooser::testcase_chooser(const std::vector<std::string>& names) : chosen_testcases_names(names)
	{}

	void testcase_chooser::filter(const std::regex& regex)
	{
		chosen_testcases_names.erase(std::remove_if(chosen_testcases_names.begin(), chosen_testcases_names.end(), 
			[&regex](const std::string& name) -> bool { return !std::regex_match(name, regex); }), chosen_testcases_names.end());
	}

	const std::vector<std::string>& testcase_chooser::names() const
	{
		return chosen_testcases_names;
	}
}