#ifndef TESTCASE_CHOOSER_H_mrioefk34980gjurtgjnmfkdlwqe9dfjwusdjkfdwqekd0983u2897hfuiejdkqw09sdkwq0d
#define TESTCASE_CHOOSER_H_mrioefk34980gjurtgjnmfkdlwqe9dfjwusdjkfdwqekd0983u2897hfuiejdkqw09sdkwq0d

#include <vector>
#include <string>
#include <regex>

namespace coverage_generator
{
	class testcase_chooser
	{
	public:
		testcase_chooser(std::vector<std::string>&& names);

		void filter(const std::regex& regex);

    	const std::vector<std::string>& names() const;
		
	private:
		std::vector<std::string> chosen_testcases_names;
	};
}

#endif