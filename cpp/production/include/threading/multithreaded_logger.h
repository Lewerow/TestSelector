#ifndef MULTITHREADED_LOGGER_wdmqoiwdj9238f34ff4f34f3nfoewifoeiwjfoiewjfoiewfsadsadsa
#define MULTITHREADED_LOGGER_wdmqoiwdj9238f34ff4f34f3nfoewifoeiwjfoiewjfoiewfsadsadsa

#include <threading/active_object.h>
#include <string>
#include <ostream>

namespace threading
{
    class logger
    {
    public:
        virtual ~logger(){}
        virtual bool log(const std::string&) = 0;
    };

    class logger_sink : public logger
    {
    public:
        logger_sink(std::ostream& sink_);

        bool log(const std::string&);

    private:
        active_object worker;
        std::ostream& sink;
    };

    class multithreaded_logger : public logger
    {
    public:
        multithreaded_logger(logger_sink& sink_);
		multithreaded_logger(multithreaded_logger&) = default;
		multithreaded_logger& operator=(multithreaded_logger&) = delete;


        bool log(const std::string&);
    private:
        logger_sink& sink;
    };

}

#endif
