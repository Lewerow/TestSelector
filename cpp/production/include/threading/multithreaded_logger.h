#ifndef MULTITHREADED_LOGGER_wdmqoiwdj9238f34ff4f34f3nfoewifoeiwjfoiewjfoiewfsadsadsa
#define MULTITHREADED_LOGGER_wdmqoiwdj9238f34ff4f34f3nfoewifoeiwjfoiewjfoiewfsadsadsa

#include <threading/active_object.h>
#include <string>
#include <ostream>

namespace threading
{
    class Logger
    {
        virtual ~Logger(){}
        virtual bool log(const std::string&) = 0;
    };

    class LoggerSink : public Logger
    {
    public:
        LoggerSink(ostream& sink_);

        bool log(const std::string&);

    private:
        active_object worker;
        ostream& sink;
    }

    class MultithreadedLogger : public Logger
    {
    public:
        MultithreadedLogger(LoggerSink& sink_);

        bool log(const std::string&);
    private:
        LoggerSink& sink;
    };

}

#endif
