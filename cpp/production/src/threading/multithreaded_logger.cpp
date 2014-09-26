#include <threading/MultithreadedLogger.h>

namespace threading
{
    LoggerSink::LoggerSink(ostream& sink_) : sink(sink)
    {}

    bool LoggerSink::log(const std::string& msg)
    {
        return worker.schedule([&](){sink << msg << std::endl;});
    }

    MultithreadedLogger::MultithreadedLogger(LoggerSink& sink_) : sink(sink_)
    {}
    
    bool MultithreadedLogger::log(const std::string& msg)
    {
        sink.log(msg);
    }
}
