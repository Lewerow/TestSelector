#include <threading/multithreaded_logger.h>

namespace threading
{
    logger_sink::logger_sink(std::ostream& sink_) : sink(sink_)
    {}

    bool logger_sink::log(const std::string& msg)
    {
        return worker.schedule([&](){sink << msg << std::endl;});
    }

    multithreaded_logger::multithreaded_logger(logger_sink& sink_) : sink(sink_)
    {}
    
    bool multithreaded_logger::log(const std::string& msg)
    {
        return sink.log(msg);
    }
}
