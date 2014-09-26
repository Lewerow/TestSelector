#include <threading/active_object.h>

namespace threading
{
    active_object::active_object() : worker(1)
    {}

    bool active_object::schedule(std::function<void(void)>&& f)
    {
        return worker.schedule(std::forward(f));
    }
}
