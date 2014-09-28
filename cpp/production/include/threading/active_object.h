#ifndef ACTIVE_OBJECT_oeiwjfioewjf8j34fuiejgrerkmflkewdoiewkjf4
#define ACTIVE_OBJECT_oeiwjfioewjf8j34fuiejgrerkmflkewdoiewkjf4

#include <threading/thread_pool.h>

namespace threading
{
    class active_object
    {
        public:
            active_object();
            
            bool schedule(std::function<void(void)>&&);
        private:
            threading::thread_pool worker;        
    };
}

#endif
