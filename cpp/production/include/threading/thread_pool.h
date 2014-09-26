#ifndef THREAD_POOL_H_mfoiefmoiewjfujf934jf9kfoerfmierjfoijrefiojr
#define THREAD_POOL_H_mfoiefmoiewjfujf934jf9kfoerfmierjfoijrefiojr

#include <vector>
#include <thread>
#include <atomic>

#include <threading/threadsafe_queue.h>

namespace threading
{
    class thread_pool
    {
        public:
            thread_pool(std::size_t pool_size); 
            ~thread_pool();

            bool schedule(std::function<void(void)>&& task);

        private:
        
        void work(); 
        void spawn_thread();
        
        threading::threadsafe_queue<std::function<void(void)> > task_queue; 
        std::vector<std::thread> workers;
    };
}

#endif
