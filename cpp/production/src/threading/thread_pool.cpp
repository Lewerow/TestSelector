#include <threading/thread_pool.h>

namespace threading
{
    thread_pool::thread_pool(std::size_t pool_size)
    {
        for(int i = 0; i < pool_size; ++i)
            spawn_thread();
    }

    thread_pool::~thread_pool()
    {
        task_queue.kill();
        for(auto& w: workers)
            w.join();
    }
    
    void thread_pool::spawn_thread()
    {
        workers.emplace_back(std::bind(&thread_pool::work, this));
    }

    bool thread_pool::schedule(std::function<void(void)>&& task)
    {
        return task_queue.push_back(task);
    }    

    void thread_pool::work()
    {
        std::function<void(void)> callee;
        while(task_queue.pop_front(callee))
            callee();
    }
}
