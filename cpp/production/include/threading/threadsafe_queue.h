#ifndef THREADSAFE_QUEUE_mfrijfierjgiutrgiewjfoewkfowejiofjewiof
#define THREADSAFE_QUEUE_mfrijfierjgiutrgiewjfoewkfowejiofjewiof

#include <queue>
#include <mutex>
#include <atomic>
#include <condition_variable>

#include <boost/optional.hpp>

#include <ts_assert.h>

namespace threading
{
    template <typename stored>
    struct nonblocking_popping_policy
    { 
       typedef std::lock_guard<std::mutex> lock;

        nonblocking_popping_policy(std::queue<stored>&) 
        {}

        bool available()
        {
            return true;
        }

        void wait_until_ready(lock&)
        {}

        void notify_action_available()
        {}
        
        void notify_of_death()
        {}
    };

    template <typename stored>
    struct blocking_popping_policy
    {
        typedef blocking_popping_policy<stored> this_type;
        typedef std::unique_lock<std::mutex> lock;
        blocking_popping_policy(std::queue<stored>& q) : queue(q)
        {}

        bool available()
        {
            return !queue.empty();
        }

        void wait_until_ready(lock& l)
        {
            condition.wait(l, std::bind(&this_type::available, this));
        }
        
        void notify_action_available()
        {
            condition.notify_one();
        }

        void notify_of_death()
        {
            condition.notify_all();
        }

        std::queue<stored>& queue;
        std::condition_variable condition;
    };
      
    template <typename stored>
    struct nonblocking_nonfailing_pushing_policy
    {
        typedef std::lock_guard<std::mutex> lock;

        nonblocking_nonfailing_pushing_policy(const boost::optional<std::size_t>& max_size, std::queue<stored>&)
        {
            TS_ASSERT(max_size == boost::none, "Cannot enforce maximum size of a queue where pushing is both nonblocking and nonfailing");
        }

        bool available()
        {
            return true;
        }        

        bool must_fail()
        {
            return false;
        }
        
        void wait_until_ready(lock&)
        {}

        void notify_action_available()
        {}

        void notify_of_death()
        {}        
    }; 
     
    template <typename stored>
    struct failing_pushing_policy
    {
        typedef std::lock_guard<std::mutex> lock;

        failing_pushing_policy(const boost::optional<std::size_t>& allowed_size, std::queue<stored>& q) : queue(q)
        {
            TS_ASSERT(allowed_size != boost::none, "Max size must be given is pushing is supposed to be failing!");
            max_size = allowed_size.get();
        }

        bool available()
        {
            return queue.size() < max_size;
        }        

        bool must_fail()
        {
            return !available();
        }
        
        void wait_until_ready(lock&)
        {}

        void notify_action_available()
        {}

        void notify_of_death()
        {}        

        std::queue<stored>& queue;
        std::size_t max_size;
    }; 


    template <typename stored>
    struct blocking_pushing_policy
    {
        typedef std::unique_lock<std::mutex> lock;
        typedef blocking_pushing_policy<stored> this_type;

        blocking_pushing_policy(const boost::optional<std::size_t>& allowed_size, std::queue<stored>& q) : queue(q)
        {
            TS_ASSERT(allowed_size != boost::none, "Max size must be given is pushing is supposed to be blocking!");
            max_size = allowed_size.get();
        }

        bool available()
        {
            return queue.size() < max_size;
        }        

        bool must_fail()
        {
            return !available();
        }
        
        void wait_until_ready(lock& l)
        {
            condition.wait(l, std::bind(&this_type::available, this));
        }

        void notify_action_available()
        {
            condition.notify_one();
        }

        void notify_of_death()
        {
            condition.notify_all();
        }        

        std::queue<stored>& queue;
        std::size_t max_size;
        std::condition_variable condition;
    }; 

    template <typename stored, template <class> class pushing_policy=nonblocking_nonfailing_pushing_policy, template <class> class popping_policy=blocking_popping_policy>
    struct threadsafe_queue
    {
    public:
        threadsafe_queue(boost::optional<std::size_t> max_size = boost::none) : pushing(max_size, queue), popping(queue), is_dead(false)
        {}
        
        ~threadsafe_queue()
        {
            TS_ASSERT(is_dead, "Cannot destroy queue that was not killed earlier");
        }

        bool pop_front(stored& s)
        {
             typename popping_policy<stored>::lock l(queue_mutex);
             while(!popping.available() && !is_dead)
                 popping.wait_until_ready(l);

             if(queue.empty())
                return false;
            
             s = std::move(queue.front());
             queue.pop();
             pushing.notify_action_available();
             return true;
        }        

        bool push_back(stored&& s)
        {
            return push_back_impl(s);
        }
    
        bool push_back(const stored& s)
        {
            return push_back_impl(std::move(s));
        }
        
        void kill()  
        {
            std::lock_guard<std::mutex> lock(queue_mutex);
            is_dead = true;
            popping.notify_of_death();
            pushing.notify_of_death();
        }

        template<typename value> 
        bool push_back_impl(value v)
        {
            typename pushing_policy<stored>::lock l(queue_mutex);
            if(pushing.available() && !is_dead)
                pushing.wait_until_ready(l);

            if(is_dead || pushing.must_fail())
                return false;

            queue.push(v);
            popping.notify_action_available();
            return true; 
        }

    private:

        bool is_dead;
        std::queue<stored> queue;
        std::mutex queue_mutex;
        
        pushing_policy<stored> pushing;
        popping_policy<stored> popping;
    };
}

#endif
