#include <boost/test/auto_unit_test.hpp>
#include <turtle/mock.hpp>

#include <thread>

#include <threading/threadsafe_queue.h>
#include <threading/thread_pool.h>

struct queue_fixture
{
    threading::threadsafe_queue<int> q;
};

BOOST_AUTO_TEST_SUITE(helpers)
BOOST_AUTO_TEST_SUITE(threading_tests)
BOOST_FIXTURE_TEST_SUITE(threadsafe_queue_sanity, queue_fixture)
BOOST_AUTO_TEST_CASE(one_can_push_back_and_pop_front_queue_get_same)
{
    int* d = new int[10];
    delete d;
    BOOST_CHECK(q.push_back(2));
    int a = 4;
    BOOST_CHECK(q.pop_front(a));
    BOOST_CHECK_EQUAL(a, 2);
    q.kill();
}

BOOST_AUTO_TEST_CASE(dead_queue_contains_only_what_was_put_there_earlier)
{
    BOOST_CHECK(q.push_back(2));
    q.kill();
    BOOST_CHECK(!q.push_back(6));
    
    int a = 5;
    BOOST_CHECK(q.pop_front(a));
    BOOST_CHECK_EQUAL(a, 2);
    BOOST_CHECK(!q.pop_front(a));   
}

BOOST_AUTO_TEST_CASE(does_not_get_crazy_on_two_threads)
{
    auto queue_examinator = [this](long& result){
        for(int i = 0; i < 5000; ++i)
        {
            if(i % 2 == 0)
                this->q.push_back(i);
            else
            {
                int a = 0xDEADBEEF;
                this->q.pop_front(a);
                result += a;
            }
        }
    };

    long th1_result = 0;
    long th2_result = 0;
    std::thread th1(queue_examinator, std::ref(th1_result));
    std::thread th2(queue_examinator, std::ref(th2_result));

    th1.join();
    th2.join();
    q.kill();

    BOOST_CHECK_EQUAL(th1_result + th2_result, (125*100 - 5)*1000);
}

BOOST_AUTO_TEST_CASE(producer_and_consumer_may_use_queue_together)
{
    auto producer = [this]() {
        for(int i = 0; i < 10000; ++i)
            this->q.push_back(i);
    };

    auto consumer = [this](long& sum){
        int a = 0xDEADBEEF;
        while(this->q.pop_front(a))
            sum+=a;
    };

    long result = 0;
    std::thread th1(producer);
    std::thread th2(consumer, std::ref(result));
    
    th1.join();
    q.kill();
    th2.join();

    BOOST_CHECK_EQUAL(result, (50*1000 - 5)*1000);
}

BOOST_AUTO_TEST_SUITE_END()
BOOST_AUTO_TEST_SUITE(thread_pool_sanity)
BOOST_AUTO_TEST_CASE(thread_pool_executes_all_given_tasks)
{
    auto worker = [](long& result, int min, int max){
        for(int i = min; i < max; ++i)
            result += i;
    };

    std::vector<long> results(100, 0);
    {
        threading::thread_pool pool(4);
        for(auto i = results.begin(); i != results.end(); ++i)
            pool.schedule(std::bind(worker, std::ref(*i), 0, 101));
    }

    for(auto i = results.begin(); i != results.end(); ++i)
        BOOST_CHECK_EQUAL(*i, 5050);
}


BOOST_AUTO_TEST_SUITE_END()
BOOST_AUTO_TEST_SUITE_END()
BOOST_AUTO_TEST_SUITE_END()
