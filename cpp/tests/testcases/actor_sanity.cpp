#include <threading/actor.h>

#include <boost/test/auto_unit_test.hpp>
#include <turtle/mock.hpp>

#include <thread>

BOOST_AUTO_TEST_SUITE(actor_sanity)
BOOST_AUTO_TEST_CASE(actor_can_handle_messages_until_dies)
{
	actor a;
	BOOST_CHECK_EQUAL(a.send(std::make_unique<message_base>()), actor::sending_ok);

	a.kill();
	BOOST_CHECK_EQUAL(a.send(std::make_unique<message_base>()), actor::actor_dying);
}

BOOST_AUTO_TEST_CASE(termination_message_kills_actor)
{
	actor a;
	BOOST_CHECK_EQUAL(a.send(std::make_unique<message_base>()), actor::sending_ok);

	struct killer_msg : message_base
	{
		bool is_termination_message() { return true; }
	};

	BOOST_CHECK_EQUAL(a.send(std::unique_ptr<message_base>(new killer_msg)), actor::sending_ok);

	a.handle_one();
	BOOST_CHECK_EQUAL(a.send(std::make_unique<message_base>()), actor::sending_ok);
	
	a.handle_one();
	BOOST_CHECK_EQUAL(a.send(std::make_unique<message_base>()), actor::actor_dying);
}

BOOST_AUTO_TEST_CASE(actor_handles_messages_in_sequence)
{
	struct fake_message : message_base
	{
		fake_message(int m) : m_(m){}

		int m_;
	};

	struct fake_actor : actor
	{
		void process(fake_message& msg)
		{
			f(msg.m_);
		}

		MOCK_METHOD_EXT(f, 1, void(int), f);
	} a;

	BOOST_CHECK_EQUAL(a.send(std::make_unique<message_base>()), actor::sending_ok);

	MOCK_EXPECT(a.f).once().with(1);
	MOCK_EXPECT(a.f).once().with(2);
	MOCK_EXPECT(a.f).once().with(3);
	MOCK_EXPECT(a.f).once().with(14);
	MOCK_EXPECT(a.f).once().with(1111);
	std::thread t(&fake_actor::run, &a);

	BOOST_CHECK_EQUAL(a.send(std::make_unique<fake_message>(1)), actor::sending_ok);
	BOOST_CHECK_EQUAL(a.send(std::make_unique<fake_message>(2)), actor::sending_ok);
	BOOST_CHECK_EQUAL(a.send(std::make_unique<fake_message>(3)), actor::sending_ok);
	BOOST_CHECK_EQUAL(a.send(std::make_unique<fake_message>(14)), actor::sending_ok);
	BOOST_CHECK_EQUAL(a.send(std::make_unique<fake_message>(1111)), actor::sending_ok);
	
	a.kill();
	t.join();
}



BOOST_AUTO_TEST_SUITE_END()