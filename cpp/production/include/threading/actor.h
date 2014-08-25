#ifndef ACTOR_H_Moifcjewiofj9834fuerijkdfmoiwdj98wefewuhfweijfioewjfdiwejfiurehguiherigfv9dcjew98cjwe98fjerfjr4f94jfij4f
#define ACTOR_H_Moifcjewiofj9834fuerijkdfmoiwdj98wefewuhfweijfioewjfdiwejfiurehguiherigfv9dcjew98cjwe98fjerfjr4f94jfij4f

#include <memory>

#include <boost/lockfree/queue.hpp>
#include <atomic>
#include <condition_variable>

#include <threading/queue.h>

class message_handler;
class message_base;

class conditional_lock
{
public:
	template<class predicate>
	void wait(predicate pred)
	{
		std::unique_lock<std::mutex> lock(mutex);
		cv.wait(lock, pred);
	}

	void notify_one();
	void notify_all();
private:
	std::mutex mutex;
	std::condition_variable cv;
};

class message_handler
{
public:
	virtual ~message_handler();

	void handle(std::unique_ptr<message_base> message);
	virtual void process(message_base& message);
};

class message_base
{
public:
	virtual ~message_base();
	
	virtual bool is_termination_message();
	virtual void visit(message_handler& handler);
	
	message_handler* destination;
};

//template <typename message_base/*, template<class> class queue_type*/>
class actor : public message_handler
{
public:

	enum sending_state
	{
		sending_ok,
		actor_dying
	};

	//typedef boost::lockfree::queue<std::unique_ptr<message_base> > queue_type;
	typedef threadsafe_queue<message_base> queue_type;

	actor() : is_paliative(false)
	{}
	
	sending_state send(std::unique_ptr<message_base> message)
	{
		if (is_paliative.load())
			return actor_dying;

		message->destination = this;
		queue.push(std::move(message));
		lock.notify_one();
		return sending_ok;
	}

	void kill()
	{
		send(std::make_unique<termination_message>());
	}

	void run()
	{
		while (!is_paliative.load())
		{
			lock.wait(std::bind(&queue_type::empty, &queue));
			handle_one();			
		}
	}

	void handle_one()
	{
		if (is_paliative.load())
			return;

		std::unique_ptr<message_base> msg(queue.pop());
		if (msg)
		{

			if (msg->is_termination_message())
				terminate();

			message_handler* destination = msg->destination;
			destination->handle(std::move(msg));
		}
	}

private:

	void terminate()
	{
		is_paliative.store(true);
		while (queue.pop() != nullptr)
			;
	}

	struct termination_message : message_base
	{};
	
	queue_type queue;
	conditional_lock lock;

	std::atomic<bool> is_paliative;
};


#endif