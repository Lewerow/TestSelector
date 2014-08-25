#ifndef QUEUE_H_mcvioerwfmgj09reokdflmlkiejwofj4fj493fjiejfosdjfoisakdkwqdkjerfghiughrjiorejgoijrgoijgerg
#define QUEUE_H_mcvioerwfmgj09reokdflmlkiejwofj4fj493fjiejfosdjfoisakdkwqdkjerfghiughrjiorejgoijrgoijgerg

#include <queue>
#include <memory>
#include <mutex>

template <class T>
class threadsafe_queue
{
public:
	void push(std::unique_ptr<T> ptr)
	{
		std::lock_guard<std::mutex> lock(mutex);
		queue.push(std::move(ptr));
	}

	std::unique_ptr<T> pop()
	{
		std::lock_guard<std::mutex> lock(mutex);
		if (queue.empty())
			return std::unique_ptr<T>(nullptr);

		std::unique_ptr<T> ptr(std::move(queue.front()));
		queue.pop();

		return ptr;
	}

	bool empty()
	{
		return queue.empty();
	}

private:
	std::queue<std::unique_ptr<T> > queue;
	std::mutex mutex;
};

#endif