#ifndef FACTORY_MDiqjd9823r74hdcjnkxsamnxkojwqoidjwiuhdfewfh4ehf87e4hfuiewjhfkdsjlksajdlksajdlsad
#define FACTORY_MDiqjd9823r74hdcjnkxsamnxkojwqoidjwiuhdfewfh4ehf87e4hfuiewjhfkdsjlksajdlksajdlsad

#include <unordered_map>
#include <memory>

#include <boost/type_traits.hpp>

#include <boost/thread/shared_mutex.hpp>
#include <boost/thread/shared_lock_guard.hpp>
#include <boost/thread/locks.hpp>

#include <ts_assert.h>

template <typename product, typename... Args>
struct creator
{
	product* create_new(Args... args)
	{
		return new product(args...);
	}
	
	product* operator()(Args... args)
	{
		return create_new(args...);
	}

	typedef std::default_delete<product> deleter;
};

namespace policies
{
	template <typename T, typename... Args>
	struct unique_ownership
	{
		typedef std::unique_ptr<T> type;

		static type wrap(std::function<T*(Args...)>& maker, Args... args)
		{
			return type(maker(args...));
		}
	};

	template <typename T, typename... Args>
	struct shared_ownership
	{
		typedef std::shared_ptr<T> type;

		static type wrap(std::function<T*(Args...)>& maker, Args... args)
		{
			return type(maker(args...), creator<T>::deleter());
		}
	};
}

/* Threading policy is defined by ownership policy */
template <typename product, template<class, class...> class ownership_policy = policies::unique_ownership, typename... Args>
class factory
{
public:

	bool atomic_register_creator(const std::string& name, std::function<product*(Args...)>&& maker)
	{
		boost::upgrade_lock<boost::shared_mutex> lock(mutex);
		if (creators.count(name) > 0)
			return false;

		boost::upgrade_to_unique_lock<boost::shared_mutex> unique_lock(lock);
		creators[name] = std::move(maker);
		return true;
	}

	bool atomic_unregister_creator(const std::string& name)
	{
		boost::upgrade_lock<boost::shared_mutex> lock(mutex);
		auto it = creators.find(name);
		if (it == creators.end())
			return false;

		boost::upgrade_to_unique_lock<boost::shared_mutex> unique_lock(lock);
		creators.erase(it);
		return true;
	}

	typename ownership_policy<product, Args...>::type produce(const std::string& name, Args... args)
	{
		boost::shared_lock<boost::shared_mutex> lock(mutex);
		TS_ASSERT(creators.count(name) == 1, "Cannot produce object of type not registered to factory");
		
		return ownership_policy<product, Args...>::wrap(creators.at(name), args...);
	}

private:
	std::unordered_map<std::string, std::function<product*(Args...)> > creators;
	boost::shared_mutex mutex;
};


#endif