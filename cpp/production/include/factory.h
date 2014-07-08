#ifndef FACTORY_MDiqjd9823r74hdcjnkxsamnxkojwqoidjwiuhdfewfh4ehf87e4hfuiewjhfkdsjlksajdlksajdlsad
#define FACTORY_MDiqjd9823r74hdcjnkxsamnxkojwqoidjwiuhdfewfh4ehf87e4hfuiewjhfkdsjlksajdlksajdlsad

#include <unordered_map>
#include <memory>

#include <boost/thread/shared_mutex.hpp>
#include <boost/thread/shared_lock_guard.hpp>
#include <boost/thread/locks.hpp>

#include <ts_assert.h>

template <typename product>
struct creator
{
	product* create_new()
	{
		return new product;
	}

	typedef std::default_delete<product> deleter;
};

namespace policies
{
	template <typename T>
	struct unique_ownership
	{
		typedef std::unique_ptr<T, typename creator<T>::deleter> type;

		static type wrap(creator<T>& maker)
		{
			return type(maker.create_new());
		}
	};

	template <typename T>
	struct shared_ownership
	{
		typedef std::shared_ptr<T> type;

		static type wrap(creator<T>& maker)
		{
			return type(maker.create_new(), creator<T>::deleter());
		}
	};
}

/* Threading policy is defined by pointer_policy */
template <typename product, template<class> class ownership_policy = policies::unique_ownership>
class factory
{
public:
	bool atomic_register_creator(const std::string& name, creator<product>&& maker)
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

	typename ownership_policy<product>::type produce(const std::string& name)
	{
		boost::shared_lock<boost::shared_mutex> lock(mutex);
		TS_ASSERT(creators.count(name) == 1, "Cannot produce object of type not registered to factory");
		
		return ownership_policy<product>::wrap(creators.at(name));
	}

private:
	std::unordered_map<std::string, creator<product> > creators;
	boost::shared_mutex mutex;
};


#endif