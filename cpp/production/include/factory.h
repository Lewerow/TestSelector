#ifndef FACTORY_MDiqjd9823r74hdcjnkxsamnxkojwqoidjwiuhdfewfh4ehf87e4hfuiewjhfkdsjlksajdlksajdlsad
#define FACTORY_MDiqjd9823r74hdcjnkxsamnxkojwqoidjwiuhdfewfh4ehf87e4hfuiewjhfkdsjlksajdlksajdlsad

#include <unordered_map>
#include <memory>

#include <boost/type_traits.hpp>
#include <boost/function_types/parameter_types.hpp>

#include <boost/thread/shared_mutex.hpp>
#include <boost/thread/shared_lock_guard.hpp>
#include <boost/thread/locks.hpp>

#include <ts_assert.h>

template <typename product>
struct creator
{
	template <typename... varargs>
	product* create_new(varargs... args)
	{
		return new product(std::forward<varargs>(args)...);
	}

	template <typename... varargs>
	product* operator()(varargs... args)
	{
		return create_new(std::forward<varargs>(args)...);
	}

	typedef std::default_delete<product> deleter;
};

namespace policies
{
	template <typename T>
	struct unique_ownership
	{
		static_assert(std::is_pointer<T>::value, "Only pointers can be used in unique ownership policy");

		typedef T input;
		typedef std::unique_ptr<typename std::remove_pointer<T>::type> type;

		template <typename... varargs>
		static type wrap(std::function<input(varargs...)>& maker, varargs... args)
		{
			return type(maker(std::forward<varargs>(args)...));
		}
	};

	template <typename T>
	struct shared_ownership
	{
		static_assert(std::is_pointer<T>::value, "Only pointers can be used in unique ownership policy");

		typedef T input;
		typedef typename std::remove_pointer<input>::type stripped_input;
		typedef std::shared_ptr<stripped_input> type;

		template <typename... varargs>
		static type wrap(std::function<input(varargs...)>& maker, varargs... args)
		{
			return type(maker(std::forward<varargs>(args)...), creator<stripped_input>::deleter());
		}
	};
}

/* Threading policy is defined by ownership policy */
template <typename creator_function, template<class> class ownership_policy = policies::unique_ownership>
class factory
{
	typedef factory<creator_function, ownership_policy> this_type;
public:

	typedef typename boost::function_traits<creator_function>::result_type product;
	typedef typename boost::function_types::parameter_types<creator_function>::type argtypes;

	static_assert(std::is_same<product, typename ownership_policy<product>::input >::value, "Only types matching ownership policy may be produced by a factory");

	bool atomic_register_creator(const std::string& name, std::function<creator_function>&& maker)
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

	template <typename... varargs>
	typename ownership_policy<product>::type produce(const std::string& name, varargs... args)
	{
		boost::shared_lock<boost::shared_mutex> lock(mutex);
		TS_ASSERT(creators.count(name) == 1, "Cannot produce object of type not registered to factory");
		
		return ownership_policy<product>::wrap(creators.at(name), std::forward<varargs>(args)...);
	}

private:
	std::unordered_map<std::string, std::function<creator_function> > creators;
	boost::shared_mutex mutex;
};

#endif