#ifndef MPL_TO_VARIADIC_HPP_jdiewu9f378gh8yfhjcnvkdmcmsidocwe9dj28ed2198d874hf87hfhfwewoqixwiomxciwecniuifhufheriferf
#define MPL_TO_VARIADIC_HPP_jdiewu9f378gh8yfhjcnvkdmcmsidocwe9dj28ed2198d874hf87hfhfwewoqixwiomxciwecniuifhufheriferf

#include <type_traits>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/bool.hpp>

template <typename... varargs>
struct variadic_to_mpl
{
	template <typename boost_container, typename... varargs>
	struct helper
	{
		template <typename boost_container, typename... varargs>
		struct helper_
		{
			typedef typename boost_container::type type;
		};

		template <typename boost_container, typename vararg, typename... varargs>
		struct helper_<boost_container, vararg, varargs...>
		{
			typedef typename helper<typename boost::mpl::push_back<boost_container, vararg>::type, varargs...>::type type;
		};

		typedef typename helper_<boost_container, varargs...>::type type;
	};

	typedef typename helper <typename boost::mpl::clear<boost::mpl::vector<int> >::type, varargs...>::type type;
};

#endif