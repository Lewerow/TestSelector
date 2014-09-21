#ifndef MPL_TO_VARIADIC_HPP_jdiewu9f378gh8yfhjcnvkdmcmsidocwe9dj28ed2198d874hf87hfhfwewoqixwiomxciwecniuifhufheriferf
#define MPL_TO_VARIADIC_HPP_jdiewu9f378gh8yfhjcnvkdmcmsidocwe9dj28ed2198d874hf87hfhfwewoqixwiomxciwecniuifhufheriferf

#include <type_traits>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/bool.hpp>

namespace metaprogramming
{
	template <typename boost_container, typename... varargs>
	struct helper
	{
		template <typename boost_container_internal, typename... varargs_internal>
		struct internal_helper
		{
			typedef typename boost_container_internal::type type;
		};

		template <typename boost_container_internal, typename vararg, typename... varargs_internal>
		struct internal_helper<boost_container_internal, vararg, varargs_internal...>
		{
			typedef typename helper<typename boost::mpl::push_back<boost_container_internal, vararg>::type, varargs_internal...>::type type;
		};

		typedef typename internal_helper<boost_container, varargs...>::type type;
	};
		
	template <typename... varargs>
	struct variadic_to_mpl
	{
		typedef typename helper <typename boost::mpl::clear<boost::mpl::vector<int> >::type, varargs...>::type type;
	};
}

#endif
