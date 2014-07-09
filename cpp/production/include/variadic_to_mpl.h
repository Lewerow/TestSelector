#ifndef MPL_TO_VARIADIC_HPP_jdiewu9f378gh8yfhjcnvkdmcmsidocwe9dj28ed2198d874hf87hfhfwewoqixwiomxciwecniuifhufheriferf
#define MPL_TO_VARIADIC_HPP_jdiewu9f378gh8yfhjcnvkdmcmsidocwe9dj28ed2198d874hf87hfhfwewoqixwiomxciwecniuifhufheriferf

#include <boost/function_types/result_type.hpp>
#include <boost/mpl/vector.hpp>

struct empty_linked_list
{};

template <typename head, typename tail>
struct link
{
	typedef struct type
	{
	    head head_;
	    tail tail_; 
	} type;

	link(head h, tail t) 
	{
		linked_list.head_ = h;
		linked_list.tail_ = t;
	}

	type linked_list;
};

template <typename boost_sequence>
struct parameter_list
{
	typedef typename boost::mpl::fold<boost_sequence, empty_linked_list, link<boost::mpl::_1, boost::mpl::_2> >::type type;
};


template <typename accumulator>
accumulator link_all(accumulator acc)
{
	return acc;
}

template <typename return_type, typename accumulator, typename vararg, typename... varargs>
return_type link_all(accumulator acc, vararg arg, varargs... args)
{
	return link_all(link<vararg, accumulator>::type(arg, acc), args);
}

template<typename boost_sequence, typename... varargs>
typename parameter_list<boost_sequence>::type instantiate_parameter_list(varargs... args)
{
	static_assert(boost::mpl::size<boost_sequence>::value == sizeof...(args), "Vararg must have same length as created sequence");
	
	return link_all(empty_linked_list(), args);
}

template<typename callee, typename... varargs>
typename boost::function_types::result_type<callee>::type call_after_unpack(callee call, empty_linked_list seq, varargs... args)
{
	return call(args...);
}

template <typename callee, typename link, typename... varargs>
typename boost::function_types::result_type<callee>::type call_after_unpack(callee call, link seq, varargs... args)
{
	return call_after_unpack(call, seq.tail_, seq.head_, args...);
}

#endif