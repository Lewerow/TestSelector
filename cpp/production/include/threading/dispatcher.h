#ifndef DISPATCHER_HDqwpodkweoifjeifjerifjeowiowidjdncjenoiewjfoiewjfoewfewfewoijfoewijfwe
#define DISPATCHER_HDqwpodkweoifjeifjerifjeowiowidjdncjenoiewjfoiewjfoewfewfewoijfoewijfwe

#include <cassert>
#include <type_traits>

namespace threading
{
    namespace detail
    {
        template <typename message>
        struct single_message_dispatcher
        {
            virtual ~dispatcher(){}
            virtual bool handle(message&) = 0;
        };

        template <typename message_base>
        struct dispatcher_base
        {
            void kill()
            {
                
            }
        };
    }

    template <typename message_base, typename message, typename... messages>
    struct dispatcher : detail::single_message_dispatcher<message>, dispatcher<message_base, messages...>
    {};

    template <typename message_base, typename message>
    struct dispatcher<message> : detail::single_message_dispatcher<message>
    {
        static_assert(std::is_base_of<message_base, message>::value, "A base class for all handled messages must be provided");
    };
} 
#endif
