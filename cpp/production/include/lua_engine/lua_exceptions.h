#ifndef LUA_EXCEPTIONS_H_mfiorfmef234098fj34ufrnmkdsmfpoqwke09832jdfurfgnjkdfjowedk239hf9432h
#define LUA_EXCEPTIONS_H_mfiorfmef234098fj34ufrnmkdsmfpoqwke09832jdfurfgnjkdfjowedk239hf9432h
#include <exception>

namespace lua
{
	class allocation_failure : public std::bad_alloc
	{};
}

#endif