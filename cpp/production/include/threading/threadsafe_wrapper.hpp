#ifndef THREADSAFE_WRAPPER_frejg23rke2kmewnfrjegj3io4dwkmsnvfewiokdsmdjkwendowedwef
#define THREADSAFE_WRAPPER_frejg23rke2kmewnfrjegj3io4dwkmsnvfewiokdsmdjkwendowedwef

#include <memory>

namespace threading
{
    template <typename ensafe>
    struct threadsafe_wrapper
    {
        struct tricky_ptr
        {
            tricky_ptr(std::unique_ptr<ensafe>& p, std::mutex& mutex) : ptr(p), lock(mutex)
            {}

            ensafe& operator*()
            {
                return *ptr;
            }

            ensafe* operator->()
            {
                return ptr.operator->();
            }
            
            std::unique_ptr<ensafe>& ptr;
            std::lock_guard<std::mutex> mut;
        };

        tricky_ptr operator*()
        {
            return get();
        }

        tricky_ptr get()
        {
            return tricky_ptr(wrapped, mut);
        }
    
    private:  
        std::unique_ptr<ensafe> wrapped;
        std::mutex mut;
    }
}

#endif
