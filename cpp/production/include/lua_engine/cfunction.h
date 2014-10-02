#ifndef cfunction_jfieowfjewfj984jferjfiejfjewofijewoifjewfewfejfo
#define cfunction_jfieowfjewfj984jferjfiejfjewofijewoifjewfewfejfo

#include <string>

//#include <boost/function_types/parameter_types.h>
//#include <boost/function_types/result_type.h>

struct lua_State;

namespace lua
{
    template <typename signature>
    class cfunction
    {
    public:
        cfunction(const std::string& new_name, signature new_f) : name_(new_name), f(new_f)
        {}
        
        void load(lua_State* machine) const
        {
            lua_pushcfunction(machine, functor());
            lua_setglobal(machine, name().c_str());
        }

        const std::string& name() const
        {
            return name_;
        }

        signature functor() const
        {
            return f;
        }

    private:
        std::string name_;
        signature f;
    };

    template <typename signature>
    cfunction<signature> make_cfunction(const std::string& name, signature f)
    {
        return cfunction<signature>(name, f);
    }
}

#endif
