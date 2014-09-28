#include <shop/component.h>

namespace shop
{
    quantity::quantity(std::size_t size) : exact(size)
    {}

    std::vector<const interface&> interface::get_compatible_interfaces() const
    {
        return compatible_interfaces;
    }

    std::set<const std::interface&> connector::get_compatible_interfaces() const
    {
        std::set<const std::interface&> interfaces;
        for(const auto& i: outputs)
            for(const auto& c : i.get_compatible_inputs())
                interfaces.insert(c);

        return interfaces;
    }

    void interface::add_compatible_interface(const interface& i)
    {
        compatible_interafces.push_back(i);
    }    

    std::set<const component&> get_compatible_components(const component_source& source) const
    {
        std::set<const std::interface&> compatible_interfaces;
        for(const auto& c: connectors)
            for(const auto& u: c.get_compatible_interfaces())
                compatible_interfaces.insert(u);

        std::set<const component&> compatible_components;
        for(const auto& i: compatible_interfaces)
            for(const auto& j: source.get_components_with_interface(i))
                compatible_components.insert(j);

        return compatible_components;
    }

    std::map<std::string, std::vector<const component&> > get_recommended_components(const recommendation_agent& agent, const component_source& source)
    {
        std::map<std::string, std::vector<const component&> > recommended_components;
        for(const auto& c: mandatory_components)
            if(selected.count(c.first) == 0)
                recommended_components.insert(c.first, agent.recommend_component(c.first, *this));

        for(const auto& c: optional_components)
            if(selected.count(c.first) == 0)
                recommended_components.insert(c.first, agent.recommend_component(c.first, *this));

        return recommended_components;
    }
 
    const interface& data_holder::get_interface(const interface_name& name) const 
    {
        return interfaces.at(name);
    }

   const component& data_holder::get_component(const component_name& name) const 
    {
        return components.at(name);
    }

    void data_holder::load_components(const component_reader& reader)
    {
    }

    void data_holder::load_interfaces(const interface_reader& reader)
    {
    }
}
