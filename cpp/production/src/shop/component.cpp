#include <shop/component.h>

namespace shop
{
    quantity::quantity(std::size_t size) : exact(size)
    {}


	configuration_change::configuration_change(const component* current, const std::vector<std::pair<const component*, recommendation_comment> >&& recommendations) : existing(current), recommended(recommendations)
	{}

    const std::vector<const interface*> interface::get_compatible_interfaces() const
    {
        return compatible_interfaces;
    }

    const std::set<const interface*> connector::get_compatible_interfaces() const
    {
        std::set<const interface*> interfaces;
        for(const auto& i: outputs)
            for(const auto& c : i->get_compatible_interfaces())
                interfaces.insert(c);

        return interfaces;
    }

    void interface::add_compatible_interface(const interface* i)
    {
        compatible_interfaces.push_back(i);
    }    

    const std::set<const component*> component::get_compatible_components(const component_source& source) const
    {
        std::set<const interface*> compatible_interfaces;
        for(const auto& c: connectors)
            for(const auto* u: c.get_compatible_interfaces())
                compatible_interfaces.insert(u);

        std::set<const component*> compatible_components;
        for(auto const * const i: compatible_interfaces)
            for(auto const * const j: source.get_components_with_interface(*i))
                compatible_components.insert(j);

        return compatible_components;
    }

    const std::map<std::string, std::vector<const component*> > product::get_recommended_components(const recommendation_agent& agent, const component_source&) const
    {
        std::map<std::string, std::vector<const component*> > recommended_components;
        for(const auto& c: mandatory_components)
            if(selected.count(c.first) == 0)
                recommended_components.insert(std::make_pair(c.first, agent.recommend_component(c.first, *this)));

        for(const auto& c: optional_components)
            if(selected.count(c.first) == 0)
                recommended_components.insert(std::make_pair(c.first, agent.recommend_component(c.first, *this)));

        return recommended_components;
    }
 
    const interface* data_holder::get_interface(const interface_name& name) const 
    {
        return &interfaces.at(name);
    }

    const component* data_holder::get_component(const component_name& name) const 
    {
        return &components.at(name);
    }

    void data_holder::load_components(const component_reader&)
    {
    }

    void data_holder::load_interfaces(const interface_reader&)
    {
    }
}
