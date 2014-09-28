#ifndef COMPONENT_H_jioewjfwjf9084jf9j34fiowedjioewjfuf4f934jfij
#define COMPONENT_H_jioewjfwjf9084jf9j34fiowedjioewjfuf4f934jfij

#include <vector>
#include <utility>
#include <set>
#include <map>
#include <string>
#include <memory>

#include <boost/optional.hpp>
#include <boost/tuple.hpp>

namespace shop
{
    class product;
    class component;
    class interface;
    
    enum class interface_type
    {
        input,
        output
    };

    typedef std::string component_name;
    typedef std::string interface_name;

    struct quantity
    {
        quantity(std::size_t size);
        
        std::size_t exact;
    };

    struct recommendation_comment
    {
        std::string comment;
    }

    struct configuration_change
    {
        const component& existing;
        std::vector<std::pair<const component&, recommendation_comment> > recommended;
    }; 

    struct review_status
    {
        enum class
        {
            ok,
            suboptimal,
            incompatible
        } status;

        boost::optional<configuration_change> recommended_changes;
    }; 

    class component_source
    {
    public:
        const std::vector<const component&> get_components_with_interface(const interface& inter) const = 0;
    };

    template <typename parent_reader>
    class generic_reader
    {
        typedef std::string serialized;

        template <typename read>
        std::pair<typename parent_reader::name_type, serialized> fetch_next()
        {
            return dynamic_cast<parent_reader&>(*this).fetch_next(read());
        }
    }

    class component_reader : public generic_reader<component_reader>
    {
    public:
        typedef component_name name_type;

        std::pair<component_name, serialized> fetch_next(component&&) = 0;
        bool more_components_available() = 0
    };

    class interface_reader : public generic_reader<interface_reader>
    {
    public:
        typedef interface_name name_type;

        std::pair<interface_name, serialized> fetch_next(interface&&) = 0;
        bool more_interfaces_available() = 0;
        std::vector<interface_name> get_compatible_interfaces_names(const interface_name&) = 0
    };
    
    class data_holder
    {
    public:
        const component& get_component(const component_name&) const;
        const interface& get_interface(const interface_name&) const;

        void load_components(const component_reader& reader);
        void load_interfaces(const interface_reader& reader);

    private:
        std::map<component_name, component> components;
        std::map<interface_name, interface> interfaces;
    };

    class recommendation_agent
    {
        std::vector<const component&> recommend_component(const std::string& type, const product& target) const = 0;
        std::vector<configuration_change> recommend_changes(const product& rated) const = 0;
        std::map<std::string, review_status> review(const product& reviewed) const = 0;
    };

    class interface
    {
    public:
        std::vector<const interface&> get_compatible_interfaces() const;

        void add_compatible_interface(const interface&);
    private:
        interface_type type;
        std::vector<const interface&> compatible_interfaces;
    };

    class connector
    {
    public:
        std::set<const std::interface&> get_compatible_interfaces() const;

    private:
        std::vector<const interface&> outputs;
    };

    class component
    {
    public:
        std::vector<const component&> get_compatible_components(const component_source& source) const;

    private:
        std::vector<connector> connectors;
    };

    class product
    {
    public:
        std::map<std::string, std::vector<const component&> > get_recommended_components(const recommendation_agent&, const component_source&) const;

        std::map<std::string, const component&> selected;
        std::map<std::string, std::pair>quantity, component&> > mandatory_components;
        std::map<std::string, std::pair<quantity, component&> > optional_components;
    };
}

#endif
