#ifndef MODULES_HPP
#define MODULES_HPP

#include <plugin.hpp>
#include <keyset.hpp>

#include <memory>

class Modules
{
public:
	Modules();
	~Modules();

	/**
	 * @return a new created plugin
	 */
	std::auto_ptr<Plugin> load(std::string const& pluginName);
	std::auto_ptr<Plugin> load(std::string const& pluginName, kdb::KeySet const& config);

private:
	kdb::KeySet modules;
};

#endif
