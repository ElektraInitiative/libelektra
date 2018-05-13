/**
 * @file
 *
 * @brief Implementation of plugin spec
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <pluginspec.hpp>

#include <toolexcept.hpp>

#include <iostream>

using namespace std;

namespace kdb
{

namespace tools
{

/**
 * @brief Construct a plugin spec with a given module name
 *
 * @param pluginName the fullname (modulename # refname)
 * @param pluginConfig the plugins config
 *
 * @throw BadPluginName if name not a-z
 *
 * @see setFullName()
 */
PluginSpec::PluginSpec (std::string pluginName, KeySet pluginConfig) : name (pluginName), refname (pluginName), config (pluginConfig)
{
	setFullName (pluginName);
}

/**
 * @brief Construct a plugin spec with a given module and ref name
 *
 * @param pluginName the module this plugin is instantiated from
 * @param refName for uniqueness for more plugins from one module
 * @param pluginConfig the plugins config
 *
 * @throw BadPluginName if name not a-z
 *
 * @see setName()
 * @see setRefName()
 */
PluginSpec::PluginSpec (std::string pluginName, std::string refName, KeySet pluginConfig)
: name (pluginName), refname (refName), config (pluginConfig)
{
	validate (pluginName);
	validate (refname);
}

/**
 * @brief Construct a plugin spec with a given module name and ref number
 *
 * @param pluginName the module this plugin is instantiated from
 * @param refNumber for uniqueness for more plugins from one module
 * @param pluginConfig the plugins config
 *
 * @throw BadPluginName if name not a-z
 *
 * @see setName()
 * @see setRefName()
 */
PluginSpec::PluginSpec (std::string pluginName, size_t refNumber, KeySet pluginConfig)
: name (pluginName), refname (), config (pluginConfig)
{
	validate (pluginName);
	setRefNumber (refNumber);
}


/**
 * @note if no reference name is given its equal to the module name
 *
 * @return the module name, then #, and then the reference name
 */
std::string PluginSpec::getFullName () const
{
	return name + "#" + refname;
}

/**
 * @return the reference name
 */
std::string PluginSpec::getRefName () const
{
	return refname;
}

/**
 * @brief Checks if reference name contains only numbers
 *
 * @retval true if only numbers
 * @retval true if a refname was given
 */
bool PluginSpec::isRefNumber () const
{
	auto it = refname.find_first_not_of ("0123456789");
	return it == std::string::npos;
}

/**
 * @return the module name
 */
std::string PluginSpec::getName () const
{
	return name;
}

/**
 * @return the config
 */
KeySet PluginSpec::getConfig () const
{
	return config;
}

/**
 * @brief Set the full name with # or only the name
 *
 * @throw BadPluginName if name not a-z (no change then)
 *
 * @param n the string to set
 */
void PluginSpec::setFullName (std::string const & n)
{
	auto it = n.find ('#');
	if (it != std::string::npos)
	{
		std::string nn = n.substr (0, it);
		std::string rn = n.substr (it + 1);
		validate (nn);
		validate (rn);
		name = nn;
		refname = rn;
	}
	else
	{
		setName (n);
	}
}

/**
 * @brief Set the reference name of the plugin
 *
 * @throw BadPluginName if name not a-z (no change then)
 *
 * Makes different plugins based from the same module
 * unique.
 *
 * @param n the string to set
 */
void PluginSpec::setRefName (std::string const & n)
{
	validate (n);
	refname = n;
}

/**
 * @brief Set a number for automatic references during parsing
 *
 * @param refnumber the number to set
 */
void PluginSpec::setRefNumber (size_t refnumber)
{
	refname = to_string (refnumber);
}

/**
 * @brief Set the module name of the plugin
 *
 * @throw BadPluginName if name not a-z (no change then)
 *
 * @param n the string to set
 */
void PluginSpec::setName (std::string const & n)
{
	validate (n);
	name = n;
}

/**
 * @brief Append to config
 *
 * @param c config to append
 */
void PluginSpec::appendConfig (KeySet c)
{
	config.append (c);
}

/**
 * @brief Set plugin config
 *
 * @param c new config to be used as plugin config
 */
void PluginSpec::setConfig (KeySet c)
{
	config.clear ();
	config.append (c);
}

/**
 * @brief Check if str starts with a-z and then only has chars a-z, 0-9 or underscore (_)
 *
 * @param n the string to check
 */
void PluginSpec::validate (std::string const & n) const
{
	if (n.empty ()) throw BadPluginName ("<empty>");
	auto begin = n.find_first_of ("abcdefghijklmnopqrstuvwxyz");
	if (begin != 0)
	{
		// must start a-z
		throw BadPluginName (n);
	}

	auto it = n.find_first_not_of ("abcdefghijklmnopqrstuvwxyz0123456789_");
	if (it != std::string::npos)
	{
		throw BadPluginName (n);
	}
}


/**
 * @brief Compare two pluginspec if their value is equal
 * @note the content of getConfig() will be only compared with keynames, not content!
 */
bool operator== (PluginSpec const & self, PluginSpec const & other)
{
	return self.getName () == other.getName () && self.getRefName () == other.getRefName () && self.getConfig () == other.getConfig ();
}

/**
 * @brief Compare two pluginspec if their value is not equal
 * @note the content of getConfig() will be only compared with keynames, not content!
 */
bool operator!= (PluginSpec const & self, PluginSpec const & other)
{
	return !(self == other);
}

/**
 * @brief Output the name, refname and size of config
 */
std::ostream & operator<< (std::ostream & os, PluginSpec const & spec)
{
	os << "name: " << spec.getName () << " refname: " << spec.getRefName () << " configsize: " << spec.getConfig ().size ();
	return os;
}
} // namespace tools
} // namespace kdb
