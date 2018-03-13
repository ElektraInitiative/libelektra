/**
 * @file
 *
 * @brief Interface to specify which plugin is meant
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef TOOLS_PLUGIN_SPEC_HPP
#define TOOLS_PLUGIN_SPEC_HPP

#include <string>
#include <vector>

#include <kdb.hpp>

namespace kdb
{

namespace tools
{

/**
 * @brief Specifies a plugin by its name and configuration
 *
 * @invariant name is valid (nonempty, starts with a-z, then a-z_0-9)
 * @invariant refname is valid (same as above or a size_t number)
 */
class PluginSpec
{
public:
	PluginSpec ()
	{
	}

	explicit PluginSpec (std::string pluginName, KeySet pluginConfig = KeySet ());

	explicit PluginSpec (std::string pluginName, std::string refName, KeySet pluginConfig = KeySet ());

	explicit PluginSpec (std::string pluginName, size_t refNumber, KeySet pluginConfig = KeySet ());

	std::string getFullName () const;
	std::string getRefName () const;
	bool isRefNumber () const;
	std::string getName () const;

	KeySet getConfig () const;


	void setFullName (std::string const & name);
	void setRefName (std::string const & name);
	void setRefNumber (size_t number);
	void setName (std::string const & name);

	void appendConfig (KeySet config);
	void setConfig (KeySet config);

	void validate (std::string const & str) const;

private:
	std::string name;
	std::string refname;
	KeySet config;
};

struct PluginSpecName
{
	bool operator() (PluginSpec const & s1, PluginSpec const & s2) const
	{
		return s1.getName () == s2.getName ();
	}
};

struct PluginSpecRefName
{
	bool operator() (PluginSpec const & s1, PluginSpec const & s2) const
	{
		return s1.getRefName () == s2.getRefName ();
	}
};

struct PluginSpecFullName
{
	bool operator() (PluginSpec const & s1, PluginSpec const & s2) const
	{
		return s1.getFullName () == s2.getFullName ();
	}
};


/**
 * @brief Only to be used with PluginSpecName!
 */
struct PluginSpecHash
{
	size_t operator() (kdb::tools::PluginSpec const & s) const
	{
		return std::hash<std::string> () (s.getName ());
	}
};

#ifdef ELEKTRA_PLUGINSPEC_WITH_COMPARE
bool operator== (PluginSpec const & self, PluginSpec const & other);
bool operator!= (PluginSpec const & self, PluginSpec const & other);
#endif

typedef std::vector<PluginSpec> PluginSpecVector;

std::ostream & operator<< (std::ostream & os, PluginSpec const & spec);
} // namespace tools
} // namespace kdb

#endif
