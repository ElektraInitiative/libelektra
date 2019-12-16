/**
 * @file
 *
 * @brief Tests for the Backend parser class
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <backendparser.hpp>

#include <functional>

#include <string>

#include <keyset.hpp>

#include <toolexcept.hpp>


namespace kdb
{

namespace tools
{


/**
 * @brief Parse a string containing information to create a KeySet
 *
 * @param pluginArguments comma (,) to separate key=value, contains no whitespaces
 *
 * @return newly created keyset with the information found in the string
 */
KeySet parsePluginArguments (std::string const & pluginArguments, std::string const & basepath)
{
	KeySet ks;
	std::istringstream sstream (pluginArguments);

	std::string keyName;
	std::string value;

	// read until the next '=', this will be the keyname
	while (std::getline (sstream, keyName, '='))
	{
		// read until a ',' or the end of line
		// if nothing is read because the '=' is the last character
		// in the config string, consider the value empty
		if (!std::getline (sstream, value, ',')) value = "";

		ks.append (Key (basepath + "/" + keyName, KEY_VALUE, value.c_str (), KEY_END));
	}
	return ks;
}

PluginSpecVector parseArguments (std::initializer_list<std::string> cmdline)
{
	return parseArguments (cmdline.begin (), cmdline.end ());
}


/**
 * @brief Parse a complete commandline
 *
 * @param cmdline contains space separated plugins with optional plugin configurations
 *
 * @note currently whitespaces are not allowed within pluginname or config, use
 * iterator interface parseArguments() if you need it.
 *
 * @see parseArguments()
 * @return a parsed PluginSpecVector
 */
PluginSpecVector parseArguments (std::string const & cmdline)
{
	std::vector<std::string> args;
	std::istringstream sstream (cmdline);
	std::string argument;
	while (std::getline (sstream, argument, ' '))
	{
		args.push_back (argument);
	}
	return parseArguments (args.begin (), args.end ());
}


namespace detail
{

/**
 * @brief Process a single argument and add it to PluginSpecVector
 *
 * @internal
 *
 * @param [in,out] arguments current list of processed arguments
 * @param [in,out] counter current counter, to be modified when argument is added
 * @param argument the argument to parse and add
 */
void processArgument (PluginSpecVector & arguments, size_t & counter, std::string argument)
{
	// ignore empty or useless arguments (whitespace , only)
	if (argument.empty ()) return;
	if (std::all_of (argument.begin (), argument.end (), [] (char c) { return std::isspace (c) || c == ','; })) return;

	if (argument.find ('=') == std::string::npos)
	{
		// we have a plugin
		PluginSpec ps (argument);
		if (argument.find ('#') == std::string::npos)
		{
			ps.setRefNumber (counter++);
			arguments.push_back (ps);
		}
		else
		{
			arguments.push_back (ps);
		}
	}
	else
	{
		// we have a plugin's configuration
		if (arguments.empty ()) throw ParseException ("config for plugin (" + argument + ") without previous plugin name");
		arguments.back ().appendConfig (parsePluginArguments (argument));
	}
}

/**
 * @brief Fix refnames after parsing
 *
 * @internal
 *
 * @param arguments to fix
 */
void fixArguments (PluginSpecVector & arguments)
{
	// fix refnames of single occurrences for backwards compatibility and cleaner names
	for (auto & a : arguments)
	{
		size_t nr = std::count_if (arguments.begin (), arguments.end (),
					   [&a] (PluginSpec const & spec) { return spec.getName () == a.getName (); });
		if (nr == 1 && a.isRefNumber ())
		{
			a.setRefName (a.getName ());
		}

		size_t identical =
			std::count_if (arguments.begin (), arguments.end (), std::bind (PluginSpecRefName (), a, std::placeholders::_1));
		if (identical > 1)
		{
			throw ParseException ("identical reference names found for plugin: " + a.getFullName ());
		}
	}

	// now fix counter to be minimal
	size_t counter = 0;
	for (auto & a : arguments)
	{
		if (a.isRefNumber ())
		{
			a.setRefNumber (counter++);
		}
	}
}
} // namespace detail
} // namespace tools
} // namespace kdb
