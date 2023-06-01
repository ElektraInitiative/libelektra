/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef CHECK_HPP
#define CHECK_HPP

#include <command.hpp>

#include "./coloredkdbio.hpp"
#include <kdb.hpp>

class PluginCheckCommand : public Command
{
public:
	PluginCheckCommand ();
	~PluginCheckCommand ();

	virtual std::string getShortOptions () override
	{
		return "c";
	}

	virtual std::string getSynopsis () override
	{
		return "[<name>]";
	}

	virtual std::string getShortHelpText () override
	{
		return "Do some basic checks on a plugin.";
	}

	virtual std::string getLongHelpText () override
	{
		return "If no arguments are given, checks on the key database\n"
		       "are done instead.\n"
		       "\n"
		       "If a plugin name is given, checks will only be done with the given plugin.\n"
		       "Use -c to pass options to the plugin.\n"
		       "\n"
		       "Please report any output caused by official plugins to https://www.libelektra.org\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
