/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef INFO_HPP
#define INFO_HPP

#include "./coloredkdbio.hpp"
#include <command.hpp>

#include <kdb.hpp>

class PluginInfoCommand : public Command
{
public:
	PluginInfoCommand ();
	~PluginInfoCommand ();

	virtual std::string getShortOptions () override
	{
		return "lc";
	}

	virtual std::string getSynopsis () override
	{
		return "<plugin name> [<clause name>]";
	}

	virtual std::string getShortHelpText () override
	{
		return "Print information about a plugin.";
	}

	virtual std::string getLongHelpText () override
	{
		return "Print out the information of a specific plugin's contract.\n"
		       "\n"
		       "E.g.\n"
		       "Print out all information about dump plugin:\n"
		       " kdb plugin-info dump\n"
		       "\n"
		       "Print out the licence of the resolver plugin:\n"
		       " kdb plugin-info resolver licence\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
