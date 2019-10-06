/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef LIST_H
#define LIST_H

#include <command.hpp>
#include <kdb.hpp>

class PluginListCommand : public Command
{
public:
	PluginListCommand ();
	~PluginListCommand ();

	virtual std::string getShortOptions () override
	{
		return "0";
	}

	virtual std::string getSynopsis () override
	{
		return "[provider]";
	}

	virtual std::string getShortHelpText () override
	{
		return "List all available or provided plugins.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
