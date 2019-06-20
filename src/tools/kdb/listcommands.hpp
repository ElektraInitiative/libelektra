/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef LIST_COMMANDS_H
#define LIST_COMMANDS_H

#include <command.hpp>
#include <kdb.hpp>

class ListCommandsCommand : public Command
{
public:
	ListCommandsCommand ();
	~ListCommandsCommand ();

	virtual std::string getShortOptions () override
	{
		return "0";
	}

	virtual std::string getSynopsis () override
	{
		return "";
	}

	virtual std::string getShortHelpText () override
	{
		return "List available kdb commands.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
