/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef LIST_H
#define LIST_H

#include <command.hpp>
#include <kdb.hpp>

class ListCommand : public Command
{
public:
	ListCommand();
	~ListCommand();

	virtual std::string getShortOptions()
	{
		return "0v";
	}

	virtual std::string getSynopsis()
	{
		return "";
	}

	virtual std::string getShortHelpText()
	{
		return "List available plugins.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"Currently it only lists plugins that were compiled\n"
			"together with the source\n"
			;
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
