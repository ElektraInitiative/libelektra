/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef EXTERNAL_HPP
#define EXTERNAL_HPP

#include <stdexcept>
#include <string>

#include <command.hpp>

struct UnknownCommand : std::exception
{
};

class ExternalCommand : public Command
{
	virtual std::string getShortOptions () override
	{
		return "";
	}

	virtual std::string getSynopsis () override
	{
		return "<anything>";
	}

	virtual std::string getShortHelpText () override
	{
		return "External command.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const &) override
	{
		throw UnknownCommand ();
	}
};

void elektraExecve (const char * filename, char * const argv[]);
void tryExternalCommand (char ** argv);
void runManPage (std::string command = "", std::string profile = "current");
bool runEditor (std::string editor, std::string file);

#endif
