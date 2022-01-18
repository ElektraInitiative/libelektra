/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef FILE_HPP
#define FILE_HPP

#include <command.hpp>

#include <kdb.hpp>

class FileCommand : public Command
{
	kdb::KDB kdb;

public:
	FileCommand ();
	~FileCommand ();

	virtual std::string getShortOptions () override
	{
		return "n";
	}

	virtual std::string getSynopsis () override
	{
		return "<name>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Prints the file where a key is located.";
	}

	virtual std::string getLongHelpText () override
	{
		return "Elektra typically stores configuration in human-readable\n"
		       "configuration files.\n"
		       "This tool outputs where the configuration file is and where\n"
		       "keys would be read from.\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
