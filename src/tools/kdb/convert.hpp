/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef CONVERT_HPP
#define CONVERT_HPP

#include <command.hpp>

#include <kdb.hpp>

class ConvertCommand : public Command
{
	kdb::KeySet ks;

public:
	ConvertCommand ();
	~ConvertCommand ();

	virtual std::string getShortOptions () override
	{
		return "";
	}

	virtual std::string getSynopsis () override
	{
		return "[<import-format>] [<export-format>] [<import-file>] [export-file]";
	}

	virtual std::string getShortHelpText () override
	{
		return "Convert configuration.";
	}

	virtual std::string getLongHelpText () override
	{
		return "The convert command allows you to convert\n"
		       "any file format supported by Elektra to any other.\n"
		       "\n"
		       "The key database is not touched.\n"
		       "\n"
		       "Example:\n"
		       "cat sw.ecf | kdb convert dump xml > sw.xml\n"
		       "To convert an Elektra dump file to xml.";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
