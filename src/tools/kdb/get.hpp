/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef GET_HPP
#define GET_HPP

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class GetCommand : public Command
{
public:
	GetCommand ();
	~GetCommand ();

	virtual std::string getShortOptions () override
	{
		return "an";
	}

	virtual std::string getSynopsis () override
	{
		return "<name>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Get the value of an individual key.";
	}

	virtual std::string getLongHelpText () override
	{
		return "When the key starts with / a cascading lookup will be done.\n"
		       "\n"
		       "Example:\n"
		       "\n"
		       "   kdb get system:/elektra/version/constants/KDB_VERSION\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
