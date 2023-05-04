/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_RECORDRESET_HPP
#define ELEKTRA_RECORDRESET_HPP

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class RecordResetCommand : public Command
{
public:
	RecordResetCommand ();
	~RecordResetCommand ();

	virtual std::string getShortOptions () override
	{
		return "";
	};

	virtual std::string getSynopsis () override
	{
		return "";
	};

	virtual std::string getShortHelpText () override
	{
		return "Reset the recording session.";
	}

	virtual std::string getLongHelpText () override
	{
		return "Reset the recording session by removing all keys that are contained in it.";
	}

	virtual int execute (Cmdline const & cmdline) override;
};


#endif // ELEKTRA_RECORDRESET_HPP
