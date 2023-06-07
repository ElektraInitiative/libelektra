/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_RECORDSTOP_HPP
#define ELEKTRA_RECORDSTOP_HPP

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class RecordStopCommand : public Command
{
public:
	RecordStopCommand ();
	~RecordStopCommand ();

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
		return "Stop the recording session.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif // ELEKTRA_RECORDSTOP_HPP
