/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_RECORDSETUP_HPP
#define ELEKTRA_RECORDSETUP_HPP


#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class RecordSetupCommand : public Command
{
public:
	RecordSetupCommand ();
	~RecordSetupCommand ();

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
		return "Initial setup for session recording. Probably only needed after first installation.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};


#endif // ELEKTRA_RECORDSETUP_HPP
