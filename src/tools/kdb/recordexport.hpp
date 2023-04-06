/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_RECORDEXPORT_HPP
#define ELEKTRA_RECORDEXPORT_HPP

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class RecordExportCommand : public Command
{
public:
	RecordExportCommand ();
	~RecordExportCommand ();

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
		return "Export the recorded session";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};


#endif // ELEKTRA_RECORDEXPORT_HPP
