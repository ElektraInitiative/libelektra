/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_RECORDSTART_HPP
#define ELEKTRA_RECORDSTART_HPP

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class RecordStartCommand : public Command
{
public:
	RecordStartCommand ();
	~RecordStartCommand ();

	virtual std::string getShortOptions () override
	{
		return "";
	};

	virtual std::string getSynopsis () override
	{
		return "[parentKey]";
	};

	virtual std::string getShortHelpText () override
	{
		return "Start a recording session.";
	}

	virtual std::string getLongHelpText () override
	{
		return "For example, use \"kdb record-start\" to start a recording session for the entire KDB,\nor "
		       "use \"kdb record-start user:/test\" to only record changes made to keys living in user:/test";
	}

	virtual int execute (Cmdline const & cmdline) override;
};


#endif // ELEKTRA_RECORDSTART_HPP
