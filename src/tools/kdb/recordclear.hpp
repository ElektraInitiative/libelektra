/**
* @file
*
* @brief
*
* @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_RECORDCLEAR_HPP
#define ELEKTRA_RECORDCLEAR_HPP

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class RecordClearCommand : public Command
{
public:
	RecordClearCommand ();
	~RecordClearCommand ();

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
		return "Clear the recording session.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};


#endif // ELEKTRA_RECORDCLEAR_HPP
