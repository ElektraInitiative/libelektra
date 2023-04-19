/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_RECORDRM_HPP
#define ELEKTRA_RECORDRM_HPP

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class RecordRemoveKeyCommand : public Command
{
public:
	RecordRemoveKeyCommand ();
	~RecordRemoveKeyCommand ();

	virtual std::string getShortOptions () override
	{
		return "";
	};

	virtual std::string getSynopsis () override
	{
		return "key";
	};

	virtual std::string getShortHelpText () override
	{
		return "Remove the given key from the recording session.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};


#endif // ELEKTRA_RECORDRM_HPP
