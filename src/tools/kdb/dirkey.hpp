/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef DIRKEY_HPP
#define DIRKEY_HPP

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class DirkeyCommand : public Command
{
	kdb::KDB kdb;

public:
	DirkeyCommand ();
	~DirkeyCommand ();

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
		return "Get the directory key of a key.";
	}
	
	virtual std::string getLongHelpText () override
	{
		return "For example, \"kdb dirkey user:/key/subkey\" will yield \"user:/key\".\n";
	}
	
	virtual int execute (Cmdline const & cmdline) override;
};

#endif
