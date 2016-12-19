/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef FSTAB_HPP
#define FSTAB_HPP

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class FstabCommand : public Command
{
	kdb::KDB kdb;

public:
	FstabCommand ();
	~FstabCommand ();

	virtual std::string getShortOptions () override
	{
		return "vC";
	}

	virtual std::string getSynopsis () override
	{
		return "<key-name> <device> <mpoint> <type> <options>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Create a new fstab entry.";
	}

	virtual std::string getLongHelpText () override
	{
		return "Because of the format of fstab entries\n"
		       "it is not possible to set individual elements\n"
		       "of new fstab entries.\n"
		       "\n"
		       "This utility creates a whole fstab entry\n"
		       "with a single call to bypass this problem.\n"
		       "The name of the entry will be ZZZNewFstabName\n"
		       "because it expects the fstab plugin to rewrite\n"
		       "the name to a proper one.\n"
		       "\n"
		       "So the command will only work with the fstab plugin mounted\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
