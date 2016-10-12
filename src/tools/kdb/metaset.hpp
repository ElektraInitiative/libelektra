/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef METASET_HPP
#define METASET_HPP

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class MetaSetCommand : public Command
{
	kdb::KDB kdb;

public:
	MetaSetCommand ();
	~MetaSetCommand ();

	virtual std::string getShortOptions () override
	{
		return "qvC";
	}

	virtual std::string getSynopsis () override
	{
		return "<key-name> <meta-name> <meta-value>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Set a meta value.";
	}

	virtual std::string getLongHelpText () override
	{
		return "Meta key are information about keys.\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
