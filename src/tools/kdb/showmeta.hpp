/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef SHOWMETA_HPP
#define SHOWMETA_HPP

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class ShowMetaCommand : public Command
{
	kdb::KDB kdb;

public:
	ShowMetaCommand ();
	~ShowMetaCommand ();

	virtual std::string getShortOptions () override
	{
		return "";
	}

	virtual std::string getSynopsis () override
	{
		return "<name>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Print all metakeys along with their value for the given key.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
