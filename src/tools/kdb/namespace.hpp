/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef NAMESPACE_HPP
#define NAMESPACE_HPP

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class NamespaceCommand : public Command
{
	kdb::KDB kdb;

public:
	NamespaceCommand ();
	~NamespaceCommand ();

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
		return "Get the namespace of a key (trailing \':\' included).";
	}

	virtual std::string getLongHelpText () override
	{
		return "For example, \"kdb namespace user:/key\" will yield \"user:\",\n"
		       "and, \"kdb namespace /key\" will yield the empty string.\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
