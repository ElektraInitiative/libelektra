/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_GEN_HPP
#define ELEKTRA_GEN_HPP

#include "command.hpp"
#include <kdb.hpp>

class GenCommand : public Command
{
public:
	~GenCommand () override = default;

	std::string getShortOptions () override
	{
		return "F";
	}

	std::string getSynopsis () override
	{
		return "<templateName> <parentKey> <outputName> [parameters...]";
	}

	std::string getShortHelpText () override
	{
		return ""; // TODO
	}

	std::string getLongHelpText () override
	{
		return ""; // TODO
	}

	int execute (Cmdline const & cmdline) override;

private:
};

#endif // ELEKTRA_GEN_HPP
