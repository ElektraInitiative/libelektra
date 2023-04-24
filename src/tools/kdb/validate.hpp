/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef VALIDATE_HPP
#define VALIDATE_HPP

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class ValidateCommand : public Command
{
	kdb::KDB kdb;

public:
	ValidateCommand () = default;
	~ValidateCommand () override = default;

	virtual std::string getShortOptions () override
	{
		return "f";
	}

	virtual std::string getSynopsis () override
	{
		return "<name>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Validate the values of string keys below a given name.";
	}

	virtual std::string getLongHelpText () override
	{
		return "This command is useful for validating configuration files against\n"
		       "their specifications.\n"
		       "For keys to be validated, they must contain the 'check'-metakeys\n"
		       "and the respective plugins for validation must be loaded\n"
		       "for the backend that was used while mounting.\n"
		       "If a validation is done while using 'kdb set'\n"
		       "the same validation is also done by 'kdb validate'\n"
		       "Only string keys are validated!\n"
		       "\n"
		       "Use -f to do a write test even if the previous read\n"
		       "from the key database has issued warnings.\n";
	}

	virtual int execute (Cmdline const & cmdline) override;

private:
	std::string getFormattedErrorString (const std::string &);
	std::string getFormattedInfoString (const std::string &);
	std::string getFormattedSuccessString (const std::string &);
};

#endif
