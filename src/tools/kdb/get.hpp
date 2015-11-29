/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef GET_HPP
#define GET_HPP

#include <command.hpp>

#include <kdb.hpp>

class GetCommand : public Command
{
	kdb::Key root;
	kdb::KDB kdb;

public:
	GetCommand();
	~GetCommand();

	virtual std::string getShortOptions() override
	{
		return "anv";
	}

	virtual std::string getSynopsis() override
	{
		return "<name>";
	}

	virtual std::string getShortHelpText() override
	{
		return "Get the value of an individual key.";
	}

	virtual std::string getLongHelpText() override
	{
		return  "When the key starts with / a cascading lookup will be done.\n"
			"Use -v to see the keyname in that case\n"
			"\n"
			"Current limitation: only keys with the mountpoint will be considered\n"
			"during a cascading lookup.\n"
			"As workaround to get all keys: pass -a\n"
			"Or check with \"kdb ls <same key>\" if a override/fallback key is considered\n"
			"\n"
			"Example:\n"
			"\n"
			"   kdb get system/elektra/version/constants/KDB_VERSION\n"
			"";
	}

	virtual int execute (Cmdline const& cmdline) override;
};

#endif
