#ifndef METALS_H
#define METALS_H

#include <command.hpp>
#include <kdb.hpp>

class MetaLsCommand : public Command
{
	kdb::KDB kdb;
	kdb::KeySet ks;

public:
	MetaLsCommand();
	~MetaLsCommand();

	virtual std::string getShortOptions()
	{
		return "";
	}

	virtual std::string getShortHelpText()
	{
		return "Retrieve meta information.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<name>\n"
			"Get all meta information of an individual\n"
			"key.\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
