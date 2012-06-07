#ifndef LS_H
#define LS_H

#include <command.hpp>
#include <kdb.hpp>

class LsCommand : public Command
{
	kdb::KDB kdb;
	kdb::KeySet ks;

public:
	LsCommand();
	~LsCommand();

	virtual std::string getShortOptions()
	{
		return "v";
	}

	virtual std::string getSynopsis()
	{
		return "<name>";
	}

	virtual std::string getShortHelpText()
	{
		return "List the names of keys below a given name.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"List all keys below given name.\n"
			"To also retrieve the value use the\n"
			"export command."
			;
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
