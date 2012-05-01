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
	int execute(int argc, char**argv);
	~LsCommand();
	virtual std::string getShortOptions()
	{
		return "";
	}

	virtual unsigned int getNrOfArguments()
	{
		return 1;
	}

	virtual std::string getShortHelpText()
	{
		return "List the names of keys.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<name>\n"
			"List all keys below given name.\n"
			"To also retrieve the value use the\n"
			"export command.\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
