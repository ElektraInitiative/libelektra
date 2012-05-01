#ifndef REMOVE_HPP
#define REMOVE_HPP

#include <command.hpp>

#include <kdb.hpp>

class RemoveCommand : public Command
{
	kdb::KDB kdb;

public:
	RemoveCommand();
	~RemoveCommand();
	int execute(int argc, char**argv);
	virtual std::string getShortOptions()
	{
		return "r";
	}

	virtual unsigned int getNrOfArguments()
	{
		return 1;
	}

	virtual std::string getShortHelpText()
	{
		return "Remove key(s) from key database.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<name>\n"
			"Remove key(s) from key database.\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
