#ifndef GET_HPP
#define GET_HPP

#include <command.hpp>

#include <kdb.hpp>

class GetCommand : public Command
{
	kdb::KDB kdb;

public:
	GetCommand();
	~GetCommand();
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
		return "Retrieve an key.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<name>\n"
			"Get an individual key.\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
