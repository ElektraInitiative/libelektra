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
		return "n";
	}

	virtual std::string getSynopsis()
	{
		return "<name>";
	}

	virtual std::string getShortHelpText()
	{
		return "Get the value of an individual key.";
	}

	virtual std::string getLongHelpText()
	{
		return "";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
