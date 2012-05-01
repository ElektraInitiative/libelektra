#ifndef MV_HPP
#define MV_HPP

#include <command.hpp>

#include <kdb.hpp>

class MvCommand : public Command
{
	kdb::KDB kdb;

public:
	MvCommand();
	~MvCommand();

	virtual std::string getShortOptions()
	{
		return "rv";
	}

	virtual std::string getShortHelpText()
	{
		return "Move configuration.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<source> <dest>\n"
			"Move configuration within the key database.\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
