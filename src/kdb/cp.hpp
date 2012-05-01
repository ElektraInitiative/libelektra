#ifndef CP_HPP
#define CP_HPP

#include <command.hpp>

#include <kdb.hpp>

class CpCommand : public Command
{
	kdb::KDB kdb;

public:
	CpCommand();
	~CpCommand();

	virtual std::string getShortOptions()
	{
		return "rv";
	}

	virtual std::string getShortHelpText()
	{
		return "Copy configuration.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<source> <dest>\n"
			"Copy configuration within the key database.\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
