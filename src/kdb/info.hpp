#ifndef INFO_HPP
#define INFO_HPP

#include <command.hpp>

#include <kdb.hpp>

class InfoCommand : public Command
{
	kdb::KDB kdb;

public:
	InfoCommand();
	~InfoCommand();
	int execute(int argc, char**argv);
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
		return "Print info about a plugin.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<name>\n"
			"Uses the configuration below system/elektra/modules/\n"
			"to print some info about an plugin.\n"
			"If this information could not be found\n"
			"(e.g. plugin not mounted anywhere)\n"
			"the module will be loaded.\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
