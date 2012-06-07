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

	virtual std::string getShortOptions()
	{
		return "";
	}

	virtual std::string getSynopsis()
	{
		return "<plugin name>";
	}

	virtual std::string getShortHelpText()
	{
		return "Print information about a plugin.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"Uses the configuration below system/elektra/modules/\n"
			"to print some info about an plugin.\n"
			"\n"
			"If this information could not be found\n"
			"(e.g. plugin not mounted anywhere)\n"
			"the module will be loaded dynamically."
			;
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
