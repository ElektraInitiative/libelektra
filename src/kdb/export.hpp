#ifndef EXPORT_H
#define EXPORT_H

#include <command.hpp>
#include <kdb.hpp>

class ExportCommand : public Command
{
	kdb::KDB kdb;
	kdb::KeySet ks;

public:
	ExportCommand();
	~ExportCommand();

	virtual std::string getShortOptions()
	{
		return "f";
	}

	virtual unsigned int getNrOfArguments()
	{
		return 1;
	}

	virtual std::string getShortHelpText()
	{
		return "Export configuration from the key database.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<source>\n"
			"The export utility allows you to export\n"
			"all or parts of the configuration to stdout.\n"
			"\n"
			"Example:\n"
			"kdb export system/sw > sw.ecf\n"
			"To make a backup of your whole configuration\n"
			"below system/sw\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
