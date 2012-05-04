#ifndef IMPORT_HPP
#define IMPORT_HPP

#include <command.hpp>
#include <kdb.hpp>

class ImportCommand : public Command
{
	kdb::KDB kdb;
	kdb::KeySet ks;

public:
	ImportCommand();
	~ImportCommand();

	virtual std::string getShortOptions()
	{
		return "s";
	}

	virtual std::string getShortHelpText()
	{
		return "Export configuration from the key database.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<destination> [<format>]\n"
			"The import utility allows you to import\n"
			"all or parts of the configuration from stdin.\n"
			"\n"
			"Example:\n"
			"cat sw.ecf | kdb import system/sw\n"
			"To restore a backup of your whole configuration\n"
			"below system/sw\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
