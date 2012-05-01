#ifndef METAGET_HPP
#define METAGET_HPP

#include <command.hpp>

#include <kdb.hpp>

class MetaGetCommand : public Command
{
	kdb::KDB kdb;

public:
	MetaGetCommand();
	~MetaGetCommand();

	virtual std::string getShortOptions()
	{
		return "";
	}

	virtual std::string getShortHelpText()
	{
		return "Get a meta value.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<key-name> <meta-name> <meta-value>\n"
			"Get a meta value.\n"
			"Meta key are information about keys.\n"
			"\n"
			"Typically there should be a more specific get/set\n"
			"interface because it is error-prone to directly\n"
			"edit metadata.\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
