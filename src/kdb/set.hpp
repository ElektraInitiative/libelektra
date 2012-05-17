#ifndef SET_HPP
#define SET_HPP

#include <command.hpp>

#include <kdb.hpp>

class SetCommand : public Command
{
	kdb::KDB kdb;
public:
	SetCommand();
	~SetCommand();

	virtual std::string getShortOptions()
	{
		return "v";
	}

	virtual std::string getShortHelpText()
	{
		return "Get a value.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<name> [<value>]\n"
			"\n"
			"Get an value from the key database.\n"
			"\n"
			"If no value is given, it will be set to a null-value"
			"To get an empty value you need to quote like \"\" (depending on shell)";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
