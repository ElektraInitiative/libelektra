#ifndef CHECK_HPP
#define CHECK_HPP

#include <command.hpp>

#include <kdb.hpp>

class CheckCommand : public Command
{
public:
	CheckCommand();
	~CheckCommand();

	virtual std::string getShortOptions()
	{
		return "v";
	}

	virtual std::string getSynopsis()
	{
		return "<name>";
	}

	virtual std::string getShortHelpText()
	{
		return "Do some basic checks on a plugin.";
	}

	virtual std::string getLongHelpText()
	{
		return "";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
