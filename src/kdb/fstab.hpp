#ifndef FSTAB_HPP
#define FSTAB_HPP

#include <command.hpp>

#include <kdb.hpp>

class FstabCommand : public Command
{
	kdb::KDB kdb;

public:
	FstabCommand();
	~FstabCommand();
	int execute(int argc, char**argv);
	virtual std::string getShortOptions()
	{
		return "v";
	}

	virtual std::string getShortHelpText()
	{
		return "Create a new fstab entry.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<key-name> <device> <mpoint> <type> <options>\n"
			"Because of the format of fstab entries\n"
			"it is not possible to set individual elements\n"
			"of new fstab entries.\n"
			"\n"
			"This utility creates a whole fstab entry\n"
			"with a single call to bypass this problem.\n"
			"The name of the entry will be ZZZNewFstabName\n"
			"because it expects the fstab backend to rewrite\n"
			"the name to a proper one.\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
