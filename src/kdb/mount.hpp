#ifndef MOUNT_HPP
#define MOUNT_HPP

#include <command.hpp>
#include <kdb.hpp>

struct NameAlreadyInUseException : public CommandException
{
	virtual const char* what() const throw()
	{
		return "Name already used, will abort";
	}
};

struct MountpointNotValid: public CommandException
{
	virtual const char* what() const throw()
	{
		return "The supplied name did not start with /\n"
			"nor is it a valid keyname";
	}
};

struct MountpointAlreadyInUseException : public CommandException
{
	virtual const char* what() const throw()
	{
		return "Mountpoint already used, will abort";
	}
};

struct PathInvalidException : public CommandException
{
	virtual const char* what() const throw()
	{
		return  "Given path could not be opened\n"
			"You must provide a valid file name for the global path";
	}
};


class MountCommand : public Command
{
	static std::string root;
public:
	MountCommand();
	bool checkFile(std::string file);
	void outputMtab();
	int execute(int argc, char** argv);
	~MountCommand();
	virtual std::string getShortOptions()
	{
		return "i";
	}

	virtual unsigned int getNrOfArguments()
	{
		return 0;
	}

	virtual std::string getShortHelpText()
	{
		return "Mount a new backend.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"\n"
			"Allows you to interactively mount\n"
			"a new backend.\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
