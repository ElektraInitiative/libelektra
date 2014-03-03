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
	void outputMtab();

public:
	MountCommand();
	~MountCommand();

	virtual std::string getShortOptions()
	{
		return "id";
	}

	virtual std::string getSynopsis()
	{
		return "[path mountpoint] [plugin [..]]";
	}

	virtual std::string getShortHelpText()
	{
		return "Mount a new backend.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"path .. a filename (absolute for system, relative for cascading or user)\n"
			"mountpoint .. where to mount the backend, start with / for cascading mp\n"
			"plugin .. a list of plugins to mount at that place"
			;
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
