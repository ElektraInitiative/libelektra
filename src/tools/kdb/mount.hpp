/**
 * \file
 *
 * \brief header file of mount command
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


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

namespace kdb
{
	class Backend;
}

class MountCommand : public Command
{
	void readMountConf();
	void outputMtab();
	void processArguments(Cmdline const& cl);
	void fixRootKey(Cmdline const& cl);
	void getName(Cmdline const& cl);
	void getMountpoint(Cmdline const& cl);
	void buildBackend(Cmdline const& cl);
	void appendPlugins(Cmdline const& cl, kdb::Backend & backend);
	void askForConfirmation(Cmdline const& cl);
	void doIt();

	kdb::KeySet mountConf;
	std::string name;
	std::string path;
	std::string mp;

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
			"plugin .. a list of plugins to mount at that place\n"
			"\n"
			"With the -i option, the mounting will be done interactively\n"
			"With no options and no arguments, the current mountpoints will be listed\n"
			;
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
