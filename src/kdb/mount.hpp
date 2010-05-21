#ifndef MOUNT_HPP
#define MOUNT_HPP

#include <command.hpp>
#include <kdb>

class NameAlreadyInUseException : public CommandException
{
	virtual const char* what() const throw()
	{
		return "Name already used, will abort";
	}
};

class MountpointAlreadyInUseException : public CommandException
{
	virtual const char* what() const throw()
	{
		return "Mountpoint already used, will abort";
	}
};

class MountpointInvalidException : public CommandException
{
	virtual const char* what() const throw()
	{
		return  "Given mountpoint is not a valid keyname, will abort\n"
			"Examples: system/hosts or user/sw/app";
	}
};

class MountCommand : public Command
{
	kdb::KDB kdb;
	static std::string root;
public:
	MountCommand();
	kdb::KeySet addPlugins(std::string name, std::string which);
	void checkFile(std::string file);
	int execute(int argc, char** argv);
	~MountCommand();
};

#endif
