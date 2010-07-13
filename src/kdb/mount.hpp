#ifndef MOUNT_HPP
#define MOUNT_HPP

#include <command.hpp>
#include <kdb>

struct NameAlreadyInUseException : public CommandException
{
	virtual const char* what() const throw()
	{
		return "Name already used, will abort";
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
	int execute(int argc, char** argv);
	~MountCommand();
};

#endif
