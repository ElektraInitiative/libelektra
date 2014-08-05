
#ifndef MOUNTBASE_HPP_
#define MOUNTBASE_HPP_

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
namespace tools
{
	class Backend;
}
}

class MountBaseCommand : public Command
{

protected:
	void readMountConf();
	void fixRootKey(Cmdline const& cl);
	void getName(Cmdline const& cl);
	void getMountpoint(Cmdline const& cl);
	void askForConfirmation(Cmdline const& cl);
	void doIt();

	kdb::KeySet mountConf;
	std::string name;
	std::string path;
	std::string mp;

public:
};



#endif /* MOUNTBASE_HPP_ */
