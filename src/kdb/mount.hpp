#ifndef MOUNT_HPP
#define MOUNT_HPP

#include <command.hpp>
#include <kdb>

class MountCommand : public Command
{
	kdb::KDB kdb;
	static std::string root;
public:
	MountCommand();
	kdb::KeySet addPlugins(std::string name, std::string which);
	int execute(int argc, char** argv);
	~MountCommand();
};

#endif
