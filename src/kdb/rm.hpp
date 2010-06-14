#ifndef REMOVE_HPP
#define REMOVE_HPP

#include <command.hpp>

#include <kdb>

class RemoveCommand : public Command
{
	kdb::KDB kdb;

public:
	RemoveCommand();
	~RemoveCommand();
	int execute(int argc, char**argv);
};

#endif
