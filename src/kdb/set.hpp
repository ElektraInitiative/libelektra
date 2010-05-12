#ifndef SET_HPP
#define SET_HPP

#include <command.hpp>

#include <kdb>

class SetCommand : public Command
{
	kdb::KDB kdb;
public:
	SetCommand();
	int execute(int argc, char**argv);
	~SetCommand();
};

#endif
