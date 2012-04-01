#ifndef GET_HPP
#define GET_HPP

#include <command.hpp>

#include <kdb.hpp>

class GetCommand : public Command
{
	kdb::KDB kdb;

public:
	GetCommand();
	~GetCommand();
	int execute(int argc, char**argv);
};

#endif
