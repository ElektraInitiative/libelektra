#ifndef GETPW_HPP
#define GETPW_HPP

#include <command.hpp>

#include <kdb.hpp>

class GetPwCommand : public Command
{
	kdb::KDB kdb;

public:
	GetPwCommand();
	~GetPwCommand();
	int execute(int argc, char**argv);
};

#endif
