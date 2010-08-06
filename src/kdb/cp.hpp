#ifndef CP_HPP
#define CP_HPP

#include <command.hpp>

#include <kdb>

class CpCommand : public Command
{
	kdb::KDB kdb;

public:
	CpCommand();
	~CpCommand();
	int execute(int argc, char**argv);
};

#endif
