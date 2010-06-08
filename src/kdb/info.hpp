#ifndef INFO_HPP
#define INFO_HPP

#include <command.hpp>

#include <kdb>

class InfoCommand : public Command
{
	kdb::KDB kdb;

public:
	InfoCommand();
	~InfoCommand();
	int execute(int argc, char**argv);
};

#endif
