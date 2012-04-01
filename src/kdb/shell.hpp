#ifndef SHELL_HPP
#define SHELL_HPP

#include <command.hpp>

#include <kdb.hpp>

class ShellCommand : public Command
{
	kdb::KDB kdb;

public:
	ShellCommand();
	~ShellCommand();
	int execute(int argc, char**argv);
};

#endif
