#ifndef VALIDATION_HPP
#define VALIDATION_HPP

#include <command.hpp>

#include <kdb.hpp>

class ValidationCommand : public Command
{
	kdb::KDB kdb;

public:
	ValidationCommand();
	~ValidationCommand();
	int execute(int argc, char**argv);
};

#endif
