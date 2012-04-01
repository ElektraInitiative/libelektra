#ifndef FSTAB_HPP
#define FSTAB_HPP

#include <command.hpp>

#include <kdb.hpp>

class FstabCommand : public Command
{
	kdb::KDB kdb;

public:
	FstabCommand();
	~FstabCommand();
	int execute(int argc, char**argv);
};

#endif
