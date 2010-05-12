#ifndef LS_H
#define LS_H

#include <command.hpp>
#include <kdb>

class LsCommand : public Command
{
	kdb::KDB kdb;
	kdb::KeySet ks;

public:
	LsCommand();
	int execute(int argc, char**argv);
	~LsCommand();
};

#endif
