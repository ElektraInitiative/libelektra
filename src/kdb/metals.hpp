#ifndef METALS_H
#define METALS_H

#include <command.hpp>
#include <kdb>

class MetaLsCommand : public Command
{
	kdb::KDB kdb;
	kdb::KeySet ks;

public:
	MetaLsCommand();
	int execute(int argc, char**argv);
	~MetaLsCommand();
};

#endif
