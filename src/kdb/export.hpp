#ifndef EXPORT_H
#define EXPORT_H

#include <command.hpp>
#include <kdb.hpp>

class ExportCommand : public Command
{
	kdb::KDB kdb;
	kdb::KeySet ks;

public:
	ExportCommand();
	int execute(int argc, char**argv);
	~ExportCommand();
};

#endif
