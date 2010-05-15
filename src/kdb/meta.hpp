#ifndef META_HPP
#define META_HPP

#include <command.hpp>

#include <kdb>

class MetaCommand : public Command
{
	kdb::KDB kdb;

public:
	MetaCommand();
	~MetaCommand();
	int execute(int argc, char**argv);
};

#endif
