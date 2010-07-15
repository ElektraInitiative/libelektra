#ifndef TEST_H
#define TEST_H

#include <command.hpp>
#include <kdb>

class TestCommand : public Command
{
	kdb::Key root;

public:
	TestCommand();
	void doTests();
	void doBasicTest();
	void doStringTest();
	int execute(int argc, char**argv);
	~TestCommand();
};

#endif
