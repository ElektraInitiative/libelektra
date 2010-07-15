#ifndef TEST_H
#define TEST_H

#include <command.hpp>
#include <kdb>

class TestCommand : public Command
{
	kdb::Key root;
	int nrTest;
	int nrError;

public:
	TestCommand();
	void doTests();

	void doBasicTest();
	void doStringTest();
	void doBinaryTest();
	void doNamingTest();

	int execute(int argc, char**argv);
	~TestCommand();
};

#endif
