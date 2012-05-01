#ifndef TEST_H
#define TEST_H

#include <command.hpp>
#include <kdb.hpp>

class TestCommand : public Command
{
	kdb::Key root;
	int nrTest;
	int nrError;

public:
	TestCommand();
	~TestCommand();

	void doTests();

	void doBasicTest();
	void doStringTest();
	void doBinaryTest();
	void doNamingTest();
	void doMetaTest();

	virtual std::string getShortOptions()
	{
		return "";
	}

	virtual std::string getShortHelpText()
	{
		return "Run tests suite.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<root-key>\n"
			"\n"
			"This command runs an internal test suite.\n"
			"The tests will set and get many keys below\n"
			"the given rootkey.\n"
			"\n"
			"The main purpose of these tests is to check\n"
			"if a backend is capable of storing and retrieving\n"
			"all kinds of configuration keys and values.\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
