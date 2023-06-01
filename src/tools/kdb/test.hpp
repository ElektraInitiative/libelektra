/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef TEST_H
#define TEST_H

#include <string>
#include <vector>

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class TestCommand : public Command
{
	kdb::Key root;
	int nrTest;
	int nrError;
	std::string testNames;

public:
	TestCommand ();
	~TestCommand ();

	void doTests (std::vector<std::string> const & arguments);

	void doBasicTest ();
	void doStringTest ();
	void doUmlautsTest ();
	void doBinaryTest ();
	void doNamingTest ();
	void doMetaTest ();

	virtual std::string getShortOptions () override
	{
		return "";
	}

	virtual std::string getSynopsis () override
	{
		return "<root-key> [<test-name> ...]";
	}

	virtual std::string getShortHelpText () override
	{
		return "Run key database test suite.";
	}

	virtual std::string getLongHelpText () override
	{
		return "This command runs an internal test suite.\n"
		       "The tests will set and get many keys below\n"
		       "the given rootkey.\n"
		       "\n"
		       "The main purpose of these tests is to check\n"
		       "if a backend is capable of storing and retrieving\n"
		       "all kinds of configuration keys and values.\n"
		       "\n"
		       "If no test name is given, every available test\n"
		       " is executed.\n"
		       "\n"
		       "Following tests are available:" +
		       testNames;
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
