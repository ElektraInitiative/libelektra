#include <shell.hpp>

#include <kdb>

#include <iostream>
#include <sstream>

using namespace std;
using namespace kdb;

ShellCommand::ShellCommand()
{}

int ShellCommand::execute(int, char**)
{
	KeySet current;
	string commandline;
	string prompt = "> ";

	cout << prompt;
	while (getline(cin, commandline))
	{
		istringstream is (commandline);
		string command;

		is >> command;
		if (command == "get")
		{
			string parent;
			is >> parent;
			Key parentKey (parent);
			cout << "return value: " << kdb.get(current, parentKey) << endl;
			current.rewind();
			while (current.next())
			{
				cout << current.current().getName() << " value: " << current.current().getString() << endl;
			}
		}

		cout << prompt;
	}

	return 0;
}

ShellCommand::~ShellCommand()
{}
