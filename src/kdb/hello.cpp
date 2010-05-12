#include <hello.hpp>
#include <iostream>

using namespace std;

HelloCommand::HelloCommand()
{
	cout << "Hello Command" << endl;
}

int HelloCommand::execute (int, char **)
{
	cout << "command()" << endl;
	return 0;
}

HelloCommand::~HelloCommand()
{
	cout << "~Hello Command" << endl;
}
