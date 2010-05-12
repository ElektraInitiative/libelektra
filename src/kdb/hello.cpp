#include <hello.hpp>
#include <iostream>

using namespace std;

HelloCommand::HelloCommand()
{
	cout << "Hello Command" << endl;
}

HelloCommand::~HelloCommand()
{
	cout << "~Hello Command" << endl;
}

int HelloCommand::execute (int argc, char**argv)
{
	cout << "command()" << endl;
	return 0;
}
