#ifndef HELLO_HPP
#define HELLO_HPP

#include <command.hpp>

class HelloCommand : public Command
{
public:
	HelloCommand();
	~HelloCommand();
	int execute (int argc, char**argv);
};

#endif
