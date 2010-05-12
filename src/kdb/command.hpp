#ifndef COMMAND_HPP
#define COMMAND_HPP

class Command
{
public:
	virtual ~Command();
	virtual int execute (int argc, char**argv) = 0;
};

#endif
