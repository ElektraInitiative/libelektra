#include <iostream>
#include <memory>

#include <factory.hpp>
#include <command.hpp>

int main(int argc, char**argv)
{
	Factory f;

	if (argc < 2)
	{
		std::cout << "You need to supply a command" << std::endl;
		std::cout << "Common commands are get, set" << std::endl;
		std::cout << "ls and mount." << std::endl;
		return 1;
	}

	std::auto_ptr<Command> c = f.get(argv[1]);
	return c->execute (argc, argv);
}
