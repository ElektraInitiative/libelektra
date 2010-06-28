#include <iostream>
#include <memory>

#include <factory.hpp>
#include <command.hpp>

#include <key>

int main(int argc, char**argv) try
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
catch (kdb::Key& key)
{
	std::cerr << "a key was thrown" << std::endl;
	printError(key);
	printWarnings(key);
}
