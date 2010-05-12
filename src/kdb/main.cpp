#include <iostream>
#include <memory>

#include <factory.hpp>
#include <command.hpp>

int main(int argc, char**argv) try
{
	Factory f;

	if (argc < 2)
	{
		std::cout << "You need to supply a command!" << std::endl;
		std::cout << "Common commands are ls, get and set." << std::endl;
		return 1;
	}

	std::auto_ptr<Command> c = f.get(argv[1]);
	return c->execute (argc, argv);
} catch (...) {
	std::cerr << argv[0] << " will exit because of unknown exception" << std::endl;
}
