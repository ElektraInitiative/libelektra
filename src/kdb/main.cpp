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
	std::cerr << "A key was thrown (temporary solution)" << std::endl;
	std::cerr << "number: " << key.getMeta<std::string>("error/number") << std::endl;
	std::cerr << "description: " << key.getMeta<std::string>("error/description") << std::endl;
	std::cerr << "ingroup: " << key.getMeta<std::string>("error/ingroup") << std::endl;
	std::cerr << "module: " << key.getMeta<std::string>("error/module") << std::endl;
	std::cerr << "at: " << key.getMeta<std::string>("error/file") << ":" << key.getMeta<std::string>("error/line") << std::endl;
	std::cerr << "reason: " << key.getMeta<std::string>("error/reason") << std::endl;
}
