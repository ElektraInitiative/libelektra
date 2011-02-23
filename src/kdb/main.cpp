#include <iostream>
#include <vector>
#include <memory>
#include <string>

#include <factory.hpp>
#include <command.hpp>

#include <key>

void displayHelp(std::string app, std::vector<std::string> commands)
{
	std::cout << app
		<< " is a program to manage elektra's key database"
		<< std::endl;
	std::cout << "Usage: " << app << " <command> [args]"
		<< std::endl;
	std::cerr << "Known commands are:" << std::endl;
	for (
		std::vector<std::string>::iterator it =
		commands.begin();
		it != commands.end();
		it++)
	{
		std::cout << *it << std::endl;
	}
}

int main(int argc, char**argv)
{
	Factory f;

	if (argc < 2)
	{
		displayHelp(argv[0], f.getCommands());
		return -1;
	}

	try {
		std::auto_ptr<Command> c = f.get(argv[1]);
		return c->execute (argc, argv);
	}
	catch (UnknownCommand const& uc)
	{
		std::cerr << "The command "
			<< argv[1]
			<< " is not known"
			<< std::endl;
		displayHelp(argv[0], f.getCommands());
		return -2;
	}
	catch (kdb::Key& key)
	{
		std::cerr << argv[0] << " failed while accessing the key database"
			<< std::endl;
		printError(key);
		printWarnings(key);
		return -3;
	}
	catch (...)
	{
		std::cerr << "Unkown error" << std::endl;
		displayHelp(argv[0], f.getCommands());
		return -4;
	}
}
