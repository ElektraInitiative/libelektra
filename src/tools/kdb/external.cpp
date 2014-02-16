#include <external.hpp>

#include <string>
#include <vector>
#include <iostream>

#include <unistd.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

extern char **environ;

// get from cmake, RPATH?
const char * buildinExecPath = "/usr/lib/kdb-tool";

void tryExternalCommand(char** argv)
{
	std::vector<std::string> pathes;

	char *execPath = getenv("KDB_EXEC_PATH");
	if (execPath)
	{
		pathes.push_back(execPath);
	}
	pathes.push_back(buildinExecPath);

	for(size_t p = 0; p<pathes.size(); ++p)
	{
		std::string command;
		char* savedArg = 0;

		if (pathes[p][0] != '/')
		{
			// no absolute path, so work with current path
			char currentPath [PATH_MAX];

			if (!getcwd(currentPath, sizeof(currentPath)))
			{
				std::cerr << "Could not determine current "
					<< "path" << strerror(errno)
					<< std::endl;
				break;
			}
			command += currentPath;
		}

		command += pathes[p];
		command += "/kdb-";
		command += argv[0];

		std::cout << "try to exec " << command << std::endl;

		savedArg = argv[0];
		argv[0] = const_cast<char*>(command.c_str());

		execve(command.c_str(), argv, environ);

		std::cerr << "Could not execute external command "
			<< command
			<< " because: " << strerror(errno)
			<< std::endl;

		argv[0] = savedArg;
	}

	throw UnknownCommand();
}
