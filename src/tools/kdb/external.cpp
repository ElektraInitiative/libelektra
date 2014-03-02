#include <external.hpp>
#include "kdbconfig.h"

#include <string>
#include <vector>
#include <iostream>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <errno.h>

extern char **environ;

const char * buildinExecPath = BUILTIN_EXEC_FOLDER;

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
				std::cerr << "Could not determine "
					<< "current path for path "
					<< pathes[p]
					<< " and command name: "
					<< argv[0]
					<< " because: "
					<< strerror(errno)
					<< std::endl;
				continue;
			}
			command += currentPath;
			command += "/";
		}

		command += pathes[p];
		command += "/";
		command += argv[0];

		struct stat buf;
		if (stat(command.c_str(), &buf) == -1)
		{
			if (errno == ENOENT)
			{
				// the file simply does not exist
				// so it seems like it is an
				// UnknownCommand
				continue;
			}
			else
			{
				std::cerr << "The external command "
					<< command
					<< " could not be found, because: "
					<< strerror(errno)
					<< std::endl;
				continue;
			}
		}

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
