/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <kdb.h>
#include <kdb.hpp>
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

const char * buildinExecPath = BUILTIN_EXEC_FOLDER;

static std::string cwd()
{
	std::vector<char> current_dir;
	current_dir.resize(KDB_MAX_PATH_LENGTH);
	errno = 0;
	while (getcwd(&current_dir[0], current_dir.size()) == NULL
			&& errno == ERANGE)
	{
		current_dir.resize(current_dir.size()*2);
	}

	if (errno != 0)
	{
		return std::string();
	}

	return std::string(&current_dir[0]);
}

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
			const std::string currentPath = cwd();

			if (currentPath.empty())
			{
				std::cerr << "Could not determine "
					<< "current path for "
					<< pathes[p]
					<< " with command name: "
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

		elektraExecve(command.c_str(), argv);

		std::cerr << "Could not execute external command "
			<< command
			<< " because: " << strerror(errno)
			<< std::endl;

		argv[0] = savedArg;
	}

	throw UnknownCommand();
}

#ifndef _WIN32
extern char **environ;
#endif

void elektraExecve(const char *filename, char *const argv[])
{
#ifndef _WIN32
		execve(filename, argv, environ);
#else
		execve(filename, argv, 0);
#endif
}


void runManPage(std::string command)
{
	command = "kdb-"+command;
	const char * man = "/usr/bin/man";
	using namespace kdb;
	std::string dirname = "/sw/kdb/current/";
	KDB kdb;
	KeySet conf;
	kdb.get(conf, dirname);

	Key k = conf.lookup(dirname+"man");
	if (k) man = k.get<std::string>().c_str();
	char * const argv [3] = {const_cast<char*>(man),
		const_cast<char*>(command.c_str()),
		0};

	elektraExecve(man, argv);

	throw UnknownCommand();
}
