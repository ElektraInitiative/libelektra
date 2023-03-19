/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/old_kdb.h>
#include <external.hpp>
#include <internal/kdb/config.h>
#include <kdb.hpp>

#include <iostream>
#include <string>
#include <vector>

#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#ifndef _WIN32
#include <sys/types.h>
#include <sys/wait.h>
#endif

const char * buildinExecPath = BUILTIN_EXEC_FOLDER;

static std::string cwd ()
{
	std::vector<char> current_dir;
	current_dir.resize (KDB_MAX_PATH_LENGTH);
	errno = 0;
	while (getcwd (&current_dir[0], current_dir.size ()) == nullptr && errno == ERANGE)
	{
		current_dir.resize (current_dir.size () * 2);
	}

	if (errno != 0)
	{
		return std::string ();
	}

	return std::string (&current_dir[0]);
}

void tryExternalCommand (char ** argv)
{
	std::vector<std::string> pathes;

	char * execPathPtr = getenv ("KDB_EXEC_PATH");
	if (execPathPtr)
	{
		// The string pointed by the pointer returned by getenv shall
		// not be modified. The constructor of std::string constructs a
		// copy of the string.
		std::string execPath (execPathPtr);
		size_t pos = 0;
		std::string token;
		std::string delimiter = ":";
		while ((pos = execPath.find (delimiter)) != std::string::npos)
		{
			token = execPath.substr (0, pos);
			pathes.push_back (token);
			execPath.erase (0, pos + delimiter.length ());
		}
		// There is one last path in execPath after the while
		pathes.push_back (execPath);
	}
	pathes.push_back (buildinExecPath);

	for (auto & pathe : pathes)
	{
		std::string command;
		char * savedArg = nullptr;

		if (pathe[0] != '/')
		{
			// no absolute path, so work with current path
			const std::string currentPath = cwd ();

			if (currentPath.empty ())
			{
				std::cerr << "Could not determine "
					  << "current path for " << pathe << " with command name: " << argv[0]
					  << " because: " << strerror (errno) << std::endl;
				continue;
			}
			command += currentPath;
			command += "/";
		}

		command += pathe;
		command += "/";
		command += argv[0];

		struct stat buf;
		if (stat (command.c_str (), &buf) == -1)
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
				std::cerr << "The external command " << command << " could not be found, because: " << strerror (errno)
					  << std::endl;
				continue;
			}
		}

		savedArg = argv[0];
		argv[0] = const_cast<char *> (command.c_str ());

		elektraExecve (command.c_str (), argv);

		std::cerr << "Could not execute external command " << command << " because: " << strerror (errno) << std::endl;

		argv[0] = savedArg;
	}

	throw UnknownCommand ();
}

#ifndef _WIN32
extern char ** environ;
#endif

void elektraExecve (const char * filename, char * const argv[])
{
#ifdef _WIN32
	execve (filename, argv, 0);
#else
	execve (filename, argv, environ);
#endif
}


void runManPage (std::string command, std::string profile)
{
	if (command.empty ())
	{
		command = "kdb";
	}
	else
	{
		command = "kdb-" + command;
	}
	const char * man = "/usr/bin/man";
	using namespace kdb;
	Key k = nullptr;
	if (profile != "nokdb")
	{
		try
		{
			KDB kdb;
			KeySet conf;
			std::string dirname;
			for (int i = 0; i <= 2; ++i)
			{
				switch (i)
				{
				case 0:
					dirname = "/sw/elektra/kdb/#0/" + profile + "/";
					break;
				case 1:
					dirname = "/sw/elektra/kdb/#0/%/";
					break;
				case 2:
					dirname = "/sw/kdb/" + profile + "/";
					break; // legacy
				}
				kdb.get (conf, dirname);
				if (!k) // first one wins, because we do not reassign
				{
					k = conf.lookup (dirname + "man");
				}
			}
		}
		catch (kdb::KDBException const & ce)
		{
			std::cerr << "There is a severe problem with your installation!\n"
				  << "kdbOpen() failed with the info:" << std::endl
				  << ce.what () << std::endl;
		}
	}
	if (k)
	{
		man = k.get<std::string> ().c_str ();
	}
	char * const argv[3] = { const_cast<char *> (man), const_cast<char *> (command.c_str ()), nullptr };

	elektraExecve (man, argv);
	std::cout << "Sorry, I was not able to execute the man-page viewer: \"" << man << "\".\n";
	std::cout << "Try to change /sw/elektra/kdb/#0/" + profile + "/man with full path to man.\n\n";
	std::cout << "If you did not modify settings related to the man-page viewer,\nplease report the issue at "
		     "https://issues.libelektra.org/"
		  << std::endl;
	exit (1);
}

#ifndef _WIN32
bool runEditor (std::string editor, std::string file)
{
	char * const argv[3] = { const_cast<char *> (editor.c_str ()), const_cast<char *> (file.c_str ()), 0 };

	pid_t childpid = fork ();
	if (!childpid)
	{
		elektraExecve (editor.c_str (), argv);
		exit (23);
	}
	else
	{
		int status;
		waitpid (childpid, &status, 0);
		if (WIFEXITED (status))
		{
			if (WEXITSTATUS (status) != 23)
			{
				return true;
			}
		}
	}
	return false;
}
#else
bool runEditor (std::string, std::string)
{
	// TODO: impl
	return false;
}
#endif
