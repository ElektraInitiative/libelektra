/**
 * @file
 *
 * @brief Code to support external programs
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <external.h>
#include <kdbease.h>

#include <errno.h>
#include <kdberrors.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <windows.h>
#include <winsock2.h>
#else
#include <spawn.h>
#include <sys/wait.h>
#endif

extern char ** environ;

const char * getExternalBin (KeySet * binaries, const char * key)
{
	Key * tmp = keyNew ("/", KEY_END);
	keySetBaseName (tmp, key);
	Key * resultKey = ksLookup (binaries, tmp, KDB_O_NONE);
	keyDel (tmp);
	if (resultKey == NULL)
	{
		return NULL;
	}
	return keyString (resultKey);
}


int tryLoadExternal (char * commandName, KeySet * binaries)
{
	char * execPathPtr = getenv ("KDB_EXEC_PATH");
	bool found = false;
	char path[PATH_MAX] = { 0 };
	char * saveptr;
	struct stat buf;

	if (execPathPtr)
	{
		char * execPath = strdup (execPathPtr);
		char * token = strtok_r (execPath, ":", &saveptr);
		while (token != NULL && !found)
		{
			snprintf (path, sizeof (path), "%s/%s", token, commandName);
			found = stat (path, &buf) != -1;
			token = strtok_r (NULL, ":", &saveptr);
		}
		elektraFree (execPath);
	}

	if (!found)
	{
		snprintf (path, sizeof (path), "%s/%s", BUILTIN_EXEC_FOLDER, commandName);
		found = stat (path, &buf) != -1;
	}

	if (found)
	{
		Key * tmp = keyNew ("/", KEY_END);
		keySetBaseName (tmp, commandName);
		keySetString (tmp, path);
		ksAppendKey (binaries, tmp);
		return 0;
	}
	return 1;
}

int loadExternalSpec (KeySet * spec, KeySet * binaries, Key * errorKey)
{
	KDB * handle = kdbOpen (NULL, errorKey);
	Key * baseKey = keyNew ("spec:" CLI_BASE_KEY, KEY_END);
	KeySet * config = ksNew (0, KS_END);
	if (kdbGet (handle, config, errorKey) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", CLI_BASE_KEY, GET_ERR (baseKey));
		keyDel (baseKey);
		ksDel (config);
		kdbClose (handle, errorKey);
		return 1;
	}
	Key * cur = NULL;
	KeySet * part = ksCut (config, baseKey);

	for (elektraCursor it = 0; it < ksGetSize (part); ++it)
	{
		cur = ksAtCursor (part, it);
		const Key * externalMeta = keyGetMeta (cur, "external");
		const Key * externalBinary = keyGetMeta (cur, "bin");
		bool isExternal = false;
		if (externalBinary != NULL && externalMeta != NULL && elektraKeyToBoolean (externalMeta, &isExternal) && isExternal)
		{ // add external spec and save path to binary
			KeySet * externalCommandSpec = ksCut (part, cur);

			Key * tmp = keyNew ("/", KEY_END);
			keySetBaseName (tmp, keyBaseName (cur));
			keySetString (tmp, keyString (externalBinary));
			ksAppendKey (binaries, tmp);

			ksAppend (spec, externalCommandSpec);
			ksDel (externalCommandSpec);
			it--;
		}
	}
	ksDel (part);
	ksDel (config);
	kdbClose (handle, errorKey);
	keyDel (baseKey);
	return 0;
}

int runExternal (const char * bin, char ** argv, Key * errorKey)
{
	// the external program should think it was called directly
	argv[1] = (char *) bin;

	int status = 0;

#ifdef _WIN32
	STARTUPINFO si;
	PROCESS_INFORMATION pi;

	ZeroMemory (&si, sizeof (si));
	si.cb = sizeof (si);
	ZeroMemory (&pi, sizeof (pi));

	// Construct command line string
	char cmdline[MAX_PATH] = "";
	for (int i = 1; argv[i]; ++i)
	{
		strcat (cmdline, "\"");
		strcat (cmdline, argv[i]);
		strcat (cmdline, "\" ");
	}

	// Start the child process.
	if (!CreateProcess (NULL,    // Module name
			    cmdline, // Command line
			    NULL,    // Process handle not inheritable
			    NULL,    // Thread handle not inheritable
			    FALSE,   // Set handle inheritance to FALSE
			    0,	     // No creation flags
			    NULL,    // Use parent's environment block
			    NULL,    // Use parent's starting directory
			    &si,     // Pointer to STARTUPINFO structure
			    &pi)     // Pointer to PROCESS_INFORMATION structure
	)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "CreateProcess failed: %lu", GetLastError ());
		return 1;
	}

	// Wait until child process exits.
	WaitForSingleObject (pi.hProcess, INFINITE);

	// Get exit code
	DWORD exitCode;
	GetExitCodeProcess (pi.hProcess, &exitCode);
	status = (int) exitCode;

	// Close process and thread handles.
	CloseHandle (pi.hProcess);
	CloseHandle (pi.hThread);
#else
	pid_t pid;

	if (posix_spawn (&pid, bin, NULL, NULL, &(argv[1]), environ) != 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "posix_spawn failed: %s", strerror (errno));
		return 1;
	}

	if (waitpid (pid, &status, 0) < 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "waitpid failed: %s", strerror (errno));
		return 1;
	}
#endif

	fflush (stdout);
	return status;
}
