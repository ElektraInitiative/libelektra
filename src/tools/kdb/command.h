/**
 * @file
 *
 * @brief Header for things used everywhere in the kdb tool
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_COMMAND_H
#define ELEKTRA_KDB_COMMAND_H

#include <colors.h>

#include <kdb.h>
#include <stdbool.h>
#include <unistd.h>

#define CLI_BASE_KEY "/sw/elektra/kdb/#0/current"

#define ADD_BASIC_OPTIONS(baseSpec, baseKeyName)                                                                                           \
	ksAppendKey (baseSpec, keyNew (baseKeyName "/debug", KEY_META, "description",                                                      \
				       "Give debug information or ask debug questions (in interactive mode).", KEY_META, "opt", "d",       \
				       KEY_META, "opt/long", "debug", KEY_META, "opt/arg", "none", KEY_END));                              \
	ksAppendKey (baseSpec, keyNew (baseKeyName "/verbose", KEY_META, "description", "Explain what is happening.", KEY_META, "opt",     \
				       "v", KEY_META, "opt/long", "verbose", KEY_META, "opt/arg", "none", KEY_END));                       \
	ksAppendKey (baseSpec, keyNew (baseKeyName "/version", KEY_META, "description", "Print version info.", KEY_META, "opt", "V",       \
				       KEY_META, "opt/long", "version", KEY_META, "opt/arg", "none", KEY_END));                            \
	ksAppendKey (baseSpec, keyNew (baseKeyName "/profile", KEY_META, "description", "Use a different profile for kdb configuration.",  \
				       KEY_META, "opt", "p", KEY_META, "opt/arg/help", "NAME", KEY_META, "opt/long", "profile", KEY_META,  \
				       "opt/arg", "required", KEY_END));                                                                   \
	ksAppendKey (baseSpec, keyNew (baseKeyName "/color", KEY_META, "description", "Print never/auto(default)/always colored output.",  \
				       KEY_META, "opt", "C", KEY_META, "opt/arg/help", "WHEN", KEY_META, "opt/long", "color", KEY_META,    \
				       "opt/arg", "required", KEY_END));                                                                   \
	ksAppendKey (baseSpec,                                                                                                             \
		     keyNew (baseKeyName "/nonewline", KEY_META, "description", "Suppress the newline at the end of the output.",          \
			     KEY_META, "opt", "n", KEY_META, "opt/long", "no-newline", KEY_META, "opt/arg", "none", KEY_END));

#define GET_OPT_KEY(options, key) ksLookupByName (options, key, 0)
#define GET_OPT(options, key) keyString (GET_OPT_KEY (options, key))

#define OR(value, def)                                                                                                                     \
	void * tmp = value;                                                                                                                \
	tmp == NULL ? (def) : tmp
#define HAS_ERR(errorKey) keyGetMeta (errorKey, "error/reason") != NULL
#define GET_ERR(errorKey) keyString (keyGetMeta (errorKey, "error/reason"))
#define COMMAND_BASE_KEY(name) CLI_BASE_KEY "/" name
#define COMMAND_SPEC_KEY(name) "spec:" COMMAND_BASE_KEY (name)

// only use in the context of a command/sub-command (options variable and GET_OPTION_KEY macro have to be in scope)
#define GET_BASIC_OPTIONS                                                                                                                  \
	bool debug = false;                                                                                                                \
	Key * tmp = GET_OPTION_KEY (options, "debug");                                                                                     \
	if (tmp != NULL)                                                                                                                   \
	{                                                                                                                                  \
		elektraKeyToBoolean (GET_OPTION_KEY (options, "debug"), &debug);                                                           \
	}                                                                                                                                  \
	/* debug -> verbose, so logLevel = debug+verbose  */                                                                               \
	bool verbose = debug;                                                                                                              \
        if (verbose) {}; /* disable unused variable warning  */                                                                            \
	tmp = GET_OPTION_KEY (options, "verbose");                                                                                         \
	if (tmp != NULL)                                                                                                                   \
	{                                                                                                                                  \
		elektraKeyToBoolean (GET_OPTION_KEY (options, "verbose"), &verbose);                                                       \
	}                                                                                                                                  \
	bool noNewLine = false;                                                                                                            \
	tmp = GET_OPTION_KEY (options, "nonewline");                                                                                       \
	if (tmp != NULL)                                                                                                                   \
	{                                                                                                                                  \
		elektraKeyToBoolean (GET_OPTION_KEY (options, "nonewline"), &noNewLine);                                                   \
	}                                                                                                                                  \
	int colorMode = CLI_COLOR_AUTO;                                                                                                    \
        if (colorMode) {}; /* disable unused variable warning  */                                                                          \
	tmp = GET_OPTION_KEY (options, "color");                                                                                           \
	if (tmp != NULL)                                                                                                                   \
	{                                                                                                                                  \
		if (elektraStrCmp ("never", keyString (tmp)) == 0)                                                                         \
		{                                                                                                                          \
			colorMode = CLI_COLOR_NEVER;                                                                                       \
		}                                                                                                                          \
		if (elektraStrCmp ("always", keyString (tmp)) == 0)                                                                        \
		{                                                                                                                          \
			colorMode = CLI_COLOR_ALWAYS;                                                                                      \
		}                                                                                                                          \
	}                                                                                                                                  \
	char * fmtBuffer = NULL;                                                                                                           \
        if (fmtBuffer) {}; /* disable unused variable warning  */                                                                          \
	if (!isatty (STDOUT_FILENO))                                                                                                       \
	{                                                                                                                                  \
		colorMode = CLI_COLOR_NEVER;                                                                                               \
	}                                                                                                                                  \
                                                                                                                                           \
	int logLevel = verbose + debug;                                                                                                    \
        if (logLevel) {}; /* disable unused variable warning  */                                                                           \
	keyDel (tmp);

#define EXEC_EXT(prog, argv, status)                                                                                                       \
	pid_t extPid;                                                                                                                      \
	int timeout = 1000;                                                                                                                \
	if (0 == (extPid = fork ()))                                                                                                       \
	{                                                                                                                                  \
		if (-1 == execve (prog, (char **) (argv), NULL))                                                                           \
		{                                                                                                                          \
			perror ("child process execve failed [%m]");                                                                       \
			return -1;                                                                                                         \
		}                                                                                                                          \
	}                                                                                                                                  \
	while (0 == waitpid (extPid, status, WNOHANG))                                                                                     \
	{                                                                                                                                  \
		if (--timeout < 0)                                                                                                         \
		{                                                                                                                          \
			perror ("timeout");                                                                                                \
			return -1;                                                                                                         \
		}                                                                                                                          \
		sleep (1);                                                                                                                 \
	}

// only print if we are at least as 'verbose' as minLogLevel
#define CLI_PRINT(minLogLevel, fmt, ...) cliPrint (logLevel, minLogLevel, fmt, __VA_ARGS__)

/**                                                                                                                                        \
 * Expands a keyname if it contains a bookmark. If @name does not contain a bookmark ref a copy of @name is returned.                      \
 *                                                                                                                                         \
 * @param name the keyname that might contain a bookmark, and where the expanded name should be saved                                      \
 * @param ks keyset that contains information about the bookmarks                                                                          \
 * @param resolved will be set to true iff a bookmark was resolved successfully                                                            \
 *                                                                                                                                         \
 * @return NULL if the bookmark could not be resolved, NULL was passed as @ks or @name                                                     \
 * @return string of the full key otherwise, has to be freed after usage                                                                   \
 */
const char * expandKeyName (KeySet * ks, const char * name, bool * resolved);

/**
 * Get a key name string from options and resolve bookmarks if present.
 *
 * @param options key set used to resolve bookmarks
 * @param rawName the keyname as it was entered by the user, may contain a bookmark
 * @param errorKey where errors should be written to, in case of: 1. can't resolve bookmark, 2. not a valid key name
 * @param verbose print more info
 * @return a pointer to the key name with the resolved bookmark(if present), has to freed
 */
const char * getKeyNameFromOptions (KeySet * options, const char * rawName, Key * errorKey, bool verbose);

/**
 * Helper for printing, handles log levels
 *
 * @param logLevel the log level set by the user
 * @param minLogLevel minimum log level so the message is printed (NONE -> always print, VERBOSE -> printf if VERBOSE or DEBUG, ...)
 * @param fmt format string for printing
 * @param ...
 */
void cliPrint (int logLevel, int minLogLevel, const char * fmt, ...);

typedef struct command
{
	const char * name;
	void (*addSpec) (KeySet * spec);
	int (*exec) (KeySet * options, Key * errorKey);
} command;

enum LOG_LEVEL
{
	CLI_LOG_NONE = 0,
	CLI_LOG_VERBOSE,
	CLI_LOG_DEBUG
};

#endif // ELEKTRA_KDB_COMMAND_H
