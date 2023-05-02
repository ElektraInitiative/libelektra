/**
 * @file
 *
 * @brief Header for things used everywhere in the kdb tool
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_COMMAND_H
#define ELEKTRA_KDB_COMMAND_H

#include <kdb.h>

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
				       KEY_META, "opt", "c", KEY_META, "opt/arg/help", "WHEN", KEY_META, "opt/long", "color", KEY_META,    \
				       "opt/arg", "required", KEY_END));                                                                   \
	ksAppendKey (baseSpec,                                                                                                             \
		     keyNew (baseKeyName "/nonewline", KEY_META, "description", "Suppress the newline at the end of the output.",          \
			     KEY_META, "opt", "n", KEY_META, "opt/long", "no-newline", KEY_META, "opt/arg", "none", KEY_END));             \
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/nullterm", KEY_META, "description", "Use binary 0 termination.",      \
				   KEY_META, "opt", "0", KEY_META, "opt/long", "null", KEY_META, "opt/arg", "none", KEY_END));

#define GET_OPT_KEY(options, key) ksLookupByName (options, key, 0)
#define GET_OPT(options, key) keyString (GET_OPT_KEY (options, key))

#define OR(value, def)                                                                                                                     \
	void * tmp = value;                                                                                                                \
	tmp == NULL ? (def) : tmp
#define HAS_ERR(errorKey) keyGetMeta (errorKey, "error/reason") != NULL
#define HAS_ERR_CODE(errorKey) keyGetMeta (errorKey, "error/number") != NULL
#define GET_ERR(errorKey) keyString (keyGetMeta (errorKey, "error/reason"))
#define GET_ERR_CODE(errorKey) keyString (keyGetMeta (errorKey, "error/number"))
#define GET_ERR_DESC(errorKey) keyString (keyGetMeta (errorKey, "error/description"))
#define COMMAND_BASE_KEY(name) CLI_BASE_KEY "/" name
#define COMMAND_SPEC_KEY(name) "spec:" COMMAND_BASE_KEY (name)

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


typedef struct command
{
	const char * name;
	void (*addSpec) (KeySet * spec);
	int (*exec) (KeySet * options, Key * errorKey);
} command;


#endif // ELEKTRA_KDB_COMMAND_H
