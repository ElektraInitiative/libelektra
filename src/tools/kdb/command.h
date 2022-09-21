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

/**
 * @def ADD_BASIC_OPTIONS
 * @brief This macro appends basic keys with option meta-data to a given key set.
 * @param baseSpec KeySet to append to.
 * @param baseKeyName The base key name for the appended keys.
 */
#define ADD_BASIC_OPTIONS(baseSpec, baseKeyName)                                                                                           \
	ksAppendKey (baseSpec, keyNew (baseKeyName "/debug", KEY_META, "description",                                                      \
				       "Give debug information or ask debug questions (in interactive mode).", KEY_META, "opt", "d",       \
				       KEY_META, "opt/long", "debug", KEY_META, "opt/arg", "none", KEY_END));                              \
	ksAppendKey (baseSpec, keyNew (baseKeyName "/verbose", KEY_META, "description", "Explain what is happening.", KEY_META, "opt",     \
				       "v", KEY_META, "opt/long", "verbose", KEY_META, "opt/arg", "none", KEY_END));                       \
	ksAppendKey (baseSpec, keyNew (baseKeyName "/version", KEY_META, "description", "Print version info.", KEY_META, "opt", "V",       \
				       KEY_META, "opt/long", "version", KEY_META, "opt/arg", "none", KEY_END));                            \
	ksAppendKey (baseSpec, keyNew (baseKeyName "/color", KEY_META, "description", "Print never/auto(default)/always colored output.",  \
				       KEY_META, "opt", "C", KEY_META, "opt/arg/help", "WHEN", KEY_META, "opt/long", "color", KEY_META,    \
				       "opt/arg", "required", KEY_END));                                                                   \
	ksAppendKey (baseSpec,                                                                                                             \
		     keyNew (baseKeyName "/nonewline", KEY_META, "description", "Suppress the newline at the end of the output.",          \
			     KEY_META, "opt", "n", KEY_META, "opt/long", "no-newline", KEY_META, "opt/arg", "none", KEY_END));             \
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/nullterm", KEY_META, "description", "Use binary 0 termination.",      \
				   KEY_META, "opt", "0", KEY_META, "opt/long", "null", KEY_META, "opt/arg", "none", KEY_END));

/**
 * @def GET_OPT_KEY
 * @brief This macro retrieves the key with a given name from an options key set. This is the more general version that #GET_OPTION_KEY in
 * command files should use.
 * @param options The options KeySet.
 * @param key The name of the key to retrieve.
 */
#define GET_OPT_KEY(options, key) ksLookupByName (options, key, 0)

/**
 * @def GET_OPT
 * @brief This macro retrieves the string value of the key with a given name from an options key set. This is the more general version that
 * #GET_OPTION in command files should use.
 * @param options The options KeySet.
 * @param key The name of the key to retrieve.
 */
#define GET_OPT(options, key) keyString (GET_OPT_KEY (options, key))

/**
 * @def HAS_ERR
 * @brief This macro checks if an error/reason meta-key exists in the given key.
 * @param errorKey The key to check for an error/reason meta-key.
 */
#define HAS_ERR(errorKey) keyGetMeta (errorKey, "error/reason") != NULL

/**
 * @def HAS_ERR_CODE
 * @brief This macro checks if an error/number meta-key exists in the given key.
 * @param errorKey The key to check for an error/number meta-key.
 */
#define HAS_ERR_CODE(errorKey) keyGetMeta (errorKey, "error/number") != NULL

/**
 * @def GET_ERR
 * @brief This macro retrieves the string value of the error/reason meta-key in the given key.
 * @param errorKey The key to get the error/reason from.
 */
#define GET_ERR(errorKey) keyString (keyGetMeta (errorKey, "error/reason"))

/**
 * @def GET_ERR_CODE
 * @brief This macro retrieves the string value of the error/number meta-key in the given key.
 * @param errorKey The key to get the error/number from.
 */
#define GET_ERR_CODE(errorKey) keyString (keyGetMeta (errorKey, "error/number"))

/**
 * @def GET_ERR_DESC
 * @brief This macro retrieves the string value of the error/description meta-key in the given key.
 * @param errorKey The key to get the error/description from.
 */
#define GET_ERR_DESC(errorKey) keyString (keyGetMeta (errorKey, "error/description"))

/**
 * @def COMMAND_BASE_KEY
 * @brief This macro generates the base key name for a given command. Used in command files for defining GET_OPTION_KEY and GET_OPTION.
 * @param name The full keyname for a command.
 */
#define COMMAND_BASE_KEY(name) CLI_BASE_KEY "/" name

/**
 * @def COMMAND_SPEC_KEY
 * @brief This macro generates the specification key name for a given command. Used in command files when setting the spec.
 * @param name The name of the command.
 */
#define COMMAND_SPEC_KEY(name) "spec:" COMMAND_BASE_KEY (name)

/**
 * @def GET_BASIC_OPTIONS
 * @brief This macro retrieves the values of the basic options from an options key set into local variables.
 * This has to be used in the context of a command, so #GET_OPTION_KEY has to be defined.
 *
 * #GET_OPTION_KEY should be `#define GET_OPTION_KEY(options, name)` and evaluate to a `Key *` given a `KeySet * options` and a `char *
 * name`.
 *
 */
#define GET_BASIC_OPTIONS                                                                                                                  \
	bool debug = false;                                                                                                                \
	Key * tmp = GET_OPTION_KEY (options, "debug");                                                                                     \
	if (tmp != NULL)                                                                                                                   \
	{                                                                                                                                  \
		elektraKeyToBoolean (GET_OPTION_KEY (options, "debug"), &debug);                                                           \
		keyDel (tmp);                                                                                                              \
	}                                                                                                                                  \
	/* debug -> verbose, so logLevel = debug+verbose  */                                                                               \
	bool verbose = debug;                                                                                                              \
	if (verbose)                                                                                                                       \
	{                                                                                                                                  \
	}; /* disable unused variable warning  */                                                                                          \
	tmp = GET_OPTION_KEY (options, "verbose");                                                                                         \
	if (tmp != NULL)                                                                                                                   \
	{                                                                                                                                  \
		elektraKeyToBoolean (GET_OPTION_KEY (options, "verbose"), &verbose);                                                       \
		keyDel (tmp);                                                                                                              \
	}                                                                                                                                  \
	bool noNewLine = false;                                                                                                            \
	tmp = GET_OPTION_KEY (options, "nonewline");                                                                                       \
	if (tmp != NULL)                                                                                                                   \
	{                                                                                                                                  \
		elektraKeyToBoolean (GET_OPTION_KEY (options, "nonewline"), &noNewLine);                                                   \
		keyDel (tmp);                                                                                                              \
	}                                                                                                                                  \
	bool nullTerm = false;                                                                                                             \
	tmp = GET_OPTION_KEY (options, "nullterm");                                                                                        \
	if (tmp != NULL)                                                                                                                   \
	{                                                                                                                                  \
		elektraKeyToBoolean (GET_OPTION_KEY (options, "nullterm"), &nullTerm);                                                     \
		keyDel (tmp);                                                                                                              \
	}                                                                                                                                  \
                                                                                                                                           \
	int colorMode = CLI_COLOR_AUTO;                                                                                                    \
	if (colorMode)                                                                                                                     \
	{                                                                                                                                  \
	}; /* disable unused variable warning  */                                                                                          \
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
		keyDel (tmp);                                                                                                              \
	}                                                                                                                                  \
	char * fmtBuffer = elektraMalloc (1024);                                                                                           \
	if (fmtBuffer)                                                                                                                     \
	{                                                                                                                                  \
	}; /* disable unused variable warning  */                                                                                          \
	if (!isatty (STDOUT_FILENO))                                                                                                       \
	{                                                                                                                                  \
		colorMode = CLI_COLOR_NEVER;                                                                                               \
	}                                                                                                                                  \
                                                                                                                                           \
	int logLevel = (verbose || debug) + debug;                                                                                         \
	if (logLevel)                                                                                                                      \
	{                                                                                                                                  \
	}; /* disable unused variable warning  */

/**
 * @def RETURN
 * @brief This macro frees the fmtBuffer and returns the given value. Use only after #GET_BASIC_OPTIONS
 * @param c The value to return.
 */
#define RETURN(c)                                                                                                                          \
	elektraFree (fmtBuffer);                                                                                                           \
	return c;

/**
 * @def CLI_PRINT
 * @brief This macro prints a formatted message if the log level is at least as high as a given level.
 * @param minLogLevel The minimum log level to print the message.
 * @param fmt The format string for the message. Is setup by #GET_BASIC_OPTIONS.
 */
#define CLI_PRINT(minLogLevel, fmt, ...) cliPrint (logLevel, minLogLevel, fmt, __VA_ARGS__)

/**
 * @def CLI_ERROR_PRINT
 * @brief This macro prints a formatted error message if the log level is at least as high as a given level.
 * @param minLogLevel The minimum log level to print the message.
 * @param fmt The format string for the message. Is setup by #GET_BASIC_OPTIONS.
 */
#define CLI_ERROR_PRINT(minLogLevel, fmt, ...) cliErrorPrint (logLevel, minLogLevel, fmt, __VA_ARGS__)

/**
 * Expands a keyname if it contains a bookmark. If @name does not contain a bookmark ref a copy of @name is returned.
 *
 * @param name the keyname that might contain a bookmark, and where the expanded name should be saved
 * @param resolved will be set to true iff a bookmark was resolved successfully
 *
 * @return NULL if the bookmark could not be resolved, NULL was passed as @ks or @name
 * @return key with expanded name, has to be freed after usage
 */
Key * expandKeyName (const char * name, bool * resolved);

/**
 * Get a key name string from options and resolve bookmarks if present.
 *
 * @param rawName the keyname as it was entered by the user, may contain a bookmark
 * @param errorKey where errors should be written to, in case of: 1. can't resolve bookmark, 2. not a valid key name
 * @param verbose print more info
 * @return a pointer to the key with the resolved bookmark(if present), has to freed with keyDel
 */
Key * getKeyFromOptions (const char * rawName, Key * errorKey, bool verbose);

/**
 * Helper for printing, handles log levels
 *
 * @param logLevel the log level set by the user
 * @param minLogLevel minimum log level so the message is printed (NONE -> always print, VERBOSE -> printf if VERBOSE or DEBUG, ...)
 * @param fmt format string for printing
 * @param ...
 */
void cliPrint (int logLevel, int minLogLevel, const char * fmt, ...);

/**
 * Helper for printing errors, handles log levels
 *
 * @param logLevel the log level set by the user
 * @param minLogLevel minimum log level so the message is printed (NONE -> always print, VERBOSE -> printf if VERBOSE or DEBUG, ...)
 * @param fmt format string for printing
 * @param ...
 */
void cliErrorPrint (int logLevel, int minLogLevel, const char * fmt, ...);

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
