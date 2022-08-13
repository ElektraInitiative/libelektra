/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef COMMAND_H
#define COMMAND_H

#include <kdb.h>

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
			     KEY_META, "opt", "n", KEY_META, "opt/long", "no-newline", KEY_META, "opt/arg", "none", KEY_END));


#define GET_OPT(options, key) keyString (ksLookupByName (options, "proc:" key, 0))
#define OR(value, def)                                                                                                                     \
	void * tmp = value;                                                                                                                \
	tmp == NULL ? (def) : tmp
#define GET_ERR(errorKey) keyString (keyGetMeta (errorKey, "error/reason"))
#define COMMAND_BASE_KEY(name) CLI_BASE_KEY "/" name
#define COMMAND_SPEC_KEY(name) "spec:" COMMAND_BASE_KEY (name)

typedef struct command
{
	const char * name;
	void (*addSpec) (KeySet * spec);
	int (*exec) (KeySet * options, Key * errorKey);
	int (*checkArgs) (KeySet * options, Key * errorKey);
} command;


#endif // COMMAND_H
