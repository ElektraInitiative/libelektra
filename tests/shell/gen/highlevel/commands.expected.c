// clang-format off


// clang-format on
/**
 * @file
 *
 * This file was automatically generated using `kdb gen highlevel`.
 * Any changes will be overwritten, when the file is regenerated.
 *
 * @copyright BSD Zero Clause License
 *
 *     Copyright (c) Elektra Initiative (https://www.libelektra.org)
 *
 *     Permission to use, copy, modify, and/or distribute this software for any
 *     purpose with or without fee is hereby granted.
 *
 *     THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 *     REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *     FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 *     INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 *     LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 *     OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 *     PERFORMANCE OF THIS SOFTWARE.
 */

#include "commands.actual.h"



#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <kdbhelper.h>
#include <kdbinvoke.h>
#include <kdbopts.h>
#include <kdbgopts.h>

#include <elektra/conversion.h>

static KeySet * embeddedSpec (void)
{
	return ksNew (14,
	keyNew ("/", KEY_META, "command", "", KEY_META, "default", "", KEY_META, "gen/command/function", "commandKdb", KEY_META, "mountpoint", "tests_gen_elektra_commands.ini", KEY_META, "type", "string", KEY_END),
	keyNew ("/dynamic/#", KEY_META, "args", "remaining", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	keyNew ("/get", KEY_META, "command", "get", KEY_META, "default", "", KEY_META, "gen/command/function", "commandKdbGet", KEY_META, "type", "string", KEY_END),
	keyNew ("/get/keyname", KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	keyNew ("/get/maxlength", KEY_META, "default", "-1", KEY_META, "opt/arg", "required", KEY_META, "opt/long", "max-length", KEY_META, "type", "long", KEY_END),
	keyNew ("/get/meta", KEY_META, "command", "meta", KEY_META, "default", "", KEY_META, "gen/command/function", "commandKdbGetMeta", KEY_META, "type", "string", KEY_END),
	keyNew ("/get/meta/keyname", KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	keyNew ("/get/meta/metaname", KEY_META, "args", "indexed", KEY_META, "args/index", "1", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	keyNew ("/get/meta/verbose", KEY_META, "default", "0", KEY_META, "opt", "v", KEY_META, "opt/arg", "none", KEY_META, "opt/long", "verbose", KEY_META, "type", "boolean", KEY_END),
	keyNew ("/get/verbose", KEY_META, "default", "0", KEY_META, "opt", "v", KEY_META, "opt/arg", "none", KEY_META, "opt/long", "verbose", KEY_META, "type", "boolean", KEY_END),
	keyNew ("/printversion", KEY_META, "default", "0", KEY_META, "opt", "v", KEY_META, "opt/arg", "none", KEY_META, "opt/long", "version", KEY_META, "type", "boolean", KEY_END),
	keyNew ("/setter", KEY_META, "command", "set", KEY_META, "default", "", KEY_META, "gen/command/function", "commandKdbSet", KEY_META, "type", "string", KEY_END),
	keyNew ("/setter/keyname", KEY_META, "args/index", "0", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	keyNew ("/setter/value", KEY_META, "args/index", "1", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	KS_END);
;
}

static const char * helpFallback = "Usage: tests_script_gen_highlevel_commands [OPTION...] [COMMAND [...]|[<dynamic>...]]\n\nOPTIONS\n  --help                      Print this help message\n  -v, --version               \n\nCOMMANDS\n  get                         \n  set                         \n\nPARAMETERS\n  dynamic...                  \n";

static int isHelpMode (int argc, const char * const * argv)
{
	for (int i = 0; i < argc; ++i)
	{
		if (strcmp (argv[i], "--help") == 0)
		{
			return 1;
		}
	}

	return 0;
}



/**
 * Initializes an instance of Elektra for the application '/tests/script/gen/highlevel/commands'.
 *
 * This can be invoked as many times as you want, however it is not a cheap operation,
 * so you should try to reuse the Elektra handle as much as possible.
 *
 * @param elektra A reference to where the Elektra instance shall be stored.
 *                Has to be disposed of with elektraClose().
 * @param error   A reference to an ElektraError pointer. Will be passed to elektraOpen().
 *
 * @retval 0  on success, @p elektra will contain a new Elektra instance coming from elektraOpen(),
 *            @p error will be unchanged
 * @retval -1 on error, @p elektra will be unchanged, @p error will be set
 * @retval 1  help mode, '--help' was specified call printHelpMessage to display
 *            the help message. @p elektra will contain a new Elektra instance. It has to be passed
 *            to printHelpMessage. You also need to elektraClose() it.
 *            @p error will be unchanged
 *
 * @see elektraOpen
 */// 
int loadConfiguration (Elektra ** elektra, 
				 int argc, const char * const * argv, const char * const * envp,
				 ElektraError ** error)
{
	KeySet * defaults = embeddedSpec ();
	

	KeySet * contract = ksNew (3,
	keyNew ("system:/elektra/contract/highlevel/check/spec/mounted", KEY_VALUE, "1", KEY_END),
	keyNew ("system:/elektra/contract/highlevel/helpmode/ignore/require", KEY_VALUE, "1", KEY_END),
	keyNew ("system:/elektra/contract/mountglobal/gopts", KEY_END),
	KS_END);
;
	Key * parentKey = keyNew ("/tests/script/gen/highlevel/commands", KEY_END);

	elektraGOptsContract (contract, argc, argv, envp, parentKey, NULL);
	

	keyDel (parentKey);

	Elektra * e = elektraOpen ("/tests/script/gen/highlevel/commands", defaults, contract, error);

	if (defaults != NULL)
	{
		ksDel (defaults);
	}

	if (contract != NULL)
	{
		ksDel (contract);
	}

	if (e == NULL)
	{
		*elektra = NULL;
		if (isHelpMode (argc, argv))
		{
			elektraErrorReset (error);
			return 1;
		}
		

		return -1;
	}

	*elektra = e;
	return elektraHelpKey (e) != NULL && strcmp (keyString (elektraHelpKey (e)), "1") == 0 ? 1 : 0;
}

/**
 * Checks whether specload mode was invoked and if so, sends the specification over stdout
 * in the format expected by specload.
 *
 * You MUST not output anything to stdout before invoking this function. Ideally invoking this
 * is the first thing you do in your main()-function.
 *
 * This function will ONLY RETURN, if specload mode was NOT invoked. Otherwise it will call `exit()`.
 *
 * @param argc pass the value of argc from main
 * @param argv pass the value of argv from main
 */
void exitForSpecload (int argc, const char * const * argv)
{
	if (argc != 2 || strcmp (argv[1], "--elektra-spec") != 0)
	{
		return;
	}

	KeySet * spec = embeddedSpec ();

	Key * parentKey = keyNew ("spec:/tests/script/gen/highlevel/commands", KEY_META, "system:/elektra/quickdump/noparent", "", KEY_END);

	KeySet * specloadConf = ksNew (1, keyNew ("system:/sendspec", KEY_END), KS_END);
	ElektraInvokeHandle * specload = elektraInvokeOpen ("specload", specloadConf, parentKey);

	int result = elektraInvoke2Args (specload, "sendspec", spec, parentKey);

	elektraInvokeClose (specload, parentKey);
	keyDel (parentKey);
	ksDel (specloadConf);
	ksDel (spec);

	exit (result == ELEKTRA_PLUGIN_STATUS_SUCCESS ? EXIT_SUCCESS : EXIT_FAILURE);
}


/**
 * Outputs the help message to stdout
 *
 * @param elektra  The Elektra instance produced by loadConfiguration.
 * @param usage	   If this is not NULL, it will be used instead of the default usage line.
 * @param prefix   If this is not NULL, it will be inserted between the usage line and the options list.
 */// 
void printHelpMessage (Elektra * elektra, const char * usage, const char * prefix)
{
	if (elektra == NULL)
	{
		printf ("%s", helpFallback);
		return;
	}

	Key * helpKey = elektraHelpKey (elektra);
	if (helpKey == NULL)
	{
		return;
	}

	char * help = elektraGetOptsHelpMessage (helpKey, usage, prefix);
	printf ("%s", help);
	elektraFree (help);
}


/**
 * Determines which sub-commands (according to `elektraGetOpts`) have been invoked and calls the associated functions in
 * order. The parameters @p elektra and @p userData will be passed through. In addition, the `terminal` parameter will be
 * set to `true` for the last command function that is called.
 *
 * @param elektra  The Elektra instance produced by loadConfiguration.
 * @param usage	   Custom applicationd defined data. Will be passed untouched to the invoked command functions. Maybe NULL.
 *
 * @return If @p elektra is NULL -1 is returned.
 *         If one of the invoked command functions returns a non-zero value, that value is returned.
 *         Otherwise the return value of the terminal command function is returned.
 */// 
int runCommands (Elektra * elektra, void * userData)
{
	if (elektra == NULL)
	{
		return -1;
	}

	KeySet * commands = ksNew (4,
				   keyNew ("/", KEY_FUNC, commandKdb, KEY_END),
				   keyNew ("/get", KEY_FUNC, commandKdbGet, KEY_END),
				   keyNew ("/get/meta", KEY_FUNC, commandKdbGetMeta, KEY_END),
				   keyNew ("/setter", KEY_FUNC, commandKdbSet, KEY_END),
				   KS_END);

	typedef int (*commandFunction) (Elektra *, kdb_boolean_t, void *);

	Key * lastCommand = keyNew ("/", KEY_END);
	const char * command = ELEKTRA_GET (String) (elektra, keyName (lastCommand) + 1);
	while (strlen (command) > 0)
	{
		Key * commandKey = ksLookup (commands, lastCommand, 0);
		const void * rawFunc = keyValue (commandKey);
		commandFunction func = *(commandFunction *) rawFunc;
		int result = func (elektra, false, userData);
		if (result != 0)
		{
			keyDel (lastCommand);
			ksDel (commands);
			return result;
		}

		keyAddBaseName (lastCommand, command);
		command = ELEKTRA_GET (String) (elektra, keyName (lastCommand) + 1);
	}

	Key * commandKey = ksLookup (commands, lastCommand, 0);
	const void * rawFunc = keyValue (commandKey);
	commandFunction func = *(commandFunction *) rawFunc;
	int result = func (elektra, true, userData);

	keyDel (lastCommand);
	ksDel (commands);
	return result;
}

// clang-format off

// clang-format on

// -------------------------
// Enum conversion functions
// -------------------------



// -------------------------
// Enum accessor functions
// -------------------------




// clang-format off

// clang-format on

// -------------------------
// Union accessor functions
// -------------------------




// clang-format off

// clang-format on

// -------------------------
// Struct accessor functions
// -------------------------



