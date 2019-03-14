/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

// clang-format on

#include "empty.actual.h"



#include <stdlib.h>
#include <string.h>

#include <kdbhelper.h>
#include <kdbinvoke.h>

#include <elektra/conversion.h>


/**
 * Initializes an instance of Elektra for the application 'tests/script/gen/elektra/empty'.
 *
 * This can be invoked as many times as you want, however it is not a cheap operation,
 * so you should try to reuse the Elektra handle as much as possible.
 *
 * @param elektra A reference to where the Elektra instance shall be stored.
 *                Has to be disposed of with elektraClose().
 * @param error   A reference to an ElektraError pointer. Will be passed to elektraOpen().
 *
 * @retval 0  on success, @p elektra will be set, @p error will be unchanged
 * @retval -1 on error, @p elektra will be unchanged, @p error will set
 * @retval 1  specload mode, exit as soon as possible and must DO NOT write anything to stdout,
 *            @p elektra and @p error are both unchanged
 * @retval 2  help mode, '-h' or '--help' was specified call printHelpMessage and exit
 *            @p elektra and @p error are both unchanged
 *
 * @see elektraOpen
 */// 
int loadConfiguration (Elektra ** elektra, ElektraError ** error)
{
	KeySet * defaults = ksNew (0,
	KS_END);
;
	Elektra * e = elektraOpen ("tests/script/gen/elektra/empty", defaults, error);

	if (e == NULL)
	{
		return -1;
	}

	if (0 /* TODO: check if help mode */)
	{
		elektraClose (e);
		printHelpMessage ();
		return 2;
	}

	*elektra = e;
	return 0;
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
void specloadCheck (int argc, const char ** argv)
{
	if (argc != 2 || strcmp (argv[1], "--elektra-specload") != 0)
	{
		return;
	}

	KeySet * spec = ksNew (0,
	KS_END);
;

	Key * errorKey = keyNew (0, KEY_END);

	KeySet * specloadConf = ksNew (1, keyNew ("system/sendspec", KEY_END), KS_END);
	ElektraInvokeHandle * specload = elektraInvokeOpen ("specload", specloadConf, errorKey);

	int result = elektraInvoke2Args (specload, "sendspec", spec, NULL);

	elektraInvokeClose (specload, errorKey);
	keyDel (errorKey);
	ksDel (specloadConf);
	ksDel (spec);

	exit (result == ELEKTRA_PLUGIN_STATUS_SUCCESS ? EXIT_SUCCESS : EXIT_FAILURE);
}

/**
 * Outputs the help message
 */
void printHelpMessage (void)
{
	// TODO
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
// Struct accessor functions
// -------------------------




