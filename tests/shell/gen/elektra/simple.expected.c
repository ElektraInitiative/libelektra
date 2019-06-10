/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

// clang-format on

#include "simple.actual.h"



#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <kdbhelper.h>
#include <kdbinvoke.h>
#include <kdbopts.h>

#include <elektra/conversion.h>

static Key * helpKey = NULL;


/**
 * Initializes an instance of Elektra for the application '/tests/script/gen/elektra/simple'.
 *
 * This can be invoked as many times as you want, however it is not a cheap operation,
 * so you should try to reuse the Elektra handle as much as possible.
 *
 * @param elektra A reference to where the Elektra instance shall be stored.
 *                Has to be disposed of with elektraClose().
 * @param error   A reference to an ElektraError pointer. Will be passed to elektraOpen().
 *
 * @retval 0  on success, @p elektra will be set, @p error will be unchanged
 * @retval -1 on error, @p elektra will be unchanged, @p error will be set
 * @retval 1  specload mode, exit as soon as possible and must DO NOT write anything to stdout,
 *            @p elektra and @p error are both unchanged
 * @retval 2  help mode, '-h' or '--help' was specified call printHelpMessage and exit
 *            @p elektra and @p error are both unchanged
 *            IMPORTANT: there will be memory leaks, if you don't call printHelpMessage !!
 *
 * @see elektraOpen
 */// 
int loadConfiguration (Elektra ** elektra, ElektraError ** error)
{
	KeySet * defaults = ksNew (5,
	keyNew ("spec/tests/script/gen/elektra/simple/mydouble", KEY_META, "default", "0.0", KEY_META, "type", "double",
	KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/myfloatarray/#", KEY_META, "default", "0.0", KEY_META, "type", "float",
	KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/myint", KEY_META, "default", "0", KEY_META, "type", "long", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/mystring", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/print", KEY_META, "default", "0", KEY_META, "type", "boolean", KEY_END),
	KS_END);
;
	Elektra * e = elektraOpen ("/tests/script/gen/elektra/simple", defaults, error);

	if (e == NULL)
	{
		return -1;
	}

	KeySet * contract = ksNew (1,
	keyNew ("system/plugins/global/gopts", KEY_VALUE, "mounted", KEY_END),
	KS_END);
;

	ElektraError * err = NULL;
	elektraEnsure (e, contract, &err);

	if (err != NULL)
	{
		*error = err;
		return -1;
	}

	helpKey = elektraHelpKey (e);
	if (helpKey != NULL)
	{
		elektraClose (e);
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
	if (argc != 2 || strcmp (argv[1], "--elektra-spec") != 0)
	{
		return;
	}

	KeySet * spec = ksNew (5,
	keyNew ("spec/tests/script/gen/elektra/simple/mydouble", KEY_META, "default", "0.0", KEY_META, "type", "double",
	KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/myfloatarray/#", KEY_META, "default", "0.0", KEY_META, "type", "float",
	KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/myint", KEY_META, "default", "0", KEY_META, "type", "long", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/mystring", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/print", KEY_META, "default", "0", KEY_META, "type", "boolean", KEY_END),
	KS_END);
;

	Key * parentKey = keyNew ("spec/tests/script/gen/elektra/simple", KEY_END);

	KeySet * specloadConf = ksNew (1, keyNew ("system/sendspec", KEY_END), KS_END);
	ElektraInvokeHandle * specload = elektraInvokeOpen ("specload", specloadConf, parentKey);

	int result = elektraInvoke2Args (specload, "sendspec", spec, parentKey);

	elektraInvokeClose (specload, parentKey);
	keyDel (parentKey);
	ksDel (specloadConf);
	ksDel (spec);

	exit (result == ELEKTRA_PLUGIN_STATUS_SUCCESS ? EXIT_SUCCESS : EXIT_FAILURE);
}

/**
 * Extracts the help message from the @p errorKey used in elektraGetOpts().
 *
 * @param errorKey The same Key as passed to elektraGetOpts() as errorKey.
 * @param usage	   If this is not NULL, it will be used instead of the default usage line.
 * @param prefix   If this is not NULL, it will be inserted between the usage line and the options list.
 *
 * @return The full help message extracted from @p errorKey, or NULL if no help message was found.
 * The returned string has to be freed with elektraFree().
 */

/**
 * Outputs the help message to stdout
 *
 * @param usage	   If this is not NULL, it will be used instead of the default usage line.
 * @param prefix   If this is not NULL, it will be inserted between the usage line and the options list.
 */
void printHelpMessage (const char * usage, const char * prefix)
{
	if (helpKey == NULL)
	{
		return;
	}

	char * help = elektraGetOptsHelpMessage (helpKey, usage, prefix);
	printf ("%s", help);
	elektraFree (help);
	keyDel (helpKey);
	helpKey = NULL;
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



