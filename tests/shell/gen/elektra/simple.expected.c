// clang-format off


// clang-format on
/**
 * @file
 *
 * This file was automatically generated using `kdb gen elektra`.
 * Any changes will be overwritten, when the file is regenerated.
 *
 * @copyright BSD Zero Clause License
 *
 *     Copyright (C) 2019 Elektra Initiative (https://libelektra.org)
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
	KeySet * defaults = ksNew (6,
	keyNew("", KEY_META, "mountpoint", "tests_gen_elektra_simple.ini", KEY_END),
	keyNew ("/mydouble", KEY_VALUE, "0.0", KEY_META, "default", "0.0", KEY_META, "type", "double", KEY_END),
	keyNew ("/myfloatarray/#", KEY_VALUE, "1.1", KEY_META, "default", "1.1", KEY_META, "type", "float", KEY_END),
	keyNew ("/myint", KEY_VALUE, "0", KEY_META, "default", "0", KEY_META, "type", "long", KEY_END),
	keyNew ("/mystring", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	keyNew ("/print", KEY_VALUE, "0", KEY_META, "default", "0", KEY_META, "type", "boolean", KEY_END),
	KS_END);
;
	Elektra * e = elektraOpen ("/tests/script/gen/elektra/simple", defaults, error);

	if (e == NULL)
	{
		return -1;
	}

	KeySet * contract = ksNew (1,
	keyNew ("system/elektra/ensure/plugins/global/gopts", KEY_VALUE, "mounted", KEY_END),
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

	KeySet * spec = ksNew (6,
	keyNew ("spec/tests/script/gen/elektra/simple", KEY_META, "mountpoint", "tests_gen_elektra_simple.ini", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/mydouble", KEY_META, "default", "0.0", KEY_META, "type", "double", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/myfloatarray/#", KEY_META, "default", "1.1", KEY_META, "type", "float", KEY_END),
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
// Union accessor functions
// -------------------------




// clang-format off

// clang-format on

// -------------------------
// Struct accessor functions
// -------------------------



