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

#include "notype.actual.h"



#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <kdbhelper.h>
#include <kdbinvoke.h>
#include <kdbopts.h>
#include <kdbgopts.h>

#include <elektra/conversion.h>

static ElektraKeyset * embeddedSpec (void)
{
	return ksNew (2,
	keyNew ("/", ELEKTRA_KEY_META, "mountpoint", "tests_gen_elektra_notype.ini", ELEKTRA_KEY_END),
	keyNew ("/notype", ELEKTRA_KEY_META, "default", "2", ELEKTRA_KEY_END),
	ELEKTRA_KS_END);
;
}

static const char * helpFallback = "Usage: tests_script_gen_highlevel_notype [OPTION...]\n\nOPTIONS\n  --help                      Print this help message\n";

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
 * Initializes an instance of Elektra for the application '/tests/script/gen/highlevel/notype'.
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
	ElektraKeyset * defaults = embeddedSpec ();
	

	ElektraKeyset * contract = ksNew (4,
	keyNew ("system:/elektra/contract/highlevel/check/spec/mounted", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
	keyNew ("system:/elektra/contract/highlevel/check/spec/token", ELEKTRA_KEY_VALUE, "1cbc70fee5ca4d36d9dbaf6b37e5595f8d12ad3fa39a6778b512bd2e4b3321bf", ELEKTRA_KEY_END),
	keyNew ("system:/elektra/contract/highlevel/helpmode/ignore/require", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
	keyNew ("system:/elektra/contract/mountglobal/gopts", ELEKTRA_KEY_END),
	ELEKTRA_KS_END);
;
	ElektraKey * parentKey = keyNew ("/tests/script/gen/highlevel/notype", ELEKTRA_KEY_END);

	elektraGOptsContract (contract, argc, argv, envp, parentKey, NULL);
	

	keyDel (parentKey);

	Elektra * e = elektraOpen ("/tests/script/gen/highlevel/notype", defaults, contract, error);

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

	ElektraKeyset * spec = embeddedSpec ();

	ElektraKey * parentKey = keyNew ("spec:/tests/script/gen/highlevel/notype", ELEKTRA_KEY_META, "system:/elektra/quickdump/noparent", "", ELEKTRA_KEY_END);

	ElektraKeyset * specloadConf = ksNew (1, keyNew ("system:/sendspec", ELEKTRA_KEY_END), ELEKTRA_KS_END);
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

	ElektraKey * helpKey = elektraHelpKey (elektra);
	if (helpKey == NULL)
	{
		return;
	}

	char * help = elektraGetOptsHelpMessage (helpKey, usage, prefix);
	printf ("%s", help);
	elektraFree (help);
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



