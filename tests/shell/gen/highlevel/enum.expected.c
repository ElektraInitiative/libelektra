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

#include "enum.actual.h"

#include "colors.h"

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
	return elektraKeysetNew (6,
	elektraKeyNew ("/", ELEKTRA_KEY_META, "mountpoint", "tests_gen_elektra_enum.ini", ELEKTRA_KEY_END),
	elektraKeyNew ("/disjointed", ELEKTRA_KEY_META, "check/enum", "#__255", ELEKTRA_KEY_META, "check/enum/#0", "black", ELEKTRA_KEY_META, "check/enum/#__255", "white", ELEKTRA_KEY_META, "default", "black", ELEKTRA_KEY_META, "type", "enum", ELEKTRA_KEY_END),
	elektraKeyNew ("/existinggentype", ELEKTRA_KEY_META, "check/enum", "#2", ELEKTRA_KEY_META, "check/enum/#0", "cyan", ELEKTRA_KEY_META, "check/enum/#1", "magenta", ELEKTRA_KEY_META, "check/enum/#2", "yellow", ELEKTRA_KEY_META, "default", "cyan", ELEKTRA_KEY_META, "gen/enum/create", "0", ELEKTRA_KEY_META, "gen/enum/type", "ExistingColors", ELEKTRA_KEY_META, "type", "enum", ELEKTRA_KEY_END),
	elektraKeyNew ("/gentype", ELEKTRA_KEY_META, "check/enum", "#3", ELEKTRA_KEY_META, "check/enum/#0", "none", ELEKTRA_KEY_META, "check/enum/#1", "red", ELEKTRA_KEY_META, "check/enum/#2", "green", ELEKTRA_KEY_META, "check/enum/#3", "blue", ELEKTRA_KEY_META, "default", "blue", ELEKTRA_KEY_META, "gen/enum/#0/value", "NO_VALUE", ELEKTRA_KEY_META, "gen/enum/#1/value", "1", ELEKTRA_KEY_META, "gen/enum/#2/value", "1 << 1", ELEKTRA_KEY_META, "gen/enum/#3/value", "1 << 2", ELEKTRA_KEY_META, "gen/enum/type", "Colors", ELEKTRA_KEY_META, "type", "enum", ELEKTRA_KEY_END),
	elektraKeyNew ("/gentype2", ELEKTRA_KEY_META, "check/enum", "#3", ELEKTRA_KEY_META, "check/enum/#0", "none", ELEKTRA_KEY_META, "check/enum/#1", "red", ELEKTRA_KEY_META, "check/enum/#2", "green", ELEKTRA_KEY_META, "check/enum/#3", "blue", ELEKTRA_KEY_META, "default", "red", ELEKTRA_KEY_META, "gen/enum/#0/value", "NO_VALUE", ELEKTRA_KEY_META, "gen/enum/#1/value", "1", ELEKTRA_KEY_META, "gen/enum/#2/value", "1 << 1", ELEKTRA_KEY_META, "gen/enum/#3/value", "1 << 2", ELEKTRA_KEY_META, "gen/enum/type", "Colors", ELEKTRA_KEY_META, "type", "enum", ELEKTRA_KEY_END),
	elektraKeyNew ("/myenum", ELEKTRA_KEY_META, "check/enum", "#5", ELEKTRA_KEY_META, "check/enum/#0", "red", ELEKTRA_KEY_META, "check/enum/#1", "green", ELEKTRA_KEY_META, "check/enum/#2", "blue", ELEKTRA_KEY_META, "check/enum/#3", "blueish", ELEKTRA_KEY_META, "check/enum/#4", "brown", ELEKTRA_KEY_META, "check/enum/#5", "gray", ELEKTRA_KEY_META, "default", "blue", ELEKTRA_KEY_META, "type", "enum", ELEKTRA_KEY_END),
	ELEKTRA_KS_END);
;
}

static const char * helpFallback = "Usage: tests_script_gen_highlevel_enum [OPTION...]\n\nOPTIONS\n  --help                      Print this help message\n";

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
 * Initializes an instance of Elektra for the application '/tests/script/gen/highlevel/enum'.
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
	

	ElektraKeyset * contract = elektraKeysetNew (4,
	elektraKeyNew ("system:/elektra/contract/highlevel/check/spec/mounted", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/contract/highlevel/check/spec/token", ELEKTRA_KEY_VALUE, "fbb054456ff70fde7e4f184ec86eb130a99b4b3712d0c9d496e78bd262fb8c8d", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/contract/highlevel/helpmode/ignore/require", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/contract/mountglobal/gopts", ELEKTRA_KEY_END),
	ELEKTRA_KS_END);
;
	ElektraKey * parentKey = elektraKeyNew ("/tests/script/gen/highlevel/enum", ELEKTRA_KEY_END);

	elektraGOptsContract (contract, argc, argv, envp, parentKey, NULL);
	

	elektraKeyDel (parentKey);

	Elektra * e = elektraOpen ("/tests/script/gen/highlevel/enum", defaults, contract, error);

	if (defaults != NULL)
	{
		elektraKeysetDel (defaults);
	}

	if (contract != NULL)
	{
		elektraKeysetDel (contract);
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
	return elektraHelpKey (e) != NULL && strcmp (elektraKeyString (elektraHelpKey (e)), "1") == 0 ? 1 : 0;
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

	ElektraKey * parentKey = elektraKeyNew ("spec:/tests/script/gen/highlevel/enum", ELEKTRA_KEY_META, "system:/elektra/quickdump/noparent", "", ELEKTRA_KEY_END);

	ElektraKeyset * specloadConf = elektraKeysetNew (1, elektraKeyNew ("system:/sendspec", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraInvokeHandle * specload = elektraInvokeOpen ("specload", specloadConf, parentKey);

	int result = elektraInvoke2Args (specload, "sendspec", spec, parentKey);

	elektraInvokeClose (specload, parentKey);
	elektraKeyDel (parentKey);
	elektraKeysetDel (specloadConf);
	elektraKeysetDel (spec);

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

ELEKTRA_KEY_TO_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed)
{
	const char * string;
	if (!elektraKeyToString (key, &string) || strlen (string) == 0)
	{
		return 0;
	}

	switch (string[0])
{
case 'b':
*variable = ELEKTRA_ENUM_DISJOINTED_BLACK;
return 1;
case 'w':
*variable = ELEKTRA_ENUM_DISJOINTED_WHITE;
return 1;
}

	

	return 0;
}

ELEKTRA_TO_STRING_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed)
{
	switch (value)
	{
	case ELEKTRA_ENUM_DISJOINTED_BLACK:
		return elektraStrDup ("black");
	case ELEKTRA_ENUM_DISJOINTED_WHITE:
		return elektraStrDup ("white");
	}

	// should be unreachable
	return elektraStrDup ("");
}

ELEKTRA_TO_CONST_STRING_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed)
{
	switch (value)
	{
	case ELEKTRA_ENUM_DISJOINTED_BLACK:
		return "black";
	case ELEKTRA_ENUM_DISJOINTED_WHITE:
		return "white";
	}

	// should be unreachable
	return "";
}
ELEKTRA_KEY_TO_SIGNATURE (ExistingColors, EnumExistingColors)
{
	const char * string;
	if (!elektraKeyToString (key, &string) || strlen (string) == 0)
	{
		return 0;
	}

	switch (string[0])
{
case 'c':
*variable = EXISTING_COLORS_CYAN;
return 1;
case 'm':
*variable = EXISTING_COLORS_MAGENTA;
return 1;
case 'y':
*variable = EXISTING_COLORS_YELLOW;
return 1;
}

	

	return 0;
}

ELEKTRA_TO_STRING_SIGNATURE (ExistingColors, EnumExistingColors)
{
	switch (value)
	{
	case EXISTING_COLORS_CYAN:
		return elektraStrDup ("cyan");
	case EXISTING_COLORS_MAGENTA:
		return elektraStrDup ("magenta");
	case EXISTING_COLORS_YELLOW:
		return elektraStrDup ("yellow");
	}

	// should be unreachable
	return elektraStrDup ("");
}

ELEKTRA_TO_CONST_STRING_SIGNATURE (ExistingColors, EnumExistingColors)
{
	switch (value)
	{
	case EXISTING_COLORS_CYAN:
		return "cyan";
	case EXISTING_COLORS_MAGENTA:
		return "magenta";
	case EXISTING_COLORS_YELLOW:
		return "yellow";
	}

	// should be unreachable
	return "";
}
ELEKTRA_KEY_TO_SIGNATURE (Colors, EnumColors)
{
	const char * string;
	if (!elektraKeyToString (key, &string) || strlen (string) == 0)
	{
		return 0;
	}

	switch (string[0])
{
case 'b':
*variable = COLORS_BLUE;
return 1;
case 'g':
*variable = COLORS_GREEN;
return 1;
case 'n':
*variable = COLORS_NONE;
return 1;
case 'r':
*variable = COLORS_RED;
return 1;
}

	

	return 0;
}

ELEKTRA_TO_STRING_SIGNATURE (Colors, EnumColors)
{
	switch (value)
	{
	case COLORS_NONE:
		return elektraStrDup ("none");
	case COLORS_RED:
		return elektraStrDup ("red");
	case COLORS_GREEN:
		return elektraStrDup ("green");
	case COLORS_BLUE:
		return elektraStrDup ("blue");
	}

	// should be unreachable
	return elektraStrDup ("");
}

ELEKTRA_TO_CONST_STRING_SIGNATURE (Colors, EnumColors)
{
	switch (value)
	{
	case COLORS_NONE:
		return "none";
	case COLORS_RED:
		return "red";
	case COLORS_GREEN:
		return "green";
	case COLORS_BLUE:
		return "blue";
	}

	// should be unreachable
	return "";
}
ELEKTRA_KEY_TO_SIGNATURE (ElektraEnumMyenum, EnumMyenum)
{
	const char * string;
	if (!elektraKeyToString (key, &string) || strlen (string) == 0)
	{
		return 0;
	}

	
	if (strcmp (string, "red") == 0)
	{
		*variable = ELEKTRA_ENUM_MYENUM_RED;
		return 1;
	}
	if (strcmp (string, "green") == 0)
	{
		*variable = ELEKTRA_ENUM_MYENUM_GREEN;
		return 1;
	}
	if (strcmp (string, "blue") == 0)
	{
		*variable = ELEKTRA_ENUM_MYENUM_BLUE;
		return 1;
	}
	if (strcmp (string, "blueish") == 0)
	{
		*variable = ELEKTRA_ENUM_MYENUM_BLUEISH;
		return 1;
	}
	if (strcmp (string, "brown") == 0)
	{
		*variable = ELEKTRA_ENUM_MYENUM_BROWN;
		return 1;
	}
	if (strcmp (string, "gray") == 0)
	{
		*variable = ELEKTRA_ENUM_MYENUM_GRAY;
		return 1;
	}

	return 0;
}

ELEKTRA_TO_STRING_SIGNATURE (ElektraEnumMyenum, EnumMyenum)
{
	switch (value)
	{
	case ELEKTRA_ENUM_MYENUM_RED:
		return elektraStrDup ("red");
	case ELEKTRA_ENUM_MYENUM_GREEN:
		return elektraStrDup ("green");
	case ELEKTRA_ENUM_MYENUM_BLUE:
		return elektraStrDup ("blue");
	case ELEKTRA_ENUM_MYENUM_BLUEISH:
		return elektraStrDup ("blueish");
	case ELEKTRA_ENUM_MYENUM_BROWN:
		return elektraStrDup ("brown");
	case ELEKTRA_ENUM_MYENUM_GRAY:
		return elektraStrDup ("gray");
	}

	// should be unreachable
	return elektraStrDup ("");
}

ELEKTRA_TO_CONST_STRING_SIGNATURE (ElektraEnumMyenum, EnumMyenum)
{
	switch (value)
	{
	case ELEKTRA_ENUM_MYENUM_RED:
		return "red";
	case ELEKTRA_ENUM_MYENUM_GREEN:
		return "green";
	case ELEKTRA_ENUM_MYENUM_BLUE:
		return "blue";
	case ELEKTRA_ENUM_MYENUM_BLUEISH:
		return "blueish";
	case ELEKTRA_ENUM_MYENUM_BROWN:
		return "brown";
	case ELEKTRA_ENUM_MYENUM_GRAY:
		return "gray";
	}

	// should be unreachable
	return "";
}

// -------------------------
// Enum accessor functions
// -------------------------

ELEKTRA_GET_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed)
{
	ElektraEnumDisjointed result;
	const ElektraKey * key = elektraFindKey (elektra, keyname, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumDisjointed) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, elektraKeyString (key)));
		return (ElektraEnumDisjointed) 0;
	}
	return result;
}

ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed)
{
	ElektraEnumDisjointed result;
	const ElektraKey * key = elektraFindArrayElementKey (elektra, keyname, index, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumDisjointed) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, elektraKeyString (key)));
		return (ElektraEnumDisjointed) 0;
	}
	return result;
}

ELEKTRA_SET_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed)
{
	char * string = ELEKTRA_TO_STRING (EnumDisjointed) (value);
	if (string == 0)
	{
		*error = elektraErrorConversionToString (KDB_TYPE_ENUM, keyname);
		return;
	}
	elektraSetRawString (elektra, keyname, string, KDB_TYPE_ENUM, error);
	elektraFree (string);
}

ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed)
{
	char * string = ELEKTRA_TO_STRING (EnumDisjointed) (value);
	if (string == 0)
	{
		*error = elektraErrorConversionToString (KDB_TYPE_ENUM, keyname);
		return;
	}
	elektraSetRawStringArrayElement (elektra, keyname, index, string, KDB_TYPE_ENUM, error);
	elektraFree (string);
}
ELEKTRA_GET_SIGNATURE (ExistingColors, EnumExistingColors)
{
	ExistingColors result;
	const ElektraKey * key = elektraFindKey (elektra, keyname, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumExistingColors) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, elektraKeyString (key)));
		return (ExistingColors) 0;
	}
	return result;
}

ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ExistingColors, EnumExistingColors)
{
	ExistingColors result;
	const ElektraKey * key = elektraFindArrayElementKey (elektra, keyname, index, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumExistingColors) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, elektraKeyString (key)));
		return (ExistingColors) 0;
	}
	return result;
}

ELEKTRA_SET_SIGNATURE (ExistingColors, EnumExistingColors)
{
	char * string = ELEKTRA_TO_STRING (EnumExistingColors) (value);
	if (string == 0)
	{
		*error = elektraErrorConversionToString (KDB_TYPE_ENUM, keyname);
		return;
	}
	elektraSetRawString (elektra, keyname, string, KDB_TYPE_ENUM, error);
	elektraFree (string);
}

ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (ExistingColors, EnumExistingColors)
{
	char * string = ELEKTRA_TO_STRING (EnumExistingColors) (value);
	if (string == 0)
	{
		*error = elektraErrorConversionToString (KDB_TYPE_ENUM, keyname);
		return;
	}
	elektraSetRawStringArrayElement (elektra, keyname, index, string, KDB_TYPE_ENUM, error);
	elektraFree (string);
}
ELEKTRA_GET_SIGNATURE (Colors, EnumColors)
{
	Colors result;
	const ElektraKey * key = elektraFindKey (elektra, keyname, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumColors) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, elektraKeyString (key)));
		return (Colors) 0;
	}
	return result;
}

ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (Colors, EnumColors)
{
	Colors result;
	const ElektraKey * key = elektraFindArrayElementKey (elektra, keyname, index, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumColors) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, elektraKeyString (key)));
		return (Colors) 0;
	}
	return result;
}

ELEKTRA_SET_SIGNATURE (Colors, EnumColors)
{
	char * string = ELEKTRA_TO_STRING (EnumColors) (value);
	if (string == 0)
	{
		*error = elektraErrorConversionToString (KDB_TYPE_ENUM, keyname);
		return;
	}
	elektraSetRawString (elektra, keyname, string, KDB_TYPE_ENUM, error);
	elektraFree (string);
}

ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (Colors, EnumColors)
{
	char * string = ELEKTRA_TO_STRING (EnumColors) (value);
	if (string == 0)
	{
		*error = elektraErrorConversionToString (KDB_TYPE_ENUM, keyname);
		return;
	}
	elektraSetRawStringArrayElement (elektra, keyname, index, string, KDB_TYPE_ENUM, error);
	elektraFree (string);
}
ELEKTRA_GET_SIGNATURE (ElektraEnumMyenum, EnumMyenum)
{
	ElektraEnumMyenum result;
	const ElektraKey * key = elektraFindKey (elektra, keyname, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumMyenum) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, elektraKeyString (key)));
		return (ElektraEnumMyenum) 0;
	}
	return result;
}

ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumMyenum, EnumMyenum)
{
	ElektraEnumMyenum result;
	const ElektraKey * key = elektraFindArrayElementKey (elektra, keyname, index, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumMyenum) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, elektraKeyString (key)));
		return (ElektraEnumMyenum) 0;
	}
	return result;
}

ELEKTRA_SET_SIGNATURE (ElektraEnumMyenum, EnumMyenum)
{
	char * string = ELEKTRA_TO_STRING (EnumMyenum) (value);
	if (string == 0)
	{
		*error = elektraErrorConversionToString (KDB_TYPE_ENUM, keyname);
		return;
	}
	elektraSetRawString (elektra, keyname, string, KDB_TYPE_ENUM, error);
	elektraFree (string);
}

ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumMyenum, EnumMyenum)
{
	char * string = ELEKTRA_TO_STRING (EnumMyenum) (value);
	if (string == 0)
	{
		*error = elektraErrorConversionToString (KDB_TYPE_ENUM, keyname);
		return;
	}
	elektraSetRawStringArrayElement (elektra, keyname, index, string, KDB_TYPE_ENUM, error);
	elektraFree (string);
}


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



