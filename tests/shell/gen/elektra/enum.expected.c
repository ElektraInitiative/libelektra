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

#include "enum.actual.h"

#include "colors.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <kdbhelper.h>
#include <kdbinvoke.h>
#include <kdbopts.h>

#include <elektra/conversion.h>

static Key * helpKey = NULL;


/**
 * Initializes an instance of Elektra for the application '/tests/script/gen/elektra/enum'.
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
	keyNew("", KEY_META, "mountpoint", "tests_gen_elektra_enum.ini", KEY_END),
	keyNew ("/disjointed", KEY_VALUE, "black", KEY_META, "check/enum", "#__255", KEY_META, "check/enum/#0", "black", KEY_META, "check/enum/#__255", "white", KEY_META, "default", "black", KEY_META, "type", "enum", KEY_END),
	keyNew ("/existinggentype", KEY_VALUE, "cyan", KEY_META, "check/enum", "#2", KEY_META, "check/enum/#0", "cyan", KEY_META, "check/enum/#1", "magenta", KEY_META, "check/enum/#2", "yellow", KEY_META, "default", "cyan", KEY_META, "gen/enum/create", "0", KEY_META, "gen/enum/type", "ExistingColors", KEY_META, "type", "enum", KEY_END),
	keyNew ("/gentype", KEY_VALUE, "blue", KEY_META, "check/enum", "#3", KEY_META, "check/enum/#0", "none", KEY_META, "check/enum/#1", "red", KEY_META, "check/enum/#2", "green", KEY_META, "check/enum/#3", "blue", KEY_META, "default", "blue", KEY_META, "gen/enum/#0/value", "NO_VALUE", KEY_META, "gen/enum/#1/value", "1", KEY_META, "gen/enum/#2/value", "1 << 1", KEY_META, "gen/enum/#3/value", "1 << 2", KEY_META, "gen/enum/type", "Colors", KEY_META, "type", "enum", KEY_END),
	keyNew ("/gentype2", KEY_VALUE, "red", KEY_META, "check/enum", "#3", KEY_META, "check/enum/#0", "none", KEY_META, "check/enum/#1", "red", KEY_META, "check/enum/#2", "green", KEY_META, "check/enum/#3", "blue", KEY_META, "default", "red", KEY_META, "gen/enum/#0/value", "NO_VALUE", KEY_META, "gen/enum/#1/value", "1", KEY_META, "gen/enum/#2/value", "1 << 1", KEY_META, "gen/enum/#3/value", "1 << 2", KEY_META, "gen/enum/type", "Colors", KEY_META, "type", "enum", KEY_END),
	keyNew ("/myenum", KEY_VALUE, "blue", KEY_META, "check/enum", "#5", KEY_META, "check/enum/#0", "red", KEY_META, "check/enum/#1", "green", KEY_META, "check/enum/#2", "blue", KEY_META, "check/enum/#3", "blueish", KEY_META, "check/enum/#4", "brown", KEY_META, "check/enum/#5", "gray", KEY_META, "default", "blue", KEY_META, "type", "enum", KEY_END),
	KS_END);
;
	Elektra * e = elektraOpen ("/tests/script/gen/elektra/enum", defaults, error);

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
	keyNew ("spec/tests/script/gen/elektra/enum", KEY_META, "mountpoint", "tests_gen_elektra_enum.ini", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/enum/disjointed", KEY_META, "check/enum", "#__255", KEY_META, "check/enum/#0", "black", KEY_META, "check/enum/#__255", "white", KEY_META, "default", "black", KEY_META, "type", "enum", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/enum/existinggentype", KEY_META, "check/enum", "#2", KEY_META, "check/enum/#0", "cyan", KEY_META, "check/enum/#1", "magenta", KEY_META, "check/enum/#2", "yellow", KEY_META, "default", "cyan", KEY_META, "gen/enum/create", "0", KEY_META, "gen/enum/type", "ExistingColors", KEY_META, "type", "enum", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/enum/gentype", KEY_META, "check/enum", "#3", KEY_META, "check/enum/#0", "none", KEY_META, "check/enum/#1", "red", KEY_META, "check/enum/#2", "green", KEY_META, "check/enum/#3", "blue", KEY_META, "default", "blue", KEY_META, "gen/enum/#0/value", "NO_VALUE", KEY_META, "gen/enum/#1/value", "1", KEY_META, "gen/enum/#2/value", "1 << 1", KEY_META, "gen/enum/#3/value", "1 << 2", KEY_META, "gen/enum/type", "Colors", KEY_META, "type", "enum", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/enum/gentype2", KEY_META, "check/enum", "#3", KEY_META, "check/enum/#0", "none", KEY_META, "check/enum/#1", "red", KEY_META, "check/enum/#2", "green", KEY_META, "check/enum/#3", "blue", KEY_META, "default", "red", KEY_META, "gen/enum/#0/value", "NO_VALUE", KEY_META, "gen/enum/#1/value", "1", KEY_META, "gen/enum/#2/value", "1 << 1", KEY_META, "gen/enum/#3/value", "1 << 2", KEY_META, "gen/enum/type", "Colors", KEY_META, "type", "enum", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/enum/myenum", KEY_META, "check/enum", "#5", KEY_META, "check/enum/#0", "red", KEY_META, "check/enum/#1", "green", KEY_META, "check/enum/#2", "blue", KEY_META, "check/enum/#3", "blueish", KEY_META, "check/enum/#4", "brown", KEY_META, "check/enum/#5", "gray", KEY_META, "default", "blue", KEY_META, "type", "enum", KEY_END),
	KS_END);
;

	Key * parentKey = keyNew ("spec/tests/script/gen/elektra/enum", KEY_END);

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
	const Key * key = elektraFindKey (elektra, keyname, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumDisjointed) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, keyString (key)));
		return (ElektraEnumDisjointed) 0;
	}
	return result;
}

ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed)
{
	ElektraEnumDisjointed result;
	const Key * key = elektraFindArrayElementKey (elektra, keyname, index, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumDisjointed) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, keyString (key)));
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
	const Key * key = elektraFindKey (elektra, keyname, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumExistingColors) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, keyString (key)));
		return (ExistingColors) 0;
	}
	return result;
}

ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ExistingColors, EnumExistingColors)
{
	ExistingColors result;
	const Key * key = elektraFindArrayElementKey (elektra, keyname, index, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumExistingColors) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, keyString (key)));
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
	const Key * key = elektraFindKey (elektra, keyname, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumColors) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, keyString (key)));
		return (Colors) 0;
	}
	return result;
}

ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (Colors, EnumColors)
{
	Colors result;
	const Key * key = elektraFindArrayElementKey (elektra, keyname, index, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumColors) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, keyString (key)));
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
	const Key * key = elektraFindKey (elektra, keyname, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumMyenum) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, keyString (key)));
		return (ElektraEnumMyenum) 0;
	}
	return result;
}

ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumMyenum, EnumMyenum)
{
	ElektraEnumMyenum result;
	const Key * key = elektraFindArrayElementKey (elektra, keyname, index, KDB_TYPE_ENUM);
	if (!ELEKTRA_KEY_TO (EnumMyenum) (key, &result))
	{
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE_ENUM, keyname, keyString (key)));
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



