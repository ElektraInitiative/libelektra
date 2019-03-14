/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

// clang-format on

#include "enum.actual.h"

#include "colors.h"

#include <stdlib.h>
#include <string.h>

#include <kdbhelper.h>
#include <kdbinvoke.h>

#include <elektra/conversion.h>


/**
 * Initializes an instance of Elektra for the application 'tests/script/gen/elektra/enum'.
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
	KeySet * defaults = ksNew (5,
	keyNew ("spec/tests/script/gen/elektra/enum/disjointed", KEY_META, "check/enum", "#__255", KEY_META, "check/enum/#0",
	"black", KEY_META, "check/enum/#__255", "white", KEY_META, "default", "black", KEY_META, "type", "enum", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/enum/existinggentype", KEY_META, "check/enum", "#2", KEY_META, "check/enum/#0",
	"cyan", KEY_META, "check/enum/#1", "magenta", KEY_META, "check/enum/#2", "yellow", KEY_META, "default", "cyan",
	KEY_META, "gen/enum/create", "0", KEY_META, "gen/enum/type", "ExistingColors", KEY_META, "type", "enum", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/enum/gentype", KEY_META, "check/enum", "#3", KEY_META, "check/enum/#0", "none",
	KEY_META, "check/enum/#0/value", "NO_VALUE", KEY_META, "check/enum/#1", "red", KEY_META, "check/enum/#1/value", "1",
	KEY_META, "check/enum/#2", "green", KEY_META, "check/enum/#2/value", "1 << 1", KEY_META, "check/enum/#3", "blue",
	KEY_META, "check/enum/#3/value", "1 << 2", KEY_META, "default", "blue", KEY_META, "gen/enum/type", "Colors", KEY_META,
	"type", "enum", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/enum/gentype2", KEY_META, "check/enum", "#3", KEY_META, "check/enum/#0",
	"none", KEY_META, "check/enum/#0/value", "NO_VALUE", KEY_META, "check/enum/#1", "red", KEY_META,
	"check/enum/#1/value", "1", KEY_META, "check/enum/#2", "green", KEY_META, "check/enum/#2/value", "1 << 1", KEY_META,
	"check/enum/#3", "blue", KEY_META, "check/enum/#3/value", "1 << 2", KEY_META, "default", "red", KEY_META,
	"gen/enum/type", "Colors", KEY_META, "type", "enum", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/enum/myenum", KEY_META, "check/enum", "#5", KEY_META, "check/enum/#0", "red",
	KEY_META, "check/enum/#1", "green", KEY_META, "check/enum/#2", "blue", KEY_META, "check/enum/#3", "blueish", KEY_META,
	"check/enum/#4", "brown", KEY_META, "check/enum/#5", "gray", KEY_META, "default", "blue", KEY_META, "type", "enum",
	KEY_END),
	KS_END);
;
	Elektra * e = elektraOpen ("tests/script/gen/elektra/enum", defaults, error);

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

	KeySet * spec = ksNew (5,
	keyNew ("spec/tests/script/gen/elektra/enum/disjointed", KEY_META, "check/enum", "#__255", KEY_META, "check/enum/#0",
	"black", KEY_META, "check/enum/#__255", "white", KEY_META, "default", "black", KEY_META, "type", "enum", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/enum/existinggentype", KEY_META, "check/enum", "#2", KEY_META, "check/enum/#0",
	"cyan", KEY_META, "check/enum/#1", "magenta", KEY_META, "check/enum/#2", "yellow", KEY_META, "default", "cyan",
	KEY_META, "gen/enum/create", "0", KEY_META, "gen/enum/type", "ExistingColors", KEY_META, "type", "enum", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/enum/gentype", KEY_META, "check/enum", "#3", KEY_META, "check/enum/#0", "none",
	KEY_META, "check/enum/#0/value", "NO_VALUE", KEY_META, "check/enum/#1", "red", KEY_META, "check/enum/#1/value", "1",
	KEY_META, "check/enum/#2", "green", KEY_META, "check/enum/#2/value", "1 << 1", KEY_META, "check/enum/#3", "blue",
	KEY_META, "check/enum/#3/value", "1 << 2", KEY_META, "default", "blue", KEY_META, "gen/enum/type", "Colors", KEY_META,
	"type", "enum", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/enum/gentype2", KEY_META, "check/enum", "#3", KEY_META, "check/enum/#0",
	"none", KEY_META, "check/enum/#0/value", "NO_VALUE", KEY_META, "check/enum/#1", "red", KEY_META,
	"check/enum/#1/value", "1", KEY_META, "check/enum/#2", "green", KEY_META, "check/enum/#2/value", "1 << 1", KEY_META,
	"check/enum/#3", "blue", KEY_META, "check/enum/#3/value", "1 << 2", KEY_META, "default", "red", KEY_META,
	"gen/enum/type", "Colors", KEY_META, "type", "enum", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/enum/myenum", KEY_META, "check/enum", "#5", KEY_META, "check/enum/#0", "red",
	KEY_META, "check/enum/#1", "green", KEY_META, "check/enum/#2", "blue", KEY_META, "check/enum/#3", "blueish", KEY_META,
	"check/enum/#4", "brown", KEY_META, "check/enum/#5", "gray", KEY_META, "default", "blue", KEY_META, "type", "enum",
	KEY_END),
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

ELEKTRA_KEY_TO_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed)
{
	const char * string;
	if (!elektraKeyToString (key, &string) || strlen (string) == 0)
	{
		return 0;
	}

	switch (string[0])
{
case 'w':
return ELEKTRA_ENUM_DISJOINTED_WHITE;
case 'b':
return ELEKTRA_ENUM_DISJOINTED_BLACK;
}

	

	return 0;
}

ELEKTRA_TO_STRING_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed)
{
	switch (value)
	{
	case ELEKTRA_ENUM_DISJOINTED_BLACK:
		return "black";
	case ELEKTRA_ENUM_DISJOINTED_WHITE:
		return "white";
	}
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
case 'r':
return COLORS_RED;
case 'b':
return COLORS_BLUE;
case 'n':
return COLORS_NONE;
case 'g':
return COLORS_GREEN;
}

	

	return 0;
}

ELEKTRA_TO_STRING_SIGNATURE (Colors, EnumColors)
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
}
ELEKTRA_KEY_TO_SIGNATURE (ElektraEnumMyenum, EnumMyenum)
{
	const char * string;
	if (!elektraKeyToString (key, &string) || strlen (string) == 0)
	{
		return 0;
	}

	switch (string[0])
{
case 'r':
return ELEKTRA_ENUM_MYENUM_RED;
case 'b':
switch (string[1])
{
case 'r':
return ELEKTRA_ENUM_MYENUM_BROWN;
case 'l':
switch (string[2])
{
case 'u':
switch (string[3])
{
case 'e':
switch (string[4])
{
case 'i':
return ELEKTRA_ENUM_MYENUM_BLUEISH;
}
return ELEKTRA_ENUM_MYENUM_BLUE;
}
break;
}
break;
}
break;
case 'g':
switch (string[1])
{
case 'r':
switch (string[2])
{
case 'e':
return ELEKTRA_ENUM_MYENUM_GREEN;
case 'a':
return ELEKTRA_ENUM_MYENUM_GRAY;
}
break;
}
break;
}

	

	return 0;
}

ELEKTRA_TO_STRING_SIGNATURE (ElektraEnumMyenum, EnumMyenum)
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
// Struct accessor functions
// -------------------------




