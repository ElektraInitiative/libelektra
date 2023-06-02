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

#include "struct.actual.h"



#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <elektra/kdb/contracts/gopts.h>
#include <elektra/opts.h>
#include <elektra/plugin/invoke.h>
#include <elektra/type/conversion.h>
#include <elektra/utility/alloc.h>
#include <elektra/utility/array.h>
#include <elektra/utility/format.h>

static KeySet * embeddedSpec (void)
{
	return ksNew (15,
	keyNew ("/", KEY_META, "mountpoint", "tests_gen_elektra_struct.ini", KEY_END),
	keyNew ("/myotherstruct", KEY_META, "check/type", "any", KEY_META, "default", "", KEY_META, "gen/struct/depth", "2", KEY_META, "type", "struct", KEY_END),
	keyNew ("/myotherstruct/x", KEY_META, "default", "4", KEY_META, "type", "long", KEY_END),
	keyNew ("/myotherstruct/x/y", KEY_META, "default", "6", KEY_META, "type", "long", KEY_END),
	keyNew ("/mystruct", KEY_META, "check/type", "any", KEY_META, "default", "", KEY_META, "type", "struct", KEY_END),
	keyNew ("/mystruct/a", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	keyNew ("/mystruct/b", KEY_META, "default", "8", KEY_META, "type", "long", KEY_END),
	keyNew ("/people", KEY_META, "check/reference", "single", KEY_META, "check/reference/restrict", "../person/_", KEY_META, "default", "", KEY_END),
	keyNew ("/people/#", KEY_META, "check/type", "any", KEY_META, "default", "", KEY_META, "type", "struct_ref", KEY_END),
	keyNew ("/person/_", KEY_META, "check/type", "any", KEY_META, "default", "", KEY_META, "gen/struct/alloc", "1", KEY_META, "gen/struct/type", "Person", KEY_META, "type", "struct", KEY_END),
	keyNew ("/person/_/age", KEY_META, "default", "30", KEY_META, "type", "short", KEY_END),
	keyNew ("/person/_/children", KEY_META, "check/reference", "recursive", KEY_META, "check/reference/restrict", "../../../person/_", KEY_META, "default", "", KEY_END),
	keyNew ("/person/_/children/#", KEY_META, "check/type", "any", KEY_META, "default", "", KEY_META, "type", "struct_ref", KEY_END),
	keyNew ("/person/_/height", KEY_META, "default", "1.80", KEY_META, "type", "float", KEY_END),
	keyNew ("/person/_/name", KEY_META, "default", "Max", KEY_META, "gen/struct/field", "fullName", KEY_META, "type", "string", KEY_END),
	KS_END);
;
}

static const char * helpFallback = "Usage: tests_script_gen_highlevel_struct [OPTION...]\n\nOPTIONS\n  --help                      Print this help message\n";

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
 * Initializes an instance of Elektra for the application '/tests/script/gen/highlevel/struct'.
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
	Key * parentKey = keyNew ("/tests/script/gen/highlevel/struct", KEY_END);

	elektraGOptsContract (contract, argc, argv, envp, parentKey, NULL);
	

	keyDel (parentKey);

	Elektra * e = elektraOpen ("/tests/script/gen/highlevel/struct", defaults, contract, error);

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

	Key * parentKey = keyNew ("spec:/tests/script/gen/highlevel/struct", KEY_META, "system:/elektra/quickdump/noparent", "", KEY_END);

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


ELEKTRA_GET_OUT_PTR_SIGNATURE (ElektraStructMyotherstruct, StructMyotherstruct)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 4 +1) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "x", 4);
	
	
	result->x = ELEKTRA_GET (Long) (elektra, field);

	strncpy (&field[nameLen], "x/y", 4);
	
	
	result->xY = ELEKTRA_GET (Long) (elektra, field);

	elektraFree (field);
}

ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_SIGNATURE (ElektraStructMyotherstruct, StructMyotherstruct)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 4 +1 + ELEKTRA_MAX_ARRAY_SIZE) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	elektraWriteArrayNumber (&field[nameLen], index);
	nameLen = strlen (field);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "x", 4);
	
	
	result->x = ELEKTRA_GET (Long) (elektra, field);

	strncpy (&field[nameLen], "x/y", 4);
	
	
	result->xY = ELEKTRA_GET (Long) (elektra, field);

	elektraFree (field);
}

ELEKTRA_SET_SIGNATURE (const ElektraStructMyotherstruct *, StructMyotherstruct)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 4 +1) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "x", 4);
	
	
	ELEKTRA_SET (Long) (elektra, field, value->x, error);
	if (error != NULL)
	{
		return;
	}

	strncpy (&field[nameLen], "x/y", 4);
	
	
	ELEKTRA_SET (Long) (elektra, field, value->xY, error);
	if (error != NULL)
	{
		return;
	}

}

ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (const ElektraStructMyotherstruct *, StructMyotherstruct)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 4 +1 + ELEKTRA_MAX_ARRAY_SIZE) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	elektraWriteArrayNumber (&field[nameLen], index);
	nameLen = strlen (field);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "x", 4);
	
	
	ELEKTRA_SET (Long) (elektra, field, value->x, error);
	if (error != NULL)
	{
		return;
	}

	strncpy (&field[nameLen], "x/y", 4);
	
	
	ELEKTRA_SET (Long) (elektra, field, value->xY, error);
	if (error != NULL)
	{
		return;
	}

}

ELEKTRA_GET_OUT_PTR_SIGNATURE (ElektraStructMystruct, StructMystruct)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 2 +1) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "a", 2);
	
	
	result->a = ELEKTRA_GET (String) (elektra, field);

	strncpy (&field[nameLen], "b", 2);
	
	
	result->b = ELEKTRA_GET (Long) (elektra, field);

	elektraFree (field);
}

ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_SIGNATURE (ElektraStructMystruct, StructMystruct)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 2 +1 + ELEKTRA_MAX_ARRAY_SIZE) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	elektraWriteArrayNumber (&field[nameLen], index);
	nameLen = strlen (field);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "a", 2);
	
	
	result->a = ELEKTRA_GET (String) (elektra, field);

	strncpy (&field[nameLen], "b", 2);
	
	
	result->b = ELEKTRA_GET (Long) (elektra, field);

	elektraFree (field);
}

ELEKTRA_SET_SIGNATURE (const ElektraStructMystruct *, StructMystruct)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 2 +1) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "a", 2);
	
	
	ELEKTRA_SET (String) (elektra, field, value->a, error);
	if (error != NULL)
	{
		return;
	}

	strncpy (&field[nameLen], "b", 2);
	
	
	ELEKTRA_SET (Long) (elektra, field, value->b, error);
	if (error != NULL)
	{
		return;
	}

}

ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (const ElektraStructMystruct *, StructMystruct)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 2 +1 + ELEKTRA_MAX_ARRAY_SIZE) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	elektraWriteArrayNumber (&field[nameLen], index);
	nameLen = strlen (field);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "a", 2);
	
	
	ELEKTRA_SET (String) (elektra, field, value->a, error);
	if (error != NULL)
	{
		return;
	}

	strncpy (&field[nameLen], "b", 2);
	
	
	ELEKTRA_SET (Long) (elektra, field, value->b, error);
	if (error != NULL)
	{
		return;
	}

}
ELEKTRA_STRUCT_FREE_SIGNATURE (Person *, StructPerson)
{
	if (*ptr == NULL)
	{
		return;
	}

	
	
	
	
	
	
	
	
	
	
	elektraFree (*ptr);
	*ptr = NULL;
}

ELEKTRA_GET_SIGNATURE (Person *, StructPerson)
{
	Person *result = elektraCalloc (sizeof (Person));
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 9 +1) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	// clang-format off

// clang-format on

strncpy (&field[nameLen], "age", 9);



result->age = ELEKTRA_GET (Short) (elektra, field);



strncpy (&field[nameLen], "children", 9);

result->childrenSize = elektraArraySize (elektra, field);
if (result->childrenSize > 0)
{
	result->children = elektraCalloc (sizeof (Person) * result->childrenSize);
	for (kdb_long_long_t i = 0; i < result->childrenSize; ++i)
	{
		const char * refname = elektraFindReferenceArrayElement (elektra, field, i);
		if (refname != NULL && refname[0] != '\0')
		{
			result->children[i] = ELEKTRA_GET (StructPerson) (elektra, refname);
			
		}
		
	}
}


strncpy (&field[nameLen], "height", 9);



result->height = ELEKTRA_GET (Float) (elektra, field);

strncpy (&field[nameLen], "name", 9);



result->fullName = ELEKTRA_GET (String) (elektra, field);



	elektraFree (field);
	return result;
}

ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (Person *, StructPerson)
{
	Person *result = elektraCalloc (sizeof (Person));
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 9 +1 + ELEKTRA_MAX_ARRAY_SIZE) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	elektraWriteArrayNumber (&field[nameLen], index);
	nameLen = strlen (field);
	field[nameLen] = '/';
	++nameLen;

	// clang-format off

// clang-format on

strncpy (&field[nameLen], "age", 9);



result->age = ELEKTRA_GET (Short) (elektra, field);



strncpy (&field[nameLen], "children", 9);

result->childrenSize = elektraArraySize (elektra, field);
if (result->childrenSize > 0)
{
	result->children = elektraCalloc (sizeof (Person) * result->childrenSize);
	for (kdb_long_long_t i = 0; i < result->childrenSize; ++i)
	{
		const char * refname = elektraFindReferenceArrayElement (elektra, field, i);
		if (refname != NULL && refname[0] != '\0')
		{
			result->children[i] = ELEKTRA_GET (StructPerson) (elektra, refname);
			
		}
		
	}
}


strncpy (&field[nameLen], "height", 9);



result->height = ELEKTRA_GET (Float) (elektra, field);

strncpy (&field[nameLen], "name", 9);



result->fullName = ELEKTRA_GET (String) (elektra, field);



	elektraFree (field);
	return result;
}




