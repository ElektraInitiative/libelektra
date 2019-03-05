/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

// clang-format on

#include "struct.actual.h"



#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h>
#include <string.h>

#include <kdbhelper.h>

#include <elektra/conversion.h>


/**
 * Initializes an instance of Elektra for the application '/tests/script/gen/elektra/struct'.
 *
 * @param error A reference to an ElektraError pointer. Will be passed to elektraOpen().
 *
 * @return A newly allocated instance of Elektra. Has to be disposed of with elektraClose().
 *
 * @see elektraOpen
 */// 
Elektra * loadConfiguration (ElektraError ** error)
{
	KeySet * defaults = ksNew (8,
	keyNew ("spec/tests/script/gen/elektra/struct/mystruct", KEY_META, "default", "", KEY_META, "type", "struct", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/struct/mystruct/a", KEY_META, "default", "test", KEY_META, "type", "string", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/struct/mystruct/b", KEY_META, "default", "8", KEY_META, "type", "long", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/struct/people/#", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/struct/person/_", KEY_META, "default", "", KEY_META, "gen/struct/alloc", "1", KEY_META, "gen/struct/type", "Person", KEY_META, "type", "struct", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/struct/person/_/age", KEY_META, "default", "30", KEY_META, "type", "short", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/struct/person/_/height", KEY_META, "default", "1.80", KEY_META, "type", "float", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/struct/person/_/name", KEY_META, "default", "Max", KEY_META, "gen/struct/field", "fullName", KEY_META, "type", "string", KEY_END),
	KS_END);
;
	return elektraOpen ("/tests/script/gen/elektra/struct", defaults, error);
}

// -------------------------
// Enum conversion functions
// -------------------------



// -------------------------
// Enum accessor functions
// -------------------------



// -------------------------
// Struct accessor functions
// -------------------------


ELEKTRA_GET_OUT_PTR_SIGNATURE (ElektraStructMystruct, StructMystruct)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 1 +1) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "a", 1);

	result->a = ELEKTRA_GET (String) (elektra, field);

	strncpy (&field[nameLen], "b", 1);

	result->b = ELEKTRA_GET (Long) (elektra, field);

	elektraFree (field);
}

ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_SIGNATURE (ElektraStructMystruct, StructMystruct)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 1 +1 + ELEKTRA_MAX_ARRAY_SIZE) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	elektraWriteArrayNumber (&field[nameLen], index);
	nameLen = strlen (field);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "a", 1);

	result->a = ELEKTRA_GET_ARRAY_ELEMENT (String) (elektra, field, index);

	strncpy (&field[nameLen], "b", 1);

	result->b = ELEKTRA_GET_ARRAY_ELEMENT (Long) (elektra, field, index);

	elektraFree (field);
}

ELEKTRA_SET_SIGNATURE (const ElektraStructMystruct *, StructMystruct)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 1 +1) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "a", 1);
	ELEKTRA_SET (String) (elektra, field, value->a, error);
	if (error != NULL)
	{
		return;
	}

	strncpy (&field[nameLen], "b", 1);
	ELEKTRA_SET (Long) (elektra, field, value->b, error);
	if (error != NULL)
	{
		return;
	}

}

ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (const ElektraStructMystruct *, StructMystruct)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 1 +1 + ELEKTRA_MAX_ARRAY_SIZE) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	elektraWriteArrayNumber (&field[nameLen], index);
	nameLen = strlen (field);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "a", 1);
	ELEKTRA_SET_ARRAY_ELEMENT (String) (elektra, field, index, value->a, error);
	if (error != NULL)
	{
		return;
	}

	strncpy (&field[nameLen], "b", 1);
	ELEKTRA_SET_ARRAY_ELEMENT (Long) (elektra, field, index, value->b, error);
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

	
	
	
	elektraFree (&ptr);
	*ptr = NULL;
}

ELEKTRA_GET_SIGNATURE (Person *, StructPerson)
{
	Person *result = elektraCalloc (sizeof (Person));
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 6 +1) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "age", 6);
	
	result->age = ELEKTRA_GET (Short) (elektra, field);

	strncpy (&field[nameLen], "height", 6);
	
	result->height = ELEKTRA_GET (Float) (elektra, field);

	strncpy (&field[nameLen], "name", 6);
	
	result->fullName = ELEKTRA_GET (String) (elektra, field);

	elektraFree (field);
	return result;
}

ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (Person *, StructPerson)
{
	Person *result = elektraCalloc (sizeof (Person));
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 6 +1 + ELEKTRA_MAX_ARRAY_SIZE) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	elektraWriteArrayNumber (&field[nameLen], index);
	nameLen = strlen (field);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "age", 6);
	
	result->age = ELEKTRA_GET_ARRAY_ELEMENT (Short) (elektra, field, index);

	strncpy (&field[nameLen], "height", 6);
	
	result->height = ELEKTRA_GET_ARRAY_ELEMENT (Float) (elektra, field, index);

	strncpy (&field[nameLen], "name", 6);
	
	result->fullName = ELEKTRA_GET_ARRAY_ELEMENT (String) (elektra, field, index);

	elektraFree (field);
	return result;
}


ELEKTRA_SET_SIGNATURE (const Person *, StructPerson)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 6 +1) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "age", 6);
	ELEKTRA_SET (Short) (elektra, field, value->age, error);
	if (error != NULL)
	{
		return;
	}

	strncpy (&field[nameLen], "height", 6);
	ELEKTRA_SET (Float) (elektra, field, value->height, error);
	if (error != NULL)
	{
		return;
	}

	strncpy (&field[nameLen], "name", 6);
	ELEKTRA_SET (String) (elektra, field, value->fullName, error);
	if (error != NULL)
	{
		return;
	}

}

ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (const Person *, StructPerson)
{
	size_t nameLen = strlen (keyname);
	char * field = elektraCalloc ((nameLen + 1 + 6 +1 + ELEKTRA_MAX_ARRAY_SIZE) * sizeof (char));
	strcpy (field, keyname);
	field[nameLen] = '/';
	++nameLen;

	elektraWriteArrayNumber (&field[nameLen], index);
	nameLen = strlen (field);
	field[nameLen] = '/';
	++nameLen;

	strncpy (&field[nameLen], "age", 6);
	ELEKTRA_SET_ARRAY_ELEMENT (Short) (elektra, field, index, value->age, error);
	if (error != NULL)
	{
		return;
	}

	strncpy (&field[nameLen], "height", 6);
	ELEKTRA_SET_ARRAY_ELEMENT (Float) (elektra, field, index, value->height, error);
	if (error != NULL)
	{
		return;
	}

	strncpy (&field[nameLen], "name", 6);
	ELEKTRA_SET_ARRAY_ELEMENT (String) (elektra, field, index, value->fullName, error);
	if (error != NULL)
	{
		return;
	}

}

#ifdef __cplusplus
}
#endif
