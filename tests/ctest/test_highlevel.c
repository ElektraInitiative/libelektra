/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <elektra.h>
#include <tests_internal.h>

static void setKeyValue (const char * parentKeyName, const char * type, const char * name, const char * value)
{
	// Open
	KeySet * config = ksNew (0, KS_END);
	Key * parentKey = keyNew (parentKeyName, KEY_END);
	KDB * handle = kdbOpen (parentKey);
	kdbGet (handle, config, parentKey);

	// Set
	Key * key = keyNew (parentKeyName, KEY_END);
	keyAddName (key, name);
	keySetMeta (key, "type", type);
	keySetString (key, value);
	ksAppendKey (config, key);

	// Save
	kdbSet (handle, config, parentKey);

	// Close
	kdbClose (handle, parentKey);
	keyDel (parentKey);
}

static void test_primitiveGetters ()
{
	const char * parentKey = "user/test/sw/elektra/kdb/#0/current";

	setKeyValue (parentKey, "string", "stringKey", "A string");
	setKeyValue (parentKey, "boolean", "booleanKey", "1");
	setKeyValue (parentKey, "char", "charKey", "c");
	setKeyValue (parentKey, "octet", "octetKey", "1");
	setKeyValue (parentKey, "short", "shortKey", "1");
	setKeyValue (parentKey, "unsigned_short", "unsignedShortKey", "1");
	setKeyValue (parentKey, "long", "longKey", "1");
	setKeyValue (parentKey, "unsigned_long", "unsignedLongKey", "1");
	setKeyValue (parentKey, "long_long", "longLongKey", "1");
	setKeyValue (parentKey, "unsigned_long_long", "unsignedLongLongKey", "1");
	setKeyValue (parentKey, "float", "floatKey", "1.1");
	setKeyValue (parentKey, "double", "doubleKey", "1.1");
	setKeyValue (parentKey, "long_double", "longDoubleKey", "1.1");

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (!elektraStrCmp (elektraGetString (elektra, "stringKey"), "A string"), "Wrong key value.");
	succeed_if (elektraGetBoolean (elektra, "booleanKey"), "Wrong key value.");
	succeed_if (elektraGetChar (elektra, "charKey") == 'c', "Wrong key value.");
	succeed_if (elektraGetOctet (elektra, "octetKey") == 1, "Wrong key value.");
	succeed_if (elektraGetShort (elektra, "shortKey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedShort (elektra, "unsignedShortKey") == 1, "Wrong key value.");
	succeed_if (elektraGetLong (elektra, "longKey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLong (elektra, "unsignedLongKey") == 1, "Wrong key value.");
	succeed_if (elektraGetLongLong (elektra, "longLongKey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLongLong (elektra, "unsignedLongLongKey") == 1, "Wrong key value.");

	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF (-Wfloat - equal)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"

	succeed_if (elektraGetFloat (elektra, "floatKey") == 1.1f, "Wrong key value.");
	succeed_if (elektraGetDouble (elektra, "doubleKey") == 1.1, "Wrong key value.");
	succeed_if (elektraGetLongDouble (elektra, "longDoubleKey") == 1.1L, "Wrong key value.");
#pragma clang diagnostic pop
	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

static void test_arrayGetters ()
{
	const char * parentKey = "user/test/sw/elektra/kdb/#0/current";

	setKeyValue (parentKey, "string", "stringArrayKey/#0", "String 1");
	setKeyValue (parentKey, "string", "stringArrayKey/#1", "String 2");

	setKeyValue (parentKey, "boolean", "booleanArrayKey/#0", "0");
	setKeyValue (parentKey, "boolean", "booleanArrayKey/#1", "1");

	setKeyValue (parentKey, "char", "charArrayKey/#0", "c");
	setKeyValue (parentKey, "char", "charArrayKey/#1", "d");

	setKeyValue (parentKey, "octet", "octetArrayKey/#0", "1");
	setKeyValue (parentKey, "octet", "octetArrayKey/#1", "2");

	setKeyValue (parentKey, "short", "shortArrayKey/#0", "1");
	setKeyValue (parentKey, "short", "shortArrayKey/#1", "2");

	setKeyValue (parentKey, "unsigned_short", "unsignedShortArrayKey/#0", "1");
	setKeyValue (parentKey, "unsigned_short", "unsignedShortArrayKey/#1", "2");

	setKeyValue (parentKey, "long", "longArrayKey/#0", "1");
	setKeyValue (parentKey, "long", "longArrayKey/#1", "2");

	setKeyValue (parentKey, "unsigned_long", "unsignedLongArrayKey/#0", "1");
	setKeyValue (parentKey, "unsigned_long", "unsignedLongArrayKey/#1", "2");

	setKeyValue (parentKey, "long_long", "longLongArrayKey/#0", "1");
	setKeyValue (parentKey, "long_long", "longLongArrayKey/#1", "2");

	setKeyValue (parentKey, "unsigned_long_long", "unsignedLongLongArrayKey/#0", "1");
	setKeyValue (parentKey, "unsigned_long_long", "unsignedLongLongArrayKey/#1", "2");

	setKeyValue (parentKey, "float", "floatArrayKey/#0", "1.1");
	setKeyValue (parentKey, "float", "floatArrayKey/#1", "2.1");

	setKeyValue (parentKey, "double", "doubleArrayKey/#0", "1.1");
	setKeyValue (parentKey, "double", "doubleArrayKey/#1", "2.1");

	setKeyValue (parentKey, "long_double", "longDoubleArrayKey/#0", "1.1");
	setKeyValue (parentKey, "long_double", "longDoubleArrayKey/#1", "2.1");

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (elektraArraySize (elektra, "stringArrayKey") == 2, "Wrong array size");
	succeed_if (!elektraStrCmp (elektraGetStringArrayElement (elektra, "stringArrayKey", 0), "String 1"), "Wrong key value.");
	succeed_if (!elektraStrCmp (elektraGetStringArrayElement (elektra, "stringArrayKey", 1), "String 2"), "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "booleanArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetBooleanArrayElement (elektra, "booleanArrayKey", 0) == 0, "Wrong key value.");
	succeed_if (elektraGetBooleanArrayElement (elektra, "booleanArrayKey", 1), "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "charArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetCharArrayElement (elektra, "charArrayKey", 0) == 'c', "Wrong key value.");
	succeed_if (elektraGetCharArrayElement (elektra, "charArrayKey", 1) == 'd', "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "octetArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetOctetArrayElement (elektra, "octetArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetOctetArrayElement (elektra, "octetArrayKey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "shortArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetShortArrayElement (elektra, "shortArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetShortArrayElement (elektra, "shortArrayKey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "unsignedShortArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetUnsignedShortArrayElement (elektra, "unsignedShortArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedShortArrayElement (elektra, "unsignedShortArrayKey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "longLongArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetLongLongArrayElement (elektra, "longLongArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetLongLongArrayElement (elektra, "longLongArrayKey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "unsignedLongLongArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedLongLongArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedLongLongArrayKey", 1) == 2, "Wrong key value.");

	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF (-Wfloat - equal)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"

	succeed_if (elektraArraySize (elektra, "floatArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetFloatArrayElement (elektra, "floatArrayKey", 0) == 1.1f, "Wrong key value.");
	succeed_if (elektraGetFloatArrayElement (elektra, "floatArrayKey", 1) == 2.1f, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "doubleArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetDoubleArrayElement (elektra, "doubleArrayKey", 0) == 1.1, "Wrong key value.");
	succeed_if (elektraGetDoubleArrayElement (elektra, "doubleArrayKey", 1) == 2.1, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "longDoubleArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longDoubleArrayKey", 0) == 1.1L, "Wrong key value.");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longDoubleArrayKey", 1) == 2.1L, "Wrong key value.");

#pragma clang diagnostic pop
	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

static void test_primitiveSetters ()
{
	const char * parentKey = "user/test/sw/elektra/kdb/#0/current";

	setKeyValue (parentKey, "string", "stringKey", "");
	setKeyValue (parentKey, "boolean", "booleanKey", "");
	setKeyValue (parentKey, "char", "charKey", "");
	setKeyValue (parentKey, "octet", "octetKey", "");
	setKeyValue (parentKey, "short", "shortKey", "1");
	setKeyValue (parentKey, "unsigned_short", "unsignedShortKey", "1");
	setKeyValue (parentKey, "long", "longKey", "1");
	setKeyValue (parentKey, "unsigned_long", "unsignedLongKey", "1");
	setKeyValue (parentKey, "long_long", "longLongKey", "1");
	setKeyValue (parentKey, "unsigned_long_long", "unsignedLongLongKey", "1");
	setKeyValue (parentKey, "float", "floatKey", "1.1");
	setKeyValue (parentKey, "double", "doubleKey", "1.1");
	setKeyValue (parentKey, "long_double", "longDoubleKey", "1.1");

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	// Overwrite existing values.
	elektraSetString (elektra, "stringKey", "A string", &error);
	elektraSetBoolean (elektra, "booleanKey", 1, &error);
	elektraSetChar (elektra, "charKey", 'c', &error);
	elektraSetOctet (elektra, "octetKey", 1, &error);
	elektraSetShort (elektra, "shortKey", 1, &error);
	elektraSetUnsignedShort (elektra, "unsignedShortKey", 1, &error);
	elektraSetLong (elektra, "longKey", 1, &error);
	elektraSetUnsignedLong (elektra, "unsignedLongKey", 1, &error);
	elektraSetLongLong (elektra, "longLongKey", 1, &error);
	elektraSetUnsignedLongLong (elektra, "unsignedLongLongKey", 1, &error);
	elektraSetFloat (elektra, "floatKey", 1.1, &error);
	elektraSetDouble (elektra, "doubleKey", 1.1, &error);
	elektraSetLongDouble (elektra, "longDoubleKey", 1.1, &error);

	// Add new keys.
	elektraSetString (elektra, "newStringKey", "A string", &error);
	elektraSetBoolean (elektra, "newBooleanKey", 1, &error);
	elektraSetChar (elektra, "newCharKey", 'c', &error);
	elektraSetOctet (elektra, "newOctetKey", 1, &error);
	elektraSetShort (elektra, "newShortKey", 1, &error);
	elektraSetUnsignedShort (elektra, "newUnsignedShortKey", 1, &error);
	elektraSetLong (elektra, "newLongKey", 1, &error);
	elektraSetUnsignedLong (elektra, "newUnsignedLongKey", 1, &error);
	elektraSetLongLong (elektra, "newLongLongKey", 1, &error);
	elektraSetUnsignedLongLong (elektra, "newUnsignedLongLongKey", 1, &error);
	elektraSetFloat (elektra, "newFloatKey", 1.1, &error);
	elektraSetDouble (elektra, "newDoubleKey", 1.1, &error);
	elektraSetLongDouble (elektra, "newLongDoubleKey", 1.1, &error);

	if (error)
	{
		yield_error ("A setter failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	// Check overwritten values.
	succeed_if (!elektraStrCmp (elektraGetString (elektra, "stringKey"), "A string"), "Wrong key value.");
	succeed_if (elektraGetBoolean (elektra, "booleanKey"), "Wrong key value.");
	succeed_if (elektraGetChar (elektra, "charKey") == 'c', "Wrong key value.");
	succeed_if (elektraGetOctet (elektra, "octetKey") == 1, "Wrong key value.");
	succeed_if (elektraGetShort (elektra, "shortKey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedShort (elektra, "unsignedShortKey") == 1, "Wrong key value.");
	succeed_if (elektraGetLong (elektra, "longKey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLong (elektra, "unsignedLongKey") == 1, "Wrong key value.");
	succeed_if (elektraGetLongLong (elektra, "longLongKey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLongLong (elektra, "unsignedLongLongKey") == 1, "Wrong key value.");
	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF (-Wfloat - equal)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"
	succeed_if (elektraGetFloat (elektra, "floatKey") == 1.1f, "Wrong key value.");
	succeed_if (elektraGetDouble (elektra, "doubleKey") == 1.1, "Wrong key value.");
	succeed_if (elektraGetLongDouble (elektra, "longDoubleKey") == 1.1L, "Wrong key value.");
#pragma clang diagnostic pop
	ELEKTRA_DIAG_RESTORE

	// Check new keys.
	succeed_if (!elektraStrCmp (elektraGetString (elektra, "newStringKey"), "A string"), "Wrong key value.");
	succeed_if (elektraGetBoolean (elektra, "newBooleanKey"), "Wrong key value.");
	succeed_if (elektraGetChar (elektra, "newCharKey") == 'c', "Wrong key value.");
	succeed_if (elektraGetOctet (elektra, "newOctetKey") == 1, "Wrong key value.");
	succeed_if (elektraGetShort (elektra, "newShortKey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedShort (elektra, "newUnsignedShortKey") == 1, "Wrong key value.");
	succeed_if (elektraGetLong (elektra, "newLongKey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLong (elektra, "newUnsignedLongKey") == 1, "Wrong key value.");
	succeed_if (elektraGetLongLong (elektra, "newLongLongKey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLongLong (elektra, "newUnsignedLongLongKey") == 1, "Wrong key value.");
	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF (-Wfloat - equal)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"
	succeed_if (elektraGetFloat (elektra, "newFloatKey") == 1.1f, "Wrong key value.");
	succeed_if (elektraGetDouble (elektra, "newDoubleKey") == 1.1, "Wrong key value.");
	succeed_if (elektraGetLongDouble (elektra, "newLongDoubleKey") == 1.1L, "Wrong key value.");
#pragma clang diagnostic pop
	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

static void test_arraySetters ()
{
	const char * parentKey = "user/test/sw/elektra/kdb/#0/current";

	setKeyValue (parentKey, "string", "stringArrayKey/#0", "");
	setKeyValue (parentKey, "string", "stringArrayKey/#1", "");

	setKeyValue (parentKey, "boolean", "booleanArrayKey/#0", "");
	setKeyValue (parentKey, "boolean", "booleanArrayKey/#1", "");

	setKeyValue (parentKey, "char", "charArrayKey/#0", "");
	setKeyValue (parentKey, "char", "charArrayKey/#1", "");

	setKeyValue (parentKey, "octet", "octetArrayKey/#0", "");
	setKeyValue (parentKey, "octet", "octetArrayKey/#1", "");

	setKeyValue (parentKey, "short", "shortArrayKey/#0", "");
	setKeyValue (parentKey, "short", "shortArrayKey/#1", "");

	setKeyValue (parentKey, "unsigned_short", "unsignedShortArrayKey/#0", "");
	setKeyValue (parentKey, "unsigned_short", "unsignedShortArrayKey/#1", "");

	setKeyValue (parentKey, "long", "longArrayKey/#0", "");
	setKeyValue (parentKey, "long", "longArrayKey/#1", "");

	setKeyValue (parentKey, "unsigned_long", "unsignedLongArrayKey/#0", "");
	setKeyValue (parentKey, "unsigned_long", "unsignedLongArrayKey/#1", "");

	setKeyValue (parentKey, "long_long", "longLongArrayKey/#0", "");
	setKeyValue (parentKey, "long_long", "longLongArrayKey/#1", "");

	setKeyValue (parentKey, "unsigned_long_long", "unsignedLongLongArrayKey/#0", "");
	setKeyValue (parentKey, "unsigned_long_long", "unsignedLongLongArrayKey/#1", "");

	setKeyValue (parentKey, "float", "floatArrayKey/#0", "");
	setKeyValue (parentKey, "float", "floatArrayKey/#1", "");

	setKeyValue (parentKey, "double", "doubleArrayKey/#0", "");
	setKeyValue (parentKey, "double", "doubleArrayKey/#1", "");

	setKeyValue (parentKey, "long_double", "longDoubleArrayKey/#0", "");
	setKeyValue (parentKey, "long_double", "longDoubleArrayKey/#1", "");

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	// Overwrite existing values.

	elektraSetStringArrayElement (elektra, "stringArrayKey", "String 1", 0, &error);
	elektraSetStringArrayElement (elektra, "stringArrayKey", "String 2", 1, &error);

	elektraSetBooleanArrayElement (elektra, "booleanArrayKey", 0, 0, &error);
	elektraSetBooleanArrayElement (elektra, "booleanArrayKey", 1, 1, &error);

	elektraSetCharArrayElement (elektra, "charArrayKey", 'c', 0, &error);
	elektraSetCharArrayElement (elektra, "charArrayKey", 'd', 1, &error);

	elektraSetOctetArrayElement (elektra, "octetArrayKey", 1, 0, &error);
	elektraSetOctetArrayElement (elektra, "octetArrayKey", 2, 1, &error);

	elektraSetShortArrayElement (elektra, "shortArrayKey", 1, 0, &error);
	elektraSetShortArrayElement (elektra, "shortArrayKey", 2, 1, &error);

	elektraSetUnsignedShortArrayElement (elektra, "unsignedShortArrayKey", 1, 0, &error);
	elektraSetUnsignedShortArrayElement (elektra, "unsignedShortArrayKey", 2, 1, &error);

	elektraSetLongArrayElement (elektra, "longArrayKey", 1, 0, &error);
	elektraSetLongArrayElement (elektra, "longArrayKey", 2, 1, &error);

	elektraSetUnsignedLongArrayElement (elektra, "unsignedLongArrayKey", 1, 0, &error);
	elektraSetUnsignedLongArrayElement (elektra, "unsignedLongArrayKey", 2, 1, &error);

	elektraSetLongLongArrayElement (elektra, "longLongArrayKey", 1, 0, &error);
	elektraSetLongLongArrayElement (elektra, "longLongArrayKey", 2, 1, &error);

	elektraSetUnsignedLongLongArrayElement (elektra, "unsignedLongLongArrayKey", 1, 0, &error);
	elektraSetUnsignedLongLongArrayElement (elektra, "unsignedLongLongArrayKey", 2, 1, &error);

	elektraSetFloatArrayElement (elektra, "floatArrayKey", 1.1, 0, &error);
	elektraSetFloatArrayElement (elektra, "floatArrayKey", 2.1, 1, &error);

	elektraSetDoubleArrayElement (elektra, "doubleArrayKey", 1.1, 0, &error);
	elektraSetDoubleArrayElement (elektra, "doubleArrayKey", 2.1, 1, &error);

	elektraSetLongDoubleArrayElement (elektra, "longDoubleArrayKey", 1.1, 0, &error);
	elektraSetLongDoubleArrayElement (elektra, "longDoubleArrayKey", 2.1, 1, &error);

	// Add new keys.

	elektraSetStringArrayElement (elektra, "newStringArrayKey", "String 1", 0, &error);
	elektraSetStringArrayElement (elektra, "newStringArrayKey", "String 2", 1, &error);

	elektraSetBooleanArrayElement (elektra, "newBooleanArrayKey", 0, 0, &error);
	elektraSetBooleanArrayElement (elektra, "newBooleanArrayKey", 1, 1, &error);

	elektraSetCharArrayElement (elektra, "newCharArrayKey", 'c', 0, &error);
	elektraSetCharArrayElement (elektra, "newCharArrayKey", 'd', 1, &error);

	elektraSetOctetArrayElement (elektra, "newOctetArrayKey", 1, 0, &error);
	elektraSetOctetArrayElement (elektra, "newOctetArrayKey", 2, 1, &error);

	elektraSetShortArrayElement (elektra, "newShortArrayKey", 1, 0, &error);
	elektraSetShortArrayElement (elektra, "newShortArrayKey", 2, 1, &error);

	elektraSetUnsignedShortArrayElement (elektra, "newUnsignedShortArrayKey", 1, 0, &error);
	elektraSetUnsignedShortArrayElement (elektra, "newUnsignedShortArrayKey", 2, 1, &error);

	elektraSetLongArrayElement (elektra, "newLongArrayKey", 1, 0, &error);
	elektraSetLongArrayElement (elektra, "newLongArrayKey", 2, 1, &error);

	elektraSetUnsignedLongArrayElement (elektra, "newUnsignedLongArrayKey", 1, 0, &error);
	elektraSetUnsignedLongArrayElement (elektra, "newUnsignedLongArrayKey", 2, 1, &error);

	elektraSetLongLongArrayElement (elektra, "newLongLongArrayKey", 1, 0, &error);
	elektraSetLongLongArrayElement (elektra, "newLongLongArrayKey", 2, 1, &error);

	elektraSetUnsignedLongLongArrayElement (elektra, "newUnsignedLongLongArrayKey", 1, 0, &error);
	elektraSetUnsignedLongLongArrayElement (elektra, "newUnsignedLongLongArrayKey", 2, 1, &error);

	elektraSetFloatArrayElement (elektra, "newFloatArrayKey", 1.1, 0, &error);
	elektraSetFloatArrayElement (elektra, "newFloatArrayKey", 2.1, 1, &error);

	elektraSetDoubleArrayElement (elektra, "newDoubleArrayKey", 1.1, 0, &error);
	elektraSetDoubleArrayElement (elektra, "newDoubleArrayKey", 2.1, 1, &error);

	elektraSetLongDoubleArrayElement (elektra, "newLongDoubleArrayKey", 1.1, 0, &error);
	elektraSetLongDoubleArrayElement (elektra, "newLongDoubleArrayKey", 2.1, 1, &error);

	if (error)
	{
		yield_error ("A setter failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	// Check overwritten values.

	succeed_if (elektraArraySize (elektra, "stringArrayKey") == 2, "Wrong array size");
	succeed_if (!elektraStrCmp (elektraGetStringArrayElement (elektra, "stringArrayKey", 0), "String 1"), "Wrong key value.");
	succeed_if (!elektraStrCmp (elektraGetStringArrayElement (elektra, "stringArrayKey", 1), "String 2"), "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "booleanArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetBooleanArrayElement (elektra, "booleanArrayKey", 0) == 0, "Wrong key value.");
	succeed_if (elektraGetBooleanArrayElement (elektra, "booleanArrayKey", 1), "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "charArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetCharArrayElement (elektra, "charArrayKey", 0) == 'c', "Wrong key value.");
	succeed_if (elektraGetCharArrayElement (elektra, "charArrayKey", 1) == 'd', "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "octetArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetOctetArrayElement (elektra, "octetArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetOctetArrayElement (elektra, "octetArrayKey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "shortArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetShortArrayElement (elektra, "shortArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetShortArrayElement (elektra, "shortArrayKey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "unsignedShortArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetUnsignedShortArrayElement (elektra, "unsignedShortArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedShortArrayElement (elektra, "unsignedShortArrayKey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "longLongArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetLongLongArrayElement (elektra, "longLongArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetLongLongArrayElement (elektra, "longLongArrayKey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "unsignedLongLongArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedLongLongArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedLongLongArrayKey", 1) == 2, "Wrong key value.");

	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF (-Wfloat - equal)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"

	succeed_if (elektraArraySize (elektra, "floatArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetFloatArrayElement (elektra, "floatArrayKey", 0) == 1.1f, "Wrong key value.");
	succeed_if (elektraGetFloatArrayElement (elektra, "floatArrayKey", 1) == 2.1f, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "doubleArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetDoubleArrayElement (elektra, "doubleArrayKey", 0) == 1.1, "Wrong key value.");
	succeed_if (elektraGetDoubleArrayElement (elektra, "doubleArrayKey", 1) == 2.1, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "longDoubleArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longDoubleArrayKey", 0) == 1.1L, "Wrong key value.");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longDoubleArrayKey", 1) == 2.1L, "Wrong key value.");

#pragma clang diagnostic pop
	ELEKTRA_DIAG_RESTORE

	// Check new keys.

	succeed_if (elektraArraySize (elektra, "newStringArrayKey") == 2, "Wrong array size");
	succeed_if (!elektraStrCmp (elektraGetStringArrayElement (elektra, "newStringArrayKey", 0), "String 1"), "Wrong key value.");
	succeed_if (!elektraStrCmp (elektraGetStringArrayElement (elektra, "newStringArrayKey", 1), "String 2"), "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newBooleanArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetBooleanArrayElement (elektra, "newBooleanArrayKey", 0) == 0, "Wrong key value.");
	succeed_if (elektraGetBooleanArrayElement (elektra, "newBooleanArrayKey", 1), "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newCharArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetCharArrayElement (elektra, "newCharArrayKey", 0) == 'c', "Wrong key value.");
	succeed_if (elektraGetCharArrayElement (elektra, "newCharArrayKey", 1) == 'd', "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newOctetArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetOctetArrayElement (elektra, "newOctetArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetOctetArrayElement (elektra, "newOctetArrayKey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newShortArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetShortArrayElement (elektra, "newShortArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetShortArrayElement (elektra, "newShortArrayKey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newUnsignedShortArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetUnsignedShortArrayElement (elektra, "newUnsignedShortArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedShortArrayElement (elektra, "newUnsignedShortArrayKey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newLongLongArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetLongLongArrayElement (elektra, "newLongLongArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetLongLongArrayElement (elektra, "newLongLongArrayKey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newUnsignedLongLongArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetUnsignedLongLongArrayElement (elektra, "newUnsignedLongLongArrayKey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLongLongArrayElement (elektra, "newUnsignedLongLongArrayKey", 1) == 2, "Wrong key value.");

	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF (-Wfloat - equal)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"

	succeed_if (elektraArraySize (elektra, "newFloatArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetFloatArrayElement (elektra, "newFloatArrayKey", 0) == 1.1f, "Wrong key value.");
	succeed_if (elektraGetFloatArrayElement (elektra, "newFloatArrayKey", 1) == 2.1f, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newDoubleArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetDoubleArrayElement (elektra, "newDoubleArrayKey", 0) == 1.1, "Wrong key value.");
	succeed_if (elektraGetDoubleArrayElement (elektra, "newDoubleArrayKey", 1) == 2.1, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newLongDoubleArrayKey") == 2, "Wrong array size");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "newLongDoubleArrayKey", 0) == 1.1L, "Wrong key value.");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "newLongDoubleArrayKey", 1) == 2.1L, "Wrong key value.");

#pragma clang diagnostic pop
	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

void test_defaultValues ()
{
	KeySet * defaults = ksNew(0, keyNew ("user/test/sw/elektra/kdb/#0/current/stringKey", KEY_VALUE, "A string", KEY_META, "type", "string", KEY_END), KS_END);

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen ("user/test/sw/elektra/kdb/#0/current", defaults, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (!elektraStrCmp (elektraGetString (elektra, "stringKey"), "A string"), "Wrong key value.");

	elektraClose (elektra);
}

int main (int argc, char ** argv)
{
	init (argc, argv);

	test_primitiveGetters ();
	test_arrayGetters ();

	test_primitiveSetters ();
	test_arraySetters ();

	test_defaultValues ();

	printf ("\ntest_highlevel RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
