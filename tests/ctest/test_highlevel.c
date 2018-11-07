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

static void test_primitiveGetters (void)
{
	const char * parentKey = "user/tests/highlevel";

	setKeyValue (parentKey, "string", "stringkey", "A string");
	setKeyValue (parentKey, "boolean", "booleankey", "1");
	setKeyValue (parentKey, "char", "charkey", "c");
	setKeyValue (parentKey, "octet", "octetkey", "1");
	setKeyValue (parentKey, "short", "shortkey", "1");
	setKeyValue (parentKey, "unsigned_short", "unsignedshortkey", "1");
	setKeyValue (parentKey, "long", "longkey", "1");
	setKeyValue (parentKey, "unsigned_long", "unsignedlongkey", "1");
	setKeyValue (parentKey, "long_long", "longlongkey", "1");
	setKeyValue (parentKey, "unsigned_long_long", "unsignedlonglongkey", "1");
	setKeyValue (parentKey, "float", "floatkey", "1.1");
	setKeyValue (parentKey, "double", "doublekey", "1.1");
	setKeyValue (parentKey, "long_double", "longdoublekey", "1.1");

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (!elektraStrCmp (elektraGetString (elektra, "stringkey"), "A string"), "Wrong key value.");
	succeed_if (elektraGetBoolean (elektra, "booleankey"), "Wrong key value.");
	succeed_if (elektraGetChar (elektra, "charkey") == 'c', "Wrong key value.");
	succeed_if (elektraGetOctet (elektra, "octetkey") == 1, "Wrong key value.");
	succeed_if (elektraGetShort (elektra, "shortkey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedShort (elektra, "unsignedshortkey") == 1, "Wrong key value.");
	succeed_if (elektraGetLong (elektra, "longkey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLong (elektra, "unsignedlongkey") == 1, "Wrong key value.");
	succeed_if (elektraGetLongLong (elektra, "longlongkey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLongLong (elektra, "unsignedlonglongkey") == 1, "Wrong key value.");

	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF_STR ("-Wfloat-equal")

	succeed_if (elektraGetFloat (elektra, "floatkey") == 1.1f, "Wrong key value.");
	succeed_if (elektraGetDouble (elektra, "doublekey") == 1.1, "Wrong key value.");
	succeed_if (elektraGetLongDouble (elektra, "longdoublekey") == 1.1L, "Wrong key value.");

	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

static void test_arrayGetters (void)
{
	const char * parentKey = "user/tests/highlevel";

	setKeyValue (parentKey, "string", "stringarraykey/#0", "String 1");
	setKeyValue (parentKey, "string", "stringarraykey/#1", "String 2");

	setKeyValue (parentKey, "boolean", "booleanarraykey/#0", "0");
	setKeyValue (parentKey, "boolean", "booleanarraykey/#1", "1");

	setKeyValue (parentKey, "char", "chararraykey/#0", "c");
	setKeyValue (parentKey, "char", "chararraykey/#1", "d");

	setKeyValue (parentKey, "octet", "octetarraykey/#0", "1");
	setKeyValue (parentKey, "octet", "octetarraykey/#1", "2");

	setKeyValue (parentKey, "short", "shortarraykey/#0", "1");
	setKeyValue (parentKey, "short", "shortarraykey/#1", "2");

	setKeyValue (parentKey, "unsigned_short", "unsignedshortarraykey/#0", "1");
	setKeyValue (parentKey, "unsigned_short", "unsignedshortarraykey/#1", "2");

	setKeyValue (parentKey, "long", "longarraykey/#0", "1");
	setKeyValue (parentKey, "long", "longarraykey/#1", "2");

	setKeyValue (parentKey, "unsigned_long", "unsignedLongarraykey/#0", "1");
	setKeyValue (parentKey, "unsigned_long", "unsignedLongarraykey/#1", "2");

	setKeyValue (parentKey, "long_long", "longlongarraykey/#0", "1");
	setKeyValue (parentKey, "long_long", "longlongarraykey/#1", "2");

	setKeyValue (parentKey, "unsigned_long_long", "unsignedlonglongarraykey/#0", "1");
	setKeyValue (parentKey, "unsigned_long_long", "unsignedlonglongarraykey/#1", "2");

	setKeyValue (parentKey, "float", "floatarraykey/#0", "1.1");
	setKeyValue (parentKey, "float", "floatarraykey/#1", "2.1");

	setKeyValue (parentKey, "double", "doublearraykey/#0", "1.1");
	setKeyValue (parentKey, "double", "doublearraykey/#1", "2.1");

	setKeyValue (parentKey, "long_double", "longdoublearraykey/#0", "1.1");
	setKeyValue (parentKey, "long_double", "longdoublearraykey/#1", "2.1");

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (elektraArraySize (elektra, "stringarraykey") == 2, "Wrong array size");
	succeed_if (!elektraStrCmp (elektraGetStringArrayElement (elektra, "stringarraykey", 0), "String 1"), "Wrong key value.");
	succeed_if (!elektraStrCmp (elektraGetStringArrayElement (elektra, "stringarraykey", 1), "String 2"), "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "booleanarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetBooleanArrayElement (elektra, "booleanarraykey", 0) == 0, "Wrong key value.");
	succeed_if (elektraGetBooleanArrayElement (elektra, "booleanarraykey", 1), "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "chararraykey") == 2, "Wrong array size");
	succeed_if (elektraGetCharArrayElement (elektra, "chararraykey", 0) == 'c', "Wrong key value.");
	succeed_if (elektraGetCharArrayElement (elektra, "chararraykey", 1) == 'd', "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "octetarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetOctetArrayElement (elektra, "octetarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetOctetArrayElement (elektra, "octetarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "shortarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetShortArrayElement (elektra, "shortarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetShortArrayElement (elektra, "shortarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "unsignedshortarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "longlongarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetLongLongArrayElement (elektra, "longlongarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetLongLongArrayElement (elektra, "longlongarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "unsignedlonglongarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 1) == 2, "Wrong key value.");

	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF_STR ("-Wfloat-equal")

	succeed_if (elektraArraySize (elektra, "floatarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetFloatArrayElement (elektra, "floatarraykey", 0) == 1.1f, "Wrong key value.");
	succeed_if (elektraGetFloatArrayElement (elektra, "floatarraykey", 1) == 2.1f, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "doublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetDoubleArrayElement (elektra, "doublearraykey", 0) == 1.1, "Wrong key value.");
	succeed_if (elektraGetDoubleArrayElement (elektra, "doublearraykey", 1) == 2.1, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "longdoublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 0) == 1.1L, "Wrong key value.");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 1) == 2.1L, "Wrong key value.");

	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

static void test_primitiveSetters (void)
{
	const char * parentKey = "user/tests/highlevel";

	setKeyValue (parentKey, "string", "stringkey", "");
	setKeyValue (parentKey, "boolean", "booleankey", "");
	setKeyValue (parentKey, "char", "charkey", "");
	setKeyValue (parentKey, "octet", "octetkey", "");
	setKeyValue (parentKey, "short", "shortkey", "1");
	setKeyValue (parentKey, "unsigned_short", "unsignedshortkey", "1");
	setKeyValue (parentKey, "long", "longkey", "1");
	setKeyValue (parentKey, "unsigned_long", "unsignedlongkey", "1");
	setKeyValue (parentKey, "long_long", "longlongkey", "1");
	setKeyValue (parentKey, "unsigned_long_long", "unsignedlonglongkey", "1");
	setKeyValue (parentKey, "float", "floatkey", "1.1");
	setKeyValue (parentKey, "double", "doublekey", "1.1");
	setKeyValue (parentKey, "long_double", "longdoublekey", "1.1");

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	// Overwrite existing values.
	elektraSetString (elektra, "stringkey", "A string", &error);
	elektraSetBoolean (elektra, "booleankey", 1, &error);
	elektraSetChar (elektra, "charkey", 'c', &error);
	elektraSetOctet (elektra, "octetkey", 1, &error);
	elektraSetShort (elektra, "shortkey", 1, &error);
	elektraSetUnsignedShort (elektra, "unsignedshortkey", 1, &error);
	elektraSetLong (elektra, "longkey", 1, &error);
	elektraSetUnsignedLong (elektra, "unsignedlongkey", 1, &error);
	elektraSetLongLong (elektra, "longlongkey", 1, &error);
	elektraSetUnsignedLongLong (elektra, "unsignedlonglongkey", 1, &error);
	elektraSetFloat (elektra, "floatkey", 1.1, &error);
	elektraSetDouble (elektra, "doublekey", 1.1, &error);
	elektraSetLongDouble (elektra, "longdoublekey", 1.1L, &error);

	// Add new keys.
	elektraSetString (elektra, "newstringkey", "A string", &error);
	elektraSetBoolean (elektra, "newbooleankey", 1, &error);
	elektraSetChar (elektra, "newcharkey", 'c', &error);
	elektraSetOctet (elektra, "newoctetkey", 1, &error);
	elektraSetShort (elektra, "newshortkey", 1, &error);
	elektraSetUnsignedShort (elektra, "newunsignedshortkey", 1, &error);
	elektraSetLong (elektra, "newlongkey", 1, &error);
	elektraSetUnsignedLong (elektra, "newunsignedlongkey", 1, &error);
	elektraSetLongLong (elektra, "newlonglongkey", 1, &error);
	elektraSetUnsignedLongLong (elektra, "newunsignedlonglongkey", 1, &error);
	elektraSetFloat (elektra, "newfloatkey", 1.1, &error);
	elektraSetDouble (elektra, "newdoublekey", 1.1, &error);
	elektraSetLongDouble (elektra, "newlongdoublekey", 1.1L, &error);

	if (error)
	{
		yield_error ("A setter failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	// Check overwritten values.
	succeed_if (!elektraStrCmp (elektraGetString (elektra, "stringkey"), "A string"), "Wrong key value.");
	succeed_if (elektraGetBoolean (elektra, "booleankey"), "Wrong key value.");
	succeed_if (elektraGetChar (elektra, "charkey") == 'c', "Wrong key value.");
	succeed_if (elektraGetOctet (elektra, "octetkey") == 1, "Wrong key value.");
	succeed_if (elektraGetShort (elektra, "shortkey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedShort (elektra, "unsignedshortkey") == 1, "Wrong key value.");
	succeed_if (elektraGetLong (elektra, "longkey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLong (elektra, "unsignedlongkey") == 1, "Wrong key value.");
	succeed_if (elektraGetLongLong (elektra, "longlongkey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLongLong (elektra, "unsignedlonglongkey") == 1, "Wrong key value.");

	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF_STR ("-Wfloat-equal")

	succeed_if (elektraGetFloat (elektra, "floatkey") == 1.1f, "Wrong key value.");
	succeed_if (elektraGetDouble (elektra, "doublekey") == 1.1, "Wrong key value.");
	succeed_if (elektraGetLongDouble (elektra, "longdoublekey") == 1.1L, "Wrong key value.");

	ELEKTRA_DIAG_RESTORE

	// Check new keys.
	succeed_if (!elektraStrCmp (elektraGetString (elektra, "newstringkey"), "A string"), "Wrong key value.");
	succeed_if (elektraGetBoolean (elektra, "newbooleankey"), "Wrong key value.");
	succeed_if (elektraGetChar (elektra, "newcharkey") == 'c', "Wrong key value.");
	succeed_if (elektraGetOctet (elektra, "newoctetkey") == 1, "Wrong key value.");
	succeed_if (elektraGetShort (elektra, "newshortkey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedShort (elektra, "newunsignedshortkey") == 1, "Wrong key value.");
	succeed_if (elektraGetLong (elektra, "newlongkey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLong (elektra, "newunsignedlongkey") == 1, "Wrong key value.");
	succeed_if (elektraGetLongLong (elektra, "newlonglongkey") == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLongLong (elektra, "newunsignedlonglongkey") == 1, "Wrong key value.");

	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF_STR ("-Wfloat-equal")

	succeed_if (elektraGetFloat (elektra, "newfloatkey") == 1.1f, "Wrong key value.");
	succeed_if (elektraGetDouble (elektra, "newdoublekey") == 1.1, "Wrong key value.");
	succeed_if (elektraGetLongDouble (elektra, "newlongdoublekey") == 1.1L, "Wrong key value.");

	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

static void test_arraySetters (void)
{
	const char * parentKey = "user/tests/highlevel";

	setKeyValue (parentKey, "string", "stringarraykey/#0", "");
	setKeyValue (parentKey, "string", "stringarraykey/#1", "");

	setKeyValue (parentKey, "boolean", "booleanarraykey/#0", "");
	setKeyValue (parentKey, "boolean", "booleanarraykey/#1", "");

	setKeyValue (parentKey, "char", "chararraykey/#0", "");
	setKeyValue (parentKey, "char", "chararraykey/#1", "");

	setKeyValue (parentKey, "octet", "octetarraykey/#0", "");
	setKeyValue (parentKey, "octet", "octetarraykey/#1", "");

	setKeyValue (parentKey, "short", "shortarraykey/#0", "");
	setKeyValue (parentKey, "short", "shortarraykey/#1", "");

	setKeyValue (parentKey, "unsigned_short", "unsignedshortarraykey/#0", "");
	setKeyValue (parentKey, "unsigned_short", "unsignedshortarraykey/#1", "");

	setKeyValue (parentKey, "long", "longarraykey/#0", "");
	setKeyValue (parentKey, "long", "longarraykey/#1", "");

	setKeyValue (parentKey, "unsigned_long", "unsignedLongarraykey/#0", "");
	setKeyValue (parentKey, "unsigned_long", "unsignedLongarraykey/#1", "");

	setKeyValue (parentKey, "long_long", "longlongarraykey/#0", "");
	setKeyValue (parentKey, "long_long", "longlongarraykey/#1", "");

	setKeyValue (parentKey, "unsigned_long_long", "unsignedlonglongarraykey/#0", "");
	setKeyValue (parentKey, "unsigned_long_long", "unsignedlonglongarraykey/#1", "");

	setKeyValue (parentKey, "float", "floatarraykey/#0", "");
	setKeyValue (parentKey, "float", "floatarraykey/#1", "");

	setKeyValue (parentKey, "double", "doublearraykey/#0", "");
	setKeyValue (parentKey, "double", "doublearraykey/#1", "");

	setKeyValue (parentKey, "long_double", "longdoublearraykey/#0", "");
	setKeyValue (parentKey, "long_double", "longdoublearraykey/#1", "");

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	// Overwrite existing values.

	elektraSetStringArrayElement (elektra, "stringarraykey", 0, "String 1", &error);
	elektraSetStringArrayElement (elektra, "stringarraykey", 1, "String 2", &error);

	elektraSetBooleanArrayElement (elektra, "booleanarraykey", 0, 0, &error);
	elektraSetBooleanArrayElement (elektra, "booleanarraykey", 1, 1, &error);

	elektraSetCharArrayElement (elektra, "chararraykey", 0, 'c', &error);
	elektraSetCharArrayElement (elektra, "chararraykey", 1, 'd', &error);

	elektraSetOctetArrayElement (elektra, "octetarraykey", 0, 1, &error);
	elektraSetOctetArrayElement (elektra, "octetarraykey", 1, 2, &error);

	elektraSetShortArrayElement (elektra, "shortarraykey", 0, 1, &error);
	elektraSetShortArrayElement (elektra, "shortarraykey", 1, 2, &error);

	elektraSetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 0, 1, &error);
	elektraSetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 1, 2, &error);

	elektraSetLongArrayElement (elektra, "longarraykey", 0, 1, &error);
	elektraSetLongArrayElement (elektra, "longarraykey", 1, 2, &error);

	elektraSetUnsignedLongArrayElement (elektra, "unsignedLongarraykey", 0, 1, &error);
	elektraSetUnsignedLongArrayElement (elektra, "unsignedLongarraykey", 1, 2, &error);

	elektraSetLongArrayElement (elektra, "longarraykey", 0, 1, &error);
	elektraSetLongArrayElement (elektra, "longarraykey", 1, 2, &error);

	elektraSetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 0, 1, &error);
	elektraSetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 1, 2, &error);

	elektraSetLongLongArrayElement (elektra, "longlongarraykey", 0, 1, &error);
	elektraSetLongLongArrayElement (elektra, "longlongarraykey", 1, 2, &error);

	elektraSetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 0, 1, &error);
	elektraSetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 1, 2, &error);

	elektraSetFloatArrayElement (elektra, "floatarraykey", 0, 1.1, &error);
	elektraSetFloatArrayElement (elektra, "floatarraykey", 1, 2.1, &error);

	elektraSetDoubleArrayElement (elektra, "doublearraykey", 0, 1.1, &error);
	elektraSetDoubleArrayElement (elektra, "doublearraykey", 1, 2.1, &error);

	elektraSetLongDoubleArrayElement (elektra, "longdoublearraykey", 0, 1.1L, &error);
	elektraSetLongDoubleArrayElement (elektra, "longdoublearraykey", 1, 2.1L, &error);

	// Add new keys.

	elektraSetStringArrayElement (elektra, "newstringarraykey", 0, "String 1", &error);
	elektraSetStringArrayElement (elektra, "newstringarraykey", 1, "String 2", &error);

	elektraSetBooleanArrayElement (elektra, "newbooleanarraykey", 0, 0, &error);
	elektraSetBooleanArrayElement (elektra, "newbooleanarraykey", 1, 1, &error);

	elektraSetCharArrayElement (elektra, "newchararraykey", 0, 'c', &error);
	elektraSetCharArrayElement (elektra, "newchararraykey", 1, 'd', &error);

	elektraSetOctetArrayElement (elektra, "newoctetarraykey", 0, 1, &error);
	elektraSetOctetArrayElement (elektra, "newoctetarraykey", 1, 2, &error);

	elektraSetShortArrayElement (elektra, "newshortarraykey", 0, 1, &error);
	elektraSetShortArrayElement (elektra, "newshortarraykey", 1, 2, &error);

	elektraSetUnsignedShortArrayElement (elektra, "newunsignedshortarraykey", 0, 1, &error);
	elektraSetUnsignedShortArrayElement (elektra, "newunsignedshortarraykey", 1, 2, &error);

	elektraSetLongArrayElement (elektra, "newlongarraykey", 0, 1, &error);
	elektraSetLongArrayElement (elektra, "newlongarraykey", 1, 2, &error);

	elektraSetUnsignedLongArrayElement (elektra, "newunsignedLongarraykey", 0, 1, &error);
	elektraSetUnsignedLongArrayElement (elektra, "newunsignedLongarraykey", 1, 2, &error);

	elektraSetLongArrayElement (elektra, "newlongarraykey", 0, 1, &error);
	elektraSetLongArrayElement (elektra, "newlongarraykey", 1, 2, &error);

	elektraSetUnsignedLongArrayElement (elektra, "newunsignedlongarraykey", 0, 1, &error);
	elektraSetUnsignedLongArrayElement (elektra, "newunsignedlongarraykey", 1, 2, &error);

	elektraSetLongLongArrayElement (elektra, "newlonglongarraykey", 0, 1, &error);
	elektraSetLongLongArrayElement (elektra, "newlonglongarraykey", 1, 2, &error);

	elektraSetUnsignedLongLongArrayElement (elektra, "newunsignedlonglongarraykey", 0, 1, &error);
	elektraSetUnsignedLongLongArrayElement (elektra, "newunsignedlonglongarraykey", 1, 2, &error);

	elektraSetFloatArrayElement (elektra, "newfloatarraykey", 0, 1.1, &error);
	elektraSetFloatArrayElement (elektra, "newfloatarraykey", 1, 2.1, &error);

	elektraSetDoubleArrayElement (elektra, "newdoublearraykey", 0, 1.1, &error);
	elektraSetDoubleArrayElement (elektra, "newdoublearraykey", 1, 2.1, &error);

	elektraSetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 0, 1.1L, &error);
	elektraSetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 1, 2.1L, &error);

	if (error)
	{
		yield_error ("A setter failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	// Check overwritten values.

	succeed_if (elektraArraySize (elektra, "stringarraykey") == 2, "Wrong array size");
	succeed_if (!elektraStrCmp (elektraGetStringArrayElement (elektra, "stringarraykey", 0), "String 1"), "Wrong key value.");
	succeed_if (!elektraStrCmp (elektraGetStringArrayElement (elektra, "stringarraykey", 1), "String 2"), "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "booleanarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetBooleanArrayElement (elektra, "booleanarraykey", 0) == 0, "Wrong key value.");
	succeed_if (elektraGetBooleanArrayElement (elektra, "booleanarraykey", 1), "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "chararraykey") == 2, "Wrong array size");
	succeed_if (elektraGetCharArrayElement (elektra, "chararraykey", 0) == 'c', "Wrong key value.");
	succeed_if (elektraGetCharArrayElement (elektra, "chararraykey", 1) == 'd', "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "octetarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetOctetArrayElement (elektra, "octetarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetOctetArrayElement (elektra, "octetarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "shortarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetShortArrayElement (elektra, "shortarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetShortArrayElement (elektra, "shortarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "unsignedshortarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "longarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetLongArrayElement (elektra, "longarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetLongArrayElement (elektra, "longarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "unsignedlongarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "longlongarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetLongLongArrayElement (elektra, "longlongarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetLongLongArrayElement (elektra, "longlongarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "unsignedlonglongarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 1) == 2, "Wrong key value.");

	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF_STR ("-Wfloat-equal")

	succeed_if (elektraArraySize (elektra, "floatarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetFloatArrayElement (elektra, "floatarraykey", 0) == 1.1f, "Wrong key value.");
	succeed_if (elektraGetFloatArrayElement (elektra, "floatarraykey", 1) == 2.1f, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "doublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetDoubleArrayElement (elektra, "doublearraykey", 0) == 1.1, "Wrong key value.");
	succeed_if (elektraGetDoubleArrayElement (elektra, "doublearraykey", 1) == 2.1, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "longdoublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 0) == 1.1L, "Wrong key value.");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 1) == 2.1L, "Wrong key value.");

	ELEKTRA_DIAG_RESTORE

	// Check new keys.

	succeed_if (elektraArraySize (elektra, "newstringarraykey") == 2, "Wrong array size");
	succeed_if (!elektraStrCmp (elektraGetStringArrayElement (elektra, "newstringarraykey", 0), "String 1"), "Wrong key value.");
	succeed_if (!elektraStrCmp (elektraGetStringArrayElement (elektra, "newstringarraykey", 1), "String 2"), "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newbooleanarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetBooleanArrayElement (elektra, "newbooleanarraykey", 0) == 0, "Wrong key value.");
	succeed_if (elektraGetBooleanArrayElement (elektra, "newbooleanarraykey", 1), "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newchararraykey") == 2, "Wrong array size");
	succeed_if (elektraGetCharArrayElement (elektra, "newchararraykey", 0) == 'c', "Wrong key value.");
	succeed_if (elektraGetCharArrayElement (elektra, "newchararraykey", 1) == 'd', "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newoctetarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetOctetArrayElement (elektra, "newoctetarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetOctetArrayElement (elektra, "newoctetarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newshortarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetShortArrayElement (elektra, "newshortarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetShortArrayElement (elektra, "newshortarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newunsignedshortarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetUnsignedShortArrayElement (elektra, "newunsignedshortarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedShortArrayElement (elektra, "newunsignedshortarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newlonglongarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetLongLongArrayElement (elektra, "newlonglongarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetLongLongArrayElement (elektra, "newlonglongarraykey", 1) == 2, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newunsignedlonglongarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetUnsignedLongLongArrayElement (elektra, "newunsignedlonglongarraykey", 0) == 1, "Wrong key value.");
	succeed_if (elektraGetUnsignedLongLongArrayElement (elektra, "newunsignedlonglongarraykey", 1) == 2, "Wrong key value.");

	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF_STR ("-Wfloat-equal")

	succeed_if (elektraArraySize (elektra, "newfloatarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetFloatArrayElement (elektra, "newfloatarraykey", 0) == 1.1f, "Wrong key value.");
	succeed_if (elektraGetFloatArrayElement (elektra, "newfloatarraykey", 1) == 2.1f, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newdoublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetDoubleArrayElement (elektra, "newdoublearraykey", 0) == 1.1, "Wrong key value.");
	succeed_if (elektraGetDoubleArrayElement (elektra, "newdoublearraykey", 1) == 2.1, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newlongdoublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 0) == 1.1L, "Wrong key value.");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 1) == 2.1L, "Wrong key value.");

	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

void test_defaultValues (void)
{
	KeySet * defaults =
		ksNew (0, keyNew ("user/tests/highlevel/stringkey", KEY_VALUE, "A string", KEY_META, "type", "string", KEY_END), KS_END);

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen ("/tests/highlevel", defaults, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (!elektraStrCmp (elektraGetString (elektra, "stringkey"), "A string"), "Wrong key value.");

	elektraClose (elektra);
}

void test_generic (void)
{
	const char * parentKey = "user/tests/highlevel";

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	ELEKTRA_TAG_VALUE (TEST_STRING, "stringkey", String)
	ELEKTRA_TAG_VALUE (TEST_BOOLEAN, "booleankey", Boolean)
	ELEKTRA_TAG_VALUE (TEST_CHAR, "charkey", Char)
	ELEKTRA_TAG_VALUE (TEST_OCTET, "octetkey", Octet)
	ELEKTRA_TAG_VALUE (TEST_SHORT, "shortkey", Short)
	ELEKTRA_TAG_VALUE (TEST_UNSIGNED_SHORT, "unsignedshortkey", UnsignedShort)
	ELEKTRA_TAG_VALUE (TEST_LONG, "longkey", Long)
	ELEKTRA_TAG_VALUE (TEST_UNSIGNED_LONG, "unsignedlongkey", UnsignedLong)
	ELEKTRA_TAG_VALUE (TEST_LONG_LONG, "longlongkey", LongLong)
	ELEKTRA_TAG_VALUE (TEST_UNSIGNED_LONG_LONG, "unsignedlonglongkey", UnsignedLongLong)
	ELEKTRA_TAG_VALUE (TEST_FLOAT, "floatkey", Float)
	ELEKTRA_TAG_VALUE (TEST_DOUBLE, "doublekey", Double)
	ELEKTRA_TAG_VALUE (TEST_LONG_DOUBLE, "longdoublekey", LongDouble)

	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_STRING), "A string", &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_BOOLEAN), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_CHAR), 'c', &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_OCTET), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_SHORT), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_SHORT), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_LONG), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_LONG_LONG), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_LONG_LONG), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_LONG_LONG), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_FLOAT), 1.1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_DOUBLE), 1.1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_LONG_DOUBLE), 1.1, &error);

	// Check values.
	succeed_if (!elektraStrCmp (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_STRING)), "A string"), "Wrong key value.");
	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_BOOLEAN)), "Wrong key value.");
	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_CHAR)) == 'c', "Wrong key value.");
	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_OCTET)) == 1, "Wrong key value.");
	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_SHORT)) == 1, "Wrong key value.");
	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_SHORT)) == 1, "Wrong key value.");
	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_LONG)) == 1, "Wrong key value.");
	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_LONG)) == 1, "Wrong key value.");
	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_LONG_LONG)) == 1, "Wrong key value.");
	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_LONG_LONG)) == 1, "Wrong key value.");

	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF_STR ("-Wfloat-equal")

	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_FLOAT)) == 1.1f, "Wrong key value.");
	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_DOUBLE)) == 1.1, "Wrong key value.");
	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_LONG_DOUBLE)) == 1.1L, "Wrong key value.");

	ELEKTRA_DIAG_RESTORE
}


typedef enum
{
	ELEKTRA_ENUM_TEST_ON = 1,
	ELEKTRA_ENUM_TEST_OFF = 0,
	ELEKTRA_ENUM_TEST_BLANK = 2
} ElektraEnumTest;

static int __elektraKeyToElektraEnumTest (const Key * key, ElektraEnumTest * variable)
{
	kdb_long_t longVariable;
	int result = elektraKeyToLong (key, &longVariable);
	if (result)
	{
		*variable = (ElektraEnumTest) longVariable;
	}
	return result;
}

ELEKTRA_TAG_DECLARATIONS (ElektraEnumTest, EnumTest)
ELEKTRA_TAG_DEFINITIONS (ElektraEnumTest, EnumTest, KDB_TYPE_ENUM, elektraLongToString, __elektraKeyToElektraEnumTest)

void test_enum (void)
{
	const char * parentKey = "user/tests/highlevel";

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	elektraSetEnumInt (elektra, "enumkey", ELEKTRA_ENUM_TEST_ON, &error);

	succeed_if (elektraGetEnum (elektra, "enumkey", ElektraEnumTest) == ELEKTRA_ENUM_TEST_ON, "Wrong key value.");
}

void test_enumArray (void)
{
	const char * parentKey = "user/tests/highlevel";

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	elektraSetEnumIntArrayElement (elektra, "enumkey", 0, ELEKTRA_ENUM_TEST_ON, &error);
	elektraSetEnumIntArrayElement (elektra, "enumkey", 1, ELEKTRA_ENUM_TEST_ON, &error);
	elektraSetEnumIntArrayElement (elektra, "enumkey", 2, ELEKTRA_ENUM_TEST_OFF, &error);
	elektraSetEnumIntArrayElement (elektra, "enumkey", 3, ELEKTRA_ENUM_TEST_OFF, &error);
	elektraSetEnumIntArrayElement (elektra, "enumkey", 4, ELEKTRA_ENUM_TEST_BLANK, &error);

	succeed_if (elektraGetEnumArrayElement (elektra, "enumkey", 0, ElektraEnumTest) == ELEKTRA_ENUM_TEST_ON, "Wrong key value.");
	succeed_if (elektraGetEnumArrayElement (elektra, "enumkey", 1, ElektraEnumTest) == ELEKTRA_ENUM_TEST_ON, "Wrong key value.");
	succeed_if (elektraGetEnumArrayElement (elektra, "enumkey", 2, ElektraEnumTest) == ELEKTRA_ENUM_TEST_OFF, "Wrong key value.");
	succeed_if (elektraGetEnumArrayElement (elektra, "enumkey", 3, ElektraEnumTest) == ELEKTRA_ENUM_TEST_OFF, "Wrong key value.");
	succeed_if (elektraGetEnumArrayElement (elektra, "enumkey", 4, ElektraEnumTest) == ELEKTRA_ENUM_TEST_BLANK, "Wrong key value.");
}

void test_enum_generic (void)
{
	const char * parentKey = "user/tests/highlevel";

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	ELEKTRA_TAG_VALUE (TEST_ENUM, "enumkey", EnumTest)

	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), ELEKTRA_ENUM_TEST_BLANK, &error);

	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_ENUM)) == ELEKTRA_ENUM_TEST_BLANK, "Wrong key value.");
}

void test_enumArray_generic (void)
{
	const char * parentKey = "user/tests/highlevel";

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	ELEKTRA_TAG_VALUE (TEST_ENUM, "enumkey", EnumTest)

	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 0, ELEKTRA_ENUM_TEST_ON, &error);
	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 1, ELEKTRA_ENUM_TEST_ON, &error);
	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 2, ELEKTRA_ENUM_TEST_OFF, &error);
	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 3, ELEKTRA_ENUM_TEST_OFF, &error);
	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 4, ELEKTRA_ENUM_TEST_BLANK, &error);

	succeed_if (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 0) == ELEKTRA_ENUM_TEST_ON, "Wrong key value.");
	succeed_if (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 1) == ELEKTRA_ENUM_TEST_ON, "Wrong key value.");
	succeed_if (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 2) == ELEKTRA_ENUM_TEST_OFF, "Wrong key value.");
	succeed_if (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 3) == ELEKTRA_ENUM_TEST_OFF, "Wrong key value.");
	succeed_if (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 4) == ELEKTRA_ENUM_TEST_BLANK, "Wrong key value.");
}

int main (int argc, char ** argv)
{
	init (argc, argv);

	test_primitiveGetters ();
	test_arrayGetters ();

	test_primitiveSetters ();
	test_arraySetters ();

	test_defaultValues ();

	test_generic ();

	test_enum ();
	test_enumArray ();

	test_enum_generic ();
	test_enumArray_generic ();

	printf ("\ntest_highlevel RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
