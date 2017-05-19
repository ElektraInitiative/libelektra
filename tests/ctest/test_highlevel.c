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
	ELEKTRA_DIAG_OFF (-Wfloat - equal)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"

	succeed_if (elektraGetFloat (elektra, "floatkey") == 1.1f, "Wrong key value.");
	succeed_if (elektraGetDouble (elektra, "doublekey") == 1.1, "Wrong key value.");
	succeed_if (elektraGetLongDouble (elektra, "longdoublekey") == 1.1L, "Wrong key value.");
#pragma clang diagnostic pop
	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

static void test_arrayGetters ()
{
	const char * parentKey = "user/test/sw/elektra/kdb/#0/current";

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
	ELEKTRA_DIAG_OFF (-Wfloat - equal)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"

	succeed_if (elektraArraySize (elektra, "floatarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetFloatArrayElement (elektra, "floatarraykey", 0) == 1.1f, "Wrong key value.");
	succeed_if (elektraGetFloatArrayElement (elektra, "floatarraykey", 1) == 2.1f, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "doublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetDoubleArrayElement (elektra, "doublearraykey", 0) == 1.1, "Wrong key value.");
	succeed_if (elektraGetDoubleArrayElement (elektra, "doublearraykey", 1) == 2.1, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "longdoublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 0) == 1.1L, "Wrong key value.");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 1) == 2.1L, "Wrong key value.");

#pragma clang diagnostic pop
	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

static void test_primitiveSetters ()
{
	const char * parentKey = "user/test/sw/elektra/kdb/#0/current";

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
	elektraSetLongDouble (elektra, "longdoublekey", 1.1, &error);

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
	elektraSetLongDouble (elektra, "", 1.1, &error);

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
	ELEKTRA_DIAG_OFF (-Wfloat - equal)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"
	succeed_if (elektraGetFloat (elektra, "floatkey") == 1.1f, "Wrong key value.");
	succeed_if (elektraGetDouble (elektra, "doublekey") == 1.1, "Wrong key value.");
	succeed_if (elektraGetLongDouble (elektra, "longdoublekey") == 1.1L, "Wrong key value.");
#pragma clang diagnostic pop
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
	ELEKTRA_DIAG_OFF (-Wfloat - equal)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"
	succeed_if (elektraGetFloat (elektra, "newfloatkey") == 1.1f, "Wrong key value.");
	succeed_if (elektraGetDouble (elektra, "newdoublekey") == 1.1, "Wrong key value.");
	succeed_if (elektraGetLongDouble (elektra, "") == 1.1L, "Wrong key value.");
#pragma clang diagnostic pop
	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

static void test_arraySetters ()
{
	const char * parentKey = "user/test/sw/elektra/kdb/#0/current";

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

	elektraSetStringArrayElement (elektra, "stringarraykey", "String 1", 0, &error);
	elektraSetStringArrayElement (elektra, "stringarraykey", "String 2", 1, &error);

	elektraSetBooleanArrayElement (elektra, "booleanarraykey", 0, 0, &error);
	elektraSetBooleanArrayElement (elektra, "booleanarraykey", 1, 1, &error);

	elektraSetCharArrayElement (elektra, "chararraykey", 'c', 0, &error);
	elektraSetCharArrayElement (elektra, "chararraykey", 'd', 1, &error);

	elektraSetOctetArrayElement (elektra, "octetarraykey", 1, 0, &error);
	elektraSetOctetArrayElement (elektra, "octetarraykey", 2, 1, &error);

	elektraSetShortArrayElement (elektra, "shortarraykey", 1, 0, &error);
	elektraSetShortArrayElement (elektra, "shortarraykey", 2, 1, &error);

	elektraSetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 1, 0, &error);
	elektraSetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 2, 1, &error);

	elektraSetLongArrayElement (elektra, "longarraykey", 1, 0, &error);
	elektraSetLongArrayElement (elektra, "longarraykey", 2, 1, &error);

	elektraSetUnsignedLongArrayElement (elektra, "unsignedLongarraykey", 1, 0, &error);
	elektraSetUnsignedLongArrayElement (elektra, "unsignedLongarraykey", 2, 1, &error);

    elektraSetLongArrayElement (elektra, "longarraykey", 1, 0, &error);
    elektraSetLongArrayElement (elektra, "longarraykey", 2, 1, &error);

    elektraSetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 1, 0, &error);
    elektraSetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 2, 1, &error);

	elektraSetLongLongArrayElement (elektra, "longlongarraykey", 1, 0, &error);
	elektraSetLongLongArrayElement (elektra, "longlongarraykey", 2, 1, &error);

	elektraSetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 1, 0, &error);
	elektraSetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 2, 1, &error);

	elektraSetFloatArrayElement (elektra, "floatarraykey", 1.1, 0, &error);
	elektraSetFloatArrayElement (elektra, "floatarraykey", 2.1, 1, &error);

	elektraSetDoubleArrayElement (elektra, "doublearraykey", 1.1, 0, &error);
	elektraSetDoubleArrayElement (elektra, "doublearraykey", 2.1, 1, &error);

	elektraSetLongDoubleArrayElement (elektra, "longdoublearraykey", 1.1, 0, &error);
	elektraSetLongDoubleArrayElement (elektra, "longdoublearraykey", 2.1, 1, &error);

	// Add new keys.

	elektraSetStringArrayElement (elektra, "newstringarraykey", "String 1", 0, &error);
	elektraSetStringArrayElement (elektra, "newstringarraykey", "String 2", 1, &error);

	elektraSetBooleanArrayElement (elektra, "newbooleanarraykey", 0, 0, &error);
	elektraSetBooleanArrayElement (elektra, "newbooleanarraykey", 1, 1, &error);

	elektraSetCharArrayElement (elektra, "newchararraykey", 'c', 0, &error);
	elektraSetCharArrayElement (elektra, "newchararraykey", 'd', 1, &error);

	elektraSetOctetArrayElement (elektra, "newoctetarraykey", 1, 0, &error);
	elektraSetOctetArrayElement (elektra, "newoctetarraykey", 2, 1, &error);

	elektraSetShortArrayElement (elektra, "newshortarraykey", 1, 0, &error);
	elektraSetShortArrayElement (elektra, "newshortarraykey", 2, 1, &error);

	elektraSetUnsignedShortArrayElement (elektra, "newunsignedshortarraykey", 1, 0, &error);
	elektraSetUnsignedShortArrayElement (elektra, "newunsignedshortarraykey", 2, 1, &error);

	elektraSetLongArrayElement (elektra, "newlongarraykey", 1, 0, &error);
	elektraSetLongArrayElement (elektra, "newlongarraykey", 2, 1, &error);

	elektraSetUnsignedLongArrayElement (elektra, "newunsignedLongarraykey", 1, 0, &error);
	elektraSetUnsignedLongArrayElement (elektra, "newunsignedLongarraykey", 2, 1, &error);

    elektraSetLongArrayElement (elektra, "newlongarraykey", 1, 0, &error);
    elektraSetLongArrayElement (elektra, "newlongarraykey", 2, 1, &error);

    elektraSetUnsignedLongArrayElement (elektra, "newunsignedlongarraykey", 1, 0, &error);
    elektraSetUnsignedLongArrayElement (elektra, "newunsignedlongarraykey", 2, 1, &error);

	elektraSetLongLongArrayElement (elektra, "newlonglongarraykey", 1, 0, &error);
	elektraSetLongLongArrayElement (elektra, "newlonglongarraykey", 2, 1, &error);

	elektraSetUnsignedLongLongArrayElement (elektra, "newunsignedlonglongarraykey", 1, 0, &error);
	elektraSetUnsignedLongLongArrayElement (elektra, "newunsignedlonglongarraykey", 2, 1, &error);

	elektraSetFloatArrayElement (elektra, "newfloatarraykey", 1.1, 0, &error);
	elektraSetFloatArrayElement (elektra, "newfloatarraykey", 2.1, 1, &error);

	elektraSetDoubleArrayElement (elektra, "newdoublearraykey", 1.1, 0, &error);
	elektraSetDoubleArrayElement (elektra, "newdoublearraykey", 2.1, 1, &error);

	elektraSetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 1.1, 0, &error);
	elektraSetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 2.1, 1, &error);

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
	ELEKTRA_DIAG_OFF (-Wfloat - equal)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"

	succeed_if (elektraArraySize (elektra, "floatarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetFloatArrayElement (elektra, "floatarraykey", 0) == 1.1f, "Wrong key value.");
	succeed_if (elektraGetFloatArrayElement (elektra, "floatarraykey", 1) == 2.1f, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "doublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetDoubleArrayElement (elektra, "doublearraykey", 0) == 1.1, "Wrong key value.");
	succeed_if (elektraGetDoubleArrayElement (elektra, "doublearraykey", 1) == 2.1, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "longdoublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 0) == 1.1L, "Wrong key value.");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 1) == 2.1L, "Wrong key value.");

#pragma clang diagnostic pop
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
	ELEKTRA_DIAG_OFF (-Wfloat - equal)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"

	succeed_if (elektraArraySize (elektra, "newfloatarraykey") == 2, "Wrong array size");
	succeed_if (elektraGetFloatArrayElement (elektra, "newfloatarraykey", 0) == 1.1f, "Wrong key value.");
	succeed_if (elektraGetFloatArrayElement (elektra, "newfloatarraykey", 1) == 2.1f, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newdoublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetDoubleArrayElement (elektra, "newdoublearraykey", 0) == 1.1, "Wrong key value.");
	succeed_if (elektraGetDoubleArrayElement (elektra, "newdoublearraykey", 1) == 2.1, "Wrong key value.");

	succeed_if (elektraArraySize (elektra, "newlongdoublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 0) == 1.1L, "Wrong key value.");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 1) == 2.1L, "Wrong key value.");

#pragma clang diagnostic pop
	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

void test_defaultValues ()
{
	KeySet * defaults = ksNew (
		0, keyNew ("user/test/sw/elektra/kdb/#0/current/stringkey", KEY_VALUE, "A string", KEY_META, "type", "string", KEY_END),
		KS_END);

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen ("user/test/sw/elektra/kdb/#0/current", defaults, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (!elektraStrCmp (elektraGetString (elektra, "stringkey"), "A string"), "Wrong key value.");

	elektraClose (elektra);
}

void test_generic ()
{
	const char * parentKey = "user/test/sw/elektra/kdb/#0/current";

	setKeyValue (parentKey, "string", "stringkey", "A string");

	ElektraError * error = NULL;
	Elektra * elektra = elektraOpen (parentKey, NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	ElektraStringTag stringTag = { "stringkey" };
	ElektraBooleanTag booleanTag = { "booleankey" };
	ElektraCharTag charTag = { "charkey" };
	ElektraOctetTag octetTag = { "octetkey" };
	ElektraShortTag shortTag = { "shortkey" };
	ElektraUnsignedShortTag unsignedShortTag = { "unsignedshortkey" };
	ElektraLongTag longTag = { "longkey" };
	ElektraUnsignedLongTag unsignedLongTag = { "unsignedlongkey" };
	ElektraLongLongTag longLongTag = { "longlongkey" };
	ElektraUnsignedLongLongTag unsignedLongLongTag = { "unsignedlonglongkey" };
	ElektraFloatTag floatTag = { "floatkey" };
	ElektraDoubleTag doubleTag = { "doublekey" };
	ElektraLongDoubleTag longDoubleTag = { "longdoublekey" };

	elektraSet (elektra, stringTag, "A string", &error);
	elektraSet (elektra, booleanTag, 1, &error);
	elektraSet (elektra, charTag, 'c', &error);
	elektraSet (elektra, octetTag, 1, &error);
	elektraSet (elektra, shortTag, 1, &error);
	elektraSet (elektra, unsignedShortTag, 1, &error);
	elektraSet (elektra, longTag, 1, &error);
	elektraSet (elektra, unsignedLongTag, 1, &error);
	elektraSet (elektra, longLongTag, 1, &error);
	elektraSet (elektra, unsignedLongLongTag, 1, &error);
	elektraSet (elektra, floatTag, 1.1, &error);
	elektraSet (elektra, doubleTag, 1.1, &error);
	elektraSet (elektra, longDoubleTag, 1.1, &error);

	// Check values.
	succeed_if (!elektraStrCmp (elektraGet (elektra, stringTag), "A string"), "Wrong key value.");
	succeed_if (elektraGet (elektra, booleanTag), "Wrong key value.");
	succeed_if (elektraGet (elektra, charTag) == 'c', "Wrong key value.");
	succeed_if (elektraGet (elektra, octetTag) == 1, "Wrong key value.");
	succeed_if (elektraGet (elektra, shortTag) == 1, "Wrong key value.");
	succeed_if (elektraGet (elektra, unsignedShortTag) == 1, "Wrong key value.");
	succeed_if (elektraGet (elektra, longTag) == 1, "Wrong key value.");
	succeed_if (elektraGet (elektra, unsignedLongTag) == 1, "Wrong key value.");
	succeed_if (elektraGet (elektra, longLongTag) == 1, "Wrong key value.");
	succeed_if (elektraGet (elektra, unsignedLongLongTag) == 1, "Wrong key value.");
	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF (-Wfloat - equal)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfloat-equal"
	succeed_if (elektraGet (elektra, floatTag) == 1.1f, "Wrong key value.");
	succeed_if (elektraGet (elektra, doubleTag) == 1.1, "Wrong key value.");
	succeed_if (elektraGet (elektra, longDoubleTag) == 1.1L, "Wrong key value.");
#pragma clang diagnostic pop
	ELEKTRA_DIAG_RESTORE
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

	printf ("\ntest_highlevel RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
