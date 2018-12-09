/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <elektra.h>
#include <elektra_conversion.h>
#include <tests_internal.h>

static const char * parentKeyName = "user/tests/highlevel";
static char * test_name = "";
static int test_line = 0;

static void fatalErrorHandler (ElektraError * error)
{
	nbError++;
	printf ("%s:%d: fatal error in test %s: %s \n", __FILE__, test_line, test_name, elektraErrorDescription (error));
	ElektraKDBError * kdbError = elektraErrorLowLevelError (error);
	if (kdbError != NULL)
	{
		const char * severityString;
		switch (elektraKDBErrorSeverity (kdbError))
		{
		case ELEKTRA_ERROR_SEVERITY_ERROR:
			severityString = "ERROR";
			break;
		case ELEKTRA_ERROR_SEVERITY_WARNING:
			severityString = "WARNING";
			break;
		default:
		case ELEKTRA_ERROR_SEVERITY_FATAL:
			severityString = "FATAL";
			break;
		}

		printf ("\tKDB %s %d [%s/%s]: %s\n", severityString, elektraKDBErrorCode (kdbError), elektraKDBErrorGroup (kdbError),
			elektraKDBErrorModule (kdbError), elektraKDBErrorDescription (kdbError));
		printf ("\t\tReason: %s\n", elektraKDBErrorReason (kdbError));
		int warningCount = elektraKDBErrorWarningCount (kdbError);
		printf ("\t\t%d Warnings:\n", warningCount);
		const ElektraKDBError ** warnings = elektraKDBErrorWarnings (kdbError);
		for (int i = 0; i < warningCount; ++i)
		{
			printf ("\t\t - Warning %d [%s/%s]: %s\n", elektraKDBErrorCode (warnings[i]), elektraKDBErrorGroup (warnings[i]),
				elektraKDBErrorModule (warnings[i]), elektraKDBErrorDescription (warnings[i]));
		}
		printf ("\t\tFrom Key: %s\n", keyName (elektraKDBErrorKey (kdbError)));
	}

	yield_error ("fatal");
	ElektraErrorCode code = elektraErrorCode (error);
	elektraFree (error);
	exit (code);
}

#define SETUP_TEST(NAME)                                                                                                                   \
	test_line = __LINE__ - 2;                                                                                                          \
	test_name = "test_" #NAME;                                                                                                         \
	resetKDB ();

static Elektra * createElektra (KeySet * defaults, ElektraError ** error)
{
	Elektra * elektra = elektraOpen ("user/tests/highlevel", defaults, error);

	if (error == NULL)
	{
		elektraFatalErrorHandler (elektra, &fatalErrorHandler);
	}

	return elektra;
}

static void setKeyValue (KDBType type, const char * name, const char * value)
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

static void resetKDB (void)
{
	Key * parentKey = keyNew (parentKeyName, KEY_END);
	KDB * handle = kdbOpen (parentKey);

	KeySet * conf = ksNew (0, KS_END);
	kdbGet (handle, conf, parentKey);

	ksDel (ksCut (conf, parentKey));

	kdbSet (handle, conf, parentKey);

	ksDel (conf);

	kdbClose (handle, parentKey);
	keyDel (parentKey);
}

static inline const char * severityString (ElektraError * error)
{
	switch (elektraErrorSeverity (error))
	{
	case ELEKTRA_ERROR_SEVERITY_ERROR:
		return "ERROR";
	case ELEKTRA_ERROR_SEVERITY_WARNING:
		return "WARNING";
	default:
	case ELEKTRA_ERROR_SEVERITY_FATAL:
		return "FATAL";
	}
}

static void test_primitiveGetters (void)
{
	SETUP_TEST (primitiveGetters);

	setKeyValue (KDB_TYPE_STRING, "stringkey", "A string");
	setKeyValue (KDB_TYPE_BOOLEAN, "booleankey", "1");
	setKeyValue (KDB_TYPE_CHAR, "charkey", "c");
	setKeyValue (KDB_TYPE_OCTET, "octetkey", "1");
	setKeyValue (KDB_TYPE_SHORT, "shortkey", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortkey", "1");
	setKeyValue (KDB_TYPE_LONG, "longkey", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG, "unsignedlongkey", "1");
	setKeyValue (KDB_TYPE_LONG_LONG, "longlongkey", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongkey", "1");
	setKeyValue (KDB_TYPE_FLOAT, "floatkey", "1.1");
	setKeyValue (KDB_TYPE_DOUBLE, "doublekey", "1.1");

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	setKeyValue (KDB_TYPE_LONG_DOUBLE, "longdoublekey", "1.1");

#endif

	ElektraError * error = NULL;
	Elektra * elektra = createElektra (NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
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

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	succeed_if (elektraGetLongDouble (elektra, "longdoublekey") == 1.1L, "Wrong key value.");

#endif

	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

static void test_arrayGetters (void)
{
	SETUP_TEST (arrayGetters);

	setKeyValue (KDB_TYPE_STRING, "stringarraykey/#0", "String 1");
	setKeyValue (KDB_TYPE_STRING, "stringarraykey/#1", "String 2");

	setKeyValue (KDB_TYPE_BOOLEAN, "booleanarraykey/#0", "0");
	setKeyValue (KDB_TYPE_BOOLEAN, "booleanarraykey/#1", "1");

	setKeyValue (KDB_TYPE_CHAR, "chararraykey/#0", "c");
	setKeyValue (KDB_TYPE_CHAR, "chararraykey/#1", "d");

	setKeyValue (KDB_TYPE_OCTET, "octetarraykey/#0", "1");
	setKeyValue (KDB_TYPE_OCTET, "octetarraykey/#1", "2");

	setKeyValue (KDB_TYPE_SHORT, "shortarraykey/#0", "1");
	setKeyValue (KDB_TYPE_SHORT, "shortarraykey/#1", "2");

	setKeyValue (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortarraykey/#0", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortarraykey/#1", "2");

	setKeyValue (KDB_TYPE_LONG, "longarraykey/#0", "1");
	setKeyValue (KDB_TYPE_LONG, "longarraykey/#1", "2");

	setKeyValue (KDB_TYPE_UNSIGNED_LONG, "unsignedLongarraykey/#0", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG, "unsignedLongarraykey/#1", "2");

	setKeyValue (KDB_TYPE_LONG_LONG, "longlongarraykey/#0", "1");
	setKeyValue (KDB_TYPE_LONG_LONG, "longlongarraykey/#1", "2");

	setKeyValue (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongarraykey/#0", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongarraykey/#1", "2");

	setKeyValue (KDB_TYPE_FLOAT, "floatarraykey/#0", "1.1");
	setKeyValue (KDB_TYPE_FLOAT, "floatarraykey/#1", "2.1");

	setKeyValue (KDB_TYPE_DOUBLE, "doublearraykey/#0", "1.1");
	setKeyValue (KDB_TYPE_DOUBLE, "doublearraykey/#1", "2.1");

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	setKeyValue (KDB_TYPE_LONG_DOUBLE, "longdoublearraykey/#0", "1.1");
	setKeyValue (KDB_TYPE_LONG_DOUBLE, "longdoublearraykey/#1", "2.1");

#endif

	ElektraError * error = NULL;
	Elektra * elektra = createElektra (NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
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

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	succeed_if (elektraArraySize (elektra, "longdoublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 0) == 1.1L, "Wrong key value.");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 1) == 2.1L, "Wrong key value.");

#endif

	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

static void test_primitiveSetters (void)
{
	SETUP_TEST (primitiveSetters);

	setKeyValue (KDB_TYPE_STRING, "stringkey", "");
	setKeyValue (KDB_TYPE_BOOLEAN, "booleankey", "");
	setKeyValue (KDB_TYPE_CHAR, "charkey", "");
	setKeyValue (KDB_TYPE_OCTET, "octetkey", "");
	setKeyValue (KDB_TYPE_SHORT, "shortkey", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortkey", "1");
	setKeyValue (KDB_TYPE_LONG, "longkey", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG, "unsignedlongkey", "1");
	setKeyValue (KDB_TYPE_LONG_LONG, "longlongkey", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongkey", "1");
	setKeyValue (KDB_TYPE_FLOAT, "floatkey", "1.1");
	setKeyValue (KDB_TYPE_DOUBLE, "doublekey", "1.1");

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	setKeyValue (KDB_TYPE_LONG_DOUBLE, "longdoublekey", "1.1");

#endif

	ElektraError * error = NULL;
	Elektra * elektra = createElektra (NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
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

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	elektraSetLongDouble (elektra, "longdoublekey", 1.1L, &error);

#endif

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

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	elektraSetLongDouble (elektra, "newlongdoublekey", 1.1L, &error);

#endif

	if (error)
	{
		yield_error ("A setter failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
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

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	succeed_if (elektraGetLongDouble (elektra, "longdoublekey") == 1.1L, "Wrong key value.");

#endif

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

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	succeed_if (elektraGetLongDouble (elektra, "newlongdoublekey") == 1.1L, "Wrong key value.");

#endif

	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

static void test_arraySetters (void)
{
	SETUP_TEST (arraySetters);

	setKeyValue (KDB_TYPE_STRING, "stringarraykey/#0", "");
	setKeyValue (KDB_TYPE_STRING, "stringarraykey/#1", "");

	setKeyValue (KDB_TYPE_BOOLEAN, "booleanarraykey/#0", "");
	setKeyValue (KDB_TYPE_BOOLEAN, "booleanarraykey/#1", "");

	setKeyValue (KDB_TYPE_CHAR, "chararraykey/#0", "");
	setKeyValue (KDB_TYPE_CHAR, "chararraykey/#1", "");

	setKeyValue (KDB_TYPE_OCTET, "octetarraykey/#0", "");
	setKeyValue (KDB_TYPE_OCTET, "octetarraykey/#1", "");

	setKeyValue (KDB_TYPE_SHORT, "shortarraykey/#0", "");
	setKeyValue (KDB_TYPE_SHORT, "shortarraykey/#1", "");

	setKeyValue (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortarraykey/#0", "");
	setKeyValue (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortarraykey/#1", "");

	setKeyValue (KDB_TYPE_LONG, "longarraykey/#0", "");
	setKeyValue (KDB_TYPE_LONG, "longarraykey/#1", "");

	setKeyValue (KDB_TYPE_UNSIGNED_LONG, "unsignedLongarraykey/#0", "");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG, "unsignedLongarraykey/#1", "");

	setKeyValue (KDB_TYPE_LONG_LONG, "longlongarraykey/#0", "");
	setKeyValue (KDB_TYPE_LONG_LONG, "longlongarraykey/#1", "");

	setKeyValue (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongarraykey/#0", "");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongarraykey/#1", "");

	setKeyValue (KDB_TYPE_FLOAT, "floatarraykey/#0", "");
	setKeyValue (KDB_TYPE_FLOAT, "floatarraykey/#1", "");

	setKeyValue (KDB_TYPE_DOUBLE, "doublearraykey/#0", "");
	setKeyValue (KDB_TYPE_DOUBLE, "doublearraykey/#1", "");

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	setKeyValue (KDB_TYPE_LONG_DOUBLE, "longdoublearraykey/#0", "");
	setKeyValue (KDB_TYPE_LONG_DOUBLE, "longdoublearraykey/#1", "");

#endif

	ElektraError * error = NULL;
	Elektra * elektra = createElektra (NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
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

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	elektraSetLongDoubleArrayElement (elektra, "longdoublearraykey", 0, 1.1L, &error);
	elektraSetLongDoubleArrayElement (elektra, "longdoublearraykey", 1, 2.1L, &error);

#endif

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

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	elektraSetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 0, 1.1L, &error);
	elektraSetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 1, 2.1L, &error);

#endif

	if (error)
	{
		yield_error ("A setter failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
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

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	succeed_if (elektraArraySize (elektra, "longdoublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 0) == 1.1L, "Wrong key value.");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 1) == 2.1L, "Wrong key value.");

#endif

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

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	succeed_if (elektraArraySize (elektra, "newlongdoublearraykey") == 2, "Wrong array size");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 0) == 1.1L, "Wrong key value.");
	succeed_if (elektraGetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 1) == 2.1L, "Wrong key value.");

#endif

	ELEKTRA_DIAG_RESTORE

	elektraClose (elektra);
}

static void test_defaultValues (void)
{
	SETUP_TEST (defaultValues);

	KeySet * defaults = ksNew (5, keyNew ("/stringkey", KEY_VALUE, "A string", KEY_META, "type", KDB_TYPE_STRING, KEY_END), KS_END);

	ElektraError * error = NULL;
	Elektra * elektra = createElektra (defaults, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (elektraStrCmp (elektraGetString (elektra, "stringkey"), "A string") == 0, "Wrong key value.");

	elektraSetString (elektra, "stringkey", "My string", &error);

	if (error)
	{
		yield_error ("elektraSet* failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (elektraStrCmp (elektraGetString (elektra, "stringkey"), "My string") == 0, "Wrong key value.");

	elektraClose (elektra);

	elektra = createElektra (defaults, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (elektraStrCmp (elektraGetString (elektra, "stringkey"), "My string") == 0, "Wrong key value.");

	elektraClose (elektra);
}

static void test_generic (void)
{
	SETUP_TEST (generic);

	ElektraError * error = NULL;
	Elektra * elektra = createElektra (NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
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

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

	ELEKTRA_TAG_VALUE (TEST_LONG_DOUBLE, "longdoublekey", LongDouble)

#endif

	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_STRING), "A string", &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_BOOLEAN), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_CHAR), 'c', &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_OCTET), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_SHORT), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_SHORT), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_LONG), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_LONG), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_LONG_LONG), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_LONG_LONG), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_FLOAT), 1.1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_DOUBLE), 1.1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_LONG_DOUBLE), 1.1, &error);

	if (error)
	{
		yield_error ("elektraSet* failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

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

	elektraClose (elektra);
}


typedef enum
{
	ELEKTRA_ENUM_TEST_ON = 1,
	ELEKTRA_ENUM_TEST_OFF = 0,
	ELEKTRA_ENUM_TEST_BLANK = 2
} ElektraEnumTest;

static int _elektraKeyToElektraEnumTest (const Key * key, ElektraEnumTest * variable)
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
ELEKTRA_TAG_DEFINITIONS (ElektraEnumTest, EnumTest, KDB_TYPE_ENUM, elektraLongToString, _elektraKeyToElektraEnumTest)

static void test_enum (void)
{
	SETUP_TEST (enum);

	ElektraError * error = NULL;
	Elektra * elektra = createElektra (NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	elektraSetEnumInt (elektra, "enumkey", ELEKTRA_ENUM_TEST_ON, &error);

	if (error)
	{
		yield_error ("elektraSet* failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (elektraGetEnumInt (elektra, "enumkey") == (int) ELEKTRA_ENUM_TEST_ON, "Wrong key value.");

	succeed_if (elektraGetEnum (elektra, "enumkey", ElektraEnumTest) == ELEKTRA_ENUM_TEST_ON, "Wrong key value.");

	elektraClose (elektra);
}

static void test_enumArray (void)
{
	SETUP_TEST (enumArray);

	ElektraError * error = NULL;
	Elektra * elektra = createElektra (NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	elektraSetEnumIntArrayElement (elektra, "enumkey", 0, ELEKTRA_ENUM_TEST_ON, &error);
	elektraSetEnumIntArrayElement (elektra, "enumkey", 1, ELEKTRA_ENUM_TEST_ON, &error);
	elektraSetEnumIntArrayElement (elektra, "enumkey", 2, ELEKTRA_ENUM_TEST_OFF, &error);
	elektraSetEnumIntArrayElement (elektra, "enumkey", 3, ELEKTRA_ENUM_TEST_OFF, &error);
	elektraSetEnumIntArrayElement (elektra, "enumkey", 4, ELEKTRA_ENUM_TEST_BLANK, &error);

	if (error)
	{
		yield_error ("elektraSet* failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (elektraGetEnumIntArrayElement (elektra, "enumkey", 0) == (int) ELEKTRA_ENUM_TEST_ON, "Wrong key value.");
	succeed_if (elektraGetEnumIntArrayElement (elektra, "enumkey", 1) == (int) ELEKTRA_ENUM_TEST_ON, "Wrong key value.");
	succeed_if (elektraGetEnumIntArrayElement (elektra, "enumkey", 2) == (int) ELEKTRA_ENUM_TEST_OFF, "Wrong key value.");
	succeed_if (elektraGetEnumIntArrayElement (elektra, "enumkey", 3) == (int) ELEKTRA_ENUM_TEST_OFF, "Wrong key value.");
	succeed_if (elektraGetEnumIntArrayElement (elektra, "enumkey", 4) == (int) ELEKTRA_ENUM_TEST_BLANK, "Wrong key value.");

	succeed_if (elektraGetEnumArrayElement (elektra, "enumkey", 0, ElektraEnumTest) == ELEKTRA_ENUM_TEST_ON, "Wrong key value.");
	succeed_if (elektraGetEnumArrayElement (elektra, "enumkey", 1, ElektraEnumTest) == ELEKTRA_ENUM_TEST_ON, "Wrong key value.");
	succeed_if (elektraGetEnumArrayElement (elektra, "enumkey", 2, ElektraEnumTest) == ELEKTRA_ENUM_TEST_OFF, "Wrong key value.");
	succeed_if (elektraGetEnumArrayElement (elektra, "enumkey", 3, ElektraEnumTest) == ELEKTRA_ENUM_TEST_OFF, "Wrong key value.");
	succeed_if (elektraGetEnumArrayElement (elektra, "enumkey", 4, ElektraEnumTest) == ELEKTRA_ENUM_TEST_BLANK, "Wrong key value.");

	elektraClose (elektra);
}

static void test_enum_generic (void)
{
	SETUP_TEST (enum_generic);

	ElektraError * error = NULL;
	Elektra * elektra = createElektra (NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	ELEKTRA_TAG_VALUE (TEST_ENUM, "enumkey", EnumTest)

	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), ELEKTRA_ENUM_TEST_BLANK, &error);

	if (error)
	{
		yield_error ("elektraSet* failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_ENUM)) == ELEKTRA_ENUM_TEST_BLANK, "Wrong key value.");

	elektraClose (elektra);
}

static void test_enumArray_generic (void)
{
	SETUP_TEST (enumArray_generic);

	ElektraError * error = NULL;
	Elektra * elektra = createElektra (NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	ELEKTRA_TAG_VALUE (TEST_ENUM, "enumkey", EnumTest)

	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 0, ELEKTRA_ENUM_TEST_ON, &error);
	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 1, ELEKTRA_ENUM_TEST_ON, &error);
	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 2, ELEKTRA_ENUM_TEST_OFF, &error);
	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 3, ELEKTRA_ENUM_TEST_OFF, &error);
	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 4, ELEKTRA_ENUM_TEST_BLANK, &error);

	if (error)
	{
		yield_error ("elektraSet* failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 0) == ELEKTRA_ENUM_TEST_ON, "Wrong key value.");
	succeed_if (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 1) == ELEKTRA_ENUM_TEST_ON, "Wrong key value.");
	succeed_if (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 2) == ELEKTRA_ENUM_TEST_OFF, "Wrong key value.");
	succeed_if (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 3) == ELEKTRA_ENUM_TEST_OFF, "Wrong key value.");
	succeed_if (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 4) == ELEKTRA_ENUM_TEST_BLANK, "Wrong key value.");

	elektraClose (elektra);
}

static void test_raw (void)
{
	SETUP_TEST (raw);

	ElektraError * error = NULL;
	Elektra * elektra = createElektra (NULL, &error);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	elektraSetLong (elektra, "longkey", 2, &error);
	elektraSetFloat (elektra, "floatkey", 2.25f, &error);
	elektraSetString (elektra, "stringkey", "abc", &error);
	elektraSetLongArrayElement (elektra, "longarray", 0, 15, &error);
	elektraSetLongArrayElement (elektra, "longarray", 1, 16, &error);
	elektraSetLongArrayElement (elektra, "longarray", 2, 17, &error);
	elektraSetLongArrayElement (elektra, "longarray", 3, 18, &error);

	elektraSetValue (elektra, "rawshortkey", "2", "short", &error);
	elektraSetValue (elektra, "rawdoublekey", "2.25", "double", &error);
	elektraSetValue (elektra, "rawkey", "aaa", "araw", &error);
	elektraSetArrayElementValue (elektra, "rawlongarray", 0, "15", "long", &error);
	elektraSetArrayElementValue (elektra, "rawlongarray", 1, "16", "long", &error);
	elektraSetArrayElementValue (elektra, "rawarray", 4, "aadd", "ttt", &error);

	if (error)
	{
		yield_error ("elektraSet* failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	succeed_if (strcmp (elektraGetValue (elektra, "longkey"), "2") == 0, "Wrong key value.");
	succeed_if (strncmp (elektraGetValue (elektra, "floatkey"), "2.25", 4) == 0, "Wrong key value.");
	succeed_if (strcmp (elektraGetValue (elektra, "stringkey"), "abc") == 0, "Wrong key value.");

	succeed_if (strcmp (elektraGetArrayElementValue (elektra, "longarray", 0), "15") == 0, "Wrong key value.");
	succeed_if (strcmp (elektraGetArrayElementValue (elektra, "longarray", 1), "16") == 0, "Wrong key value.");
	succeed_if (strcmp (elektraGetArrayElementValue (elektra, "longarray", 2), "17") == 0, "Wrong key value.");
	succeed_if (strcmp (elektraGetArrayElementValue (elektra, "longarray", 3), "18") == 0, "Wrong key value.");

	succeed_if (elektraGetShort (elektra, "rawshortkey") == 2, "Wrong key value.");

	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF_STR ("-Wfloat-equal")

	succeed_if (elektraGetDouble (elektra, "rawdoublekey") == 2.25, "Wrong key value.");

	ELEKTRA_DIAG_RESTORE

	succeed_if (elektraGetLongArrayElement (elektra, "rawlongarray", 0) == 15, "Wrong key value.");
	succeed_if (elektraGetLongArrayElement (elektra, "rawlongarray", 1) == 16, "Wrong key value.");

	succeed_if (strcmp (elektraGetValue (elektra, "rawkey"), "aaa") == 0, "Wrong key value.");
	succeed_if (strcmp (elektraGetType (elektra, "rawkey"), "araw") == 0, "Wrong key value.");

	succeed_if (strcmp (elektraGetArrayElementType (elektra, "rawarray", 4), "ttt") == 0, "Wrong key value.");
	succeed_if (strcmp (elektraGetArrayElementValue (elektra, "rawarray", 4), "aadd") == 0, "Wrong key value.");

	elektraClose (elektra);
}

static bool enforce_failed = false;
static void test_enforceMetadataHandler (ElektraError * error)
{
	elektraFree (error);
	enforce_failed = true;
}

static void test_enforceMetadata (void)
{
	SETUP_TEST (enforceMetadata);

	ElektraError * error = NULL;
	Elektra * elektra = createElektra (NULL, &error);

	elektraFatalErrorHandler (elektra, &test_enforceMetadataHandler);

	if (error)
	{
		yield_error ("elektraOpen failed");
		printf ("ElektraError: [%s] %s\n", severityString (error), elektraErrorDescription (error));
		elektraErrorReset (&error);
	}

	elektraSetLong (elektra, "testkey", 2, &error);
	elektraSetLong (elektra, "testkey2", 1, &error);

	enforce_failed = false;
	elektraGetBoolean (elektra, "testkey");
	succeed_if (enforce_failed, "Didn't enforce type.");

	elektraEnforceTypeMetadata (elektra, false);

	succeed_if (elektraGetBoolean (elektra, "testkey") == false, "Wrong key value.");
	succeed_if (elektraGetBoolean (elektra, "testkey2") == true, "Wrong key value.");

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

	test_generic ();

	test_enum ();
	test_enumArray ();

	test_enum_generic ();
	test_enumArray_generic ();

	test_raw ();

	test_enforceMetadata ();

	printf ("\ntest_highlevel RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
