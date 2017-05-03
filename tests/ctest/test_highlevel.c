/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <tests_internal.h>
#include <elektra.h>

static void setKeyValue (const char * parentKeyName, const char * type, const char * name, const char * value) {
    // Open
    KeySet * config = ksNew (0, KS_END);
    Key * parentKey = keyNew (parentKeyName, KEY_END);
    KDB * handle = kdbOpen (parentKey);
    kdbGet (handle, config, parentKey);

    // Set
    Key *key = keyNew (parentKeyName, KEY_END);
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

    ElektraError *error = NULL;
    Elektra *elektra = elektraOpen(parentKey, &error);

    if (error) {
        yield_error ("elektraOpen failed");
        printf ("ElektraError: %s\n", elektraErrorDescription(error));
        elektraErrorReset(&error);
    }

    succeed_if (!elektraStrCmp(elektraGetString(elektra, "stringKey"), "A string"), "Wrong key value.");
    succeed_if (elektraGetBoolean(elektra, "booleanKey"), "Wrong key value.");
    succeed_if (elektraGetChar(elektra, "charKey") == 'c', "Wrong key value.");
    succeed_if (elektraGetOctet(elektra, "octetKey") == 1, "Wrong key value.");
    succeed_if (elektraGetShort(elektra, "shortKey") == 1, "Wrong key value.");
    succeed_if (elektraGetUnsignedShort(elektra, "unsignedShortKey") == 1, "Wrong key value.");
    succeed_if (elektraGetLong(elektra, "longKey") == 1, "Wrong key value.");
    succeed_if (elektraGetUnsignedLong(elektra, "unsignedLongKey") == 1, "Wrong key value.");
    succeed_if (elektraGetLongLong(elektra, "longLongKey") == 1, "Wrong key value.");
    succeed_if (elektraGetUnsignedLongLong(elektra, "unsignedLongLongKey") == 1, "Wrong key value.");

    ELEKTRA_DIAG_STORE
    ELEKTRA_DIAG_OFF (-Wfloat-equal)

    succeed_if (elektraGetFloat(elektra, "floatKey") == 1.1f, "Wrong key value.");
    succeed_if (elektraGetDouble(elektra, "doubleKey") == 1.1, "Wrong key value.");
    succeed_if (elektraGetLongDouble(elektra, "longDoubleKey") == 1.1L, "Wrong key value.");

    ELEKTRA_DIAG_RESTORE

    elektraClose(elektra);
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

    ElektraError *error = NULL;
    Elektra *elektra = elektraOpen(parentKey, &error);

    if (error) {
        yield_error ("elektraOpen failed");
        printf ("ElektraError: %s\n", elektraErrorDescription(error));
        elektraErrorReset(&error);
    }

    succeed_if (elektraArraySize(elektra, "stringArrayKey") == 2, "Wrong array size");
    succeed_if (!elektraStrCmp(elektraGetStringArrayElement(elektra, "stringArrayKey", 0), "String 1"), "Wrong key value.");
    succeed_if (!elektraStrCmp(elektraGetStringArrayElement(elektra, "stringArrayKey", 1), "String 2"), "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "booleanArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetBooleanArrayElement(elektra, "booleanArrayKey", 0) == 0, "Wrong key value.");
    succeed_if (elektraGetBooleanArrayElement(elektra, "booleanArrayKey", 1), "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "charArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetCharArrayElement(elektra, "charArrayKey", 0) == 'c', "Wrong key value.");
    succeed_if (elektraGetCharArrayElement(elektra, "charArrayKey", 1) == 'd', "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "octetArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetOctetArrayElement(elektra, "octetArrayKey", 0) == 1, "Wrong key value.");
    succeed_if (elektraGetOctetArrayElement(elektra, "octetArrayKey", 1) == 2, "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "shortArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetShortArrayElement(elektra, "shortArrayKey", 0) == 1, "Wrong key value.");
    succeed_if (elektraGetShortArrayElement(elektra, "shortArrayKey", 1) == 2, "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "unsignedShortArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetUnsignedShortArrayElement(elektra, "unsignedShortArrayKey", 0) == 1, "Wrong key value.");
    succeed_if (elektraGetUnsignedShortArrayElement(elektra, "unsignedShortArrayKey", 1) == 2, "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "longLongArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetLongLongArrayElement(elektra, "longLongArrayKey", 0) == 1, "Wrong key value.");
    succeed_if (elektraGetLongLongArrayElement(elektra, "longLongArrayKey", 1) == 2, "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "unsignedLongLongArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetUnsignedLongLongArrayElement(elektra, "unsignedLongLongArrayKey", 0) == 1, "Wrong key value.");
    succeed_if (elektraGetUnsignedLongLongArrayElement(elektra, "unsignedLongLongArrayKey", 1) == 2, "Wrong key value.");

    ELEKTRA_DIAG_STORE
    ELEKTRA_DIAG_OFF (-Wfloat-equal)

    succeed_if (elektraArraySize(elektra, "floatArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetFloatArrayElement(elektra, "floatArrayKey", 0) == 1.1f, "Wrong key value.");
    succeed_if (elektraGetFloatArrayElement(elektra, "floatArrayKey", 1) == 2.1f, "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "doubleArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetDoubleArrayElement(elektra, "doubleArrayKey", 0) == 1.1, "Wrong key value.");
    succeed_if (elektraGetDoubleArrayElement(elektra, "doubleArrayKey", 1) == 2.1, "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "longDoubleArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetLongDoubleArrayElement(elektra, "longDoubleArrayKey", 0) == 1.1L, "Wrong key value.");
    succeed_if (elektraGetLongDoubleArrayElement(elektra, "longDoubleArrayKey", 1) == 2.1L, "Wrong key value.");

    ELEKTRA_DIAG_RESTORE

    elektraClose(elektra);
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

    ElektraError *error = NULL;
    Elektra *elektra = elektraOpen(parentKey, &error);

    if (error) {
        yield_error ("elektraOpen failed");
        printf ("ElektraError: %s\n", elektraErrorDescription(error));
        elektraErrorReset(&error);
    }

    elektraSetString(elektra, "stringKey", "A string");
    elektraSetBoolean(elektra, "booleanKey", 1);
    elektraSetChar(elektra, "charKey", 'c');
    elektraSetOctet(elektra, "octetKey", 1);
    elektraSetShort(elektra, "shortKey", 1);
    elektraSetUnsignedShort(elektra, "unsignedShortKey", 1);
    elektraSetLong(elektra, "longKey", 1);
    elektraSetUnsignedLong(elektra, "unsignedLongKey", 1);
    elektraSetLongLong(elektra, "longLongKey", 1);
    elektraSetUnsignedLongLong(elektra, "unsignedLongLongKey", 1);
    elektraSetFloat(elektra, "floatKey", 1.1);
    elektraSetDouble(elektra, "doubleKey", 1.1);
    elektraSetLongDouble(elektra, "longDoubleKey", 1.1);

    succeed_if (!elektraStrCmp(elektraGetString(elektra, "stringKey"), "A string"), "Wrong key value.");
    succeed_if (elektraGetBoolean(elektra, "booleanKey"), "Wrong key value.");
    succeed_if (elektraGetChar(elektra, "charKey") == 'c', "Wrong key value.");
    succeed_if (elektraGetOctet(elektra, "octetKey") == 1, "Wrong key value.");
    succeed_if (elektraGetShort(elektra, "shortKey") == 1, "Wrong key value.");
    succeed_if (elektraGetUnsignedShort(elektra, "unsignedShortKey") == 1, "Wrong key value.");
    succeed_if (elektraGetLong(elektra, "longKey") == 1, "Wrong key value.");
    succeed_if (elektraGetUnsignedLong(elektra, "unsignedLongKey") == 1, "Wrong key value.");
    succeed_if (elektraGetLongLong(elektra, "longLongKey") == 1, "Wrong key value.");
    succeed_if (elektraGetUnsignedLongLong(elektra, "unsignedLongLongKey") == 1, "Wrong key value.");

    ELEKTRA_DIAG_STORE
    ELEKTRA_DIAG_OFF (-Wfloat-equal)

    succeed_if (elektraGetFloat(elektra, "floatKey") == 1.1f, "Wrong key value.");
    succeed_if (elektraGetDouble(elektra, "doubleKey") == 1.1, "Wrong key value.");
    succeed_if (elektraGetLongDouble(elektra, "longDoubleKey") == 1.1L, "Wrong key value.");

    ELEKTRA_DIAG_RESTORE

    elektraClose(elektra);
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

    ElektraError *error = NULL;
    Elektra *elektra = elektraOpen(parentKey, &error);

    if (error) {
        yield_error ("elektraOpen failed");
        printf ("ElektraError: %s\n", elektraErrorDescription(error));
        elektraErrorReset(&error);
    }

    elektraSetStringArrayElement(elektra, "stringArrayKey", "String 1", 0);
    elektraSetStringArrayElement(elektra, "stringArrayKey", "String 2", 1);

    elektraSetBooleanArrayElement(elektra, "booleanArrayKey", 0, 0);
    elektraSetBooleanArrayElement(elektra, "booleanArrayKey", 1, 1);

    elektraSetCharArrayElement(elektra, "charArrayKey", 'c', 0);
    elektraSetCharArrayElement(elektra, "charArrayKey", 'd', 1);

    elektraSetOctetArrayElement(elektra, "octetArrayKey", 1, 0);
    elektraSetOctetArrayElement(elektra, "octetArrayKey", 2, 1);

    elektraSetShortArrayElement(elektra, "shortArrayKey", 1, 0);
    elektraSetShortArrayElement(elektra, "shortArrayKey", 2, 1);

    elektraSetUnsignedShortArrayElement(elektra, "unsignedShortArrayKey", 1, 0);
    elektraSetUnsignedShortArrayElement(elektra, "unsignedShortArrayKey", 2, 1);

    elektraSetLongArrayElement(elektra, "longArrayKey", 1, 0);
    elektraSetLongArrayElement(elektra, "longArrayKey", 2, 1);

    elektraSetUnsignedLongArrayElement(elektra, "unsignedLongArrayKey", 1, 0);
    elektraSetUnsignedLongArrayElement(elektra, "unsignedLongArrayKey", 2, 1);

    elektraSetLongLongArrayElement(elektra, "longLongArrayKey", 1, 0);
    elektraSetLongLongArrayElement(elektra, "longLongArrayKey", 2, 1);

    elektraSetUnsignedLongLongArrayElement(elektra, "unsignedLongLongArrayKey", 1, 0);
    elektraSetUnsignedLongLongArrayElement(elektra, "unsignedLongLongArrayKey", 2, 1);

    elektraSetFloatArrayElement(elektra, "floatArrayKey", 1.1, 0);
    elektraSetFloatArrayElement(elektra, "floatArrayKey", 2.1, 1);

    elektraSetDoubleArrayElement(elektra, "doubleArrayKey", 1.1, 0);
    elektraSetDoubleArrayElement(elektra, "doubleArrayKey", 2.1, 1);

    elektraSetLongDoubleArrayElement(elektra, "longDoubleArrayKey", 1.1, 0);
    elektraSetLongDoubleArrayElement(elektra, "longDoubleArrayKey", 2.1, 1);

    succeed_if (elektraArraySize(elektra, "stringArrayKey") == 2, "Wrong array size");
    succeed_if (!elektraStrCmp(elektraGetStringArrayElement(elektra, "stringArrayKey", 0), "String 1"), "Wrong key value.");
    succeed_if (!elektraStrCmp(elektraGetStringArrayElement(elektra, "stringArrayKey", 1), "String 2"), "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "booleanArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetBooleanArrayElement(elektra, "booleanArrayKey", 0) == 0, "Wrong key value.");
    succeed_if (elektraGetBooleanArrayElement(elektra, "booleanArrayKey", 1), "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "charArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetCharArrayElement(elektra, "charArrayKey", 0) == 'c', "Wrong key value.");
    succeed_if (elektraGetCharArrayElement(elektra, "charArrayKey", 1) == 'd', "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "octetArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetOctetArrayElement(elektra, "octetArrayKey", 0) == 1, "Wrong key value.");
    succeed_if (elektraGetOctetArrayElement(elektra, "octetArrayKey", 1) == 2, "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "shortArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetShortArrayElement(elektra, "shortArrayKey", 0) == 1, "Wrong key value.");
    succeed_if (elektraGetShortArrayElement(elektra, "shortArrayKey", 1) == 2, "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "unsignedShortArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetUnsignedShortArrayElement(elektra, "unsignedShortArrayKey", 0) == 1, "Wrong key value.");
    succeed_if (elektraGetUnsignedShortArrayElement(elektra, "unsignedShortArrayKey", 1) == 2, "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "longLongArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetLongLongArrayElement(elektra, "longLongArrayKey", 0) == 1, "Wrong key value.");
    succeed_if (elektraGetLongLongArrayElement(elektra, "longLongArrayKey", 1) == 2, "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "unsignedLongLongArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetUnsignedLongLongArrayElement(elektra, "unsignedLongLongArrayKey", 0) == 1, "Wrong key value.");
    succeed_if (elektraGetUnsignedLongLongArrayElement(elektra, "unsignedLongLongArrayKey", 1) == 2, "Wrong key value.");

    ELEKTRA_DIAG_STORE
    ELEKTRA_DIAG_OFF (-Wfloat-equal)

    succeed_if (elektraArraySize(elektra, "floatArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetFloatArrayElement(elektra, "floatArrayKey", 0) == 1.1f, "Wrong key value.");
    succeed_if (elektraGetFloatArrayElement(elektra, "floatArrayKey", 1) == 2.1f, "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "doubleArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetDoubleArrayElement(elektra, "doubleArrayKey", 0) == 1.1, "Wrong key value.");
    succeed_if (elektraGetDoubleArrayElement(elektra, "doubleArrayKey", 1) == 2.1, "Wrong key value.");

    succeed_if (elektraArraySize(elektra, "longDoubleArrayKey") == 2, "Wrong array size");
    succeed_if (elektraGetLongDoubleArrayElement(elektra, "longDoubleArrayKey", 0) == 1.1L, "Wrong key value.");
    succeed_if (elektraGetLongDoubleArrayElement(elektra, "longDoubleArrayKey", 1) == 2.1L, "Wrong key value.");

    ELEKTRA_DIAG_RESTORE

    elektraClose(elektra);
}

int main (int argc, char ** argv)
{
	init (argc, argv);

	test_primitiveGetters ();
    test_arrayGetters ();

    test_primitiveSetters ();
    test_arraySetters ();

	printf ("\ntest_highlevel RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
