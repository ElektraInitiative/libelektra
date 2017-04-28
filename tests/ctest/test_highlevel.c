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

static void test_getters ()
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

    setKeyValue (parentKey, "string", "stringArrayKey/#0", "A string in an array");
    setKeyValue (parentKey, "string", "stringArrayKey/#1", "Another string in an array");

    ElektraError *error = NULL;
    Elektra *elektra = elektraOpen(parentKey, &error);

    if (error) {
        yield_error ("elektraOpen failed");
        printf ("ElektraError: %s\n", elektraErrorDescription(error));
        elektraErrorFree(error);
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

    succeed_if (elektraArraySize(elektra, "stringArrayKey") == 2, "Wrong array size");
    succeed_if (!elektraStrCmp(elektraGetStringArrayElement(elektra, "stringArrayKey", 0), "A string in an array"), "Wrong key value.");
}

int main (int argc, char ** argv)
{
	init (argc, argv);

	test_getters ();

	printf ("\ntest_highlevel RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
