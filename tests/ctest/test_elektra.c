/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <tests_internal.h>
#include <elektra.h>

static void set (const char * parentKeyName, const char * name, const char * value) {
    // Open
    KeySet * config = ksNew (0, KS_END);
    Key * parentKey = keyNew (parentKeyName, KEY_END);
    KDB * handle = kdbOpen (parentKey);
    int ret = kdbGet (handle, config, parentKey);

    printf ("Error occurred: %d %s\n", ret, keyString (keyGetMeta (parentKey, "error/description")));
    printf ("File name is: %s\n", keyString(parentKey));

    // Set
    Key *key = keyNew(parentKeyName, KEY_END);
    keyAddName(key, name);
    keySetString(key, value);

    printf ("Key name is: %s\n", keyName(key));
    keySetString(key, "huhu");
    ksAppendKey(config, key);

    // Check
    Key *checkKey = ksLookupByName (config, name, 0);
    printf ("SET: value is %s\n", keyString (checkKey));

    // Save
    int result = kdbSet (handle, config, parentKey);
    printf ("SET: result is %d\n", result);
    if (result == -1) {
        Key * problemKey = ksCurrent (config);
        printf ("SET: problemKey is %s\n", keyName (problemKey));
    }

    // Close
    kdbClose (handle, parentKey);
    keyDel (parentKey);
}

static void check (const char * parentKeyName, const char * name) {
    // Open
    KeySet * config = ksNew (0, KS_END);
    Key * parentKey = keyNew (parentKeyName, KEY_END);
    KDB * handle = kdbOpen (parentKey);
    kdbGet (handle, config, parentKey);

    Key *key = keyNew(parentKeyName, KEY_END);
    keyAddName(key, name);

    // Check
    Key *checkKey = ksLookup (config, key, 0);
    printf ("CHECK: value is %s\n", keyString(checkKey));
    printf ("Key name is: %s\n", keyName(checkKey));

    // Close
    kdbClose (handle, parentKey);
    keyDel (parentKey);
}

static void test_development ()
{
    const char * parentKey = "user/test/sw/elektra/kdb/#0/current";
    const char * name = "test";
    const char * value = "1";

    set (parentKey, name, value);
    check (parentKey, name);
}

int main (int argc, char ** argv)
{
	printf ("KEY      TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_development ();

	printf ("\ntest_highlevel RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
