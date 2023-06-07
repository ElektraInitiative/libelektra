/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdberrors.h>
#include <tests.h>

static void test_elektraSetErrorINTERNAL_shouldAddError (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	Key * errorKey = keyNew ("/root", KEY_VALUE, "value", KEY_END);

	// Act
	elektraSetErrorINTERNAL (errorKey, "myfile.c", "117", "mymodule", "test error");

	// Assert
	KeySet * meta = keyMeta (errorKey);
	succeed_if (keyGetMeta (errorKey, "meta:/error") != NULL, "expected meta:/error to be present");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/number", "C01310");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/description", "Internal");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/module", "mymodule");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/file", "myfile.c");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/line", "117");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/mountpoint", "/root");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/configfile", "value");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/reason", "test error");

	keyDel (errorKey);
}

static void test_elektraErrorCopy_shouldCopyError (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	Key * errorKey = keyNew ("/root", KEY_VALUE, "value", KEY_END);
	elektraSetErrorINTERNAL (errorKey, "myfile.c", "117", "mymodule", "test error");
	keySetMeta (errorKey, "meta:/other", "test");

	Key * copyKey = keyNew ("/", KEY_END);

	// Act
	elektraCopyError (copyKey, errorKey);

	// Assert
	KeySet * meta = keyMeta (copyKey);
	succeed_if (keyGetMeta (copyKey, "meta:/error") != NULL, "expected meta:/error to be present");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/number", "C01310");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/description", "Internal");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/module", "mymodule");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/file", "myfile.c");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/line", "117");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/mountpoint", "/root");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/configfile", "value");
	succeed_if_keyset_contains_key_with_string (meta, "meta:/error/reason", "test error");
	succeed_if (keyGetMeta (copyKey, "meta:/other") == NULL, "should not contain meta:/other");

	keyDel (errorKey);
	keyDel (copyKey);
}

static void test_elektraCopyWarnings_shouldCopy (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	int count = 50;
	Key * warningsKey = keyNew ("/root", KEY_VALUE, "value", KEY_END);

	for (int i = 0; i < count; i++)
	{
		ELEKTRA_ADD_INTERNAL_WARNINGF (warningsKey, "Test Warning %d", i);
	}

	Key * copyKey = keyNew ("/", KEY_END);

	// Act
	elektraCopyWarnings (copyKey, warningsKey);

	// Assert
	char * formatted = elektraFormat ("#_%d", count - 1);
	succeed_if_keyset_contains_key_with_string (keyMeta (copyKey), "meta:/warnings", formatted);
	elektraFree (formatted);

	KeySet * sourceMeta = keyMeta (warningsKey);

	for (elektraCursor i = 0; i < ksGetSize (sourceMeta); i++)
	{
		Key * metaKey = ksAtCursor (sourceMeta, i);
		const char * name = keyName (metaKey);
		const char * expected = keyString (metaKey);
		succeed_if_keyset_contains_key_with_string (keyMeta (copyKey), name, expected);
	}

	keyDel (warningsKey);
	keyDel (copyKey);
}

static void test_elektraCopyWarnings_shouldAppend (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	Key * warningsKey = keyNew ("/root", KEY_VALUE, "value", KEY_END);
	ELEKTRA_ADD_INTERNAL_WARNING (warningsKey, "Test Warning 1");

	Key * copyKey = keyNew ("/", KEY_END);
	ELEKTRA_ADD_INTERNAL_WARNING (copyKey, "Existing Warning");

	// Act
	elektraCopyWarnings (copyKey, warningsKey);

	// Assert
	succeed_if_keyset_contains_key_with_string (keyMeta (copyKey), "meta:/warnings", "#1");
	succeed_if_keyset_contains_key_with_string (keyMeta (copyKey), "meta:/warnings/#0/reason", "Existing Warning");
	succeed_if_keyset_contains_key_with_string (keyMeta (copyKey), "meta:/warnings/#1/reason", "Test Warning 1");

	keyDel (warningsKey);
	keyDel (copyKey);
}

int main (int argc, char ** argv)
{
	printf ("ERRORS       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_elektraSetErrorINTERNAL_shouldAddError ();
	test_elektraErrorCopy_shouldCopyError ();
	test_elektraCopyWarnings_shouldCopy ();
	test_elektraCopyWarnings_shouldAppend ();

	printf ("\ntest_errors RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
