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

int main (int argc, char ** argv)
{
	printf ("ERRORS       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_elektraSetErrorINTERNAL_shouldAddError ();
	test_elektraErrorCopy_shouldCopyError ();

	printf ("\ntest_errors RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
