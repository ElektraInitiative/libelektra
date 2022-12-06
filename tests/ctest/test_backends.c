/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>
#include <elektra/kdbprivate.h>
#include <stdlib.h>
#include <tests.h>

static void addBackendForDivide (KeySet * backends, const char * mountpoint)
{
	BackendData data = {
		.backend = NULL,
		.keys = ksNew (0, KS_END),
		.definition = NULL,
		.plugins = NULL,
		.initialized = false,
	};
	ksAppendKey (backends, keyNew (mountpoint, KEY_BINARY, KEY_SIZE, sizeof (data), KEY_VALUE, &data, KEY_END));
}

static void deleteBackends (KeySet * backends)
{
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		BackendData * data = (BackendData *) keyValue (ksAtCursor (backends, i));
		ksDel (data->keys);
	}

	ksDel (backends);
}

// TODO [new_backend]: more thorough tests

void test_backendsDivide (void)
{
	printf ("Test backendsDivide");

	KeySet * backends = ksNew (0, KS_END);

	addBackendForDivide (backends, "dir:/");
	addBackendForDivide (backends, "user:/");
	addBackendForDivide (backends, "user:/bar");
	addBackendForDivide (backends, "user:/bar/bar");
	addBackendForDivide (backends, "user:/bar/baz");
	addBackendForDivide (backends, "user:/bar/foo");
	addBackendForDivide (backends, "user:/baz");
	addBackendForDivide (backends, "user:/foo");
	addBackendForDivide (backends, "system:/");
	addBackendForDivide (backends, "default:/");

	// clang-format off
	KeySet * ks = ksNew (20,
				keyNew ("spec:/xyz", KEY_END),
				keyNew ("user:/abc", KEY_END),
				keyNew ("user:/bak/abc", KEY_END),
				keyNew ("user:/bar/abc", KEY_END),
				keyNew ("user:/bar/bar/abc", KEY_END),
				keyNew ("user:/bar/bar/def", KEY_END),
				keyNew ("user:/bar/bar/xyz", KEY_END),
				keyNew ("user:/bar/foo/abc", KEY_END),
				keyNew ("user:/bar/foo/xyz", KEY_END),
				keyNew ("user:/bar/xyz", KEY_END),
				keyNew ("user:/foo/abc", KEY_END),
				keyNew ("user:/foo/xyz", KEY_END),
				keyNew ("user:/xyz", KEY_END),
				keyNew ("default:/xyz", KEY_END),
			     KS_END);
	// clang-format on

	succeed_if (backendsDivide (backends, ks) == 1, "couldn't split ks");

	KeySet * ks0 = ksNew (10, KS_END);
	KeySet * ks1 = ksNew (10, keyNew ("user:/abc", KEY_END), keyNew ("user:/bak/abc", KEY_END), keyNew ("user:/xyz", KEY_END), KS_END);
	KeySet * ks2 = ksNew (10, keyNew ("user:/bar/abc", KEY_END), keyNew ("user:/bar/xyz", KEY_END), KS_END);
	KeySet * ks3 = ksNew (10, keyNew ("user:/bar/bar/abc", KEY_END), keyNew ("user:/bar/bar/def", KEY_END),
			      keyNew ("user:/bar/bar/xyz", KEY_END), KS_END);
	KeySet * ks4 = ksNew (10, KS_END);
	KeySet * ks5 = ksNew (10, keyNew ("user:/bar/foo/abc", KEY_END), keyNew ("user:/bar/foo/xyz", KEY_END), KS_END);
	KeySet * ks6 = ksNew (10, KS_END);
	KeySet * ks7 = ksNew (10, keyNew ("user:/foo/abc", KEY_END), keyNew ("user:/foo/xyz", KEY_END), KS_END);
	KeySet * ks8 = ksNew (10, KS_END);
	KeySet * ks9 = ksNew (10, keyNew ("default:/xyz", KEY_END), keyNew ("spec:/xyz", KEY_END), KS_END);

	compare_keyset (ks0, ((const BackendData *) keyValue (ksLookupByName (backends, "dir:/", 0)))->keys);
	compare_keyset (ks1, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/", 0)))->keys);
	compare_keyset (ks2, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/bar", 0)))->keys);
	compare_keyset (ks3, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/bar/bar", 0)))->keys);
	compare_keyset (ks4, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/bar/baz", 0)))->keys);
	compare_keyset (ks5, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/bar/foo", 0)))->keys);
	compare_keyset (ks6, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/baz", 0)))->keys);
	compare_keyset (ks7, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/foo", 0)))->keys);
	compare_keyset (ks8, ((const BackendData *) keyValue (ksLookupByName (backends, "system:/", 0)))->keys);
	compare_keyset (ks9, ((const BackendData *) keyValue (ksLookupByName (backends, "default:/", 0)))->keys);

	succeed_if (((const BackendData *) keyValue (ksLookupByName (backends, "dir:/", 0)))->keyNeedsSync == false, "shouldn't need sync");
	succeed_if (((const BackendData *) keyValue (ksLookupByName (backends, "user:/", 0)))->keyNeedsSync == true, "should need sync");
	succeed_if (((const BackendData *) keyValue (ksLookupByName (backends, "user:/bar", 0)))->keyNeedsSync == true, "should need sync");
	succeed_if (((const BackendData *) keyValue (ksLookupByName (backends, "user:/bar/bar", 0)))->keyNeedsSync == true,
		    "should need sync");
	succeed_if (((const BackendData *) keyValue (ksLookupByName (backends, "user:/bar/baz", 0)))->keyNeedsSync == false,
		    "shouldn't need sync");
	succeed_if (((const BackendData *) keyValue (ksLookupByName (backends, "user:/bar/foo", 0)))->keyNeedsSync == true,
		    "should need sync");
	succeed_if (((const BackendData *) keyValue (ksLookupByName (backends, "user:/baz", 0)))->keyNeedsSync == false,
		    "shouldn't need sync");
	succeed_if (((const BackendData *) keyValue (ksLookupByName (backends, "user:/foo", 0)))->keyNeedsSync == true, "should need sync");
	succeed_if (((const BackendData *) keyValue (ksLookupByName (backends, "system:/", 0)))->keyNeedsSync == false,
		    "shouldn't need sync");
	succeed_if (((const BackendData *) keyValue (ksLookupByName (backends, "default:/", 0)))->keyNeedsSync == true, "should need sync");

	deleteBackends (backends);
	ksDel (ks);
	ksDel (ks0);
	ksDel (ks1);
	ksDel (ks2);
	ksDel (ks3);
	ksDel (ks4);
	ksDel (ks5);
	ksDel (ks6);
	ksDel (ks7);
	ksDel (ks8);
	ksDel (ks9);
}

int main (int argc, char ** argv)
{
	printf ("BACKENDS       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_backendsDivide ();

	printf ("\ntest_backends RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
