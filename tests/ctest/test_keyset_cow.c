/**
* @file
*
* @brief
*
* @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
*/

#include "kdbprivate.h"
#include <tests_internal.h>

static void ksBelow_root_in_keyset_should_work (void)
{
	printf("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("system:/root", KS_END);

	KeySet * ks = ksNew (KEYSET_SIZE, root, keyNew ("system:/root/abc", KEY_END), keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * below = ksBelow (ks, root);

	// Assert
	succeed_if_fmt(ksGetSize (below) == 2, "should contain 2 keys, was %zd", ksGetSize (below));
	succeed_if (ksLookupByName (below, "system:/root", 0) != NULL, "should contain root");
	succeed_if (ksLookupByName (below, "system:/root/abc", 0) != NULL, "should contain key below root");
	succeed_if (ksGetSize (ks) == 3, "all keys should remain in keyset");

	keyDel (root);
	ksDel (ks);
	ksDel (below);
}

static void ksBelow_key_copied_from_root_in_keyset_should_work (void)
{
	printf("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("system:/root", KS_END);
	Key * copy = keyCopy (keyNew ("/", KEY_END), root, KEY_CP_ALL);

	KeySet * ks = ksNew (KEYSET_SIZE, copy, keyNew ("system:/root/abc", KEY_END), keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * below = ksBelow (ks, root);

	// Assert
	succeed_if_fmt(ksGetSize (below) == 2, "should contain 2 keys, was %zd", ksGetSize (below));
	succeed_if (ksLookupByName (below, "system:/root", 0) != NULL, "should contain root");
	succeed_if (ksLookupByName (below, "system:/root/abc", 0) != NULL, "should contain key below root");
	succeed_if (ksGetSize (ks) == 3, "all keys should remain in keyset");

	keyDel (root);
	keyDel (copy);
	ksDel (ks);
	ksDel (below);
}

static void ksBelow_cascading_root_in_keyset_should_work (void)
{
	printf("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("/root", KS_END);

	KeySet * ks = ksNew (KEYSET_SIZE, root, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END), keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * below = ksBelow (ks, root);

	// Assert
	succeed_if_fmt(ksGetSize (below) == 3, "should contain 3 keys, was %zd", ksGetSize (below));
	succeed_if (ksLookupByName (below, "/root", 0) != NULL, "should contain root");
	succeed_if (ksLookupByName (below, "/root/def", 0) != NULL, "should contain /root/def");
	succeed_if (ksLookupByName (below, "system:/root/abc", 0) != NULL, "should contain system:/root/abc");
	succeed_if (ksGetSize (ks) == 4, "all keys should remain in keyset");

	keyDel (root);
	ksDel (ks);
	ksDel (below);
}

static void ksBelow_cascading_key_copied_from_root_in_keyset_should_work (void)
{
	printf("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("/root", KS_END);
	Key * copy = keyCopy (keyNew ("/", KEY_END), root, KEY_CP_ALL);

	KeySet * ks = ksNew (KEYSET_SIZE, copy, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END), keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * below = ksBelow (ks, root);

	// Assert
	succeed_if_fmt(ksGetSize (below) == 3, "should contain 3 keys, was %zd", ksGetSize (below));
	succeed_if (ksLookupByName (below, "/root", 0) != NULL, "should contain root");
	succeed_if (ksLookupByName (below, "/root/def", 0) != NULL, "should contain /root/def");
	succeed_if (ksLookupByName (below, "system:/root/abc", 0) != NULL, "should contain system:/root/abc");
	succeed_if (ksGetSize (ks) == 4, "all keys should remain in keyset");

	keyDel (root);
	keyDel (copy);
	ksDel (ks);
	ksDel (below);
}



static void ksCut_root_in_keyset_should_work (void)
{
	printf("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("system:/root", KS_END);

	KeySet * ks = ksNew (KEYSET_SIZE, root, keyNew ("system:/root/abc", KEY_END), keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * cut = ksCut (ks, root);

	// Assert
	succeed_if_fmt(ksGetSize (cut) == 2, "should contain 2 keys, was %zd", ksGetSize (cut));
	succeed_if (ksLookupByName (cut, "system:/root", 0) != NULL, "should contain root");
	succeed_if (ksLookupByName (cut, "system:/root/abc", 0) != NULL, "should contain key below root");
	succeed_if (ksGetSize (ks) == 1, "only 1 key should remain in keyset");
	succeed_if (ksLookupByName (ks, "system:/other", 0) != NULL, "ks should contain other key");

	keyDel (root);
	ksDel (ks);
	ksDel (cut);
}

static void ksCut_key_copied_from_root_in_keyset_should_work (void)
{
	printf("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("system:/root", KS_END);
	Key * copy = keyCopy (keyNew ("/", KEY_END), root, KEY_CP_ALL);

	KeySet * ks = ksNew (KEYSET_SIZE, copy, keyNew ("system:/root/abc", KEY_END), keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * cut = ksCut (ks, root);

	// Assert
	succeed_if_fmt(ksGetSize (cut) == 2, "should contain 2 keys, was %zd", ksGetSize (cut));
	succeed_if (ksLookupByName (cut, "system:/root", 0) != NULL, "should contain root");
	succeed_if (ksLookupByName (cut, "system:/root/abc", 0) != NULL, "should contain key below root");
	succeed_if (ksGetSize (ks) == 1, "only 1 key should remain in keyset");
	succeed_if (ksLookupByName (ks, "system:/other", 0) != NULL, "ks should contain other key");

	keyDel (root);
	keyDel (copy);
	ksDel (ks);
	ksDel (cut);
}

static void ksCut_cascading_root_in_keyset_should_work (void)
{
	printf("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("/root", KS_END);

	KeySet * ks = ksNew (KEYSET_SIZE, root, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END), keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * cut = ksCut (ks, root);

	// Assert
	succeed_if_fmt(ksGetSize (cut) == 3, "should contain 3 keys, was %zd", ksGetSize (cut));
	succeed_if (ksLookupByName (cut, "/root", 0) != NULL, "should contain root");
	succeed_if (ksLookupByName (cut, "/root/def", 0) != NULL, "should contain /root/def");
	succeed_if (ksLookupByName (cut, "system:/root/abc", 0) != NULL, "should contain system:/root/abc");
	succeed_if (ksGetSize (ks) == 1, "only 1 key should remain in keyset");
	succeed_if (ksLookupByName (ks, "system:/other", 0) != NULL, "ks should contain other key");

	keyDel (root);
	ksDel (ks);
	ksDel (cut);
}

static void ksCut_cascading_key_copied_from_root_in_keyset_should_work (void)
{
	printf("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("/root", KS_END);
	Key * copy = keyCopy (keyNew ("/", KEY_END), root, KEY_CP_ALL);

	KeySet * ks = ksNew (KEYSET_SIZE, copy, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END), keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * cut = ksCut (ks, root);

	// Assert
	succeed_if_fmt(ksGetSize (cut) == 3, "should contain 3 keys, was %zd", ksGetSize (cut));
	succeed_if (ksLookupByName (cut, "/root", 0) != NULL, "should contain root");
	succeed_if (ksLookupByName (cut, "/root/def", 0) != NULL, "should contain /root/def");
	succeed_if (ksLookupByName (cut, "system:/root/abc", 0) != NULL, "should contain system:/root/abc");
	succeed_if (ksGetSize (ks) == 1, "only 1 key should remain in keyset");
	succeed_if (ksLookupByName (ks, "system:/other", 0) != NULL, "ks should contain other key");

	keyDel (root);
	keyDel (copy);
	ksDel (ks);
	ksDel (cut);
}



static void ksFindHierarchy_root_in_keyset_should_work (void)
{
	printf("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("system:/root", KS_END);

	KeySet * ks = ksNew (KEYSET_SIZE, root, keyNew ("system:/root/abc", KEY_END), keyNew ("system:/other", KEY_END), KS_END);

	elektraCursor end = 999;

	// Act
	elektraCursor start = ksFindHierarchy (ks, root, &end);

	// Assert
	// keys in the keyset are ordered alphabetically.
	// - system:/other
	// - system:/root
	// - system:/root/abc

	succeed_if_fmt(start == 1, "start should be 1, was %zd", start);
	succeed_if_fmt(end == 3, "end should be 3, was %zd", end);

	keyDel (root);
	ksDel (ks);
}

static void ksFindHierarchy_key_copied_from_root_in_keyset_should_work (void)
{
	printf("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("system:/root", KS_END);
	Key * copy = keyCopy (keyNew ("/", KEY_END), root, KEY_CP_ALL);

	KeySet * ks = ksNew (KEYSET_SIZE, copy, keyNew ("system:/root/abc", KEY_END), keyNew ("system:/other", KEY_END), KS_END);

	elektraCursor end = 999;

	// Act
	elektraCursor start = ksFindHierarchy (ks, root, &end);

	// Assert
	// keys in the keyset are ordered alphabetically.
	// - system:/other
	// - system:/root
	// - system:/root/abc

	succeed_if_fmt(start == 1, "start should be 1, was %zd", start);
	succeed_if_fmt(end == 3, "end should be 3, was %zd", end);

	keyDel (root);
	keyDel (copy);
	ksDel (ks);
}

static void ksFindHierarchy_cascading_root_in_keyset_should_work (void)
{
	printf("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("/root", KS_END);

	KeySet * ks = ksNew (KEYSET_SIZE, root, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END), keyNew ("system:/other", KEY_END), KS_END);

	elektraCursor end = 999;

	// Act
	elektraCursor start = ksFindHierarchy (ks, root, &end);

	// Assert
	// keys in the keyset are ordered alphabetically.
	// - /root
	// - /root/def
	// - system:/other
	// - system:/root/abc

	succeed_if_fmt(start == 0, "start should be 0, was %zd", start);
	succeed_if_fmt(end == 2, "end should be 2, was %zd", end);

	keyDel (root);
	ksDel (ks);
}

static void ksFindHierarchy_cascading_key_copied_from_root_in_keyset_should_work (void)
{
	printf("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("/root", KS_END);
	Key * copy = keyCopy (keyNew ("/", KEY_END), root, KEY_CP_ALL);

	KeySet * ks = ksNew (KEYSET_SIZE, copy, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END), keyNew ("system:/other", KEY_END), KS_END);

	elektraCursor end = 999;

	// Act
	elektraCursor start = ksFindHierarchy (ks, root, &end);

	// Assert
	// keys in the keyset are ordered alphabetically.
	// - /root
	// - /root/def
	// - system:/other
	// - system:/root/abc

	succeed_if_fmt(start == 0, "start should be 0, was %zd", start);
	succeed_if_fmt(end == 2, "end should be 2, was %zd", end);

	keyDel (root);
	keyDel (copy);
	ksDel (ks);
}


int main (int argc, char ** argv)
{
	printf ("KEYSET COW   TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	ksBelow_root_in_keyset_should_work ();
	ksBelow_key_copied_from_root_in_keyset_should_work ();
	ksBelow_cascading_root_in_keyset_should_work ();
	ksBelow_cascading_key_copied_from_root_in_keyset_should_work ();

	ksCut_root_in_keyset_should_work ();
	ksCut_key_copied_from_root_in_keyset_should_work ();
	ksCut_cascading_root_in_keyset_should_work ();
	ksCut_cascading_key_copied_from_root_in_keyset_should_work ();

	ksFindHierarchy_root_in_keyset_should_work ();
	ksFindHierarchy_key_copied_from_root_in_keyset_should_work ();
	ksFindHierarchy_cascading_root_in_keyset_should_work ();
	ksFindHierarchy_cascading_key_copied_from_root_in_keyset_should_work ();

	print_result ("test_keyset_cow");
	return nbError;
}
