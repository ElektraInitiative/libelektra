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
	printf ("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("system:/root", KS_END);

	KeySet * ks = ksNew (KEYSET_SIZE, root, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END),
			     keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * below = ksBelow (ks, root);

	// Assert
	succeed_if_fmt (ksGetSize (below) == 2, "should contain 2 keys, was %zd", ksGetSize (below));
	succeed_if (ksLookupByName (below, "system:/root", 0) != NULL, "should contain root");
	succeed_if (ksLookupByName (below, "system:/root/abc", 0) != NULL, "should contain key below root");
	succeed_if (ksGetSize (ks) == 4, "all keys should remain in keyset");

	keyDel (root);
	ksDel (ks);
	ksDel (below);
}

static void ksBelow_key_copied_from_root_in_keyset_should_work (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("system:/root", KS_END);
	Key * copy = keyCopy (keyNew ("/", KEY_END), root, KEY_CP_ALL);

	KeySet * ks = ksNew (KEYSET_SIZE, copy, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END),
			     keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * below = ksBelow (ks, root);

	// Assert
	succeed_if_fmt (ksGetSize (below) == 2, "should contain 2 keys, was %zd", ksGetSize (below));
	succeed_if (ksLookupByName (below, "system:/root", 0) != NULL, "should contain root");
	succeed_if (ksLookupByName (below, "system:/root/abc", 0) != NULL, "should contain key below root");
	succeed_if (ksGetSize (ks) == 4, "all keys should remain in keyset");

	keyDel (root);
	keyDel (copy);
	ksDel (ks);
	ksDel (below);
}

static void ksBelow_cascading_root_in_keyset_should_work (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("/root", KS_END);

	KeySet * ks = ksNew (KEYSET_SIZE, root, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END),
			     keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * below = ksBelow (ks, root);

	// Assert
	succeed_if_fmt (ksGetSize (below) == 3, "should contain 3 keys, was %zd", ksGetSize (below));
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
	printf ("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("/root", KS_END);
	Key * copy = keyCopy (keyNew ("/", KEY_END), root, KEY_CP_ALL);

	KeySet * ks = ksNew (KEYSET_SIZE, copy, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END),
			     keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * below = ksBelow (ks, root);

	// Assert
	succeed_if_fmt (ksGetSize (below) == 3, "should contain 3 keys, was %zd", ksGetSize (below));
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
	printf ("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("system:/root", KS_END);

	KeySet * ks = ksNew (KEYSET_SIZE, root, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END),
			     keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * cut = ksCut (ks, root);

	// Assert
	succeed_if_fmt (ksGetSize (cut) == 2, "should contain 2 keys, was %zd", ksGetSize (cut));
	succeed_if (ksLookupByName (cut, "system:/root", 0) != NULL, "should contain root");
	succeed_if (ksLookupByName (cut, "system:/root/abc", 0) != NULL, "should contain key below root");
	succeed_if_fmt (ksGetSize (ks) == 2, "2 keys should remain in keyset, was %zd", ksGetSize (ks));
	succeed_if (ksLookupByName (ks, "system:/other", 0) != NULL, "ks should contain system:/other");
	succeed_if (ksLookupByName (ks, "/root/def", 0) != NULL, "ks should contain /root/def");

	keyDel (root);
	ksDel (ks);
	ksDel (cut);
}

static void ksCut_key_copied_from_root_in_keyset_should_work (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("system:/root", KS_END);
	Key * copy = keyCopy (keyNew ("/", KEY_END), root, KEY_CP_ALL);

	KeySet * ks = ksNew (KEYSET_SIZE, copy, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END),
			     keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * cut = ksCut (ks, root);

	// Assert
	succeed_if_fmt (ksGetSize (cut) == 2, "should contain 2 keys, was %zd", ksGetSize (cut));
	succeed_if (ksLookupByName (cut, "system:/root", 0) != NULL, "should contain root");
	succeed_if (ksLookupByName (cut, "system:/root/abc", 0) != NULL, "should contain key below root");
	succeed_if_fmt (ksGetSize (ks) == 2, "2 keys should remain in keyset, was %zd", ksGetSize (ks));
	succeed_if (ksLookupByName (ks, "system:/other", 0) != NULL, "ks should contain system:/other key");
	succeed_if (ksLookupByName (ks, "/root/def", 0) != NULL, "ks should contain /root/def");

	keyDel (root);
	keyDel (copy);
	ksDel (ks);
	ksDel (cut);
}

static void ksCut_cascading_root_in_keyset_should_work (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("/root", KS_END);

	KeySet * ks = ksNew (KEYSET_SIZE, root, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END),
			     keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * cut = ksCut (ks, root);

	// Assert
	succeed_if_fmt (ksGetSize (cut) == 3, "should contain 3 keys, was %zd", ksGetSize (cut));
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
	printf ("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("/root", KS_END);
	Key * copy = keyCopy (keyNew ("/", KEY_END), root, KEY_CP_ALL);

	KeySet * ks = ksNew (KEYSET_SIZE, copy, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END),
			     keyNew ("system:/other", KEY_END), KS_END);

	// Act
	KeySet * cut = ksCut (ks, root);

	// Assert
	succeed_if_fmt (ksGetSize (cut) == 3, "should contain 3 keys, was %zd", ksGetSize (cut));
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
	printf ("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("system:/root", KS_END);

	KeySet * ks = ksNew (KEYSET_SIZE, root, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END),
			     keyNew ("system:/other", KEY_END), KS_END);

	elektraCursor end = 999;

	// Act
	elektraCursor start = ksFindHierarchy (ks, root, &end);

	// Assert
	// keys in the keyset are ordered alphabetically.
	// - /root/def
	// - system:/other
	// - system:/root
	// - system:/root/abc

	succeed_if_fmt (start == 2, "start should be 2, was %zd", start);
	succeed_if_fmt (end == 4, "end should be 4, was %zd", end);

	keyDel (root);
	ksDel (ks);
}

static void ksFindHierarchy_key_copied_from_root_in_keyset_should_work (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("system:/root", KS_END);
	Key * copy = keyCopy (keyNew ("/", KEY_END), root, KEY_CP_ALL);

	KeySet * ks = ksNew (KEYSET_SIZE, copy, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END),
			     keyNew ("system:/other", KEY_END), KS_END);

	elektraCursor end = 999;

	// Act
	elektraCursor start = ksFindHierarchy (ks, root, &end);

	// Assert
	// keys in the keyset are ordered alphabetically.
	// - /root/def
	// - system:/other
	// - system:/root
	// - system:/root/abc

	succeed_if_fmt (start == 2, "start should be 2, was %zd", start);
	succeed_if_fmt (end == 4, "end should be 4, was %zd", end);

	keyDel (root);
	keyDel (copy);
	ksDel (ks);
}

static void ksFindHierarchy_cascading_root_in_keyset_should_work (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("/root", KS_END);

	KeySet * ks = ksNew (KEYSET_SIZE, root, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END),
			     keyNew ("system:/other", KEY_END), KS_END);

	elektraCursor end = 999;

	// Act
	elektraCursor start = ksFindHierarchy (ks, root, &end);

	// Assert
	// keys in the keyset are ordered alphabetically.
	// - /root
	// - /root/def
	// - system:/other
	// - system:/root/abc

	succeed_if_fmt (start == 0, "start should be 0, was %zd", start);
	succeed_if_fmt (end == 2, "end should be 2, was %zd", end);

	keyDel (root);
	ksDel (ks);
}

static void ksFindHierarchy_cascading_key_copied_from_root_in_keyset_should_work (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * root = keyNew ("/root", KS_END);
	Key * copy = keyCopy (keyNew ("/", KEY_END), root, KEY_CP_ALL);

	KeySet * ks = ksNew (KEYSET_SIZE, copy, keyNew ("system:/root/abc", KEY_END), keyNew ("/root/def", KEY_END),
			     keyNew ("system:/other", KEY_END), KS_END);

	elektraCursor end = 999;

	// Act
	elektraCursor start = ksFindHierarchy (ks, root, &end);

	// Assert
	// keys in the keyset are ordered alphabetically.
	// - /root
	// - /root/def
	// - system:/other
	// - system:/root/abc

	succeed_if_fmt (start == 0, "start should be 0, was %zd", start);
	succeed_if_fmt (end == 2, "end should be 2, was %zd", end);

	keyDel (root);
	keyDel (copy);
	ksDel (ks);
}


static void ksNew_with_0_should_leave_empty_data (void)
{
	printf ("Test %s\n", __func__);

	// Arrange & Act
	KeySet * ks = ksNew (0, KS_END);

	// Assert
	succeed_if (ks->data == NULL, "ks->data should be NULL");

	ksDel (ks);
}

static void ksNew_should_alloc_data (void)
{
	printf ("Test %s\n", __func__);

	// Arrange & Act
	KeySet * ks = ksNew (KEYSET_SIZE, KS_END);

	// Assert
	succeed_if (ks->data != NULL, "ks->data should not be NULL");
	succeed_if (ks->data->alloc == KEYSET_SIZE + 1, "ks->data->alloc should be KEYSET_SIZE + 1");
	succeed_if (ks->data->size == 0, "ks->data->size should be 0");

	ksDel (ks);
}

static void ksDup_should_point_to_the_same_data (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_END);
	KeySet * original = ksNew (KEYSET_SIZE, key, KS_END);

	// Act
	KeySet * copy = ksDup (original);

	// Assert
	succeed_if (original->data == copy->data, "original->data and copy->data should point to the same thing");
	succeed_if (original->data->refs == 2, "data->refs should be 2");
	succeed_if (key->refs == 1, "key->refs should be 1");

	ksDel (original);
	ksDel (copy);
}

static void ksDup_of_key_with_NULL_data_should_point_NULL (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * original = ksNew (0, KS_END);

	// Act
	KeySet * copy = ksDup (original);

	// Assert
	succeed_if (copy->data == NULL, "copy->data should be NULL");

	ksDel (original);
	ksDel (copy);
}

static void ksCopy_should_point_to_the_same_data (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_END);

	KeySet * original = ksNew (KEYSET_SIZE, key, KS_END);
	KeySet * copied = ksNew (KEYSET_SIZE, KS_END);

	// Act
	ksCopy (copied, original);

	// Assert
	succeed_if (original->data == copied->data, "original->data and copied->data should point to the same thing");
	succeed_if (original->data->refs == 2, "data->refs should be 2");
	succeed_if (key->refs == 1, "key->refs should be 1");

	ksDel (copied);
	ksDel (original);
}

static void ksDel_should_decrease_reference_pointer (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * original = ksNew (KEYSET_SIZE, KS_END);
	KeySet * copy = ksDup (original);

	succeed_if (copy->data->refs == 2, "data->refs should be 2");

	// Act
	ksDel (original);

	// Assert
	succeed_if (copy->data->refs == 1, "data->refs should be 1");

	ksDel (copy);
}

static void ksAppendKey_should_not_replace_data_when_no_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * ks = ksNew (10, keyNew ("system:/test", KEY_END), KS_END);
	const struct _KeySetData * originalData = ks->data;

	// Act
	ksAppendKey (ks, keyNew ("system:/test2", KEY_END));

	// Assert
	succeed_if (ks->data == originalData, "should not replace data instance");
	succeed_if (ks->data->refs == 1, "reference counter of data should still be 1");
	succeed_if (ksLookupByName (ks, "system:/test", 0) != NULL, "should contain key system:/test");
	succeed_if (ksLookupByName (ks, "system:/test2", 0) != NULL, "should contain key system:/test2");

	ksDel (ks);
}

static void ksAppendKey_should_replace_data_when_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * k = keyNew ("system:/test", KEY_END);
	Key * addedKey = keyNew ("system:/test2", KEY_END);

	KeySet * ks = ksNew (10, k, KS_END);
	const struct _KeySetData * originalData = ks->data;
	KeySet * dup = ksDup (ks);

	succeed_if (keyGetRef (k) == 1, "key should have 1 reference");

	// Act
	ksAppendKey (ks, addedKey);

	// Assert
	succeed_if (ks->data != originalData, "should replace data instance");
	succeed_if (ks->data->refs == 1, "reference counter of data should still be 1");

	succeed_if (dup->data == originalData, "should not replace data instance of other keyset");
	succeed_if (dup->data->refs == 1, "reference counter of dup data should still be 1");

	succeed_if (ksLookupByName (ks, "system:/test", 0) != NULL, "should contain key system:/test");
	succeed_if (ksLookupByName (ks, "system:/test2", 0) != NULL, "should contain key system:/test2");

	succeed_if (ksLookupByName (dup, "system:/test", 0) != NULL, "dup should contain key system:/test");
	succeed_if (ksLookupByName (dup, "system:/test2", 0) == NULL, "dup should not contain key system:/test2");

	succeed_if (keyGetRef (k) == 2, "key should have 2 references");
	succeed_if (keyGetRef (addedKey) == 1, "added key should have 1 reference");

	ksDel (ks);
	ksDel (dup);
}

static void ksAppend_should_not_replace_data_when_no_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * ks = ksNew (10, keyNew ("system:/test", KEY_END), KS_END);
	const struct _KeySetData * originalData = ks->data;

	KeySet * ksOther = ksNew (10, keyNew ("system:/test2", KEY_END), KS_END);

	// Act
	ksAppend (ks, ksOther);

	// Assert
	succeed_if (ks->data == originalData, "should not replace data instance");
	succeed_if (ks->data->refs == 1, "reference counter of data should still be 1");
	succeed_if (ksLookupByName (ks, "system:/test", 0) != NULL, "should contain key system:/test");
	succeed_if (ksLookupByName (ks, "system:/test2", 0) != NULL, "should contain key system:/test2");

	succeed_if (keyGetRef (ksLookupByName (ks, "system:/test", 0)) == 1, "system:/test should have 1 reference");
	succeed_if (keyGetRef (ksLookupByName (ks, "system:/test2", 0)) == 2, "system:/test2 should have 2 references");

	ksDel (ks);
	ksDel (ksOther);
}

static void ksAppend_should_replace_data_when_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * k = keyNew ("system:/test", KEY_END);
	Key * addedKey = keyNew ("system:/test2", KEY_END);
	KeySet * addedKs = ksNew (10, addedKey, KS_END);

	KeySet * ks = ksNew (10, k, KS_END);
	const struct _KeySetData * originalData = ks->data;
	KeySet * dup = ksDup (ks);

	succeed_if (keyGetRef (k) == 1, "key should have 1 reference");
	succeed_if (keyGetRef (addedKey) == 1, "addedKey should have 1 reference");

	// Act
	ksAppend (ks, addedKs);

	// Assert
	succeed_if (ks->data != originalData, "should replace data instance");
	succeed_if (ks->data->refs == 1, "reference counter of data should still be 1");

	succeed_if (dup->data == originalData, "should not replace data instance of other keyset");
	succeed_if (dup->data->refs == 1, "reference counter of dup data should still be 1");

	succeed_if (ksLookupByName (ks, "system:/test", 0) != NULL, "should contain key system:/test");
	succeed_if (ksLookupByName (ks, "system:/test2", 0) != NULL, "should contain key system:/test2");

	succeed_if (ksLookupByName (dup, "system:/test", 0) != NULL, "dup should contain key system:/test");
	succeed_if (ksLookupByName (dup, "system:/test2", 0) == NULL, "dup should not contain key system:/test2");

	succeed_if (keyGetRef (k) == 2, "key should have 2 references");
	succeed_if (keyGetRef (addedKey) == 2, "added key should have 2 references");

	ksDel (ks);
	ksDel (dup);
	ksDel (addedKs);
}

static void ksCut_should_not_replace_data_when_no_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * k1 = keyNew ("system:/test1", KEY_END);
	Key * k2 = keyNew ("system:/test2", KEY_END);
	Key * cutpoint = keyNew ("system:/test2", KEY_END);

	KeySet * ks = ksNew (10, k1, k2, KS_END);
	const struct _KeySetData * originalData = ks->data;

	// Act
	KeySet * cut = ksCut (ks, cutpoint);

	// Assert
	succeed_if (ks->data == originalData, "should not replace data instance");
	succeed_if (ks->data->refs == 1, "reference counter of data should still be 1");
	succeed_if (ksLookupByName (ks, "system:/test1", 0) != NULL, "keyset should contain key system:/test1");
	succeed_if (ksLookupByName (ks, "system:/test2", 0) == NULL, "keyset should not contain key system:/test2");

	succeed_if (cut->data->refs == 1, "cut reference counter of data should still be 1");
	succeed_if (ksLookupByName (cut, "system:/test1", 0) == NULL, "cut should not contain key system:/test1");
	succeed_if (ksLookupByName (cut, "system:/test2", 0) != NULL, "cut should contain key system:/test2");

	succeed_if (keyGetRef (k1) == 1, "system:/test1 should have 1 reference");
	succeed_if (keyGetRef (k2) == 1, "system:/test2 should have 1 reference");

	ksDel (ks);
	ksDel (cut);
	keyDel (cutpoint);
}

static void ksCut_should_replace_data_when_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * k1 = keyNew ("system:/test1", KEY_END);
	Key * k2 = keyNew ("system:/test2", KEY_END);
	Key * cutpoint = keyNew ("system:/test2", KEY_END);

	KeySet * ks = ksNew (10, k1, k2, KS_END);
	const struct _KeySetData * originalData = ks->data;
	KeySet * dup = ksDup (ks);

	// Act
	KeySet * cut = ksCut (ks, cutpoint);

	// Assert
	succeed_if (ks->data != originalData, "should replace data instance");

	succeed_if (ks->data->refs == 1, "reference counter of data should still be 1");
	succeed_if (ksLookupByName (ks, "system:/test1", 0) != NULL, "keyset should contain key system:/test1");
	succeed_if (ksLookupByName (ks, "system:/test2", 0) == NULL, "keyset should not contain key system:/test2");

	succeed_if (cut->data->refs == 1, "cut reference counter of data should still be 1");
	succeed_if (ksLookupByName (cut, "system:/test1", 0) == NULL, "cut should not contain key system:/test1");
	succeed_if (ksLookupByName (cut, "system:/test2", 0) != NULL, "cut should contain key system:/test2");

	succeed_if (dup->data == originalData, "dup->data should point to original instance");
	succeed_if (dup->data->refs == 1, "dup references should be 1");
	succeed_if (ksLookupByName (dup, "system:/test1", 0) != NULL, "dup should contain key system:/test1");
	succeed_if (ksLookupByName (dup, "system:/test2", 0) != NULL, "dup should contain key system:/test2");

	succeed_if (keyGetRef (k1) == 2, "system:/test1 should have 2 references");
	succeed_if (keyGetRef (k2) == 2, "system:/test2 should have 2 references");

	ksDel (ks);
	ksDel (dup);
	ksDel (cut);
	keyDel (cutpoint);
}

static void ksRename_with_root_part_of_ks_should_work (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * k1 = keyNew ("system:/test", KEY_END);
	Key * k2 = keyNew ("system:/test/1", KEY_END);
	Key * k3 = keyNew ("system:/other", KEY_END);
	Key * newRoot = keyNew ("system:/renamed", KEY_END);

	KeySet * ks = ksNew (3, k1, k2, k3, KS_END);

	// Act
	ksRename (ks, k1, newRoot);

	// Assert
	succeed_if (ksLookupByName (ks, "system:/renamed", 0) != NULL, "system:/renamed should be in ks");
	succeed_if (ksLookupByName (ks, "system:/renamed/1", 0) != NULL, "system:/renamed/1 should be in ks");
	succeed_if (ksLookupByName (ks, "system:/other", 0) != NULL, "system:/other should be in ks");

	keyDel (newRoot);
	ksDel (ks);
}

static void ksRename_with_copy_of_key_as_root_should_work (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * k1 = keyNew ("system:/test", KEY_END);
	Key * k2 = keyNew ("system:/test/1", KEY_END);
	Key * k3 = keyNew ("system:/other", KEY_END);
	Key * root = keyDup (k1, KEY_CP_ALL);
	Key * newRoot = keyNew ("system:/renamed", KEY_END);

	KeySet * ks = ksNew (3, k1, k2, k3, KS_END);

	// Act
	ksRename (ks, root, newRoot);

	// Assert
	succeed_if (ksLookupByName (ks, "system:/renamed", 0) != NULL, "system:/renamed should be in ks");
	succeed_if (ksLookupByName (ks, "system:/renamed/1", 0) != NULL, "system:/renamed/1 should be in ks");
	succeed_if (ksLookupByName (ks, "system:/other", 0) != NULL, "system:/other should be in ks");
	succeed_if (strcmp (keyName (root), "system:/test") == 0, "should NOT rename root");

	keyDel (root);
	keyDel (newRoot);
	ksDel (ks);
}

static void ksRename_should_not_replace_data_when_no_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * k1 = keyNew ("system:/test", KEY_END);
	Key * k2 = keyNew ("system:/test/1", KEY_END);
	Key * k3 = keyNew ("system:/other", KEY_END);
	Key * root = keyNew ("system:/test", KEY_END);
	Key * newRoot = keyNew ("system:/renamed", KEY_END);

	KeySet * ks = ksNew (3, k1, k2, k3, KS_END);
	const struct _KeySetData * original = ks->data;

	// Act
	ksRename (ks, root, newRoot);

	// Assert
	succeed_if (ks->data == original, "should not replace data");

	keyDel (root);
	keyDel (newRoot);
	ksDel (ks);
}

static void ksRename_should_replace_data_when_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * k1 = keyNew ("system:/test", KEY_END);
	Key * k2 = keyNew ("system:/test/1", KEY_END);
	Key * k3 = keyNew ("system:/other", KEY_END);
	Key * root = keyNew ("system:/test", KEY_END);
	Key * newRoot = keyNew ("system:/renamed", KEY_END);

	KeySet * ks = ksNew (3, k1, k2, k3, KS_END);
	const struct _KeySetData * original = ks->data;
	KeySet * dup = ksDup (ks);

	// Act
	ksRename (ks, root, newRoot);

	// Assert
	succeed_if (ks->data != original, "should not replace data");
	succeed_if (ksLookupByName (ks, "system:/renamed", 0) != NULL, "system:/renamed should be in ks");
	succeed_if (ksLookupByName (ks, "system:/renamed/1", 0) != NULL, "system:/renamed/1 should be in ks");
	succeed_if (ksLookupByName (ks, "system:/other", 0) != NULL, "system:/other should be in ks");
	succeed_if (ksLookupByName (dup, "system:/test", 0) != NULL, "system:/test should be in dup");
	succeed_if (ksLookupByName (dup, "system:/test/1", 0) != NULL, "system:/test/1 should be in dup");
	succeed_if (ksLookupByName (dup, "system:/other", 0) != NULL, "system:/other should be in dup");

	succeed_if (keyGetRef (k1) == 1, "k1 should have 1 reference");
	succeed_if (keyGetRef (k2) == 1, "k2 should have 1 reference");
	succeed_if (keyGetRef (k3) == 2, "k3 should have 2 reference");

	keyDel (root);
	keyDel (newRoot);
	ksDel (ks);
}

static void ksPop_with_null_data_should_return_null (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * ks = ksNew (0, KS_END);

	// Act
	Key * poped = ksPop (ks);

	// Assert
	succeed_if (ks->data == NULL, "data should be NULL");
	succeed_if (poped == NULL, "popped key should be NULL");

	ksDel (ks);
}

static void ksPop_should_not_replace_data_when_no_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * k1 = keyNew ("system:/test1", KEY_END);
	Key * k2 = keyNew ("system:/test2", KEY_END);

	KeySet * ks = ksNew (2, k1, k2, KS_END);
	const struct _KeySetData * original = ks->data;

	// Act
	Key * poped = ksPop (ks);

	// Assert
	succeed_if (ks->data == original, "should not replace data");
	succeed_if (ks->data->refs == 1, "data references should be 1");
	succeed_if (k1->refs == 1, "system:/test1 refs should be 1");
	succeed_if (k2->refs == 0, "system:/test2 refs should be 0");

	keyDel (poped);
	ksDel (ks);
}

static void ksPop_replace_data_when_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * k1 = keyNew ("system:/test1", KEY_END);
	Key * k2 = keyNew ("system:/test2", KEY_END);

	KeySet * ks = ksNew (2, k1, k2, KS_END);
	const struct _KeySetData * original = ks->data;
	KeySet * dup = ksDup (ks);

	// Act
	ksPop (ks);

	// Assert
	succeed_if (ks->data != original, "should replace data");
	succeed_if (ks->data->refs == 1, "data references should be 1");
	succeed_if (k1->refs == 2, "system:/test1 refs should be 2");
	succeed_if (k2->refs == 1, "system:/test2 refs should be 1");
	succeed_if (ksGetSize (dup) == 2, "dup should contain 2 keys");

	ksDel (ks);
	ksDel (dup);
}

static void ksRewind_ksNext_ksCurrent_should_not_replace_data (void)
{
	printf ("Test %s\n", __func__);

	Key * k1 = keyNew ("system:/test1", KEY_END);
	Key * k2 = keyNew ("system:/test2", KEY_END);

	KeySet * ks = ksNew (2, k1, k2, KS_END);
	const struct _KeySetData * original = ks->data;
	KeySet * dup = ksDup (ks);

	ksNext (ks);
	succeed_if (ks->data == original, "ksNext should not replace data");
	succeed_if (ks->data->refs == 2, "data references should be 2");

	ksRewind (ks);
	succeed_if (ks->data == original, "ksRewind should not replace data");
	succeed_if (ks->data->refs == 2, "data references should be 2");

	ksCurrent (ks);
	succeed_if (ks->data == original, "ksCurrent should not replace data");
	succeed_if (ks->data->refs == 2, "data references should be 2");

	succeed_if (k1->refs == 1, "system:/test1 refs should be 1");
	succeed_if (k2->refs == 1, "system:/test2 refs should be 0");

	ksDel (ks);
	ksDel (dup);
}

static void ksSetCursor_should_not_replace_data (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * k1 = keyNew ("system:/test1", KEY_END);
	Key * k2 = keyNew ("system:/test2", KEY_END);

	KeySet * ks = ksNew (2, k1, k2, KS_END);
	const struct _KeySetData * original = ks->data;
	KeySet * dup = ksDup (ks);

	// Act
	ksSetCursor (ks, 1);

	// Assert
	succeed_if (ks->data == original, "should not replace data");
	succeed_if (ks->data->refs == 2, "data references should be 2");
	succeed_if (k1->refs == 1, "system:/test1 refs should be 1");
	succeed_if (k2->refs == 1, "system:/test2 refs should be 0");

	ksDel (ks);
	ksDel (dup);
}

static void ksLookup_modifying_flags_should_replace_data_when_references (void)
{
	printf ("Test %s\n", __func__);

	elektraLookupFlags modifyingFlags[] = { KDB_O_CREATE, KDB_O_POP };

	for (size_t i = 0; i < sizeof (modifyingFlags) / sizeof (elektraLookupFlags); i++)
	{
		elektraLookupFlags flag = modifyingFlags[i];

		// Arrange
		KeySet * ks = ksNew (2, keyNew ("system:/test1", KEY_END), keyNew ("system:/test2", KEY_END), KS_END);
		const struct _KeySetData * original = ks->data;
		KeySet * dup = ksDup (ks);

		Key * lookupKey = keyNew ("system:/test1", KEY_END);

		if (flag == KDB_O_CREATE)
		{
			keySetName (lookupKey, "system:/unknown");
		}

		// Act
		ksLookup (ks, lookupKey, flag);

		// Assert
		printf ("  - testing flag %zu\n", i);
		succeed_if (ks->data != original, "should replace data");

		ksDel (ks);
		ksDel (dup);
		keyDel (lookupKey);
	}
}

static void ksLookup_modifying_flags_should_not_replace_data_when_no_references (void)
{
	printf ("Test %s\n", __func__);

	elektraLookupFlags modifyingFlags[] = { KDB_O_CREATE, KDB_O_POP };

	for (size_t i = 0; i < sizeof (modifyingFlags) / sizeof (elektraLookupFlags); i++)
	{
		elektraLookupFlags flag = modifyingFlags[i];

		// Arrange
		KeySet * ks = ksNew (2, keyNew ("system:/test1", KEY_END), keyNew ("system:/test2", KEY_END), KS_END);
		const struct _KeySetData * original = ks->data;

		Key * lookupKey = keyNew ("system:/test1", KEY_END);

		if (flag == KDB_O_CREATE)
		{
			keySetName (lookupKey, "system:/unknown");
		}

		// Act
		ksLookup (ks, lookupKey, flag);

		// Assert
		printf ("  - testing flag %zu\n", i);
		succeed_if (ks->data == original, "should not replace data");

		ksDel (ks);
		keyDel (lookupKey);
	}
}

static void ksLookup_non_modifying_flags_should_not_replace_data (void)
{
	printf ("Test %s\n", __func__);

	elektraLookupFlags modifyingFlags[] = { KDB_O_NONE, KDB_O_DEL };

	for (size_t i = 0; i < sizeof (modifyingFlags) / sizeof (elektraLookupFlags); i++)
	{
		elektraLookupFlags flag = modifyingFlags[i];

		// Arrange
		KeySet * ks = ksNew (2, keyNew ("system:/test1", KEY_END), keyNew ("system:/test2", KEY_END), KS_END);
		const struct _KeySetData * original = ks->data;
		KeySet * dup = ksDup (ks);

		Key * lookupKey = keyNew ("system:/test1", KEY_END);

		if (flag == KDB_O_CREATE)
		{
			keySetName (lookupKey, "system:/unknown");
		}

		// Act
		ksLookup (ks, lookupKey, flag);

		// Assert
		printf ("  - testing flag %zu\n", i);
		succeed_if (ks->data == original, "should not replace data");

		ksDel (ks);
		ksDel (dup);

		if (flag != KDB_O_DEL)
		{
			keyDel (lookupKey);
		}
	}
}

static void test_mmap_flag_methods (void)
{
	printf ("Test %s\n", __func__);

	struct _KeySetData * data = elektraCalloc (sizeof (struct _KeySetData));
	succeed_if (isKeySetDataInMmap (data) == false, "newly created keyset data should not have MMAP flag");

	setKeySetDataIsInMmap (data, true);
	succeed_if (isKeySetDataInMmap (data) == true, "keyset data should have MMAP flag");

	setKeySetDataIsInMmap (data, false);
	succeed_if (isKeySetDataInMmap (data) == false, "keyset data should not have MMAP flag");

	elektraFree(data);
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

	ksNew_with_0_should_leave_empty_data ();
	ksNew_should_alloc_data ();

	ksDup_should_point_to_the_same_data ();
	ksDup_of_key_with_NULL_data_should_point_NULL ();

	ksCopy_should_point_to_the_same_data ();

	ksDel_should_decrease_reference_pointer ();

	ksAppendKey_should_not_replace_data_when_no_other_references ();
	ksAppendKey_should_replace_data_when_other_references ();

	ksAppend_should_not_replace_data_when_no_other_references ();
	ksAppend_should_replace_data_when_other_references ();

	ksCut_should_not_replace_data_when_no_other_references ();
	ksCut_should_replace_data_when_other_references ();

	ksRename_with_root_part_of_ks_should_work ();
	ksRename_with_copy_of_key_as_root_should_work ();
	ksRename_should_not_replace_data_when_no_references ();
	ksRename_should_replace_data_when_references ();

	ksPop_with_null_data_should_return_null ();
	ksPop_should_not_replace_data_when_no_other_references ();
	ksPop_replace_data_when_other_references ();

	ksRewind_ksNext_ksCurrent_should_not_replace_data ();
	ksSetCursor_should_not_replace_data ();

	ksLookup_modifying_flags_should_replace_data_when_references ();
	ksLookup_modifying_flags_should_not_replace_data_when_no_references ();
	ksLookup_non_modifying_flags_should_not_replace_data ();

	test_mmap_flag_methods ();

	print_result ("test_keyset_cow");
	return nbError;
}
