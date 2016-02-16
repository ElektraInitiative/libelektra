/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <tests.h>

KeySet * get_dump()
{
	Key *k1, *k2;
	KeySet *ks = ksNew(10,
			k1 = keyNew("user/tests/dump",
			       KEY_VALUE, "root key",
			       KEY_META, "a", "b",
			       KEY_END),
			k2 = keyNew("user/tests/dump/a",
			       KEY_VALUE, "a value",
			       KEY_META, "ab", "cd",
			       KEY_END),
			keyNew("user/tests/dump/b",
			       KEY_VALUE, "b value",
			       KEY_META, "longer val", "here some even more with ugly €@\\1¹²³¼ chars",
			       KEY_END),
			KS_END
		);
	keyCopyMeta(k1, k2, "ab");

	return ks;
}

#if 0

void test_writedump(const char *file)
{
	KDB *kdb = kdbOpen();
	Key *mnt;
	KeySet *conf;
	KeySet *ks = get_dump();

	printf("Test write dump\n");

	succeed_if (kdbMount(kdb,mnt=keyNew("user/tests/dump",KEY_VALUE,"dump", KEY_END),
		conf=ksNew (2,keyNew("system/path", KEY_VALUE, file, KEY_END), KS_END)) == 0,
		"could not mount dump");
	succeed_if (kdbSet(kdb,ks,keyNew("user/tests/dump",KEY_END),KDB_O_DEL) >= 0, "could not set keys");
	ksDel (conf);
	keyDel(mnt);

	ksDel (ks);
	kdbClose (kdb);
}

void test_readdump(const char *file)
{
	KDB *kdb = kdbOpen();
	Key *mnt;
	KeySet *conf;
	KeySet *ks = get_dump();
	KeySet *read = ksNew(0, KS_END);
	Key *k1, *k2;

	printf("Test read dump\n");

	succeed_if (kdbMount(kdb,mnt=keyNew("user/tests/dump",KEY_VALUE,"dump", KEY_END),
		conf=ksNew (2,keyNew("system/path", KEY_VALUE, file, KEY_END), KS_END)) == 0,
		"could not mount dump");
	succeed_if (kdbGet(kdb,read,keyNew("user/tests/dump",KEY_END),KDB_O_DEL) >= 0, "could not get keys");
	ksDel (conf);
	keyDel(mnt);

	compare_keyset (read, ks, 0, 0);

	k1 = ksLookupByName(ks, "user/tests/dump", 0);
	succeed_if (k1 != 0, "did not find key");
	k2 = ksLookupByName(ks, "user/tests/dump/a", 0);
	succeed_if (k2 != 0, "did not find key");

	succeed_if (!strcmp(keyValue(keyGetMeta(k1, "ab")), "cd"), "meta value not correct");
	succeed_if (!strcmp(keyValue(keyGetMeta(k2, "ab")), "cd"), "meta value not correct");
	succeed_if (keyGetMeta(k1, "ab") == keyGetMeta(k2, "ab"), "does not point to the same storage");

	// ksOutput (read, stdout, KEY_VALUE);

	ksDel (ks);
	ksDel (read);
	kdbClose (kdb);
}

#endif

int main(int argc, char** argv)
{
	printf("MOUNT       TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	/*
	test_writedump("dump_mount_test.edf");
	test_readdump("dump_mount_test.edf");
	*/

	printf("\ntest_mount RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

