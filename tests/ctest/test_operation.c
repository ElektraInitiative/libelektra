/***************************************************************************
 *      test_operation.c  -  KeySet operation test suite
 *                  -------------------
 *  begin                : Thu Dez 12 2006
 *  copyright            : (C) 2006 by Markus Raab
 *  email                : sizon5@gmail.com
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#include <tests_internal.h>

KeySet * set_a ()
{
	return ksNew(16,
		keyNew ("user/0", KEY_END),
		keyNew ("user/a", KEY_END),
		keyNew ("user/a/a", KEY_END),
		keyNew ("user/a/a/a", KEY_END),
		keyNew ("user/a/a/b", KEY_END),
		keyNew ("user/a/b", KEY_END),
		keyNew ("user/a/b/a", KEY_END),
		keyNew ("user/a/b/b", KEY_END),
		keyNew ("user/a/c", KEY_END),
		keyNew ("user/a/d", KEY_END),
		keyNew ("user/a/x/a", KEY_END),
		keyNew ("user/a/x/b", KEY_END),
		keyNew ("user/a/x/c", KEY_END),
		keyNew ("user/a/x/c/a", KEY_END),
		keyNew ("user/a/x/c/b", KEY_END),
		keyNew ("user/x", KEY_END),
		KS_END);
}

KeySet * set_oa ()
{
	return ksNew(14,
		keyNew ("user/a", KEY_END),
		keyNew ("user/a/a", KEY_END),
		keyNew ("user/a/a/a", KEY_END),
		keyNew ("user/a/a/b", KEY_END),
		keyNew ("user/a/b", KEY_END),
		keyNew ("user/a/b/a", KEY_END),
		keyNew ("user/a/b/b", KEY_END),
		keyNew ("user/a/c", KEY_END),
		keyNew ("user/a/d", KEY_END),
		keyNew ("user/a/x/a", KEY_END),
		keyNew ("user/a/x/b", KEY_END),
		keyNew ("user/a/x/c", KEY_END),
		keyNew ("user/a/x/c/a", KEY_END),
		keyNew ("user/a/x/c/b", KEY_END),
		KS_END);
}

ssize_t ksSearchInternal(const KeySet *ks, const Key *toAppend);

static void test_search()
{
	printf ("Testing operation search (internal)\n");

	KeySet *a = set_a();
	Key *s = keyNew("user/a", KEY_END);
	ssize_t result;

	keySetName (s, "user/0");
	result = ksSearchInternal (a, s);
	succeed_if (result == 0, "insertpos wrong");

	keySetName (s, "user/a");
	result = ksSearchInternal (a, s);
	succeed_if (result == 1, "insertpos wrong");

	keySetName (s, "user/a/0");
	result = ksSearchInternal (a, s);
	succeed_if (result == -3, "insertpos wrong");

	keySetName (s, "user/a/a");
	result = ksSearchInternal (a, s);
	succeed_if (result == 2, "insertpos wrong");

	keySetName (s, "user/a/a/a");
	result = ksSearchInternal (a, s);
	succeed_if (result == 3, "insertpos wrong");

	keySetName (s, "user/a/a/b");
	result = ksSearchInternal (a, s);
	succeed_if (result == 4, "insertpos wrong");

	keySetName (s, "user/a/b");
	result = ksSearchInternal (a, s);
	succeed_if (result == 5, "insertpos wrong");

	keySetName (s, "user/a/b/a");
	result = ksSearchInternal (a, s);
	succeed_if (result == 6, "insertpos wrong");

	keySetName (s, "user/a/b/b");
	result = ksSearchInternal (a, s);
	succeed_if (result == 7, "insertpos wrong");

	keySetName (s, "user/a/c");
	result = ksSearchInternal (a, s);
	succeed_if (result == 8, "insertpos wrong");

	keySetName (s, "user/a/d");
	result = ksSearchInternal (a, s);
	succeed_if (result == 9, "insertpos wrong");

	keySetName (s, "user/a/x");
	result = ksSearchInternal (a, s);
	succeed_if (result == -11, "insertpos wrong");

	keySetName (s, "user/a/x/a");
	result = ksSearchInternal (a, s);
	succeed_if (result == 10, "insertpos wrong");

	keySetName (s, "user/a/x/b");
	result = ksSearchInternal (a, s);
	succeed_if (result == 11, "insertpos wrong");

	keySetName (s, "user/a/x/c");
	result = ksSearchInternal (a, s);
	succeed_if (result == 12, "insertpos wrong");

	keySetName (s, "user/a/x/c/a");
	result = ksSearchInternal (a, s);
	succeed_if (result == 13, "insertpos wrong");

	keySetName (s, "user/a/x/c/b");
	result = ksSearchInternal (a, s);
	succeed_if (result == 14, "insertpos wrong");

	keySetName (s, "user/x");
	result = ksSearchInternal (a, s);
	succeed_if (result == 15, "insertpos wrong");

	/*
	   Generation of new Testcases:
	for (int i=0; i< 16; ++i)
	{
		s = a->array[i];
		printf ("keySetName (s, \"%s\");\n", keyName(s));
		printf ("result = ksSearchInternal (a, s);\n");
		printf ("succeed_if (result == %zd, \"insertpos wrong\");\n\n", ksSearchInternal (a, s));
	}
	*/

	keyDel (s);
	ksDel (a);
}

static void test_cut()
{
	printf ("Testing operation cut\n");

	KeySet *orig;
	Key *cutpoint;
	KeySet *result;
	KeySet *real_orig;

	orig = set_oa();
	cutpoint = keyNew ("user/a", KEY_END);
	result = ksCut(orig, cutpoint);
	succeed_if (ksGetSize(orig) == 0, "orig not empty");
	real_orig = set_oa();
	compare_keyset(result, real_orig);
	ksDel (orig);
	ksDel (result);
	ksDel (real_orig);
	keyDel (cutpoint);


	KeySet *cmp_orig[16];
	KeySet *cmp_result[16];
#include "data_cut.c"

	for (int i=0; i<16; ++i)
	{
		orig = set_a();
		cutpoint = keyDup (orig->array[i]);
		result = ksCut(orig, cutpoint);

		compare_keyset(result, cmp_result[i]);
		compare_keyset(orig, cmp_orig[i]);

		/*
		Key *key;
		printf ("orig[%d] = ksNew (%zd, ", i, ksGetSize(orig));
		ksRewind(orig); while ((key=ksNext(orig))!= 0) printf ("keyNew (\"%s\", KEY_END), ", keyName(key));
		printf ("KS_END);\n");

		printf ("result[%d] = ksNew (%zd, ", i, ksGetSize(result));
		ksRewind(result); while ((key=ksNext(result))!= 0) printf ("keyNew (\"%s\", KEY_END), ", keyName(key));
		printf ("KS_END);\n");
		*/

		keyDel (cutpoint);
		ksDel (result);
		ksDel (orig);
		ksDel (cmp_orig[i]);
		ksDel (cmp_result[i]);
	}
}

static void test_cutpoint()
{
	printf ("Testing operation cut point\n");

	Key *cutpoint = keyNew("user/a/b/c", KEY_END);
	KeySet *orig = ksNew(30,
			keyNew("user/a", KEY_END),
			keyNew("user/a/b", KEY_END),
			cutpoint,
			keyNew("user/a/b/c/d", KEY_END),
			keyNew("user/a/b/c/d/e", KEY_END),
			keyNew("user/a/b/c/e", KEY_END),
			keyNew("user/a/b/c/e/d", KEY_END),
			KS_END);
	ksRewind(orig);
	ksNext(orig);
	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/a"), "wrong cursor");
	ksNext(orig);
	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/a/b"), "wrong cursor");

	KeySet *part = ksCut(orig, cutpoint);

	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/a/b"), "cursor should stay");

	KeySet *cmp_orig = ksNew(15,
			keyNew("user/a", KEY_END),
			keyNew("user/a/b", KEY_END),
			KS_END);
	compare_keyset(orig, cmp_orig);
	ksDel (orig);
	ksDel (cmp_orig);

	KeySet *cmp_part = ksNew(15,
			cutpoint,
			keyNew("user/a/b/c/d", KEY_END),
			keyNew("user/a/b/c/d/e", KEY_END),
			keyNew("user/a/b/c/e", KEY_END),
			keyNew("user/a/b/c/e/d", KEY_END),
			KS_END);
	compare_keyset(part, cmp_part);
	ksDel (part);
	ksDel (cmp_part);
}

static void test_cutpoint_1()
{
	printf ("Testing operation cut point 1\n");

	Key *cutpoint = keyNew("user/a/b/c", KEY_END);
	KeySet *orig = ksNew(30,
			keyNew("user/a", KEY_END),
			keyNew("user/a/b", KEY_END),
			cutpoint,
			keyNew("user/a/b/c/d", KEY_END),
			keyNew("user/a/b/c/d/e", KEY_END),
			keyNew("user/a/b/c/e", KEY_END),
			keyNew("user/a/b/c/e/d", KEY_END),
			KS_END);
	ksRewind(orig);
	ksNext(orig);
	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/a"), "wrong cursor");
	ksNext(orig);
	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/a/b"), "wrong cursor");
	ksNext(orig);
	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/a/b/c"), "wrong cursor");

	KeySet *part = ksCut(orig, cutpoint);

	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/a/b"),
			"cursor should jump for cutpoint");

	KeySet *cmp_orig = ksNew(15,
			keyNew("user/a", KEY_END),
			keyNew("user/a/b", KEY_END),
			KS_END);
	compare_keyset(orig, cmp_orig);
	ksDel (orig);
	ksDel (cmp_orig);

	KeySet *cmp_part = ksNew(15,
			cutpoint,
			keyNew("user/a/b/c/d", KEY_END),
			keyNew("user/a/b/c/d/e", KEY_END),
			keyNew("user/a/b/c/e", KEY_END),
			keyNew("user/a/b/c/e/d", KEY_END),
			KS_END);
	compare_keyset(part, cmp_part);
	ksDel (part);
	ksDel (cmp_part);
}

static void test_unique_cutpoint()
{
	printf ("Testing operation cut with unique cutpoint\n");

	Key *cutpoint = keyNew("user/a/b/c", KEY_END);
	KeySet *orig = ksNew(30,
			keyNew("user/a", KEY_END),
			keyNew("user/a/b", KEY_END),
			keyNew("user/a/b/c", KEY_END),
			keyNew("user/a/b/c/d", KEY_END),
			keyNew("user/a/b/c/d/e", KEY_END),
			keyNew("user/a/b/c/e", KEY_END),
			keyNew("user/a/b/c/e/d", KEY_END),
			KS_END);

	KeySet *part = ksCut(orig, cutpoint);

	KeySet *cmp_orig = ksNew(15,
			keyNew("user/a", KEY_END),
			keyNew("user/a/b", KEY_END),
			KS_END);
	compare_keyset(orig, cmp_orig);
	ksDel (orig);
	ksDel (cmp_orig);

	KeySet *cmp_part = ksNew(15,
			keyNew("user/a/b/c", KEY_END),
			keyNew("user/a/b/c/d", KEY_END),
			keyNew("user/a/b/c/d/e", KEY_END),
			keyNew("user/a/b/c/e", KEY_END),
			keyNew("user/a/b/c/e/d", KEY_END),
			KS_END);
	compare_keyset(part, cmp_part);
	ksDel (part);
	ksDel (cmp_part);
	keyDel (cutpoint);
}

static void test_cutbelow()
{
	printf ("Testing cutting below some keys\n");

	Key *cutpoint = keyNew("user/export", KEY_END);
	KeySet *orig = ksNew(30,
			keyNew("user/export-backup-2/x", KEY_END),
			keyNew("user/export-backup/b", KEY_END),
			keyNew("user/export/a", KEY_END),
			keyNew("user/export/c", KEY_END),
			keyNew("user/export/c/x", KEY_END),
			keyNew("user/export/c/x/b/blah", KEY_END),
			keyNew("user/export/xyz", KEY_END),
			KS_END);
	ksRewind(orig);
	ksNext(orig);
	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/export-backup-2/x"), "wrong cursor");
	ksNext(orig);
	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/export-backup/b"), "wrong cursor");

	KeySet *part = ksCut(orig, cutpoint);

	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/export-backup/b"), "wrong cursor");

	KeySet *cmp_orig = ksNew(15,
			keyNew("user/export-backup-2/x", KEY_END),
			keyNew("user/export-backup/b", KEY_END),
			KS_END);
	compare_keyset(orig, cmp_orig);
	ksDel (orig);
	ksDel (cmp_orig);

	KeySet *cmp_part = ksNew(15,
			keyNew("user/export/a", KEY_END),
			keyNew("user/export/c", KEY_END),
			keyNew("user/export/c/x", KEY_END),
			keyNew("user/export/c/x/b/blah", KEY_END),
			keyNew("user/export/xyz", KEY_END),
			KS_END);
	compare_keyset(part, cmp_part);
	ksDel (part);
	ksDel (cmp_part);
	keyDel (cutpoint);
}

static void test_cutbelow_1()
{
	printf ("Testing cutting below some keys\n");

	Key *cutpoint = keyNew("user/export", KEY_END);
	KeySet *orig = ksNew(30,
			keyNew("user/export-backup-2/x", KEY_END),
			keyNew("user/export-backup/b", KEY_END),
			keyNew("user/export/a", KEY_END),
			keyNew("user/export/c", KEY_END),
			keyNew("user/export/c/x", KEY_END),
			keyNew("user/export/c/x/b/blah", KEY_END),
			keyNew("user/export/xyz", KEY_END),
			KS_END);
	ksRewind(orig);
	ksNext(orig);
	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/export-backup-2/x"), "wrong cursor");
	ksNext(orig);
	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/export-backup/b"), "wrong cursor");
	ksNext(orig);
	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/export/a"), "wrong cursor");

	KeySet *part = ksCut(orig, cutpoint);

	succeed_if (!strcmp(keyName(ksCurrent(orig)), "user/export-backup/b"), "wrong cursor");

	KeySet *cmp_orig = ksNew(15,
			keyNew("user/export-backup-2/x", KEY_END),
			keyNew("user/export-backup/b", KEY_END),
			KS_END);
	compare_keyset(orig, cmp_orig);
	ksDel (orig);
	ksDel (cmp_orig);

	KeySet *cmp_part = ksNew(15,
			keyNew("user/export/a", KEY_END),
			keyNew("user/export/c", KEY_END),
			keyNew("user/export/c/x", KEY_END),
			keyNew("user/export/c/x/b/blah", KEY_END),
			keyNew("user/export/xyz", KEY_END),
			KS_END);
	compare_keyset(part, cmp_part);
	ksDel (part);
	ksDel (cmp_part);
	keyDel (cutpoint);
}

ssize_t ksCopyInternal(KeySet *ks, size_t to, size_t from);

static void test_copy()
{
	printf ("Testing operation copy (internal)\n");

	KeySet *copy[17][17];
#include "data_copy.c"

	KeySet *current;

	for (int i=0; i<17; ++i)
	{
		for (int j=0; j<17; ++j)
		{
			/* There are some cases which contain duplicates, we have to jump these...*/
			if (i>j) goto cleanup;
			if (i==0 && j==16) goto cleanup;

			current = set_a();
			/* Some blocks are lost in the next operation */
			succeed_if (ksCopyInternal (current, i, j) != -1, "ksCopyInternal failed");
			compare_keyset(current, copy[i][j]);
			ksDel (current);

cleanup:
			ksDel (copy[i][j]);
		}
	}
}

KeySet *set_simple()
{
	return ksNew(50,
		keyNew ("system/elektra/mountpoints/simple", KEY_END),

		keyNew ("system/elektra/mountpoints/simple/config", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/config/anything", KEY_VALUE, "backend", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/config/more", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/config/more/config", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/config/more/config/below", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/config/path", KEY_END),

		keyNew ("system/elektra/mountpoints/simple/getplugins", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer", KEY_VALUE, "tracer", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer/config", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer/config/anything", KEY_VALUE, "plugin", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer/config/more", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer/config/more/config", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer/config/more/config/below", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer/config/path", KEY_END),

		keyNew ("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/backend/simple", KEY_END),

		keyNew ("system/elektra/mountpoints/simple/setplugins", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/setplugins/#1tracer", KEY_VALUE, "tracer", KEY_END),
		KS_END);

}

static void test_simple()
{
	KeySet *config = set_simple();
	KeySet * result_res = ksNew( 16 ,
		keyNew ("system/elektra/mountpoints/simple/config" , KEY_END),
		keyNew ("system/elektra/mountpoints/simple/config/anything",  KEY_VALUE, "backend", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/config/more" , KEY_END),
		keyNew ("system/elektra/mountpoints/simple/config/more/config" , KEY_END),
		keyNew ("system/elektra/mountpoints/simple/config/more/config/below" , KEY_END),
		keyNew ("system/elektra/mountpoints/simple/config/path" , KEY_END),
		KS_END);
	KeySet *result_config = ksNew( 22 ,
		keyNew ("system/elektra/mountpoints/simple" , KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins" , KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer", KEY_VALUE, "tracer", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer/config" , KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer/config/anything", KEY_VALUE, "plugin", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer/config/more" , KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer/config/more/config" , KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer/config/more/config/below" , KEY_END),
		keyNew ("system/elektra/mountpoints/simple/getplugins/#1tracer/config/path" , KEY_END),
		keyNew ("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/backend/simple", KEY_END),
		keyNew ("system/elektra/mountpoints/simple/setplugins" , KEY_END),
		keyNew ("system/elektra/mountpoints/simple/setplugins/#1tracer", KEY_VALUE, "tracer", KEY_END),
		KS_END);
	Key *key = ksLookup(config, keyNew("system/elektra/mountpoints/simple/config", KEY_END), KDB_O_DEL);
	succeed_if (ksGetCursor(config) == 1, "cursor not set correctly");
	KeySet *res = ksCut (config, key);
	succeed_if (ksGetCursor(config) == 0, "cursor should stay as is");
	compare_keyset(config, result_config);
	compare_keyset(res, result_res);

	ksDel (result_config);
	ksDel (result_res);
	ksDel (res);
	ksDel (config);
}

static void test_cursor()
{
	printf ("test cut cursor\n");

	KeySet *config = set_simple();

	ksRewind (config);
	succeed_if (ksGetCursor(config) == -1, "should be invalid cursor");
	succeed_if (ksNext(config) != 0, "should be root key");
	succeed_if (ksGetCursor(config) == 0, "cursor on first position");
	succeed_if (!strcmp (keyName(ksCurrent(config)), "system/elektra/mountpoints/simple"),
			"not pointing to the root key");
	succeed_if (ksNext(config) != 0, "should be on config");
	succeed_if (ksGetCursor(config) == 1, "cursor on config");
	succeed_if (!strcmp (keyName(ksCurrent(config)), "system/elektra/mountpoints/simple/config"),
			"not pointing to the correct key");

	KeySet *res = ksCut(config, ksCurrent(config));
	succeed_if (ksGetCursor(config) == 0, "cursor on first position");
	succeed_if (!strcmp (keyName(ksCurrent(config)), "system/elektra/mountpoints/simple"),
			"not pointing to the root key again");

	succeed_if (ksNext(config) != 0, "should be on config");
	succeed_if (ksGetCursor(config) == 1, "cursor on getplugins");
	succeed_if (!strcmp (keyName(ksCurrent(config)), "system/elektra/mountpoints/simple/getplugins"),
			"not pointing to the correct key");

	KeySet *getplugins = ksCut(config, ksCurrent(config));
	succeed_if (ksGetCursor(getplugins) == -1, "should be invalid cursor");
	succeed_if (ksNext(getplugins) != 0, "should be root key");
	succeed_if (ksGetCursor(getplugins) == 0, "cursor on first position");

	succeed_if (ksNext(getplugins) != 0, "should be tracer");
	succeed_if (ksGetCursor(getplugins) == 1, "cursor not correct");

	KeySet *gettracer = ksCut (getplugins, ksCurrent (getplugins));
	succeed_if (ksNext(getplugins) == 0, "should be no more getplugins");

	succeed_if (ksNext(config) != 0, "next did not work");
	succeed_if (ksGetCursor(config ) == 1, "cursor not correct");
	succeed_if (!strcmp (keyName(ksCurrent(config)), "system/elektra/mountpoints/simple/mountpoint"),
			"not pointing to the correct key");

	succeed_if (ksNext(config) != 0, "next did not work");
	succeed_if (ksGetCursor(config ) == 2, "cursor not correct");
	succeed_if (!strcmp (keyName(ksCurrent(config)), "system/elektra/mountpoints/simple/setplugins"),
			"not pointing to the correct key");

	KeySet *setplugins = ksCut(config, ksCurrent(config));
	succeed_if (ksNext(config) == 0, "should be no more config");
	succeed_if (ksNext(setplugins) != 0, "ksnext did not work");
	succeed_if (!strcmp (keyName(ksCurrent(setplugins)), "system/elektra/mountpoints/simple/setplugins"),
			"not pointing to the correct key");
	succeed_if (ksNext(setplugins) != 0, "ksnext did not work");

	KeySet *settracer = ksCut (setplugins, ksCurrent (setplugins));
	succeed_if (ksNext(setplugins) == 0, "should be no more setplugins");
	succeed_if (ksGetSize(settracer) == 1, "should be only one key");

	succeed_if (ksGetSize(config) == 2, "should be only three keys remaining: root, mountpoint");


	ksDel (setplugins);
	ksDel (getplugins);
	ksDel (settracer);
	ksDel (gettracer);
	ksDel (config);
	ksDel (res);
}

static void test_morecut()
{
	printf ("More cut test cases\n");

	KeySet *ks = ksNew (
		5,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	// printf ("%s\n", keyName(ksCurrent(ks)));
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "system/valid/key2"),
			"cursor jumped somewhere else");
	ksNext(ks);
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "user/valid/key1"), "wrong cursor");
	ksNext(ks);
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "user/valid/key2"), "wrong cursor");
	// printf ("%s\n", keyName(ksCurrent(ks)));
	/*
	ksNext(ks);
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "system/valid/key1"), "wrong cursor");
	*/

	KeySet *split1 = ksNew (
		3,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		KS_END);
	KeySet *split2 = ksNew (
		3,
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);

	Key *userKey = keyNew("user", KEY_END);

	KeySet *cut = ksCut (ks, userKey);
	// printf ("%s\n", keyName(ksCurrent(ks)));
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "system/valid/key2"),
			"cursor jumped somewhere else");

	compare_keyset(cut, split1);
	compare_keyset(ks, split2);
	ksDel (cut);

	keyDel (userKey);

	ksDel (ks);
	ksDel (split1);
	ksDel (split2);
}

static void test_cutafter()
{
	printf ("More cut after\n");

	KeySet *ks = ksNew (
		5,
		keyNew ("user/a/valid/key", KEY_END),
		keyNew ("user/a/x/valid/key", KEY_END),
		keyNew ("user/b/valid/key", KEY_END),
		keyNew ("user/b/x/valid/key", KEY_END),
		keyNew ("user/c/valid/key", KEY_END),
		keyNew ("user/c/x/valid/key", KEY_END),
		KS_END);
	ksRewind(ks);
	ksNext(ks);
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "user/a/valid/key"), "wrong cursor");
	ksNext(ks);
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "user/a/x/valid/key"), "wrong cursor");
	ksNext(ks);
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "user/b/valid/key"), "wrong cursor");
	ksNext(ks);
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "user/b/x/valid/key"), "wrong cursor");
	ksNext(ks);
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "user/c/valid/key"), "wrong cursor");
	// printf ("%s\n", keyName(ksCurrent(ks)));

	KeySet *split1 = ksNew (
		8,
		keyNew ("user/b/valid/key", KEY_END),
		keyNew ("user/b/x/valid/key", KEY_END),
		KS_END);
	KeySet *split2 = ksNew (
		8,
		keyNew ("user/a/valid/key", KEY_END),
		keyNew ("user/a/x/valid/key", KEY_END),
		keyNew ("user/c/valid/key", KEY_END),
		keyNew ("user/c/x/valid/key", KEY_END),
		KS_END);

	Key *userKey = keyNew("user/b", KEY_END);

	KeySet *cut = ksCut (ks, userKey);
	// printf ("%s\n", keyName(ksCurrent(ks)));
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "user/c/valid/key"), "wrong cursor");

	compare_keyset(cut, split1);
	compare_keyset(ks, split2);
	ksDel (cut);

	keyDel (userKey);

	ksDel (ks);
	ksDel (split1);
	ksDel (split2);
}

int main(int argc, char** argv)
{
	printf("OPERATION    TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_search();
	test_cut();
	test_cutpoint();
	test_cutpoint_1();
	test_unique_cutpoint();
	test_cutbelow();
	test_cutbelow_1();
	// test_copy(); // TODO has memory problems...
	test_simple();
	test_cursor();
	test_morecut();
	test_cutafter();

	printf("\ntest_operation RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

