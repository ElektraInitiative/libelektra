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


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <tests.h>

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

void test_search()
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

void test_cut()
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
	compare_keyset (result, real_orig, 0, 0);
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

		compare_keyset(result, cmp_result[i], 0, 0);
		compare_keyset(orig, cmp_orig[i], 0, 0);

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

ssize_t ksCopyInternal(KeySet *ks, size_t to, size_t from);

void test_copy()
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
			compare_keyset(current, copy[i][j], 0, 0);
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

void test_simple()
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
	compare_keyset(config, result_config, 0, 0);
	compare_keyset(res, result_res, 0, 0);

	ksDel (result_config);
	ksDel (result_res);
	ksDel (res);
	ksDel (config);
}

void test_cursor()
{
}

int main(int argc, char** argv)
{
	printf("OPERATION    TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_search();
	test_cut();
	// test_copy(); // has memory problems...
	test_simple();
	test_cursor();

	printf("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

