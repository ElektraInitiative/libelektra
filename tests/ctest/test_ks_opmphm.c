/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

#include <opmphm.c>
#include <tests_internal.h>

ssize_t ksCopyInternal (ElektraKeyset * ks, size_t to, size_t from);

void test_keyNotFound (void)
{
	ElektraKeyset * ks = ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
			     keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/g", ELEKTRA_KEY_END), keyNew ("/h", ELEKTRA_KEY_END),
			     keyNew ("/i", ELEKTRA_KEY_END), keyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKey * found = ksLookupByName (ks, "/nothere", ELEKTRA_KDB_O_OPMPHM);
	succeed_if (!found, "key found");

	exit_if_fail (ks->opmphm, "build opmphm");
	succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

	ksDel (ks);
}

void test_Copy (void)
{
	// ksDup
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
				     keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/g", ELEKTRA_KEY_END), keyNew ("/h", ELEKTRA_KEY_END),
				     keyNew ("/i", ELEKTRA_KEY_END), keyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = ksLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		ElektraKeyset * copy = ksDup (ks);
		succeed_if (copy, "copy");

		// opmphm should be build
		exit_if_fail (copy->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (copy->opmphm), "build opmphm");

		// test opmphm
		ElektraKey * iter;
		ksRewind (copy);
		while ((iter = ksNext (copy)))
		{
			ElektraKey * f = ksLookup (copy, iter, ELEKTRA_KDB_O_OPMPHM);
			succeed_if (f, "key found");
		}

		// cleanup
		ksDel (ks);
		ksDel (copy);
	}
	// ksDeepDup
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
				     keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/g", ELEKTRA_KEY_END), keyNew ("/h", ELEKTRA_KEY_END),
				     keyNew ("/i", ELEKTRA_KEY_END), keyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = ksLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		ElektraKeyset * copy = ksDeepDup (ks);
		succeed_if (copy, "copy");

		// opmphm should be build
		exit_if_fail (copy->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (copy->opmphm), "build opmphm");

		// test opmphm
		ElektraKey * iter;
		ksRewind (copy);
		while ((iter = ksNext (copy)))
		{
			ElektraKey * f = ksLookup (copy, iter, ELEKTRA_KDB_O_OPMPHM);
			succeed_if (f, "key found");
		}

		// cleanup
		ksDel (ks);
		ksDel (copy);
	}
	// ksCopy
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
				     keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/g", ELEKTRA_KEY_END), keyNew ("/h", ELEKTRA_KEY_END),
				     keyNew ("/i", ELEKTRA_KEY_END), keyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = ksLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		ElektraKeyset * copy = ksNew (0, ELEKTRA_KS_END);
		succeed_if (ksCopy (copy, ks) == 1, "copy");

		// opmphm should be build
		exit_if_fail (copy->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (copy->opmphm), "build opmphm");

		// test opmphm
		ElektraKey * iter;
		ksRewind (copy);
		while ((iter = ksNext (copy)))
		{
			ElektraKey * f = ksLookup (copy, iter, ELEKTRA_KDB_O_OPMPHM);
			succeed_if (f, "key found");
		}

		// cleanup
		ksDel (ks);
		ksDel (copy);
	}
	// test not build copy
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
				     keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/g", ELEKTRA_KEY_END), keyNew ("/h", ELEKTRA_KEY_END),
				     keyNew ("/i", ELEKTRA_KEY_END), keyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		ElektraKeyset * copy = ksDup (ks);
		succeed_if (copy, "copy");

		// opmphm should not be build
		succeed_if (!copy->opmphm, "not build opmphm");

		// cleanup
		ksDel (ks);
		ksDel (copy);
	}
}

void test_Invalidate (void)
{
	// ksClose
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
				     keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/g", ELEKTRA_KEY_END), keyNew ("/h", ELEKTRA_KEY_END),
				     keyNew ("/i", ELEKTRA_KEY_END), keyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = ksLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		succeed_if (ksClose (ks) == 0, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
	}
	// ksClear
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
				     keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/g", ELEKTRA_KEY_END), keyNew ("/h", ELEKTRA_KEY_END),
				     keyNew ("/i", ELEKTRA_KEY_END), keyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = ksLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		succeed_if (ksClear (ks) == 0, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
	}
	// ksAppendKey
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
				     keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/g", ELEKTRA_KEY_END), keyNew ("/h", ELEKTRA_KEY_END),
				     keyNew ("/i", ELEKTRA_KEY_END), keyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = ksLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		// insert existing one
		succeed_if (ksAppendKey (ks, keyNew ("/a", ELEKTRA_KEY_END)) > 0, "not invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		// insert new one
		succeed_if (ksAppendKey (ks, keyNew ("/k", ELEKTRA_KEY_END)) > 0, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
	}
	// ksAppend
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
				     keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/g", ELEKTRA_KEY_END), keyNew ("/h", ELEKTRA_KEY_END),
				     keyNew ("/i", ELEKTRA_KEY_END), keyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// subset of ks
		ElektraKeyset * appendSub = ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END),
					    keyNew ("/d", ELEKTRA_KEY_END), keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// super set of ks
		ElektraKeyset * appendSuper =
			ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
			       keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/k", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = ksLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		succeed_if (ksAppend (ks, appendSub) > 0, "non invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		succeed_if (ksAppend (ks, appendSuper) > 0, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
		ksDel (appendSub);
		ksDel (appendSuper);
	}
	// ksCopyInternal
	{
		ElektraKeyset * ks = ksNew (20, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
				     keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/g", ELEKTRA_KEY_END), keyNew ("/h", ELEKTRA_KEY_END),
				     keyNew ("/i", ELEKTRA_KEY_END), keyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = ksLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		succeed_if (ksCopyInternal (ks, 2, 2) > 0, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
	}
	// ksCut
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
				     keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/g", ELEKTRA_KEY_END), keyNew ("/h", ELEKTRA_KEY_END),
				     keyNew ("/i", ELEKTRA_KEY_END), keyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = ksLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		ElektraKey * cutPoint = keyNew ("/b", ELEKTRA_KEY_END);
		ElektraKeyset * cut = ksCut (ks, cutPoint);
		succeed_if (cut, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
		ksDel (cut);
		keyDel (cutPoint);
	}
	// ksPop
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
				     keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/g", ELEKTRA_KEY_END), keyNew ("/h", ELEKTRA_KEY_END),
				     keyNew ("/i", ELEKTRA_KEY_END), keyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = ksLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		ElektraKey * popKey = ksPop (ks);
		succeed_if (popKey, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
		keyDel (popKey);
	}
	// elektraKsPopAtCursor
	{
		ElektraKeyset * ks = ksNew (10, keyNew ("/a", ELEKTRA_KEY_END), keyNew ("/b", ELEKTRA_KEY_END), keyNew ("/c", ELEKTRA_KEY_END), keyNew ("/d", ELEKTRA_KEY_END),
				     keyNew ("/e", ELEKTRA_KEY_END), keyNew ("/f", ELEKTRA_KEY_END), keyNew ("/g", ELEKTRA_KEY_END), keyNew ("/h", ELEKTRA_KEY_END),
				     keyNew ("/i", ELEKTRA_KEY_END), keyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = ksLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		ElektraKey * popKey = elektraKsPopAtCursor (ks, 1);
		succeed_if (popKey, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
		keyDel (popKey);
	}
}

int main (int argc, char ** argv)
{
	printf ("KS OPMPHM      TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_keyNotFound ();
	test_Copy ();
	test_Invalidate ();

	print_result ("test_ks_opmphm");

	return nbError;
}
