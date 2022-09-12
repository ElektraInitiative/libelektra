/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

#include <opmphm.c>
#include <tests_internal.h>

ssize_t elektraKeysetCopyInternal (ElektraKeyset * ks, size_t to, size_t from);

void test_keyNotFound (void)
{
	ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
			     elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/g", ELEKTRA_KEY_END), elektraKeyNew ("/h", ELEKTRA_KEY_END),
			     elektraKeyNew ("/i", ELEKTRA_KEY_END), elektraKeyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKey * found = elektraKeysetLookupByName (ks, "/nothere", ELEKTRA_KDB_O_OPMPHM);
	succeed_if (!found, "key found");

	exit_if_fail (ks->opmphm, "build opmphm");
	succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

	elektraKeysetDel (ks);
}

void test_Copy (void)
{
	// ksDup
	{
		ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
				     elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/g", ELEKTRA_KEY_END), elektraKeyNew ("/h", ELEKTRA_KEY_END),
				     elektraKeyNew ("/i", ELEKTRA_KEY_END), elektraKeyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = elektraKeysetLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		ElektraKeyset * copy = elektraKeysetDup (ks);
		succeed_if (copy, "copy");

		// opmphm should be build
		exit_if_fail (copy->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (copy->opmphm), "build opmphm");

		// test opmphm
		ElektraKey * iter;
		elektraKeysetRewind (copy);
		while ((iter = elektraKeysetNext (copy)))
		{
			ElektraKey * f = elektraKeysetLookup (copy, iter, ELEKTRA_KDB_O_OPMPHM);
			succeed_if (f, "key found");
		}

		// cleanup
		elektraKeysetDel (ks);
		elektraKeysetDel (copy);
	}
	// ksDeepDup
	{
		ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
				     elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/g", ELEKTRA_KEY_END), elektraKeyNew ("/h", ELEKTRA_KEY_END),
				     elektraKeyNew ("/i", ELEKTRA_KEY_END), elektraKeyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = elektraKeysetLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		ElektraKeyset * copy = elektraKeysetDeepDup (ks);
		succeed_if (copy, "copy");

		// opmphm should be build
		exit_if_fail (copy->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (copy->opmphm), "build opmphm");

		// test opmphm
		ElektraKey * iter;
		elektraKeysetRewind (copy);
		while ((iter = elektraKeysetNext (copy)))
		{
			ElektraKey * f = elektraKeysetLookup (copy, iter, ELEKTRA_KDB_O_OPMPHM);
			succeed_if (f, "key found");
		}

		// cleanup
		elektraKeysetDel (ks);
		elektraKeysetDel (copy);
	}
	// ksCopy
	{
		ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
				     elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/g", ELEKTRA_KEY_END), elektraKeyNew ("/h", ELEKTRA_KEY_END),
				     elektraKeyNew ("/i", ELEKTRA_KEY_END), elektraKeyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = elektraKeysetLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		ElektraKeyset * copy = elektraKeysetNew (0, ELEKTRA_KS_END);
		succeed_if (elektraKeysetCopy (copy, ks) == 1, "copy");

		// opmphm should be build
		exit_if_fail (copy->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (copy->opmphm), "build opmphm");

		// test opmphm
		ElektraKey * iter;
		elektraKeysetRewind (copy);
		while ((iter = elektraKeysetNext (copy)))
		{
			ElektraKey * f = elektraKeysetLookup (copy, iter, ELEKTRA_KDB_O_OPMPHM);
			succeed_if (f, "key found");
		}

		// cleanup
		elektraKeysetDel (ks);
		elektraKeysetDel (copy);
	}
	// test not build copy
	{
		ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
				     elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/g", ELEKTRA_KEY_END), elektraKeyNew ("/h", ELEKTRA_KEY_END),
				     elektraKeyNew ("/i", ELEKTRA_KEY_END), elektraKeyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		ElektraKeyset * copy = elektraKeysetDup (ks);
		succeed_if (copy, "copy");

		// opmphm should not be build
		succeed_if (!copy->opmphm, "not build opmphm");

		// cleanup
		elektraKeysetDel (ks);
		elektraKeysetDel (copy);
	}
}

void test_Invalidate (void)
{
	// ksClose
	{
		ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
				     elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/g", ELEKTRA_KEY_END), elektraKeyNew ("/h", ELEKTRA_KEY_END),
				     elektraKeyNew ("/i", ELEKTRA_KEY_END), elektraKeyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = elektraKeysetLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		succeed_if (elektraKeysetClose (ks) == 0, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		elektraKeysetDel (ks);
	}
	// ksClear
	{
		ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
				     elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/g", ELEKTRA_KEY_END), elektraKeyNew ("/h", ELEKTRA_KEY_END),
				     elektraKeyNew ("/i", ELEKTRA_KEY_END), elektraKeyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = elektraKeysetLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		succeed_if (elektraKeysetClear (ks) == 0, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		elektraKeysetDel (ks);
	}
	// ksAppendKey
	{
		ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
				     elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/g", ELEKTRA_KEY_END), elektraKeyNew ("/h", ELEKTRA_KEY_END),
				     elektraKeyNew ("/i", ELEKTRA_KEY_END), elektraKeyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = elektraKeysetLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		// insert existing one
		succeed_if (elektraKeysetAppendKey (ks, elektraKeyNew ("/a", ELEKTRA_KEY_END)) > 0, "not invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		// insert new one
		succeed_if (elektraKeysetAppendKey (ks, elektraKeyNew ("/k", ELEKTRA_KEY_END)) > 0, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		elektraKeysetDel (ks);
	}
	// ksAppend
	{
		ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
				     elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/g", ELEKTRA_KEY_END), elektraKeyNew ("/h", ELEKTRA_KEY_END),
				     elektraKeyNew ("/i", ELEKTRA_KEY_END), elektraKeyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// subset of ks
		ElektraKeyset * appendSub = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END),
					    elektraKeyNew ("/d", ELEKTRA_KEY_END), elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// super set of ks
		ElektraKeyset * appendSuper =
			elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
			       elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/k", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = elektraKeysetLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		succeed_if (elektraKeysetAppend (ks, appendSub) > 0, "non invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		succeed_if (elektraKeysetAppend (ks, appendSuper) > 0, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		elektraKeysetDel (ks);
		elektraKeysetDel (appendSub);
		elektraKeysetDel (appendSuper);
	}
	// ksCopyInternal
	{
		ElektraKeyset * ks = elektraKeysetNew (20, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
				     elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/g", ELEKTRA_KEY_END), elektraKeyNew ("/h", ELEKTRA_KEY_END),
				     elektraKeyNew ("/i", ELEKTRA_KEY_END), elektraKeyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = elektraKeysetLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		succeed_if (elektraKeysetCopyInternal (ks, 2, 2) > 0, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		elektraKeysetDel (ks);
	}
	// ksCut
	{
		ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
				     elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/g", ELEKTRA_KEY_END), elektraKeyNew ("/h", ELEKTRA_KEY_END),
				     elektraKeyNew ("/i", ELEKTRA_KEY_END), elektraKeyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = elektraKeysetLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		ElektraKey * cutPoint = elektraKeyNew ("/b", ELEKTRA_KEY_END);
		ElektraKeyset * cut = elektraKeysetCut (ks, cutPoint);
		succeed_if (cut, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		elektraKeysetDel (ks);
		elektraKeysetDel (cut);
		elektraKeyDel (cutPoint);
	}
	// ksPop
	{
		ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
				     elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/g", ELEKTRA_KEY_END), elektraKeyNew ("/h", ELEKTRA_KEY_END),
				     elektraKeyNew ("/i", ELEKTRA_KEY_END), elektraKeyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = elektraKeysetLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		ElektraKey * popKey = elektraKeysetPop (ks);
		succeed_if (popKey, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		elektraKeysetDel (ks);
		elektraKeyDel (popKey);
	}
	// elektraKsPopAtCursor
	{
		ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_END), elektraKeyNew ("/b", ELEKTRA_KEY_END), elektraKeyNew ("/c", ELEKTRA_KEY_END), elektraKeyNew ("/d", ELEKTRA_KEY_END),
				     elektraKeyNew ("/e", ELEKTRA_KEY_END), elektraKeyNew ("/f", ELEKTRA_KEY_END), elektraKeyNew ("/g", ELEKTRA_KEY_END), elektraKeyNew ("/h", ELEKTRA_KEY_END),
				     elektraKeyNew ("/i", ELEKTRA_KEY_END), elektraKeyNew ("/j", ELEKTRA_KEY_END), ELEKTRA_KS_END);

		// trigger build
		ElektraKey * found = elektraKeysetLookupByName (ks, "/a", ELEKTRA_KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->opmphm), "build opmphm");

		ElektraKey * popKey = elektraKsPopAtCursor (ks, 1);
		succeed_if (popKey, "invalidate");

		exit_if_fail (ks->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->opmphm), "empty opmphm");

		// cleanup
		elektraKeysetDel (ks);
		elektraKeyDel (popKey);
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
