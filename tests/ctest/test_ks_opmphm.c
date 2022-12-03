/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

#include <opmphm.c>
#include <tests_internal.h>

ssize_t ksCopyInternal (KeySet * ks, size_t to, size_t from);

void test_keyNotFound (void)
{
	KeySet * ks = ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
			     keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/g", KEY_END), keyNew ("/h", KEY_END),
			     keyNew ("/i", KEY_END), keyNew ("/j", KEY_END), KS_END);

	Key * found = ksLookupByName (ks, "/nothere", KDB_O_OPMPHM);
	succeed_if (!found, "key found");

	exit_if_fail (ks->data->opmphm, "build opmphm");
	succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

	ksDel (ks);
}

void test_Copy (void)
{
	// ksDup
	{
		KeySet * ks = ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
				     keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/g", KEY_END), keyNew ("/h", KEY_END),
				     keyNew ("/i", KEY_END), keyNew ("/j", KEY_END), KS_END);

		// trigger build
		Key * found = ksLookupByName (ks, "/a", KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

		KeySet * copy = ksDup (ks);
		succeed_if (copy, "copy");

		// opmphm should be build
		exit_if_fail (copy->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (copy->data->opmphm), "build opmphm");

		// test opmphm
		Key * iter;
		ksRewind (copy);
		while ((iter = ksNext (copy)))
		{
			Key * f = ksLookup (copy, iter, KDB_O_OPMPHM);
			succeed_if (f, "key found");
		}

		// cleanup
		ksDel (ks);
		ksDel (copy);
	}
	// ksDeepDup
	{
		KeySet * ks = ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
				     keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/g", KEY_END), keyNew ("/h", KEY_END),
				     keyNew ("/i", KEY_END), keyNew ("/j", KEY_END), KS_END);

		// trigger build
		Key * found = ksLookupByName (ks, "/a", KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

		KeySet * copy = ksDeepDup (ks);
		succeed_if (copy, "copy");

		// opmphm should be build
		exit_if_fail (copy->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (copy->data->opmphm), "build opmphm");

		// test opmphm
		Key * iter;
		ksRewind (copy);
		while ((iter = ksNext (copy)))
		{
			Key * f = ksLookup (copy, iter, KDB_O_OPMPHM);
			succeed_if (f, "key found");
		}

		// cleanup
		ksDel (ks);
		ksDel (copy);
	}
	// ksCopy
	{
		KeySet * ks = ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
				     keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/g", KEY_END), keyNew ("/h", KEY_END),
				     keyNew ("/i", KEY_END), keyNew ("/j", KEY_END), KS_END);

		// trigger build
		Key * found = ksLookupByName (ks, "/a", KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

		KeySet * copy = ksNew (0, KS_END);
		succeed_if (ksCopy (copy, ks) == 1, "copy");

		// opmphm should be build
		exit_if_fail (copy->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (copy->data->opmphm), "build opmphm");

		// test opmphm
		Key * iter;
		ksRewind (copy);
		while ((iter = ksNext (copy)))
		{
			Key * f = ksLookup (copy, iter, KDB_O_OPMPHM);
			succeed_if (f, "key found");
		}

		// cleanup
		ksDel (ks);
		ksDel (copy);
	}
	// test not build copy
	{
		KeySet * ks = ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
				     keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/g", KEY_END), keyNew ("/h", KEY_END),
				     keyNew ("/i", KEY_END), keyNew ("/j", KEY_END), KS_END);

		KeySet * copy = ksDup (ks);
		succeed_if (copy, "copy");

		// opmphm should not be build
		succeed_if (!copy->data->opmphm, "not build opmphm");

		// cleanup
		ksDel (ks);
		ksDel (copy);
	}
}

void test_Invalidate (void)
{
	// ksClose
	{
		KeySet * ks = ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
				     keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/g", KEY_END), keyNew ("/h", KEY_END),
				     keyNew ("/i", KEY_END), keyNew ("/j", KEY_END), KS_END);

		// trigger build
		Key * found = ksLookupByName (ks, "/a", KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

		succeed_if (ksClose (ks) == 0, "invalidate");

		// TODO (atmaxinger): ksClose removes and deletes opmphm, we can't check it here - maybe somewhere else?
		// exit_if_fail (ks->data->opmphm, "build opmphm");
		// succeed_if (!opmphmIsBuild (ks->data->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
	}
	// ksClear
	{
		KeySet * ks = ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
				     keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/g", KEY_END), keyNew ("/h", KEY_END),
				     keyNew ("/i", KEY_END), keyNew ("/j", KEY_END), KS_END);

		// trigger build
		Key * found = ksLookupByName (ks, "/a", KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

		succeed_if (ksClear (ks) == 0, "invalidate");

		// TODO (atmaxinger): ksClear removes and deletes opmphm, we can't check it here - maybe somewhere else?
		// exit_if_fail (ks->data->opmphm, "build opmphm");
		// succeed_if (!opmphmIsBuild (ks->data->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
	}
	// ksAppendKey
	{
		KeySet * ks = ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
				     keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/g", KEY_END), keyNew ("/h", KEY_END),
				     keyNew ("/i", KEY_END), keyNew ("/j", KEY_END), KS_END);

		// trigger build
		Key * found = ksLookupByName (ks, "/a", KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

		// insert existing one
		succeed_if (ksAppendKey (ks, keyNew ("/a", KEY_END)) > 0, "not invalidate");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

		// insert new one
		succeed_if (ksAppendKey (ks, keyNew ("/k", KEY_END)) > 0, "invalidate");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->data->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
	}
	// ksAppend
	{
		KeySet * ks = ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
				     keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/g", KEY_END), keyNew ("/h", KEY_END),
				     keyNew ("/i", KEY_END), keyNew ("/j", KEY_END), KS_END);

		// subset of ks
		KeySet * appendSub = ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END),
					    keyNew ("/d", KEY_END), keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), KS_END);

		// super set of ks
		KeySet * appendSuper =
			ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
			       keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/k", KEY_END), KS_END);

		// trigger build
		Key * found = ksLookupByName (ks, "/a", KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

		succeed_if (ksAppend (ks, appendSub) > 0, "non invalidate");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

		succeed_if (ksAppend (ks, appendSuper) > 0, "invalidate");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->data->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
		ksDel (appendSub);
		ksDel (appendSuper);
	}
	// ksCopyInternal
	{
		KeySet * ks = ksNew (20, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
				     keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/g", KEY_END), keyNew ("/h", KEY_END),
				     keyNew ("/i", KEY_END), keyNew ("/j", KEY_END), KS_END);

		// trigger build
		Key * found = ksLookupByName (ks, "/a", KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

		succeed_if (ksCopyInternal (ks, 2, 2) > 0, "invalidate");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->data->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
	}
	// ksCut
	{
		KeySet * ks = ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
				     keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/g", KEY_END), keyNew ("/h", KEY_END),
				     keyNew ("/i", KEY_END), keyNew ("/j", KEY_END), KS_END);

		// trigger build
		Key * found = ksLookupByName (ks, "/a", KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

		Key * cutPoint = keyNew ("/b", KEY_END);
		KeySet * cut = ksCut (ks, cutPoint);
		succeed_if (cut, "invalidate");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->data->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
		ksDel (cut);
		keyDel (cutPoint);
	}
	// ksPop
	{
		KeySet * ks = ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
				     keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/g", KEY_END), keyNew ("/h", KEY_END),
				     keyNew ("/i", KEY_END), keyNew ("/j", KEY_END), KS_END);

		// trigger build
		Key * found = ksLookupByName (ks, "/a", KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

		Key * popKey = ksPop (ks);
		succeed_if (popKey, "invalidate");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->data->opmphm), "empty opmphm");

		// cleanup
		ksDel (ks);
		keyDel (popKey);
	}
	// elektraKsPopAtCursor
	{
		KeySet * ks = ksNew (10, keyNew ("/a", KEY_END), keyNew ("/b", KEY_END), keyNew ("/c", KEY_END), keyNew ("/d", KEY_END),
				     keyNew ("/e", KEY_END), keyNew ("/f", KEY_END), keyNew ("/g", KEY_END), keyNew ("/h", KEY_END),
				     keyNew ("/i", KEY_END), keyNew ("/j", KEY_END), KS_END);

		// trigger build
		Key * found = ksLookupByName (ks, "/a", KDB_O_OPMPHM);
		succeed_if (found, "key found");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (opmphmIsBuild (ks->data->opmphm), "build opmphm");

		Key * popKey = elektraKsPopAtCursor (ks, 1);
		succeed_if (popKey, "invalidate");

		exit_if_fail (ks->data->opmphm, "build opmphm");
		succeed_if (!opmphmIsBuild (ks->data->opmphm), "empty opmphm");

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
