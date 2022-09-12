/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <../../src/libs/elektra/backend.c>
#include <../../src/libs/elektra/mount.c>
#include <../../src/libs/elektra/split.c>
#include <../../src/libs/elektra/trie.c>
#include <tests_internal.h>

#if 1 == 0
ElektraKeyset * modules_config (void)
{
	return elektraKeysetNew (5, elektraKeyNew ("system:/elektra/modules", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

ElektraKeyset * simple_config (void)
{
	return elektraKeysetNew (5, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/root", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/root/mountpoint", ELEKTRA_KEY_VALUE, "/", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/mountpoint", ELEKTRA_KEY_VALUE, "user:/tests/simple", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

ElektraKeyset * simple_cascading (void)
{
	return elektraKeysetNew (5, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/root", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/root/mountpoint", ELEKTRA_KEY_VALUE, "/", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/mountpoint", ELEKTRA_KEY_VALUE, "/cascading/simple", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}


ElektraKeyset * set_realworld (void)
{
	return elektraKeysetNew (50, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/root", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/root/mountpoint", ELEKTRA_KEY_VALUE, "/", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/users", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/users/mountpoint", ELEKTRA_KEY_VALUE, "system:/users", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/groups", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/groups/mountpoint", ELEKTRA_KEY_VALUE, "system:/groups", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/hosts", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/hosts/mountpoint", ELEKTRA_KEY_VALUE, "system:/hosts", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/kde", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/kde/mountpoint", ELEKTRA_KEY_VALUE, "user:/sw/kde/default", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/app1", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/app1/mountpoint", ELEKTRA_KEY_VALUE, "user:/sw/apps/app1/default", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/app2", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/app2/mountpoint", ELEKTRA_KEY_VALUE, "user:/sw/apps/app2", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

// FIXME: lots of commented out tests

static void test_simple (void)
{
	//	printf ("Test simple trie\n");
	//
	//	KDB * handle = elektraCalloc (sizeof (struct _KDB));
	//	KeySet * modules = modules_config ();
	//	Plugin * backend;
	//
	//	handle->split = splitNew ();
	//	handle->defaultBackend = elektraCalloc (sizeof (struct _Backend));
	//	mountOpen (handle, simple_config (), modules, 0);
	//
	//	KeySet * ks = ksNew (15, keyNew ("user:/testkey1/below/here", KEY_END), keyNew ("user:/testkey/below1/here", KEY_END),
	//			     keyNew ("user:/testkey/below2/here", KEY_END), keyNew ("user:/tests/simple/testkey/b1/b2/down",
	// KEY_END), 			     keyNew ("user:/tests/simple/testkey/b1/b2/up", KEY_END), KS_END);
	//
	//	Split * split;
	//	Key * parentKey;
	//	Key * mp;
	//
	//	split = splitNew ();
	//
	//	parentKey = keyNew ("user:/tests/simple/below", KEY_END);
	//	succeed_if (splitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	//	succeed_if (split->size == 1, "user root + simple");
	//
	//	succeed_if (split->specsizes[0] == -1, "spec size wrong");
	//	succeed_if (split->usersizes[0] == -1, "user size wrong");
	//	succeed_if (split->systemsizes[0] == -1, "system size wrong");
	//	succeed_if (split->dirsizes[0] == -1, "dir size wrong");
	//
	//	// elektraGetCheckUpdateNeeded(split, parentKey)
	//
	//	succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
	//
	//	mp = keyNew ("user:/tests/simple", KEY_VALUE, "simple", KEY_END);
	//	compare_key (split->parents[0], mp);
	//	succeed_if (split->handles[0] != handle->defaultBackend, "should be not the default backend");
	//	keyDel (mp);
	//
	//	backend = trieLookup (handle->trie, parentKey);
	//	succeed_if (split->handles[0] == backend, "should be user backend");
	//
	//	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys");
	//	succeed_if (split->size == 2, "not correct size after appointing");
	//
	//	succeed_if (split->handles[0] != -0, "no backend");
	//	if (split->handles[0] != -0)
	//	{
	//		succeed_if (split->specsizes[0] == -1, "spec size wrong");
	//		succeed_if (split->usersizes[0] == -1, "user size wrong");
	//		succeed_if (split->systemsizes[0] == -1, "system size wrong");
	//		succeed_if (split->dirsizes[0] == -1, "dir size wrong");
	//	}
	//
	//	succeed_if (split->handles[1] == -0, "backend at bypass?");
	//
	//	succeed_if (ksGetSize (split->keysets[0]) == 2, "wrong size");
	//	succeed_if (ksGetSize (split->keysets[1]) == 3, "wrong size");
	//	succeed_if (split->handles[0] == backend, "should be user backend");
	//	succeed_if (split->handles[1] == 0, "should be default backend");
	//
	//	splitDel (split);
	//	keyDel (parentKey);
	//
	//	ksDel (ks);
	//	splitDel (handle->split);
	//	trieClose (handle->trie, 0);
	//	elektraFree (handle->defaultBackend);
	//	elektraFree (handle);
	//	ksDel (modules);
}


static void test_cascading (void)
{
	// TODO: test not fully done
	// TODO: uncomment
	//	printf ("Test simple cascading\n");
	//
	//	KDB * handle = elektraCalloc (sizeof (struct _KDB));
	//	KeySet * modules = modules_config ();
	//	Plugin * backend;
	//
	//	handle->split = splitNew ();
	//	handle->defaultBackend = elektraCalloc (sizeof (struct _Backend));
	//	mountOpen (handle, simple_cascading (), modules, 0);
	//
	//	KeySet * ks = ksNew (15, keyNew ("user:/testkey1/below/here", KEY_END), keyNew ("user:/testkey/below1/here", KEY_END),
	//			     keyNew ("user:/testkey/below2/here", KEY_END), keyNew ("user:/tests/simple/testkey/b1/b2/down",
	// KEY_END), 			     keyNew ("user:/tests/simple/testkey/b1/b2/up", KEY_END), KS_END);
	//
	//	Split * split;
	//	Key * parentKey;
	//	Key * mp;
	//
	//	split = splitNew ();
	//
	//	parentKey = keyNew ("user:/tests/simple/below", KEY_END);
	//	succeed_if (splitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	//	succeed_if (split->size == 1, "user root + simple");
	//
	//	succeed_if (split->specsizes[0] == -1, "spec size wrong");
	//	succeed_if (split->usersizes[0] == -1, "user size wrong");
	//	succeed_if (split->systemsizes[0] == -1, "system size wrong");
	//	succeed_if (split->dirsizes[0] == -1, "dir size wrong");
	//
	//	// elektraGetCheckUpdateNeeded(split, parentKey)
	//
	//	succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
	//
	//	mp = keyNew ("user", KEY_VALUE, "root", KEY_END);
	//	compare_key (split->parents[0], mp);
	//	succeed_if (split->handles[0] != handle->defaultBackend, "should be not the default backend");
	//	keyDel (mp);
	//
	//	backend = trieLookup (handle->trie, parentKey);
	//	succeed_if (split->handles[0] == backend, "should be user backend");
	//
	//	keySetName (parentKey, "user:/cascading/simple/below");
	//	/*backend = */ trieLookup (handle->trie, parentKey);
	//	// succeed_if (split->handles[1] == backend, "should be cascading backend");
	//
	//	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys");
	//	succeed_if (split->size == 2, "not correct size after appointing");
	//
	//	succeed_if (split->handles[0] != -0, "no backend");
	//	if (split->handles[0] != -0)
	//	{
	//		succeed_if (split->specsizes[0] == -1, "spec size wrong");
	//		succeed_if (split->usersizes[0] == -1, "user size wrong");
	//		succeed_if (split->systemsizes[0] == -1, "system size wrong");
	//		succeed_if (split->dirsizes[0] == -1, "dir size wrong");
	//	}
	//
	//	succeed_if (split->handles[1] == -0, "backend at bypass?");
	//
	//	// output_split (split);
	//	succeed_if (ksGetSize (split->keysets[0]) == 5, "wrong size");
	//	succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
	//	// succeed_if (split->handles[0] == backend, "should be user backend");
	//	succeed_if (split->handles[1] == 0, "should be default backend");
	//
	//	splitDel (split);
	//	keyDel (parentKey);
	//
	//	ksDel (ks);
	//	splitDel (handle->split);
	//	trieClose (handle->trie, 0);
	//	elektraFree (handle->defaultBackend);
	//	elektraFree (handle);
	//	ksDel (modules);
}


static void test_get (void)
{
	printf ("Test basic get\n");
	ElektraKdb * handle = elektraCalloc (sizeof (struct _KDB));
	handle->split = splitNew ();
	ElektraKeyset * modules = modules_config ();
	/* So we had 2 keys before in the keyset */

	ElektraKeyset * ks = elektraKeysetNew (15, elektraKeyNew ("user:/testkey1/below/here", ELEKTRA_KEY_END), elektraKeyNew ("user:/testkey/below1/here", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/testkey/below2/here", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	Split * split = splitNew ();
	ElektraKey * parentKey = elektraKeyNew ("user:/", ELEKTRA_KEY_VALUE, "default", ELEKTRA_KEY_END);

	succeed_if (mountDefault (handle, modules, 1, 0) == 0, "could not mount default backends");
	succeed_if (splitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");

	succeed_if (split->size == 1, "size of split wrong");
	succeed_if (split->specsizes[0] == -1, "spec size wrong");
	succeed_if (split->usersizes[0] == -1, "user size wrong");
	succeed_if (split->systemsizes[0] == -1, "system size wrong");
	succeed_if (split->dirsizes[0] == -1, "dir size wrong");

	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys to split");

	succeed_if (split->size == 2, "not correct size after appointing");
	succeed_if (split->handles[0] != -0, "no backend");
	if (split->handles[0] != -0)
	{
		succeed_if (split->specsizes[0] == -1, "spec size wrong");
		succeed_if (split->usersizes[0] == -1, "user size wrong");
		succeed_if (split->systemsizes[0] == -1, "system size wrong");
		succeed_if (split->dirsizes[0] == -1, "dir size wrong");
	}
	succeed_if (split->handles[1] == -0, "backend at bypass?");

	split->syncbits[0] = 3; /* Simulate a kdbGet() */
	succeed_if (splitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");
	succeed_if (split->size == 2, "not correct size after get");
	succeed_if (split->handles[0] != -0, "no backend");
	if (split->handles[0] != -0)
	{
		succeed_if (split->specsizes[0] == -1, "spec size wrong");
		succeed_if (split->usersizes[0] == 3, "user size wrong");
		succeed_if (split->systemsizes[0] == -1, "system size wrong");
		succeed_if (split->dirsizes[0] == -1, "dir size wrong");
	}

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (elektraKeysetGetSize (split->keysets[0]) == 3, "wrong size");
	succeed_if (elektraKeyNeedSync (split->keysets[0]->array[0]) == 0, "key should not need sync");
	succeed_if (elektraKeyNeedSync (split->keysets[0]->array[1]) == 0, "key should not need sync");
	succeed_if (elektraKeyNeedSync (split->keysets[0]->array[2]) == 0, "key should not need sync");
	compare_keyset (split->keysets[0], ks);
	succeed_if (elektraKeysetGetSize (split->keysets[1]) == 0, "wrong size");
	compare_key (split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 3, "should be marked as sync");

	splitDel (split);
	elektraKeyDel (parentKey);


	split = splitNew ();
	parentKey = elektraKeyNew ("system:/", ELEKTRA_KEY_VALUE, "default", ELEKTRA_KEY_END);

	succeed_if (splitBuildup (split, handle, parentKey) == 1, "system backend should be added");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");

	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys to split");
	succeed_if (splitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (elektraKeysetGetSize (split->keysets[0]) == 0, "wrong size");
	succeed_if (elektraKeysetGetSize (split->keysets[1]) == 3, "keys with other domain should go to default keyset");
	compare_keyset (split->keysets[1], ks);
	compare_key (split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");


	splitDel (split);
	elektraKeyDel (parentKey);

	elektraKeysetDel (ks);
	elektraKdbClose (handle, 0);
	elektraKeysetDel (modules);
}

static void test_limit (void)
{
	printf ("Test limit\n");
	ElektraKdb * handle = elektraCalloc (sizeof (struct _KDB));
	handle->split = splitNew ();
	/* So we had 2 keys before in the keyset */
	ElektraKeyset * modules = modules_config ();

	ElektraKeyset * ks = elektraKeysetNew (15, elektraKeyNew ("user:/testkey1/below/here", ELEKTRA_KEY_END), elektraKeyNew ("user:/testkey/below1/here", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/testkey/below2/here", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	Split * split = splitNew ();
	ElektraKey * parentKey = elektraKeyNew ("user:/", ELEKTRA_KEY_VALUE, "default", ELEKTRA_KEY_END);

	succeed_if (mountDefault (handle, modules, 1, 0) == 0, "could not mount default backends");
	succeed_if (splitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");

	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys to split");
	split->syncbits[0] = 3; /* Simulate a kdbGet() */
	elektraKeysetAppendKey (split->keysets[0], elektraKeyNew ("system:/wrong", ELEKTRA_KEY_END));
	succeed_if (splitGet (split, parentKey, handle) == 1, "could not postprocess get");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (elektraKeysetGetSize (split->keysets[0]) == 3, "wrong size");
	succeed_if (elektraKeyNeedSync (split->keysets[0]->array[0]) == 0, "key should not need sync");
	succeed_if (elektraKeyNeedSync (split->keysets[0]->array[1]) == 0, "key should not need sync");
	succeed_if (elektraKeyNeedSync (split->keysets[0]->array[2]) == 0, "key should not need sync");
	compare_keyset (split->keysets[0], ks);
	succeed_if (elektraKeysetGetSize (split->keysets[1]) == 0, "wrong size");

	// we know that system:/wrong will produce a warning
	compare_key_name (split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 3, "should be marked as root");

	splitDel (split);
	elektraKeyDel (parentKey);


	split = splitNew ();
	parentKey = elektraKeyNew ("system:/", ELEKTRA_KEY_VALUE, "default", ELEKTRA_KEY_END);

	succeed_if (splitBuildup (split, handle, parentKey) == 1, "system backend should be added");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");

	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys to split");
	split->syncbits[1] = 1; /* Simulate a kdbGet() */
	elektraKeysetAppendKey (split->keysets[1], elektraKeyNew ("user:/wrong", ELEKTRA_KEY_END));
	succeed_if (splitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (elektraKeysetGetSize (split->keysets[0]) == 0, "wrong size");
	succeed_if (elektraKeysetGetSize (split->keysets[1]) == 4, "default should stay untouched");
	succeed_if (elektraKeyNeedSync (split->keysets[1]->array[0]) == 0, "key should not need sync");
	succeed_if (elektraKeyNeedSync (split->keysets[1]->array[1]) == 0, "key should not need sync");
	succeed_if (elektraKeyNeedSync (split->keysets[1]->array[2]) == 0, "key should not need sync");
	compare_key (split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");

	splitDel (split);
	elektraKeyDel (parentKey);


	elektraKeysetDel (ks);
	elektraKdbClose (handle, 0);
	elektraKeysetDel (modules);
}


static void test_nobackend (void)
{
	printf ("Test keys without backends in split\n");

	ElektraKdb * handle = elektraCalloc (sizeof (struct _KDB));
	handle->split = splitNew ();
	ElektraKeyset * modules = modules_config ();
	Plugin * backend;

	mountOpen (handle, simple_config (), modules, 0);

	ElektraKeyset * ks = elektraKeysetNew (15, elektraKeyNew ("user:/testkey1/below/here", ELEKTRA_KEY_END), elektraKeyNew ("user:/testkey/below1/here", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/testkey/below2/here", ELEKTRA_KEY_END), elektraKeyNew ("user:/tests/simple/testkey/b1/b2/down", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/simple/testkey/b1/b2/up", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	Split * split;
	ElektraKey * parentKey;
	ElektraKey * mp;

	split = splitNew ();

	parentKey = elektraKeyNew ("user:/tests/simple/below", ELEKTRA_KEY_END);
	mp = elektraKeyNew ("user:/tests/simple", ELEKTRA_KEY_VALUE, "simple", ELEKTRA_KEY_END);
	succeed_if (splitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");

	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys");
	split->syncbits[0] = 1; /* Simulate a kdbGet() */
	elektraKeysetAppendKey (split->keysets[0], elektraKeyNew ("system:/wrong", ELEKTRA_KEY_END));

	succeed_if (splitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error (parentKey), "error found");
	// there will be a warning
	// succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (split->size == 2, "not correct size after appointing");
	succeed_if (elektraKeysetGetSize (split->keysets[0]) == 2, "wrong size");
	succeed_if (elektraKeysetGetSize (split->keysets[1]) == 3, "wrong size");
	compare_key (split->parents[0], mp);
	backend = trieLookup (handle->trie, parentKey);
	succeed_if (split->handles[0] == backend, "should be user backend");
	succeed_if (split->handles[1] == 0, "should be default backend");


	splitDel (split);
	elektraKeyDel (parentKey);
	elektraKeyDel (mp);

	// output_trie(trie);

	elektraKeysetDel (ks);
	elektraKdbClose (handle, 0);
	elektraKeysetDel (modules);
}


static void test_sizes (void)
{
	//	printf ("Test sizes\n");
	//	KDB * handle = elektraCalloc (sizeof (struct _KDB));
	//	handle->split = splitNew ();
	//	KeySet * modules = modules_config ();
	//
	//	KeySet * ks = ksNew (15, keyNew ("user:/testkey1/below/here", KEY_END), keyNew ("user:/testkey/below1/here", KEY_END),
	//			     keyNew ("user:/testkey/below2/here", KEY_END), KS_END);
	//
	//
	//	succeed_if (mountDefault (handle, modules, 1, 0) == 0, "could not mount default backends");
	//	succeed_if (handle->defaultBackend->specsize == -1, "specsize not initialized correct");
	//	succeed_if (handle->defaultBackend->dirsize == -1, "dirsize not initialized correct");
	//	succeed_if (handle->defaultBackend->usersize == -1, "usersize not initialized correct");
	//	succeed_if (handle->defaultBackend->systemsize == -1, "systemsize not initialized correct");
	//
	//	Split * split = splitNew ();
	//	Key * parentKey = keyNew ("user", KEY_VALUE, "default", KEY_END);
	//
	//	succeed_if (splitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	//	succeed_if (output_error (parentKey), "error found");
	//	succeed_if (output_warnings (parentKey), "warning(s) found");
	//
	//	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys to split");
	//	split->syncbits[0] = 3; /* Simulate a kdbGet() */
	//	ksAppendKey (split->keysets[0], keyNew ("system:/wrong", KEY_END));
	//	succeed_if (splitGet (split, parentKey, handle) == 1, "could not postprocess get");
	//	succeed_if (output_error (parentKey), "error found");
	//	// there will be a warning
	//	// succeed_if (output_warnings(parentKey), "warning(s) found");
	//	succeed_if_same_string (keyString (keyGetMeta (parentKey, "warnings/#00/number")), ELEKTRA_ERROR_INTERFACE) // drop key
	//
	//		succeed_if (handle->defaultBackend->usersize == 3, "usersize not updated by splitGet");
	//	succeed_if (handle->defaultBackend->specsize == -1, "specsize not initialized correct");
	//	succeed_if (handle->defaultBackend->dirsize == -1, "dirsize not initialized correct");
	//	succeed_if (handle->defaultBackend->systemsize == -1, "systemsize not initialized correct");
	//	succeed_if (split->size == 2, "there is an empty keset");
	//	succeed_if (ksGetSize (split->keysets[0]) == 3, "wrong size");
	//	succeed_if (keyNeedSync (split->keysets[0]->array[0]) == 0, "key should not need sync");
	//	succeed_if (keyNeedSync (split->keysets[0]->array[1]) == 0, "key should not need sync");
	//	succeed_if (keyNeedSync (split->keysets[0]->array[2]) == 0, "key should not need sync");
	//	compare_keyset (split->keysets[0], ks);
	//	succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
	//	compare_key_name (split->parents[0], parentKey);
	//	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	//	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	//	succeed_if (split->syncbits[0] == 3, "should be marked as root");
	//
	//	splitDel (split);
	//	keyDel (parentKey);
	//
	//
	//	split = splitNew ();
	//	parentKey = keyNew ("system", KEY_VALUE, "default", KEY_END);
	//
	//	succeed_if (splitBuildup (split, handle, parentKey) == 1, "system backend should be added");
	//	succeed_if (output_error (parentKey), "error found");
	//	succeed_if (output_warnings (parentKey), "warning(s) found");
	//
	//	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys to split");
	//	split->syncbits[1] = 1; /* Simulate a kdbGet() */
	//	ksAppendKey (split->keysets[1], keyNew ("user:/wrong", KEY_END));
	//	succeed_if (splitGet (split, parentKey, handle) == 1, "could not postprocess get");
	//	succeed_if (output_error (parentKey), "error found");
	//	succeed_if (output_warnings (parentKey), "warning(s) found");
	//
	//	succeed_if (handle->defaultBackend->usersize == 3, "usersize should not be updated");
	//	succeed_if (handle->defaultBackend->systemsize == 0, "systemsize not set to zero");
	//	succeed_if (handle->defaultBackend->specsize == -1, "specsize not initialized correct");
	//	succeed_if (handle->defaultBackend->dirsize == -1, "dirsize not initialized correct");
	//	succeed_if (split->size == 2, "there is an empty keset");
	//	succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
	//	succeed_if (ksGetSize (split->keysets[1]) == 4, "default should stay untouched");
	//	succeed_if (keyNeedSync (split->keysets[1]->array[0]) == 0, "key should not need sync");
	//	succeed_if (keyNeedSync (split->keysets[1]->array[1]) == 0, "key should not need sync");
	//	succeed_if (keyNeedSync (split->keysets[1]->array[2]) == 0, "key should not need sync");
	//	compare_key (split->parents[0], parentKey);
	//	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	//	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	//	succeed_if (split->syncbits[0] == 2, "should be marked as root");
	//
	//	splitDel (split);
	//	keyDel (parentKey);
	//
	//
	//	ksDel (ks);
	//	kdbClose (handle, 0);
	//	ksDel (modules);
}


static void test_triesizes (void)
{
	//	printf ("Test sizes in backends with trie\n");
	//
	//	KDB * handle = elektraCalloc (sizeof (struct _KDB));
	//	handle->split = splitNew ();
	//	KeySet * modules = modules_config ();
	//	Backend * backend = 0;
	//	Backend * rootBackend = 0;
	//
	//	mountOpen (handle, simple_config (), modules, 0);
	//	succeed_if (mountDefault (handle, modules, 1, 0) == 0, "could not mount default backends");
	//	succeed_if (handle->defaultBackend->specsize == -1, "specsize not initialized correct");
	//	succeed_if (handle->defaultBackend->dirsize == -1, "dirsize not initialized correct");
	//	succeed_if (handle->defaultBackend->usersize == -1, "usersize  not initialized correct");
	//	succeed_if (handle->defaultBackend->systemsize == -1, "systemsize not initialized correct");
	//
	//	KeySet * ks = ksNew (15, keyNew ("user:/testkey1/below/here", KEY_END), keyNew ("user:/testkey/below1/here", KEY_END),
	//			     keyNew ("user:/testkey/below2/here", KEY_END), keyNew ("user:/tests/simple/testkey/b1/b2/down",
	// KEY_END), 			     keyNew ("user:/tests/simple/testkey/b1/b2/up", KEY_END), KS_END);
	//
	//	Split * split;
	//	Key * parentKey;
	//
	//	split = splitNew ();
	//
	//	parentKey = keyNew ("user", KEY_END);
	//
	//	rootBackend = trieLookup (handle->trie, parentKey);
	//	keySetName (parentKey, "user:/tests/simple/below");
	//	backend = trieLookup (handle->trie, parentKey);
	//
	//	// now clear name so that we process all backends
	//	succeed_if (keySetName (parentKey, 0) == 0, "could not delete name of parentKey");
	//	succeed_if (handle->defaultBackend->specsize == -1, "specsize not initialized correct");
	//	succeed_if (handle->defaultBackend->dirsize == -1, "dirsize not initialized correct");
	//	succeed_if (handle->defaultBackend->usersize == -1, "usersize  not initialized correct");
	//	succeed_if (handle->defaultBackend->systemsize == -1, "systemsize not initialized correct");
	//
	//	succeed_if (splitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	//	succeed_if (output_error (parentKey), "error found");
	//	succeed_if (output_warnings (parentKey), "warning(s) found");
	//
	//	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys");
	//	split->syncbits[2] = 3; /* Simulate a kdbGet() */
	//	split->syncbits[4] = 3; /* Simulate a kdbGet() */
	//	split->syncbits[5] = 1; /* Simulate a kdbGet() */
	//
	//	succeed_if (handle->defaultBackend->specsize == -1, "specsize not initialized correct");
	//	succeed_if (handle->defaultBackend->dirsize == -1, "dirsize not initialized correct");
	//	succeed_if (handle->defaultBackend->usersize == -1, "usersize  not initialized correct");
	//	succeed_if (handle->defaultBackend->systemsize == -1, "systemsize not initialized correct");
	//
	//	succeed_if (splitGet (split, parentKey, handle) == 1, "could not postprocess get");
	//	succeed_if (output_error (parentKey), "error found");
	//	succeed_if (output_warnings (parentKey), "warning(s) found");
	//
	//	succeed_if (backend->usersize == 2, "usersize should be updated");
	//	succeed_if (backend->systemsize == -1, "systemsize should not change");
	//	succeed_if (backend->dirsize == -1, "dirsize should not change");
	//	succeed_if (backend->specsize == -1, "specsize should not change");
	//
	//	succeed_if (rootBackend->usersize == 3, "usersize of rootBackend should be updated");
	//	succeed_if (rootBackend->systemsize == 0, "systemsize of rootBackend should be updated");
	//	succeed_if (rootBackend->dirsize == 0, "dirsize of rootBackend should be updated");
	//	succeed_if (rootBackend->specsize == 0, "specsize of rootBackend should be updated");
	//
	//	succeed_if (handle->defaultBackend->specsize == -1, "specsize not initialized correct");
	//	succeed_if (handle->defaultBackend->dirsize == -1, "dirsize not initialized correct");
	//	succeed_if (handle->defaultBackend->usersize == -1, "usersize  not initialized correct");
	//	succeed_if (handle->defaultBackend->systemsize == 0, "systemsize should be updated");
	//
	//	succeed_if (split->size == 7, "not correct size after appointing");
	//	succeed_if (ksGetSize (split->keysets[2]) == 3, "wrong size");
	//	succeed_if (ksGetSize (split->keysets[4]) == 2, "wrong size");
	//
	//	succeed_if (split->handles[0] == rootBackend, "should be root backend");
	//	succeed_if (split->handles[1] == rootBackend, "should be root backend");
	//	succeed_if (split->handles[2] == rootBackend, "should be root backend");
	//	succeed_if (split->handles[3] == rootBackend, "should be root backend");
	//	succeed_if (split->handles[4] == backend, "should be mountedbackend");
	//	succeed_if (split->handles[5] == handle->defaultBackend, "should be defaultBackend");
	//
	//	Key * mp = keyNew ("user:/tests/simple", KEY_VALUE, "simple", KEY_END);
	//	compare_key (split->parents[4], mp);
	//	keyDel (mp);
	//
	//	splitDel (split);
	//	keyDel (parentKey);
	//
	//	ksDel (ks);
	//	kdbClose (handle, 0);
	//	ksDel (modules);
}


static void test_merge (void)
{
	//	printf ("Test sizes in backends with trie\n");
	//
	//	KDB * handle = elektraCalloc (sizeof (struct _KDB));
	//	handle->split = splitNew ();
	//	KeySet * modules = modules_config ();
	//	Plugin * backend = 0;
	//	Plugin * rootBackend = 0;
	//
	//	mountOpen (handle, simple_config (), modules, 0);
	//	succeed_if (mountDefault (handle, modules, 1, 0) == 0, "could not mount default backends");
	//	succeed_if (handle->defaultBackend->specsize == -1, "specsize not initialized correct");
	//	succeed_if (handle->defaultBackend->dirsize == -1, "dirsize not initialized correct");
	//	succeed_if (handle->defaultBackend->usersize == -1, "usersize  not initialized correct");
	//	succeed_if (handle->defaultBackend->systemsize == -1, "systemsize not initialized correct");
	//
	//	KeySet * ks = ksNew (15, keyNew ("user:/testkey1/below/here", KEY_END), keyNew ("user:/testkey/below1/here", KEY_END),
	//			     keyNew ("user:/testkey/below2/here", KEY_END), keyNew ("user:/tests/simple/testkey/b1/b2/down",
	// KEY_END), 			     keyNew ("user:/tests/simple/testkey/b1/b2/up", KEY_END), KS_END);
	//
	//	Split * split;
	//	Key * parentKey;
	//	Key * mp;
	//
	//	split = splitNew ();
	//
	//	parentKey = keyNew ("user", KEY_END);
	//
	//	rootBackend = trieLookup (handle->trie, parentKey);
	//	keySetName (parentKey, "user:/tests/simple/below");
	//	backend = trieLookup (handle->trie, parentKey);
	//	succeed_if (keySetName (parentKey, 0) == 0, "could not delete name of parentKey");
	//	succeed_if (backend->specsize == -1, "specsize not initialized correct");
	//	succeed_if (backend->dirsize == -1, "dirsize not initialized correct");
	//	succeed_if (backend->usersize == -1, "usersize  not initialized correct");
	//	succeed_if (backend->systemsize == -1, "systemsize not initialized correct");
	//
	//	mp = keyNew ("user:/tests/simple", KEY_VALUE, "simple", KEY_END);
	//
	//	succeed_if (splitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	//	succeed_if (output_error (parentKey), "error found");
	//	succeed_if (output_warnings (parentKey), "warning(s) found");
	//
	//	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys");
	//	split->syncbits[2] = 3; /* Simulate a kdbGet() */
	//	split->syncbits[4] = 3; /* Simulate a kdbGet() */
	//	split->syncbits[5] = 1; /* Simulate a kdbGet() */
	//
	//	succeed_if (splitGet (split, parentKey, handle) == 1, "could not postprocess get");
	//	succeed_if (output_error (parentKey), "error found");
	//	succeed_if (output_warnings (parentKey), "warning(s) found");
	//
	//	succeed_if (backend->usersize == 2, "usersize should be updated");
	//	succeed_if (backend->systemsize == -1, "systemsize should not change");
	//
	//	succeed_if (rootBackend->usersize == 3, "usersize of rootBackend should be updated");
	//	succeed_if (rootBackend->systemsize == 0, "systemsize  of rootBackend should not change");
	//
	//	succeed_if (handle->defaultBackend->specsize == -1, "specsize not initialized correct");
	//	succeed_if (handle->defaultBackend->dirsize == -1, "dirsize not initialized correct");
	//	succeed_if (handle->defaultBackend->usersize == -1, "usersize  not initialized correct");
	//	succeed_if (handle->defaultBackend->systemsize == 0, "systemsize not initialized correct");
	//
	//	// output_split(split);
	//	succeed_if (split->size == 7, "not correct size after appointing");
	//	succeed_if (ksGetSize (split->keysets[2]) == 3, "wrong size");
	//	succeed_if (ksGetSize (split->keysets[4]) == 2, "wrong size");
	//	compare_key (split->parents[4], mp);
	//	succeed_if (split->handles[0] == rootBackend, "should be root backend");
	//	succeed_if (split->handles[1] == rootBackend, "should be root backend");
	//	succeed_if (split->handles[2] == rootBackend, "should be root backend");
	//	succeed_if (split->handles[3] == rootBackend, "should be root backend");
	//	succeed_if (split->handles[4] == backend, "should be backend");
	//
	//	KeySet * nks = ksNew (0, KS_END);
	//	succeed_if (splitMergeBackends (split, nks) == 1, "could not merge together keysets from backend");
	//	succeed_if (splitMergeDefault (split, nks) == 1, "could not merge together keysets from default split");
	//	compare_keyset (ks, nks);
	//	// output_keyset (nks);
	//	ksDel (nks);
	//
	//
	//	splitDel (split);
	//	keyDel (parentKey);
	//	keyDel (mp);
	//
	//	ksDel (ks);
	//	kdbClose (handle, 0);
	//	ksDel (modules);
}


static void test_realworld (void)
{
	printf ("Test real world example\n");

	ElektraKey * parent = 0;
	ElektraKdb * handle = elektraCalloc (sizeof (struct _KDB));
	handle->split = splitNew ();
	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);

	mountOpen (handle, set_realworld (), modules, 0);
	succeed_if (mountDefault (handle, modules, 1, 0) == 0, "could not mount default backends");

	ElektraKeyset * ks = elektraKeysetNew (
		18, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/new", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/mountpoints/new/mountpoint", ELEKTRA_KEY_VALUE, "something", ELEKTRA_KEY_END), elektraKeyNew ("system:/users", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/users/markus", ELEKTRA_KEY_END), elektraKeyNew ("system:/users/harald", ELEKTRA_KEY_END), elektraKeyNew ("system:/users/n", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/users/albert", ELEKTRA_KEY_END), elektraKeyNew ("system:/hosts", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/hosts/markusbyte", ELEKTRA_KEY_VALUE, "127.0.0.1", ELEKTRA_KEY_END), elektraKeyNew ("system:/hosts/mobilebyte", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/hosts/n900", ELEKTRA_KEY_END), elektraKeyNew ("user:/sw/apps/app1/default", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/sw/apps/app1/default/maximize", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/sw/apps/app1/default/download", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/sw/apps/app1/default/keys/a", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/sw/apps/app1/default/keys/b", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/sw/apps/app1/default/keys/c", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_END), elektraKeyNew ("user:/outside", ELEKTRA_KEY_VALUE, "test", ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
	ElektraKeyset * split0 = elektraKeysetNew (9, elektraKeyNew ("user:/sw/apps/app1/default", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/sw/apps/app1/default/maximize", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/sw/apps/app1/default/download", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/sw/apps/app1/default/keys/a", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/sw/apps/app1/default/keys/b", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/sw/apps/app1/default/keys/c", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * split3 = elektraKeysetNew (9, elektraKeyNew ("system:/hosts", ELEKTRA_KEY_END), elektraKeyNew ("system:/hosts/markusbyte", ELEKTRA_KEY_VALUE, "127.0.0.1", ELEKTRA_KEY_END),
				 elektraKeyNew ("system:/hosts/mobilebyte", ELEKTRA_KEY_END), elektraKeyNew ("system:/hosts/n900", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * split7 = elektraKeysetNew (3, elektraKeyNew ("user:/outside", ELEKTRA_KEY_VALUE, "test", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * split9 = elektraKeysetNew (9, elektraKeyNew ("system:/users", ELEKTRA_KEY_END), elektraKeyNew ("system:/users/markus", ELEKTRA_KEY_END),
				 elektraKeyNew ("system:/users/harald", ELEKTRA_KEY_END), elektraKeyNew ("system:/users/n", ELEKTRA_KEY_END),
				 elektraKeyNew ("system:/users/albert", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * split10 = elektraKeysetNew (9, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/new", ELEKTRA_KEY_END),
				  elektraKeyNew ("system:/elektra/mountpoints/new/mountpoint", ELEKTRA_KEY_VALUE, "something", ELEKTRA_KEY_END), ELEKTRA_KS_END);


	Split * split = splitNew ();

	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
	succeed_if (output_error (parent), "error found");
	succeed_if (output_warnings (parent), "warning(s) found");

	succeed_if (split->size == 11, "size not correct");
	succeed_if (split->handles[5] == split->handles[6], "root backends have same handle");
	succeed_if (split->syncbits[5] == 2, "sync state for root not correct");
	succeed_if (split->syncbits[6] == 2, "sync state for root not correct");
	succeed_if_same_string (elektraKeyName (split->parents[0]), "user:/sw/apps/app1/default");
	succeed_if_same_string (elektraKeyName (split->parents[1]), "user:/sw/apps/app2");
	succeed_if_same_string (elektraKeyName (split->parents[2]), "system:/groups");
	succeed_if_same_string (elektraKeyName (split->parents[3]), "system:/hosts");
	succeed_if_same_string (elektraKeyName (split->parents[4]), "user:/sw/kde/default");
	succeed_if_same_string (elektraKeyName (split->parents[5]), "spec:/");
	succeed_if_same_string (elektraKeyName (split->parents[6]), "dir:/");
	succeed_if_same_string (elektraKeyName (split->parents[7]), "user:/");
	succeed_if_same_string (elektraKeyName (split->parents[8]), "system:/");
	succeed_if_same_string (elektraKeyName (split->parents[9]), "system:/users");
	succeed_if_same_string (elektraKeyName (split->parents[10]), "system:/elektra");

	succeed_if (splitAppoint (split, handle, ks) == 1, "should need sync");

	succeed_if (split->size == 12, "size not correct (def not added)");
	succeed_if (split->syncbits[0] == 0, "sync state not correct");
	succeed_if (split->syncbits[1] == 0, "sync state not correct");
	succeed_if (split->syncbits[2] == 0, "sync state not correct");
	succeed_if (split->syncbits[3] == 0, "sync state not correct");
	succeed_if (split->syncbits[4] == 0, "sync state not correct");
	succeed_if (split->syncbits[5] == 2, "sync state not correct");
	succeed_if (split->syncbits[6] == 2, "sync state not correct");
	succeed_if (split->syncbits[7] == 2, "sync state not correct");
	succeed_if (split->syncbits[8] == 2, "sync state not correct");
	succeed_if (split->syncbits[9] == 0, "sync state not correct");
	succeed_if (split->syncbits[10] == 2, "sync state not correct");
	compare_keyset (split->keysets[0], split0);
	compare_keyset (split->keysets[3], split3);
	compare_keyset (split->keysets[7], split7);
	compare_keyset (split->keysets[9], split9);
	compare_keyset (split->keysets[10], split10);

	split->syncbits[0] |= 1;
	split->syncbits[3] |= 1;
	split->syncbits[6] |= 1;
	split->syncbits[7] |= 1;
	succeed_if (splitGet (split, parent, handle) == 1, "postprocessing failed");
	succeed_if (output_error (parent), "error found");
	succeed_if (output_warnings (parent), "warning(s) found");

	succeed_if (split->usersizes[0] == 6, "wrong size");
	succeed_if (split->systemsizes[3] == 4, "wrong size");
	succeed_if (split->usersizes[6] == 1, "wrong size");
	succeed_if (split->systemsizes[9] == 5, "wrong size");


	ElektraKeyset * dest = elektraKeysetNew (5, elektraKeyNew ("user:/test", ELEKTRA_KEY_VALUE, "should be gone", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	elektraKeysetClear (dest);
	succeed_if (splitMergeBackends (split, dest) == 1, "split merge backends");
	succeed_if (splitMergeDefault (split, dest) == 1, "split merge default");
	compare_keyset (dest, ks);
	elektraKeysetDel (dest);

	splitDel (split);


	elektraKeysetDel (ks);
	elektraKeysetDel (split0);
	elektraKeysetDel (split3);
	elektraKeysetDel (split7);
	elektraKeysetDel (split9);
	elektraKeysetDel (split10);
	elektraModulesClose (modules, 0);
	elektraKeysetDel (modules);

	elektraKdbClose (handle, parent);
	elektraKeyDel (parent);
}
#endif

int main (int argc, char ** argv)
{
	printf ("SPLIT GET   TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

#if 1 == 0
	test_simple ();
	test_cascading ();
	test_get ();
	test_limit ();
	test_nobackend ();
	test_sizes ();
	test_triesizes ();
	test_merge ();
	test_realworld ();
#endif

	printf ("\ntest_splitget RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
