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

KeySet * modules_config (void)
{
	return ksNew (5, keyNew ("system/elektra/modules", KEY_END), KS_END);
}

KeySet * simple_config (void)
{
	return ksNew (5, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/root", KEY_END),
		      keyNew ("system/elektra/mountpoints/root/mountpoint", KEY_VALUE, "/", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/simple", KEY_END), KS_END);
}

KeySet * simple_cascading (void)
{
	return ksNew (5, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/root", KEY_END),
		      keyNew ("system/elektra/mountpoints/root/mountpoint", KEY_VALUE, "/", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "/cascading/simple", KEY_END), KS_END);
}


KeySet * set_realworld (void)
{
	return ksNew (50, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/root", KEY_END),
		      keyNew ("system/elektra/mountpoints/root/mountpoint", KEY_VALUE, "/", KEY_END),
		      keyNew ("system/elektra/mountpoints/users", KEY_END),
		      keyNew ("system/elektra/mountpoints/users/mountpoint", KEY_VALUE, "system/users", KEY_END),
		      keyNew ("system/elektra/mountpoints/groups", KEY_END),
		      keyNew ("system/elektra/mountpoints/groups/mountpoint", KEY_VALUE, "system/groups", KEY_END),
		      keyNew ("system/elektra/mountpoints/hosts", KEY_END),
		      keyNew ("system/elektra/mountpoints/hosts/mountpoint", KEY_VALUE, "system/hosts", KEY_END),
		      keyNew ("system/elektra/mountpoints/kde", KEY_END),
		      keyNew ("system/elektra/mountpoints/kde/mountpoint", KEY_VALUE, "user/sw/kde/default", KEY_END),
		      keyNew ("system/elektra/mountpoints/app1", KEY_END),
		      keyNew ("system/elektra/mountpoints/app1/mountpoint", KEY_VALUE, "user/sw/apps/app1/default", KEY_END),
		      keyNew ("system/elektra/mountpoints/app2", KEY_END),
		      keyNew ("system/elektra/mountpoints/app2/mountpoint", KEY_VALUE, "user/sw/apps/app2", KEY_END), KS_END);
}


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
//	KeySet * ks = ksNew (15, keyNew ("user/testkey1/below/here", KEY_END), keyNew ("user/testkey/below1/here", KEY_END),
//			     keyNew ("user/testkey/below2/here", KEY_END), keyNew ("user/tests/simple/testkey/b1/b2/down", KEY_END),
//			     keyNew ("user/tests/simple/testkey/b1/b2/up", KEY_END), KS_END);
//
//	Split * split;
//	Key * parentKey;
//	Key * mp;
//
//	split = splitNew ();
//
//	parentKey = keyNew ("user/tests/simple/below", KEY_END);
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
//	mp = keyNew ("user/tests/simple", KEY_VALUE, "simple", KEY_END);
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
//	KeySet * ks = ksNew (15, keyNew ("user/testkey1/below/here", KEY_END), keyNew ("user/testkey/below1/here", KEY_END),
//			     keyNew ("user/testkey/below2/here", KEY_END), keyNew ("user/tests/simple/testkey/b1/b2/down", KEY_END),
//			     keyNew ("user/tests/simple/testkey/b1/b2/up", KEY_END), KS_END);
//
//	Split * split;
//	Key * parentKey;
//	Key * mp;
//
//	split = splitNew ();
//
//	parentKey = keyNew ("user/tests/simple/below", KEY_END);
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
//	keySetName (parentKey, "user/cascading/simple/below");
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
	KDB * handle = elektraCalloc (sizeof (struct _KDB));
	handle->split = splitNew ();
	KeySet * modules = modules_config ();
	/* So we had 2 keys before in the keyset */

	KeySet * ks = ksNew (15, keyNew ("user/testkey1/below/here", KEY_END), keyNew ("user/testkey/below1/here", KEY_END),
			     keyNew ("user/testkey/below2/here", KEY_END), KS_END);

	Split * split = splitNew ();
	Key * parentKey = keyNew ("user", KEY_VALUE, "default", KEY_END);

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
	succeed_if (ksGetSize (split->keysets[0]) == 3, "wrong size");
	succeed_if (keyNeedSync (split->keysets[0]->array[0]) == 0, "key should not need sync");
	succeed_if (keyNeedSync (split->keysets[0]->array[1]) == 0, "key should not need sync");
	succeed_if (keyNeedSync (split->keysets[0]->array[2]) == 0, "key should not need sync");
	compare_keyset (split->keysets[0], ks);
	succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
	compare_key (split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 3, "should be marked as sync");

	splitDel (split);
	keyDel (parentKey);


	split = splitNew ();
	parentKey = keyNew ("system", KEY_VALUE, "default", KEY_END);

	succeed_if (splitBuildup (split, handle, parentKey) == 1, "system backend should be added");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");

	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys to split");
	succeed_if (splitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
	succeed_if (ksGetSize (split->keysets[1]) == 3, "keys with other domain should go to default keyset");
	compare_keyset (split->keysets[1], ks);
	compare_key (split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");


	splitDel (split);
	keyDel (parentKey);

	ksDel (ks);
	kdbClose (handle, 0);
	ksDel (modules);
}

static void test_limit (void)
{
	printf ("Test limit\n");
	KDB * handle = elektraCalloc (sizeof (struct _KDB));
	handle->split = splitNew ();
	/* So we had 2 keys before in the keyset */
	KeySet * modules = modules_config ();

	KeySet * ks = ksNew (15, keyNew ("user/testkey1/below/here", KEY_END), keyNew ("user/testkey/below1/here", KEY_END),
			     keyNew ("user/testkey/below2/here", KEY_END), KS_END);

	Split * split = splitNew ();
	Key * parentKey = keyNew ("user", KEY_VALUE, "default", KEY_END);

	succeed_if (mountDefault (handle, modules, 1, 0) == 0, "could not mount default backends");
	succeed_if (splitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");

	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys to split");
	split->syncbits[0] = 3; /* Simulate a kdbGet() */
	ksAppendKey (split->keysets[0], keyNew ("system/wrong", KEY_END));
	succeed_if (splitGet (split, parentKey, handle) == 1, "could not postprocess get");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize (split->keysets[0]) == 3, "wrong size");
	succeed_if (keyNeedSync (split->keysets[0]->array[0]) == 0, "key should not need sync");
	succeed_if (keyNeedSync (split->keysets[0]->array[1]) == 0, "key should not need sync");
	succeed_if (keyNeedSync (split->keysets[0]->array[2]) == 0, "key should not need sync");
	compare_keyset (split->keysets[0], ks);
	succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");

	// we know that system/wrong will produce a warning
	compare_key_name (split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 3, "should be marked as root");

	splitDel (split);
	keyDel (parentKey);


	split = splitNew ();
	parentKey = keyNew ("system", KEY_VALUE, "default", KEY_END);

	succeed_if (splitBuildup (split, handle, parentKey) == 1, "system backend should be added");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");

	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys to split");
	split->syncbits[1] = 1; /* Simulate a kdbGet() */
	ksAppendKey (split->keysets[1], keyNew ("user/wrong", KEY_END));
	succeed_if (splitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
	succeed_if (ksGetSize (split->keysets[1]) == 4, "default should stay untouched");
	succeed_if (keyNeedSync (split->keysets[1]->array[0]) == 0, "key should not need sync");
	succeed_if (keyNeedSync (split->keysets[1]->array[1]) == 0, "key should not need sync");
	succeed_if (keyNeedSync (split->keysets[1]->array[2]) == 0, "key should not need sync");
	compare_key (split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");

	splitDel (split);
	keyDel (parentKey);


	ksDel (ks);
	kdbClose (handle, 0);
	ksDel (modules);
}


static void test_nobackend (void)
{
	printf ("Test keys without backends in split\n");

	KDB * handle = elektraCalloc (sizeof (struct _KDB));
	handle->split = splitNew ();
	KeySet * modules = modules_config ();
	Plugin * backend;

	mountOpen (handle, simple_config (), modules, 0);

	KeySet * ks = ksNew (15, keyNew ("user/testkey1/below/here", KEY_END), keyNew ("user/testkey/below1/here", KEY_END),
			     keyNew ("user/testkey/below2/here", KEY_END), keyNew ("user/tests/simple/testkey/b1/b2/down", KEY_END),
			     keyNew ("user/tests/simple/testkey/b1/b2/up", KEY_END), KS_END);

	Split * split;
	Key * parentKey;
	Key * mp;

	split = splitNew ();

	parentKey = keyNew ("user/tests/simple/below", KEY_END);
	mp = keyNew ("user/tests/simple", KEY_VALUE, "simple", KEY_END);
	succeed_if (splitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (output_error (parentKey), "error found");
	succeed_if (output_warnings (parentKey), "warning(s) found");

	succeed_if (splitAppoint (split, handle, ks) == 1, "could not appoint keys");
	split->syncbits[0] = 1; /* Simulate a kdbGet() */
	ksAppendKey (split->keysets[0], keyNew ("system/wrong", KEY_END));

	succeed_if (splitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error (parentKey), "error found");
	// there will be a warning
	// succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (split->size == 2, "not correct size after appointing");
	succeed_if (ksGetSize (split->keysets[0]) == 2, "wrong size");
	succeed_if (ksGetSize (split->keysets[1]) == 3, "wrong size");
	compare_key (split->parents[0], mp);
	backend = trieLookup (handle->trie, parentKey);
	succeed_if (split->handles[0] == backend, "should be user backend");
	succeed_if (split->handles[1] == 0, "should be default backend");


	splitDel (split);
	keyDel (parentKey);
	keyDel (mp);

	// output_trie(trie);

	ksDel (ks);
	kdbClose (handle, 0);
	ksDel (modules);
}


static void test_sizes (void)
{
//	printf ("Test sizes\n");
//	KDB * handle = elektraCalloc (sizeof (struct _KDB));
//	handle->split = splitNew ();
//	KeySet * modules = modules_config ();
//
//	KeySet * ks = ksNew (15, keyNew ("user/testkey1/below/here", KEY_END), keyNew ("user/testkey/below1/here", KEY_END),
//			     keyNew ("user/testkey/below2/here", KEY_END), KS_END);
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
//	ksAppendKey (split->keysets[0], keyNew ("system/wrong", KEY_END));
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
//	ksAppendKey (split->keysets[1], keyNew ("user/wrong", KEY_END));
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
//	KeySet * ks = ksNew (15, keyNew ("user/testkey1/below/here", KEY_END), keyNew ("user/testkey/below1/here", KEY_END),
//			     keyNew ("user/testkey/below2/here", KEY_END), keyNew ("user/tests/simple/testkey/b1/b2/down", KEY_END),
//			     keyNew ("user/tests/simple/testkey/b1/b2/up", KEY_END), KS_END);
//
//	Split * split;
//	Key * parentKey;
//
//	split = splitNew ();
//
//	parentKey = keyNew ("user", KEY_END);
//
//	rootBackend = trieLookup (handle->trie, parentKey);
//	keySetName (parentKey, "user/tests/simple/below");
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
//	Key * mp = keyNew ("user/tests/simple", KEY_VALUE, "simple", KEY_END);
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
//	KeySet * ks = ksNew (15, keyNew ("user/testkey1/below/here", KEY_END), keyNew ("user/testkey/below1/here", KEY_END),
//			     keyNew ("user/testkey/below2/here", KEY_END), keyNew ("user/tests/simple/testkey/b1/b2/down", KEY_END),
//			     keyNew ("user/tests/simple/testkey/b1/b2/up", KEY_END), KS_END);
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
//	keySetName (parentKey, "user/tests/simple/below");
//	backend = trieLookup (handle->trie, parentKey);
//	succeed_if (keySetName (parentKey, 0) == 0, "could not delete name of parentKey");
//	succeed_if (backend->specsize == -1, "specsize not initialized correct");
//	succeed_if (backend->dirsize == -1, "dirsize not initialized correct");
//	succeed_if (backend->usersize == -1, "usersize  not initialized correct");
//	succeed_if (backend->systemsize == -1, "systemsize not initialized correct");
//
//	mp = keyNew ("user/tests/simple", KEY_VALUE, "simple", KEY_END);
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

	Key * parent = 0;
	KDB * handle = elektraCalloc (sizeof (struct _KDB));
	handle->split = splitNew ();
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	mountOpen (handle, set_realworld (), modules, 0);
	succeed_if (mountDefault (handle, modules, 1, 0) == 0, "could not mount default backends");

	KeySet * ks =
		ksNew (18, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/new", KEY_END),
		       keyNew ("system/elektra/mountpoints/new/mountpoint", KEY_VALUE, "something", KEY_END),
		       keyNew ("system/users", KEY_END), keyNew ("system/users/markus", KEY_END), keyNew ("system/users/harald", KEY_END),
		       keyNew ("system/users/n", KEY_END), keyNew ("system/users/albert", KEY_END), keyNew ("system/hosts", KEY_END),
		       keyNew ("system/hosts/markusbyte", KEY_VALUE, "127.0.0.1", KEY_END), keyNew ("system/hosts/mobilebyte", KEY_END),
		       keyNew ("system/hosts/n900", KEY_END), keyNew ("user/sw/apps/app1/default", KEY_END),
		       keyNew ("user/sw/apps/app1/default/maximize", KEY_VALUE, "1", KEY_END),
		       keyNew ("user/sw/apps/app1/default/download", KEY_VALUE, "0", KEY_END),
		       keyNew ("user/sw/apps/app1/default/keys/a", KEY_VALUE, "a", KEY_END),
		       keyNew ("user/sw/apps/app1/default/keys/b", KEY_VALUE, "b", KEY_END),
		       keyNew ("user/sw/apps/app1/default/keys/c", KEY_VALUE, "c", KEY_END),
		       keyNew ("user/outside", KEY_VALUE, "test", KEY_END), KS_END);
	KeySet * split0 = ksNew (9, keyNew ("user/sw/apps/app1/default", KEY_END),
				 keyNew ("user/sw/apps/app1/default/maximize", KEY_VALUE, "1", KEY_END),
				 keyNew ("user/sw/apps/app1/default/download", KEY_VALUE, "0", KEY_END),
				 keyNew ("user/sw/apps/app1/default/keys/a", KEY_VALUE, "a", KEY_END),
				 keyNew ("user/sw/apps/app1/default/keys/b", KEY_VALUE, "b", KEY_END),
				 keyNew ("user/sw/apps/app1/default/keys/c", KEY_VALUE, "c", KEY_END), KS_END);
	KeySet * split3 = ksNew (9, keyNew ("system/hosts", KEY_END), keyNew ("system/hosts/markusbyte", KEY_VALUE, "127.0.0.1", KEY_END),
				 keyNew ("system/hosts/mobilebyte", KEY_END), keyNew ("system/hosts/n900", KEY_END), KS_END);
	KeySet * split7 = ksNew (3, keyNew ("user/outside", KEY_VALUE, "test", KEY_END), KS_END);
	KeySet * split9 = ksNew (9, keyNew ("system/users", KEY_END), keyNew ("system/users/markus", KEY_END),
				 keyNew ("system/users/harald", KEY_END), keyNew ("system/users/n", KEY_END),
				 keyNew ("system/users/albert", KEY_END), KS_END);
	KeySet * split10 = ksNew (9, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/new", KEY_END),
				  keyNew ("system/elektra/mountpoints/new/mountpoint", KEY_VALUE, "something", KEY_END), KS_END);


	Split * split = splitNew ();

	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
	succeed_if (output_error (parent), "error found");
	succeed_if (output_warnings (parent), "warning(s) found");

	succeed_if (split->size == 11, "size not correct");
	succeed_if (split->handles[5] == split->handles[6], "root backends have same handle");
	succeed_if (split->syncbits[5] == 2, "sync state for root not correct");
	succeed_if (split->syncbits[6] == 2, "sync state for root not correct");
	succeed_if_same_string (keyName (split->parents[0]), "user/sw/apps/app1/default");
	succeed_if_same_string (keyName (split->parents[1]), "user/sw/apps/app2");
	succeed_if_same_string (keyName (split->parents[2]), "system/groups");
	succeed_if_same_string (keyName (split->parents[3]), "system/hosts");
	succeed_if_same_string (keyName (split->parents[4]), "user/sw/kde/default");
	succeed_if_same_string (keyName (split->parents[5]), "spec");
	succeed_if_same_string (keyName (split->parents[6]), "dir");
	succeed_if_same_string (keyName (split->parents[7]), "user");
	succeed_if_same_string (keyName (split->parents[8]), "system");
	succeed_if_same_string (keyName (split->parents[9]), "system/users");
	succeed_if_same_string (keyName (split->parents[10]), "system/elektra");

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


	KeySet * dest = ksNew (5, keyNew ("user/test", KEY_VALUE, "should be gone", KEY_END), KS_END);
	ksClear (dest);
	succeed_if (splitMergeBackends (split, dest) == 1, "split merge backends");
	succeed_if (splitMergeDefault (split, dest) == 1, "split merge default");
	compare_keyset (dest, ks);
	ksDel (dest);

	splitDel (split);


	ksDel (ks);
	ksDel (split0);
	ksDel (split3);
	ksDel (split7);
	ksDel (split9);
	ksDel (split10);
	elektraModulesClose (modules, 0);
	ksDel (modules);

	kdbClose (handle, parent);
	keyDel (parent);
}


int main (int argc, char ** argv)
{
	printf ("SPLIT GET   TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_simple ();
	test_cascading ();
	test_get ();
	test_limit ();
	test_nobackend ();
	test_sizes ();
	test_triesizes ();
	test_merge ();
	test_realworld ();


	printf ("\ntest_splitget RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
