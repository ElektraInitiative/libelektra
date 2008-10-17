/***************************************************************************
 *          test_kdb.c  -  Backend test suite
 *                -------------------
 *  begin                : Thu Aug 03 2006
 *  copyright            : (C) 2006 by Yannick Lecaillez
 *  email                : sizon5@gmail.com
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


#include <tests.h>

/* Remove all keys below and with root.
 *
 * Will remove but not delete the root element!
 *
 * */
void remove_keys (KDB *handle, Key *root)
{
	KeySet *ks;

	ks = ksNew(0);
	succeed_if (kdbGet (handle, ks, root, KDB_O_INACTIVE) > 0, "could not get keys below root key");
	ksRewind (ks);
	succeed_if (kdbSet (handle, ks, root,KDB_O_REMOVEONLY) != -1, "could not remove root");
	ksDel (ks);
}

int test_keyCompare(Key *key1, Key *key2, unsigned int ignoreFlags)
{
	int	ret;
	int	err = nbError;

	ret = keyCompare(key1, key2);
	if ( (ret & ignoreFlags) != ret ) {
		if ( !(ignoreFlags & KEY_TYPE) )	succeed_if ( (ret & KEY_TYPE) == 0,	"Key type isn't same");
		if ( !(ignoreFlags & KEY_NAME) )	succeed_if ( (ret & KEY_NAME) == 0,	"Key name isn't same");
		if ( !(ignoreFlags & KEY_VALUE))	succeed_if ( (ret & KEY_VALUE) == 0,	"Key value isn't same");
		if ( !(ignoreFlags & KEY_OWNER))	succeed_if ( (ret & KEY_OWNER) == 0,	"Key owner isn't same");
		if ( !(ignoreFlags & KEY_COMMENT))	succeed_if ( (ret & KEY_COMMENT) == 0,	"Key comment isn't same");
		if ( !(ignoreFlags & KEY_UID) ) 	succeed_if ( (ret & KEY_UID) == 0,	"Key UID isn't name");
		if ( !(ignoreFlags & KEY_GID) ) 	succeed_if ( (ret & KEY_GID) == 0,	"Key GID isn't name");
		if ( !(ignoreFlags & KEY_MODE))	succeed_if ( (ret & KEY_MODE) == 0,	"Key mode isn't name");
	}

	return nbError-err;
}

/* Test kdbGet()/kdbSet()
 * ----------------------------
 *
 * kdbSetKey the supplied key. Then kdbGetKey it
 * Compare these two.
 *
 * NOTE: The tested key isn't kdbRemoved().
 *
 * return 0 if succeed, otherwise number of errors
 */
int testcase_kdbSetKeyAndkdbGetKey(KDB *handle, Key *key)
{
	Key	*check;
	int	err = nbError;
	int	ignoreFlags = 0;

	// printf("\t* testing kdbSetKey()/kdbGetKey()\n");

	check = keyDup(key);
	succeed_if( kdbSetKey(handle, key) == 0, "kdbSetKey failed.");
	if ( !err ) {
		succeed_if( kdbGetKey(handle, check) == 0, "kdbGetKey failed.");
		test_keyCompare(check, key, ignoreFlags);
	}

	keyDel(check);

	return nbError-err;
}

/* Test kdbStatKey()
 * -----------------
 *
 * kdbGetKey() then kdbStatKey() the supplied key. 
 * Compare the "STATed" version against the getted version.
 *
 * NOTE: The tested key must exist.
 */
int testcase_kdbStatKey(KDB *handle, Key *k)
{
	Key	*tmp;
	Key	*key = keyDup (k);

	int err = nbError;


	/* printf("\t* testing kdbStatKey()\n"); */
	
	tmp = keyNew(keyName(key), KEY_END);

	succeed_if( kdbGetKey(handle, key) == 0, "kdbGetKey failed.");

	keyStat (key);
	succeed_if( kdbGetKey(handle, tmp) == 0, "kdbStatKey failed.");
	succeed_if( (keyGetUID(tmp) == keyGetUID(key)), "kdbStatKey returned wrong UID.");
	succeed_if( (keyGetGID(tmp) == keyGetGID(key)), "kdbStatKey returned wrong GID.");

	succeed_if( (keyGetMode(tmp) == keyGetMode(key)), "kdbStatKey returned wrong Mode.");

	succeed_if( (keyGetATime(tmp) >= keyGetATime(key)), "kdbStatKey returned wrong mode time.");
	succeed_if( (keyGetMTime(tmp) == keyGetMTime(key)), "kdbStatKey returned wrong modification time.");
	succeed_if( (keyGetCTime(tmp) == keyGetCTime(key)), "kdbStatKey returned wrong last change time.");

	keyDel(tmp);
	
	keyDel (key);

	return nbError-err;
}

/*
 * Renames the key to a given newname in backend in an atomic way.
 *
 * @note the key will not exist afterwards in database
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param key an initialized Key retrieved from backend
 * @param newname the new name which the name should have in backend
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @see kdbSet()
 * @ingroup kdb
 */
int kdbRename(KDB *handle, const Key *key, const char *newname) {
	KeySet * ks = ksNew(0);
	Key * toRename = keyDup(key);
	Key * toRemove = keyDup(key);
	keyRemove (toRemove);
	ksAppendKey(ks, toRemove);

	if (keySetName (toRename, newname) == -1)
	{
		ksDel (ks);
		return -1;
	}
	ksAppendKey(ks, toRename);

	if (kdbSet (handle, ks,0,0) == -1)
	{
#if DEBUG
		printf ("kdbRename: kdbSet failed\n");
#endif
		ksDel (ks);
		return -1;
	}

	ksDel (ks);
	return 0;
}

/* Test kdbRename()
 * ----------------
 *
 * Rename the tested key to <keyname>-renamed.
 * Then check if <keyname> is removed & <keyname>-renamed
 * exists.
 *
 * NOTE: The tested key must be set.
 * At the end, the key is renamed to its original name,
 * thus only the original will be there after test.
 */
int testcase_kdbRename(KDB *handle, Key *k)
{
	Key	*tmp;
	char	buf[KEY_LENGTH];

	Key	*key=keyDup(k);
	
	int err = nbError;

	snprintf(buf, sizeof(buf), "%s-renamed", keyName(key));
	tmp = keyNew(buf, KEY_END);

	succeed_if( kdbRename(handle, key, buf) == 0, "kdbRename failed.");
	succeed_if( kdbGetKey(handle, tmp) == 0, "kdbGetKey on the renamed key failed.");
	
	succeed_if( kdbGetKey(handle, key) , "kdbGetKey succeed. The old renamed key is still existing.");
	succeed_if( kdbRename(handle, tmp, keyName(key)) == 0, "kdbRename failed. Can't reverse to the original name.");

	// already renamed back
	// keyRemove (tmp);
	// succeed_if (kdbSetKey(handle, tmp) == -1, "the renamed back key still exists");
	
	keyDel(tmp);
	keyDel(key);
	
	return nbError-err;
}


/* Make sure that it is possible to create, set and get keys
 * in the default backend before trying to mount others*/
void test_default(KDB * handle, Key * root)
{
	Key     	*key, *key2;

	// test if current directory works
	key = keyDup(root);
	succeed_if (key, "Could not create new key");
	keyAddBaseName(key, "test_delete");
	succeed_if (testcase_kdbSetKeyAndkdbGetKey (handle, key) == 0, "Set and Get key failed");
	succeed_if (keyRemove (key) != -1, "Could not mark key to remove");
	succeed_if (kdbSetKey (handle, key) != -1, "Could not remove key");
	succeed_if (kdbGetKey (handle, key) == -1, "Key seems to exists");
	keyDel (key);
	
	// test if current directory works - with value, comment
	key = keyDup(root);
	keySetString(key, "myvalue");
	keySetComment (key, "mycomment");
	keyAddBaseName(key, "test_key");
	succeed_if (key, "Could not create new key");
	//succeed_if (strcmp (keyName (key), root) == 0, "Name for key is not correct");
	succeed_if (testcase_kdbSetKeyAndkdbGetKey (handle, key) == 0, "Set and Get key failed");
	succeed_if (keyRemove (key) != -1, "Could not mark key to remove");
	succeed_if (kdbSetKey (handle, key) != -1, "Could not remove key");
	succeed_if (kdbGetKey (handle, key) == -1, "Key seems to exists");
	keyDel(key);

	// remove it again
	key = keyDup (root);
	succeed_if (key, "Could not create new key");
	keyAddBaseName(key, "test_delete");
	succeed_if (testcase_kdbSetKeyAndkdbGetKey (handle, key) == 0, "Set and Get key failed");
	keyRemove (key);
	succeed_if (kdbSetKey (handle, key) != -1, "Could not remove key");
	succeed_if (kdbGetKey (handle, key) == -1, "Key seems to exists");
	keyDel (key);

	// clone key with keyDup
	key = keyDup (root);
	keyAddBaseName (key, "dup_test");
	succeed_if (testcase_kdbSetKeyAndkdbGetKey (handle, key) == 0, "Set and Get key failed");
	keyRemove (key);
	succeed_if (kdbSetKey (handle, key) != -1, "Could not remove key");
	succeed_if (kdbGetKey (handle, key) == -1, "Key seems to exists");
	keyDel (key);

	// last test with a subdirectory
	key = keyDup (root);
	keyAddBaseName (key, "default");
		key2 = keyDup (key);
		keyAddBaseName (key2, "key");
		succeed_if (testcase_kdbSetKeyAndkdbGetKey (handle, key2) == 0, "Set and Get key failed");
		keyRemove (key2);
		succeed_if (kdbSetKey (handle, key2) != -1, "Could not remove key");
		succeed_if (kdbGetKey (handle, key2) == -1, "Key seems to exists");
		keyDel (key2);
	succeed_if (testcase_kdbSetKeyAndkdbGetKey (handle, key) == 0, "Set and Get key failed");
	keyRemove (key);
	succeed_if (kdbSetKey (handle, key) != -1, "Could not remove directory key");
	succeed_if (kdbGetKey (handle, key) == -1, "Key seems to exists");
	keyDel (key);
	
	// now lets see if root key survived the procedure
	key = keyDup (root);
	succeed_if (key, "Could not create new key");
	exit_if_fail (kdbGetKey (handle, key) == 0, "Root key suddenly missing?");
	keyDel (key);

	keyDel (root);
	// default backends seems to work
}


void test_backend(KDB * handle, Key * root, char *fileName, KeySet *conf)
{
	KeySet		*ks, *ks2, *ksd;
	Key     	*cur;
	unsigned long options = KDB_O_POP;
	KDBCap	*cap;

	succeed_if (testcase_kdbSetKeyAndkdbGetKey (handle, root) == 0, "Set and Get backend root key failed");

	printf("Testing elektra-%s backend with file %s, Root: %s.\n", (char*)keyValue(root), fileName,  keyName (root));
	printf("==========================\n");

	if (kdbMount (handle, root, conf) == -1)
	{
		printf ("Could not mount backend %s\n", (char*)keyValue(root));
		printf ("Will not continue with tests on that backend\n");

		remove_keys(handle, root);

		keyDel (root);
		ksDel (conf);

		return;
	}

	cap = kdbGetCapability (handle, root);
	exit_if_fail (cap != NULL, "can't test backend not declaring its capabilities");

	ks = ksNew(0);
	printf("Getting file from %s.\n", fileName);
	exit_if_fail( ksFromXMLfile(ks, fileName) == 0, "ksFromXMLfile failed.");
	/* ksOutput (ks, stdout, 0); */
	/* ksGenerate (ks, stdout,  0); */

	if (kdbcGetonlyFullSet(cap))
	{
		/* Add a duplication of root key so that kdbSet does not fail */
		ksAppendKey(ks, keyDup(root));
		options = 0;

		/* ksOutput (ks, stdout, 0); */
		succeed_if( kdbSet(handle, ks, root, KDB_O_SYNC) > 0, "kdbSet with FullSet failed.");
	} else {
		exit_if_fail (kdbGetKey (handle, root) == 0, "No root key?");
		ksRewind(ks);
		ksSort(ks);
		while ( (cur = ksNext(ks)) ) {
			/* keyGenerate (cur); */

			keySetUID (cur, nbUid);
			keySetGID (cur, nbGid);

			succeed_if (testcase_kdbSetKeyAndkdbGetKey (handle, cur) == 0, "Set and Get key failed");
			succeed_if (testcase_kdbRename(handle, cur) == 0, "Could not rename key");
			succeed_if (testcase_kdbStatKey(handle, cur) == 0, "Could not stat key");

			// to get sync flag away, to allow next testcase
			succeed_if (testcase_kdbSetKeyAndkdbGetKey (handle, cur) == 0, "Set and Get key failed");
		}
	}

	printf ("Testing kdbGet with 2 inactive keys\n");
	ks2 = ksNew(0);
	succeed_if( kdbGet(handle, ks2, root, KDB_O_INACTIVE | options) >= 0, "kdbGet failed.");
	succeed_if( compare_keyset(ks, ks2, 0, cap) == 0, "compare keyset with all keys failed");
	/*
	printf ("We got :\n"); ksGenerate (ks2, stdout, KDB_O_HEADER);
	printf ("We want:\n"); ksGenerate (ks, stdout, KDB_O_HEADER);
	*/
	ksDel(ks2);

	printf ("Testing kdbGet with 2 inactive keys not yet updating (fails)\n");
	ks2 = ksNew(0);
	succeed_if( kdbGet(handle, ks2, root, KDB_O_INACTIVE | options) >= 0, "kdbGet failed.");
	succeed_if( compare_keyset(ks, ks2, 0, cap) == 0, "compare keyset with all keys failed");
	/*
	printf ("We got :\n"); ksGenerate (ks2, stdout, KDB_O_HEADER);
	printf ("We want:\n"); ksGenerate (ks, stdout, KDB_O_HEADER);
	*/
	ksDel(ks2);

	printf ("Testing kdbGet with no option\n");
	ks2 = ksNew(0);
	succeed_if( kdbGet(handle, ks2, root, options) >= 0, "kdbGet failed.");
	succeed_if( compare_keyset(ks, ks2, KDB_O_INACTIVE, cap) == 0, "compare keyset INACTIVE failed");
	/*
	ksOutput (ks, stdout, KDB_O_HEADER);
	ksOutput (ks2, stdout, KDB_O_HEADER);
	*/
	ksDel(ks2);

	printf ("Testing kdbGet with 3 directories\n");
	ks2 = ksNew(0);
	succeed_if( kdbGet(handle, ks2, root, KDB_O_NODIR  | options) >= 0, "kdbGet failed.");
	succeed_if( compare_keyset(ks, ks2, KDB_O_NODIR | KDB_O_INACTIVE, cap) == 0, "compare keyset NODIR failed");
	ksDel(ks2);

	printf ("Testing kdbGet only with directories\n");
	ks2 = ksNew(0);
	succeed_if( kdbGet(handle, ks2, root, KDB_O_INACTIVE | KDB_O_DIRONLY  | options) >= 0, "kdbGet failed.");
	succeed_if( compare_keyset(ks, ks2, KDB_O_DIRONLY, cap) == 0, "compare keyset DIRONLY failed");
	ksDel(ks2);

	remove_keys(handle, root);
	succeed_if( kdbSet(handle, ks, root, KDB_O_SYNC) != -1, "kdbSet failed.");

	printf ("Testing kdbGet with inactive keys\n");
	ks2 = ksNew(0);
	succeed_if( kdbGet(handle, ks2, root, KDB_O_INACTIVE | options) >= 0, "kdbGet failed.");
	succeed_if( compare_keyset(ks, ks2, 0, cap) == 0, "compare keyset with all keys failed");
	/*
	ksGenerate (ks, stdout, 0);
	printf ("but is "); ksGenerate (ks2, stdout, 0);
	*/
	ksDel(ks2);

	printf ("Testing kdbGet with no option\n");
	ks2 = ksNew(0);
	succeed_if( kdbGet(handle, ks2, root, options) >= 0, "kdbGet failed.");
	succeed_if( compare_keyset(ks, ks2, KDB_O_INACTIVE, cap) == 0, "compare keyset INACTIVE failed");
	ksDel(ks2);

	printf ("Testing kdbGet without directories\n");
	ks2 = ksNew(0);
	succeed_if( kdbGet(handle, ks2, root, KDB_O_NODIR  | options) >= 0, "kdbGet failed.");
	succeed_if( compare_keyset(ks, ks2, KDB_O_NODIR | KDB_O_INACTIVE, cap) == 0, "compare keyset NODIR failed");
	ksDel(ks2);

	printf ("Testing kdbGet only with directories\n");
	ks2 = ksNew(0);
	succeed_if( kdbGet(handle, ks2, root, KDB_O_DIRONLY  | options) >= 0, "kdbGet failed.");
	succeed_if( compare_keyset(ks, ks2, KDB_O_DIRONLY | KDB_O_INACTIVE, cap) == 0, "compare keyset DIRONLY failed");
	ksDel(ks2);

	printf ("Testing kdbGet with only stat\n");
	ks2 = ksNew(0);
	keyStat(root);
	succeed_if( kdbGet(handle, ks2, root, KDB_O_STATONLY | options) >= 0, "kdbGet failed.");
	succeed_if (keyNeedStat(root) == 1, "root should need stat");
	ksRewind (ks2); while (ksNext(ks2) != 0) succeed_if (keyNeedStat(ksCurrent(ks2)) == 1, "all keys should need stat");
	succeed_if( compare_keyset(ks, ks2, KDB_O_INACTIVE | KDB_O_STATONLY, cap) == 0, "compare keyset STATONLY failed");
	ksDel(ks2);

	printf ("Testing kdbGet no stat\n");
	ks2 = ksNew(0);
	keyStat(root);
	succeed_if( kdbGet(handle, ks2, root, KDB_O_NOSTAT | options) >= 0, "kdbGet failed.");
	succeed_if (keyNeedStat(root) == 0, "root should not need stat");
	ksRewind (ks2); while (ksNext(ks2) != 0) succeed_if (keyNeedStat(ksCurrent(ks2)) == 0, "all keys should not need stat");
	succeed_if( compare_keyset(ks, ks2, KDB_O_INACTIVE, cap) == 0, "compare keyset NOSTAT failed");
	ksDel(ks2);

	printf ("Testing kdbGet with inactive keys but no recursive\n");
	ks2 = ksNew(0);
	succeed_if( kdbGet(handle, ks2, root, KDB_O_INACTIVE | KDB_O_NORECURSIVE | options) >= 0, "kdbGet failed.");
	ksd = ksNew (0);
	ksRewind (ks);
	while ((cur = ksNext(ks)) != 0)
	{
		if (!strcmp(keyName(root), keyName(cur)) || keyIsDirectBelow(root, cur))
		{
			ksAppendKey(ksd, cur);
		}
	}
	succeed_if( compare_keyset(ksd, ks2, 0, cap) == 0, "compare keyset with all keys no recursive failed");
	/*
	printf ("ks was "); ksGenerate (ks, stdout, 0);
	printf ("ksd is "); ksGenerate (ksd, stdout, 0);
	printf ("but is "); ksGenerate (ks2, stdout, 0);
	*/
	ksDel(ks2);
	ksDel(ksd);

	ksDel(ks);
	remove_keys(handle, root);

	kdbUnmount (handle, root);
	keyDel (root);
	ksDel (conf);
}

int main()
{
	printf("ELEKTRA BACKENDS TEST SUITE\n");
	printf("========================================\n\n");

	init ();

	KDB	*handle = kdbOpen();
	exit_if_fail(handle, "kdbOpen() failed.");

	test_default(handle, create_root_key(""));

	test_backend(handle, create_root_key("filesys"),srcdir_file("filesys.xml"),create_conf (".kdb"));
	test_backend(handle, create_root_key("filesys"),srcdir_file("keyset.xml"), create_conf (".kdb"));
	test_backend(handle, create_root_key("fstab"),  srcdir_file("fstab.xml"),  create_conf (".kdb/fstab_kdb"));
	test_backend(handle, create_root_key("hosts"),  srcdir_file("hosts.xml"),  create_conf (".kdb/hosts_kdb"));

	printf("\ntest_kdb RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	exit_if_fail (kdbClose(handle) == 0, "Could not close libelektra");

	return nbError;
}

