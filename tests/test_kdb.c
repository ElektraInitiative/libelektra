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

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <libgen.h>

#include <ltdl.h>
#include <kdb.h>
#include "kdbLibLoader.h"

// List of backends to test
char *backend[] = { "filesys", 
			"berkeleydb", 
			"daemon",
			NULL };

const char *currentTestCase;
int nbError = 0;
int nbTest = 0;

int (*ksFromXMLfile)(KeySet *ks,const char *filename);
	

int succeed_if(int test, const char *failedMessage)
{
	extern int nbError;
	extern int nbTest;

	nbTest++;
	
	if ( !test ) {
		nbError++;
		printf("\t\t**FAILED** %s: %s\n", currentTestCase, failedMessage);
		
		return 1;
	}

	return 0;
}

void exit_if_fail(int test, const char *failedMessage)
{
	if ( !test ) {
		printf("%s\n", failedMessage);
		exit(nbError);
	}
}

void *my_malloc(size_t size)
{
	void *buf;

	if ( (buf = malloc(size)) == NULL ) {
		perror("my_malloc");
		exit(-1);
	}
	memset(buf, '@', size);	// Fill with '@' since '\0' mean end of string

	return buf;
}

int loadToolsLib(void) {
	kdbLibHandle dlhandle=0;
	
	kdbLibInit();
	
	dlhandle=kdbLibLoad("libelektratools");
	if (dlhandle == 0) {
		return 1;
	}
	
	ksFromXMLfile=kdbLibSym(dlhandle,"ksFromXMLfile");
	if ( ksFromXMLfile == NULL ) {
		return 1;
	}
	
	return 0;
}

int delete_keysRecurse(KDBHandle handle, const char *root)
{
	Key	*key, *cur;
	KeySet	*ks;
	int	ret = 0;

	/* Fetch all _not_ directory keys */
	key = keyNew(root, KEY_SWITCH_END);
	ks = ksNew();
	if ( kdbGetKeyChildKeys(handle, key, ks, KDB_O_RECURSIVE | KDB_O_STATONLY | KDB_O_INACTIVE) == -1 ) {
		ksDel(ks);
		keyDel(key);
		return 1;
	}
	
	/* Delete these ... */
	ksRewind(ks);
	while ( (cur = ksPop(ks)) != NULL ) {
		if ( kdbRemoveKey(handle, cur) )
			ret = 1;
	}
	ksClose(ks);

	/* Fetch all directory keys */
	ksInit(ks);
	if ( kdbGetKeyChildKeys(handle, key, ks, KDB_O_RECURSIVE | KDB_O_DIRONLY | KDB_O_DIR | KDB_O_SORT | KDB_O_INACTIVE) == -1 ) {
		ksDel(ks);
		keyDel(key);
		return 1;
	}

	/* Delete these ... */
	while ( (cur = ksPopLast(ks)) != NULL ) {
		if ( kdbRemoveKey(handle, cur) )
			ret = 1;
	}
	ksDel(ks);
	keyDel(key);
	
	return ret;
}

int test_keyCompare(Key *key1, Key *key2, unsigned int ignoreFlags)
{
	int	ret;
	int	err = 0;

	ret = keyCompare(key1, key2);
	if ( (ret & ignoreFlags) != ret ) {
		succeed_if(0, "Differences between keys :");
		if ( !(ignoreFlags & KEY_SWITCH_TYPE) )	err += succeed_if ( (ret & KEY_SWITCH_TYPE) == 0, "\t* Key type isn't same");
		if ( !(ignoreFlags & KEY_SWITCH_NAME) )	err += succeed_if ( (ret & KEY_SWITCH_NAME) == 0, "\t*Key name isn't same");
		if ( !(ignoreFlags & KEY_SWITCH_VALUE) )err += succeed_if ( (ret & KEY_SWITCH_VALUE) == 0, "\t*Key value isn't same");
		if ( !(ignoreFlags & KEY_SWITCH_OWNER) )err += succeed_if ( (ret & KEY_SWITCH_OWNER) == 0, "\t*Key owner isn't same");
		if ( !(ignoreFlags & KEY_SWITCH_COMMENT) ) err += succeed_if ( (ret & KEY_SWITCH_COMMENT) == 0, "\t*Key comment isn't same");
		if ( !(ignoreFlags & KEY_SWITCH_UID) ) err += succeed_if ( (ret & KEY_SWITCH_UID) == 0, "\t*Key UID isn't name");
		if ( !(ignoreFlags & KEY_SWITCH_GID) ) err += succeed_if ( (ret & KEY_SWITCH_GID) == 0, "\t*Key GID isn't name");
		if ( !(ignoreFlags & KEY_SWITCH_MODE) ) err += succeed_if ( (ret & KEY_SWITCH_MODE) == 0, "\t*Key mode isn't name");
		if ( !(ignoreFlags & KEY_SWITCH_NEEDSYNC) ) err += succeed_if ( (ret & KEY_SWITCH_NEEDSYNC) == 0, "\t*Key NEESYNC isn't name");
		if ( !(ignoreFlags & KEY_SWITCH_FLAG) ) err += succeed_if ( (ret & KEY_SWITCH_FLAG) == 0, "\t*Key FLAG isn't name");
	}

	return err;
}

/* Test kdbGetKey()/kdbSetKey()
 * ----------------------------
 *
 * kdbSetKey the supplied key. Then kdbGetKey it
 * Compare these two.
 *
 * NOTE: The tested key isn't kdbRemoved().
 *
 * return 0 if succeed, otherwise number of errors
 */
int testcase_kdbSetKeyAndkdbGetKey(KDBHandle handle, Key *key)
{
	Key	*tmp;
	int	err;
	int	ignoreFlags;
	
	currentTestCase = __FUNCTION__;

	// Ignore difference on key value if key is a directory
	ignoreFlags = (keyIsDir(key)) ? (KEY_SWITCH_VALUE) : (0);
	
	tmp = keyNew(keyStealName(key), KEY_SWITCH_END);
	err = succeed_if( kdbSetKey(handle, key) == 0, "kdbSetKey failed.");
	if ( !err ) {
		err += succeed_if( kdbGetKey(handle, tmp) == 0, "kdbGetKey failed.");
		err += test_keyCompare(tmp, key, ignoreFlags);
	}

	keyDel(tmp);

	return err;
}

/* Test kdbStatKey()
 * -----------------
 *  
 * kdbGetKey() then kdbStatKey() the supplied key. 
 * Compare the "STATed" version agaisnt the getted version.
 *
 * NOTE: The tested key must exist.
 * 
 * return 0 if succeed, otherwise number of errors
 */
int testcase_kdbStatKey(KDBHandle handle, Key *key)
{
	Key	*tmp;
	int	err;

	currentTestCase = __FUNCTION__;

	tmp = keyNew(keyStealName(key), KEY_SWITCH_END);

	err = succeed_if( kdbGetKey(handle, key) == 0, "kdbGetKey failed.");
	if ( !err ) {
		err += succeed_if( kdbStatKey(handle, tmp) == 0, "kdbStatKey failed.");
		err += succeed_if( (keyGetUID(tmp) == keyGetUID(key)), "kdbStatKey returned wrong UID.");
		err += succeed_if( (keyGetGID(tmp) == keyGetGID(key)), "kdbStatKey returned wrong GID.");
		err += succeed_if( (keyGetAccess(tmp) == keyGetAccess(key)), "kdbStatKey returned wrong Access.");
		err += succeed_if( (keyGetATime(tmp) == keyGetATime(key)), "kdbStatKey returned wrong access time.");
		err += succeed_if( (keyGetMTime(tmp) == keyGetMTime(key)), "kdbStatKey returned wrong modification time.");
		err += succeed_if( (keyGetCTime(tmp) == keyGetCTime(key)), "kdbStatKey returned wrong last change time.");
	}

	keyDel(tmp);
	
	return err;
}

/* Test kdbRename()
 * ----------------
 *
 * Rename the tested key to <keyname>-renamed.
 * Then check if <keyname> is removed & <keyname>-renamed
 * exists.
 *
 * NOTE: The tested key must exist.
 * At the end, the key is renamed to its original name
 * 
 * return 0 if suceed, otherwise number of errors
 */
int testcase_kdbRename(KDBHandle handle, Key *key)
{
	Key	*tmp;
	char	buf[1024];
	int	err;
	
	currentTestCase = __FUNCTION__;
	
	snprintf(buf, sizeof(buf), "%s-renamed", keyStealName(key));
	tmp = keyNew(buf, KEY_SWITCH_END);

	err = succeed_if( kdbRename(handle, key, buf) == 0, "kdbRename failed.");
	if ( !err ) {
		err += succeed_if( kdbGetKey(handle, tmp) == 0, "kdbGetKey on the renamed key failed.");
		err += succeed_if( kdbStatKey(handle, key) , "kdbStatKey succeed. The old renamed key is still existing.");
		err += succeed_if( kdbRename(handle, tmp, keyStealName(key)) == 0, "kdbRename failed. Can't reverse to the original name.");
	}
	
	keyDel(tmp);

	return err;
}

/* Test kdbLink()
 * --------------
 *
 * Create a link from tested key to <keyname>-linked
 */
int testcase_kdbLink(KDBHandle handle, Key *key)
{
	Key	*tmp;
	char	buf[1024];
	int	ret, err;
	int	ignoreFlags;
	
	currentTestCase = __FUNCTION__;

	/* Ignore comparison of name and type since
	 * the link source could have different name and type. */
	ignoreFlags = KEY_SWITCH_NAME;
	ignoreFlags |= KEY_SWITCH_TYPE;
	
	/* Ignore comparison of value for directory key */
	ignoreFlags |= (keyIsDir(key) ? (KEY_SWITCH_VALUE) : (0));
	
		
	snprintf(buf, sizeof(buf), "%s-linked", keyStealName(key));
	tmp = keyNew(buf, KEY_SWITCH_END);
	
	err = succeed_if( kdbLink(handle, keyStealName(key), buf) == 0, "kdbLink failed.");
	if ( !err ) {
		err += succeed_if( kdbStatKey(handle, tmp) == 0, "kdbStatKey failed on link key");
		err += succeed_if( strcmp(keyStealValue(tmp), keyStealName(key)) == 0, "kdbLink link target isn't set correctly.");
		err += succeed_if( kdbGetKey(handle, key) == 0, "kdbGetKey failed on source key");
		err += succeed_if( kdbGetKey(handle, tmp) == 0, "kdbGetKey failed on link key");
		err += test_keyCompare(key, tmp, ignoreFlags);
		err += succeed_if( kdbRemoveKey(handle, tmp) == 0, "kdbRemoveKey failed on link key");
	}

	keyDel(tmp);
	
	return err;
}


void test_backend(char *backendName)
{
	KDBHandle	handle;
	KeySet		*ks, *ks2, *removed;
	Key     	*cur, *key, *key2;
	int		ret, exist;
	char		buf[1024], root[1024];
	char		tmpname[L_tmpnam];
	int		counter;
	
	printf("Testing elektra-%s backend.\n", backendName);
	printf("----------------------------------------\n\n");

	exit_if_fail( kdbOpenBackend(&handle, backendName) == 0, "kdbOpen() failed.");
			
	/* Create a root key name to prepend to all key
	 * this permis previous test to not interfere
	 * with this one.
	 */
	exist = 1;
        while ( exist ) {
		tmpnam(tmpname);
		snprintf(root, sizeof(root), "user/elektra-tests/%s", basename(tmpname));
		key = keyNew(root, KEY_SWITCH_END);
		exist = (kdbStatKey(handle, key) == 0);
		keyDel(key);
	}
	printf("Making tests into = %s\n\n", root);
	
	
	/* ============================================= 
	 * Here we're testing functions which act on Key
	 * ============================================= */

	ks = ksNew();
	exit_if_fail( ksFromXMLfile(ks, "key.xml") == 0, "ksFromXMLfile(key.xml) failed.");
	counter = 0;
	ksRewind(ks);
	while ( (cur = ksNext(ks)) ) {
		/* Prepend key root */	
		snprintf(buf, sizeof(buf), "%s/%s", root, keyStealName(cur));
		keySetName(cur, buf);

		/* Make tests ... */	
		printf("\tTesting %s\n", keyStealComment(cur));
		
		testcase_kdbSetKeyAndkdbGetKey(handle, cur);
		testcase_kdbStatKey(handle, cur);
		testcase_kdbRename(handle, cur);
		testcase_kdbLink(handle, cur);

		printf("\n");
	}
	
	succeed_if( delete_keysRecurse(handle, root) == 0, "delete_keysRecurse failed.");
	ksDel(ks);

	
	
#if 0
	
	/*
	 * Here we'll test functions which act on KeySet
	 */
	ks2 = ksNew();
	ksRewind(ks);
	key = keyNew(root, KEY_SWITCH_END);
	
	succeed_if( kdbSetKeys(handle, ks) == 0, "kdbSetKeys failed.");
	succeed_if( kdbGetKeyChildKeys(handle, key, ks2, KDB_O_RECURSIVE | KDB_O_DIR | KDB_O_INACTIVE ) == 0, "kdbGetKeyChildKeys failed.");

	/* I would have a _true_ ksCompare() ... */
	keyDel(key);
	ksSort(ks); ksRewind(ks);
	ksSort(ks2); ksRewind(ks2);
	if ( ksGetSize(ks) == ksGetSize(ks2) ) {
		while ( ((key = ksNext(ks)) != NULL) && ((key2 = ksNext(ks2)) != NULL) ) {
			ret = keyCompare(key, key2);
			if ( ret )
				break;
		}
		succeed_if( ret == 0, "kdbGetKeyChildKeys: Keys fetched aren't equat to the submited ones.");
	} else {
		succeed_if( 0, "kdbGetKeyChildKeys: There are less keys fetched than keys which have been submited.");
	}
	ksDel(ks);
	ksDel(ks2);
	
	succeed_if( delete_keysRecurse(handle, root) == 0, "delete_keysRecurse failed.");
#endif

	kdbClose(&handle);
	
}

int main()
{
	int	i;

	printf("\n");
	printf("========================================\n");	
	printf("ELEKTRA BACKENDS TEST SUITE\n");
	printf("========================================\n\n");

	if ( loadToolsLib() ) {
		printf("Unable to load elektratools\n");
		return 1;
	}
	
	for(i = 0 ; backend[i] != NULL ; i++)
		test_backend(backend[i]);

	printf("\ntest_kdb RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
	
	return nbError;
}
