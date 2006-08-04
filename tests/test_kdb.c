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

int nbError = 0;
int nbTest = 0;

int (*ksFromXMLfile)(KeySet *ks,const char *filename);
	

void succeed_if(int test, const char *failedMessage)
{
	extern int nbError;
	extern int nbTest;

	nbTest++;
	
	if ( !test ) {
		nbError++;
		printf("Test failed in %s: %s\n", __FILE__, failedMessage);
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
	if ( kdbGetKeyChildKeys(handle, key, ks, KDB_O_DIRONLY | KDB_O_DIR | KDB_O_SORT | KDB_O_INACTIVE) == -1 ) {
		ksDel(ks);
		keyDel(key);
		return 1;
	}

	/* Delete these ... */
	ksRewind(ks);
	while ( (cur = ksPopLast(ks)) != NULL ) {
		if ( kdbRemoveKey(handle, cur) )
			ret = 1;
	}
	ksDel(ks);
	keyDel(key);
	
	return ret;
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

	ks = ksNew();
	if ( ksFromXMLfile(ks, "keyset.xml") ) {
		perror("ksFromXMLfile");
		return;
	}
	
	succeed_if( (ret = kdbOpenBackend(&handle, backendName)) == 0, "kdbOpen() failed.");
	if ( ret ) {
		/* Can't continue test if backend can't be opened ... */
		return;
	}
			
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
	printf("\tkey root = %s\n", root);
	
	
	/* 
	 * Here we'll test functions which act on Key 
	 */
	counter = 0;
	ksRewind(ks);
	while ( (cur = ksNext(ks)) ) {
		key = keyNew(KEY_SWITCH_END);
		key2 = keyNew(KEY_SWITCH_END);
		
		/* Prepend key root */	
		snprintf(buf, sizeof(buf), "%s/%s", root, keyStealName(cur));
		keySetName(cur, buf);
		
		/* Test kdbGetKey()/kdbSetKey() 
		 * ----------------------------
		 *  
		 * Import a key to the backend. Fetch the same key from the backend.
		 * Compare key stored/key fetched.
		 * The fetched key will be used as basis for other tests.
		 */
		keyInit(key);
		keySetName(key, keyStealName(cur));
		succeed_if( kdbSetKey(handle, cur) == 0, "kdbSetKey failed.");
		succeed_if( kdbGetKey(handle, key) == 0, "kdbGetKey failed.");
		succeed_if( keyCompare( cur, key) == 0, "keyCompare failed : Differences between key stored/key readed");

		/* Test kdbStatKey() 
		 * -----------------
		 *  
		 * Stat the key previously stored. Compare stat infotmation
		 * from the "stated" key against the previously fetched one.
		 * */
		keyInit(key2);
		keySetName(key2, keyStealName(cur));
		
		succeed_if( kdbStatKey(handle, key2) == 0, "kdbStatKey failed. Key probably not found.");
		succeed_if( (keyGetUID(key2) == keyGetUID(key)), "kdbStatKey failed : different UID.");
		succeed_if( (keyGetGID(key2) == keyGetGID(key)), "kdbStatKey failed : different GID.");
		succeed_if( (keyGetAccess(key2) == keyGetAccess(key)), "kdbStatKey failed : different Access.");
		succeed_if( (keyGetATime(key2) == keyGetATime(key)), "kdbStatKey failed : different access time.");
		succeed_if( (keyGetMTime(key2) == keyGetMTime(key)), "kdbStatKey failed : different modification time.");
		succeed_if( (keyGetCTime(key2) == keyGetCTime(key)), "kdbStatKey failed : different last change time.");
		keyClose(key2);

		/* Test kdbRename()
		 * ----------------
		 *  
		 * Rename the prefiously stored key ro <keyname>-renamed.
		 * Stat the renamed key for check if existing
		 */
		snprintf(buf, sizeof(buf), "%s-renamed", keyStealName(key));
		keyInit(key2);
		keySetName(key2, buf);
		
		succeed_if( kdbRename(handle, key, buf) == 0, "kdbRename failed.");
		succeed_if( kdbStatKey(handle, key2) == 0, "kdbRename failed : kdbStatKey on the renamed key failed.");
		keySetName(cur, buf);
		keySetName(key, buf);
		keyClose(key2);

		/* Test kdbLink()
		 * --------------
		 *  
		 * Link the previously stored key to root/link-<counter>
		 */
		snprintf(buf, sizeof(buf), "%s/link-%d", root, counter++);
		keyInit(key2);
		keySetName(key2, buf);
	
		succeed_if( kdbLink(handle, keyStealName(key), buf) == 0, "kdbLink failed.");
		succeed_if( kdbStatKey(handle, key2) == 0, "kdbStatKey failed on link key");
		succeed_if( strcmp(keyStealValue(key2), keyStealName(key)) == 0, "kdbLink link target isn't set correctly.");
		succeed_if( kdbGetKey(handle, key2) == 0, "kdbGetKey failed on link key");
		succeed_if( keyCompare(key, key2) == 0, "kdbLink kdbGetKey(link)/kdbGetKey(link-target) not equal.");
		succeed_if( kdbRemoveKey(handle, key2) == 0, "kdbRemoveKey failed on link key");
		keyClose(key2);
				
		keyDel(key);
		keyDel(key2);
	}
	
	succeed_if( delete_keysRecurse(handle, root) == 0, "delete_keysRecurse failed.");
	
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
	
	printf("ELEKTRA BACKENDS TESTS\n");
	printf("----------------------\n\n");

	if ( loadToolsLib() ) {
		printf("Unable to load elektratools\n");
		return 1;
	}
	
	for(i = 0 ; backend[i] != NULL ; i++)
		test_backend(backend[i]);

	printf("\ntest_kdb RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
	
	return nbError;
}
