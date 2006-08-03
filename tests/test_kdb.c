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

#include <ltdl.h>
#include <kdb.h>
#include "kdbLibLoader.h"

// List of backends to test
char *backend[] = { "filesys", "berkeleydb", NULL };

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
	
	return 0;
}


void test_backend(char *backendName)
{
	KDBHandle	handle;
	KeySet		*ks;
	Key     	*cur, *key, *key2;
	char		buf[1024];
	int		ret;
	
	printf("Testing elektra-%s backend.\n", backendName);

	ks = ksNew();
	if ( ksFromXMLfile(ks, "keyset.xml") ) {
		perror("ksFromXMLfile");
		return;
	}
	
	succeed_if( kdbOpenBackend(&handle, backendName) == 0, "kdbOpen() failed.");

	/* Testing key import 
	 *   - Import key one by one to 
	 *   - Fetch the same key from backend
	 *   - Compare these two keys using keyCompare()
	 */
	ksRewind(ks);
	while ( (cur = ksNext(ks)) ) {
		key = keyNew(KEY_SWITCH_END);
		key2 = keyNew(KEY_SWITCH_END);
		
		/* Import a key to the backend. Then fetch the same 
		 * key from the backend. Compare these two */
		keyInit(key);
		keySetName(key, keyStealName(cur));
		
		succeed_if( kdbSetKey(handle, cur) == 0, "kdbSetKey failed.");
		succeed_if( kdbGetKey(handle, key) == 0, "kdbGetKey failed.");
		succeed_if( keyCompare( cur, key) == 0, "keyCompare failed : Differences between key stored/key readed");

		/* Test kdbStatKey() 
		 * Compare stat info from the previously loaded key
		 * against a new one loaded by kdbStatKey()
		 * */
		keyInit(key2);
		keySetName(key2, keyStealName(cur));
		
		succeed_if( kdbStatKey(handle, key2) == 0, "kdbStatKey failed.");
		succeed_if( (keyGetUID(key2) == keyGetUID(key)), "kdbStatKey failed : different UID.");
		succeed_if( (keyGetGID(key2) == keyGetGID(key)), "kdbStatKey failed : different GID.");
		succeed_if( (keyGetAccess(key2) == keyGetAccess(key)), "kdbStatKey failed : different Access.");
		succeed_if( (keyGetATime(key2) == keyGetATime(key)), "kdbStatKey failed : different access time.");
		succeed_if( (keyGetMTime(key2) == keyGetMTime(key)), "kdbStatKey failed : different modification time.");
		succeed_if( (keyGetCTime(key2) == keyGetCTime(key)), "kdbStatKey failed : different last change time.");
		keyClose(key2);

		/* Test kdbRename()
		 * Rename the current key to <keyname>-renamed
		 * stat the renamed key
		 */
		snprintf(buf, sizeof(buf), "%s-renamed", keyStealName(key));
		keyInit(key2);
		keySetName(key2, buf);
		
		succeed_if( kdbRename(handle, key, buf) == 0, "kdbRename failed.");
		succeed_if( kdbStatKey(handle, key2) == 0, "kdbRename failed : kdbStatKey on the renamed key failed.");
		keyClose(key2);

		/* Test kdbRemove()
		 * Try to remove DIR key. Key type DIR shouldn't be removed
		 * since kdbRemove() isn't recursive */
		if ( keyIsDir(key) ) {
			succeed_if( kdbRemove(handle, key), "kdbRemove failed : Allowed deletion of a non-empty directory.");
		}
				
		keyDel(key);
	}

	kdbClose(&handle);
	
}

int main()
{
	KDBHandle	handle;
	char	buf[1024];
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
