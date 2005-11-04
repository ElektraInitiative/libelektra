#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <kdb.h>

/**example.c
 * This is comprehensive Test for the
 * whole Elektra Api and the Backends
 * in special.
 * main() can be used to start the
 * wished test-functions.
 */

void printKey (Key * k);
void printKeySet (KeySet * set);
void useKeySet();
void getChildKeys ();
void getKey();
void setNullKey();
void setAnotherKey();

#define CHILD_KEY_ROOT "user/"
#define KEY_ROOT "user/test/another/key"

void printKey (Key * k)
{
	size_t s;
	char * str;

	size_t c;
	char * com;

	size_t n;
	char * nam;
	
	n = keyGetNameSize (k);
	nam = (char*) malloc (n);
	if (nam == NULL) {fprintf (stderr, "malloc error\n");}
	keyGetName (k, nam, n);
	
	s = keyGetDataSize (k);
	str = (char*) malloc (s+100);
	if (str == NULL) {fprintf (stderr, "malloc error\n");}
	keyGetString (k, str, s);

	c = keyGetCommentSize (k);
	com = (char*) malloc (c+100);
	if (com == NULL) {fprintf (stderr, "malloc error\n");}
	keyGetComment (k, com, c);
	
	printf ("Name[%d,%d]: %s\t", n, strlen(nam)+1, nam);
	printf ("String[%d,%d]: %s\t", s, strlen(str)+1, str);
	printf ("Kommentar[%d,%d]: %s\n", c, strlen(com)+1, com);

	free (nam);
	free (str);
	free (com);
}

void printKeySet (KeySet * set)
{
	Key * k;
	fprintf (stderr, "Will print keys\n");

	ksRewind (set);

	fprintf (stderr, "Get next key\n");
	k = ksNext (set);
	while (k) {
		printKey (k);
		k = ksNext (set);
	}
}

void useKeySet()
{
	KeySet *ks=ksNew();

	ksAppend(ks,keyNew(KEY_SWITCH_END));       // an empty key
	    
	ksAppend(ks,keyNew("user/sw",              // a simple key
	    KEY_SWITCH_END));                      // no more args
	    
	ksAppend(ks,keyNew("system/sw",
	    KEY_SWITCH_NEEDSYNC,                   // a key retrieved from storage
	    KEY_SWITCH_END));                      // end of args               
	    
	ksAppend(ks,keyNew("user/tmp/ex1",
	    KEY_SWITCH_VALUE,"some data",          // with a simple value
	    KEY_SWITCH_END));                      // end of args
	    
	ksAppend(ks,keyNew("user/tmp/ex2",
	    KEY_SWITCH_VALUE,"some data",          // with a simple value
	    KEY_SWITCH_MODE,0777,                  // permissions
	    KEY_SWITCH_END));                      // end of args
	    
	ksAppend(ks,keyNew("user/tmp/ex3",
	    KEY_SWITCH_TYPE,KEY_TYPE_LINK,         // only type
	    KEY_SWITCH_VALUE,"system/mtp/x",       // link destination
	    KEY_SWITCH_MODE,0654,                  // weird permissions
	    KEY_SWITCH_END));                      // end of args
	    
	/*ksAppend(ks,keyNew("user/tmp/ex4",
	    KEY_SWITCH_TYPE,KEY_TYPE_BINARY,       // key type and value size (because it is binary)
	    KEY_SWITCH_DOMAIN,"root",              // owner (not uid) is root
	    KEY_SWITCH_VALUE,"some data",          // value that will be truncated
	    KEY_SWITCH_COMMENT,"value is truncated",
	    KEY_SWITCH_UID,0,                      // root uid
	    KEY_SWITCH_END));                      // end of args*/
	    
	ksAppend(ks,keyNew("user/env/alias/ls",    // a key we know we have
	    KEY_SWITCH_NEEDSYNC,                   // retrieve from storage
	    KEY_SWITCH_END));                      // do nothing more
	    
	ksAppend(ks,keyNew("user/env/alias/ls",    // same key
	    KEY_SWITCH_NEEDSYNC,                   // retrieve from storage
	    KEY_SWITCH_DOMAIN,"root",              // set new owner (not uid) as root
	    KEY_SWITCH_COMMENT,"new comment",      // set new comment
	    KEY_SWITCH_END));                      // end of args
	    
	ksToStream(ks,stdout,KDB_O_XMLHEADERS);
	
	printKeySet(ks);
	    
	ksDel(ks);
}	

void getChildKeys ()
{
	int ret;
	Key * root;
	KeySet * set;

/**BEGIN - should be same as kdbGetChildKeys*/
/*	Key *parentKey;
        ssize_t rc;
	set = ksNew();

        parentKey=keyNew("user/sw/MyApp",KEY_SWITCH_END);
        rc=kdbGetKeyChildKeys(parentKey,set,0);

        keyDel(parentKey);*/
/**END*/
	
	/* Get all value keys for this application */
	set = ksNew();
	root = keyNew (CHILD_KEY_ROOT, KEY_SWITCH_END);
	ret = kdbGetKeyChildKeys (root, set, 0); // should be renamed to kdbGetKeys

	if (ret >= 0)
	{
		fprintf (stderr, "Got all keys\n");
	} else {
		fprintf (stderr, "Error in kdbGetChildKeys\n");
//		exit (0);
	}
		
	printKeySet(set);

	ksDel (set);
}

void getKey ()
{
	Key * k;
	k = keyNew(KEY_ROOT, KEY_SWITCH_END);

	if (kdbGetKey (k) >= 0) {
		fprintf (stderr, "got key\n");
	} else {
		fprintf (stderr, "Error in kdbGetKey, errno: %d\n", errno);
		exit (0);
	}
	
	printKey (k);

	keyDel (k);
}

/**This function sets a new made Key with keyNew.
 * This must not sigfault*/
void setNullKey ()
{
	Key * k;
	k = keyNew(KEY_ROOT, KEY_SWITCH_END);
	
	fprintf (stderr, "Write same key back\n");
	if (kdbSetKey (k) >= 0) {
		fprintf (stderr, "key set\n");
	} else {
		fprintf (stderr, "Error in kdbSetKey, errno: %d\n", errno);
		exit (0);
	}
	
	printKey (k);

	keyDel (k);
}

/**This sets a valid key with a string and comment*/
void setAnotherKey ()
{
	Key * k;
	k = keyNew(KEY_ROOT, KEY_SWITCH_END);
	
	keySetString (k, "norei");
	keySetComment(k, "commi");
	
	if (kdbSetKey (k) >= 0) {
		fprintf (stderr, "key set\n");
	} else {
		fprintf (stderr, "Error in kdbSetKey, errno: %d\n", errno);
		exit (0);
	}
	
	printKey (k);

	keyDel (k);
}


int main(int argc, char **argv) {
	fprintf(stderr, "start app\n");
	/* Open the kdb */
	kdbOpen();
	fprintf(stderr, "after kdbOpen\n");

	getChildKeys();
	// setAnotherKey();
	// setNullKey();
	// getKey();
	// useKeySet();
	
	/* Close the Key database */
	kdbClose();
	return 0;
}

