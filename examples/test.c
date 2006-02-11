#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <kdb.h>

/**test.c
 * This is comprehensive Test for the
 * whole Elektra Api and the Backends
 * in special.
 * main() can be used to start the
 * wished test-functions.
 * In the end it should self check, if
 * the results are ok and print that.
 */

#define ROOT_KEY "user/test"

#define NUL_KEY "user/test/nul/key"

#define NEW_KEY "user/test/new/key"
#define NEW_VAL "value"
#define NEW_COM "comme"

#define ANO_KEY "user/test/new/ano"
#define ANO_VAL "ano=th;va\nl\0ue"
#define ANO_COM "com=en;tt\nh\0hi"

#define DIR_KEY "user/test/dir"

#define TEST_OK 0
#define TEST_FAIL 1

/**Return new various keys
 * These Keys will be set and get in various
 * tests. It will be evaluated if the backend
 * does everything correctly.*/
Key * newNulKey ()
{
	return
	keyNew (NUL_KEY, KEY_SWITCH_END);
}

Key * newNewKey ()
{
	return 
	keyNew (NEW_KEY,
		KEY_SWITCH_VALUE, NEW_VAL,
		KEY_SWITCH_COMMENT, NEW_COM,
		KEY_SWITCH_END);
}

Key * newAnoKey ()
{
	return 
	keyNew (ANO_KEY,
		KEY_SWITCH_VALUE, ANO_VAL,
		KEY_SWITCH_COMMENT, ANO_COM,
		KEY_SWITCH_END);
}

Key * newDirKey ()
{
	return 
	keyNew (DIR_KEY,
		KEY_SWITCH_TYPE, KEY_TYPE_DIR);
}



/*Printing Functions for information/errors*/
void bailOut (const char * msg);
void headerOut (const char * msg);
void testOut (uint32_t mask, const char * msg);

/*These are the only functions not using
 * the above printing functions. They are
 * not used by the testing suite.*/
void printKey (Key * k);
void printKeys (KeySet * set);

/*Get all keys and check if valid*/
void getKey();
void getKeys();

/*Sets all above keys*/
void setKey();
void setKeys();

/*Deletes all above keys*/
void delKeys();


/**Prints header for failures*/
void bailOut (const char * msg)
{
	fprintf (stderr, "********      E R R O R      ******** %s\n", msg);
	//TODO:
	// kdbPrintError (msg);
	perror (msg);
	fprintf (stderr, "********      E R R O R      ******** %s\n", msg);
	/**failures are not critical*/
	// exit (0);
}

/**Prints header for what is done now*/
void headerOut (const char * msg)
{
	fprintf (stderr, "-------- %19s --------\n", msg);
}

void testOut (uint32_t failed, const char * msg)
{
	fprintf (stderr, "Test ");
	if (failed) fprintf (stderr, "failed");
	else fprintf (stderr, "suceeded");
	fprintf (stderr, ": %s\n", msg);
	if ((failed & KEY_SWITCH_TYPE) == 0) 
	{fprintf (stderr, "%s differs\n", KEY_SWITCH_TYPE);}
	if ((failed & KEY_SWITCH_NAME) == 0) {fprintf (stderr, "%s differs\n", KEY_SWITCH_NAME);}
	if ((failed & KEY_SWITCH_VALUE) == 0) {fprintf (stderr, "%s differs\n", KEY_SWITCH_VALUE);}
	if ((failed & KEY_SWITCH_OWNER) == 0) {fprintf (stderr, "%s differs\n", KEY_SWITCH_OWNER);}
	if ((failed & KEY_SWITCH_COMMENT) == 0) {fprintf (stderr, "%s differs\n", KEY_SWITCH_COMMENT);}
	if ((failed & KEY_SWITCH_UID) == 0) {fprintf (stderr, "%s differs\n", KEY_SWITCH_UID);}
	if ((failed & KEY_SWITCH_GID) == 0) {fprintf (stderr, "%s differs\n", KEY_SWITCH_GID);}
	if ((failed & KEY_SWITCH_MODE) == 0) {fprintf (stderr, "%s differs\n", KEY_SWITCH_MODE);}
	if ((failed & KEY_SWITCH_NEEDSYNC) == 0) {fprintf (stderr, "%s differs\n", KEY_SWITCH_NEEDSYNC);}
	if ((failed & KEY_SWITCH_FLAG) == 0) {fprintf (stderr, "%s differs\n", KEY_SWITCH_FLAG);}
}
	

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
	if (nam == NULL) {bailOut ("malloc error");}
	keyGetName (k, nam, n);
	
	s = keyGetDataSize (k);
	str = (char*) malloc (s+100);
	if (str == NULL) {bailOut ("malloc error");}
	keyGetString (k, str, s);

	c = keyGetCommentSize (k);
	com = (char*) malloc (c+100);
	if (com == NULL) {bailOut ("malloc error");}
	keyGetComment (k, com, c);
	
	printf ("Name[%d,%d]: %s\t", n, strlen(nam)+1, nam);
	printf ("String[%d,%d]: %s\t", s, strlen(str)+1, str);
	printf ("Kommentar[%d,%d]: %s\n", c, strlen(com)+1, com);

	free (nam);
	free (str);
	free (com);
}

void printKeys (KeySet * set)
{
	Key * k;
	ksRewind (set);
	k = ksNext (set);
	while (k) {
		printKey (k);
		k = ksNext (set);
	}
}

void getKeys ()
{
	int ret;
	Key * root;
	KeySet * set;

	Key *parentKey;
        ssize_t rc;
	set = ksNew();

	/* Get all value keys for this application */
	headerOut ("getChildKeys");
	set = ksNew();
	if (kdbGetChildKeys (ROOT_KEY, set, 0) == -1)
		bailOut("kdbGetChildKeys");
	//TODO testing functions...
		
	ksDel (set);
}


void getKey ()
{
	Key * k;

	headerOut ("getKey");
	k = keyNew(NEW_KEY, KEY_SWITCH_END);
	if (kdbGetKey (k) == -1) bailOut ("newkey kdbGetKey");
	else testOut (keyCompare (newNewKey(),k), "newkey compare");
	
	/*k = keyNew(ANO_KEY, KEY_SWITCH_END);
	if (kdbGetKey (k) == -1) bailOut ("kdbGetKey");
	else testOut (keyCompare (newAnoKey(),k), "anokey compare");
	keyDel (k);*/
}

void setKey ()
{
	Key * k;
/*	k = newNulKey();
	if (kdbSetKey (k) == -1) bailOut ("Error in kdbSetKeys");
	keyDel (k);
*/	
	k = newNewKey();
	if (kdbSetKey (k) == -1) bailOut ("Error in kdbSetKeys");
	keyDel (k);
/*	
	k = newAnoKey();
	if (kdbSetKey (k) == -1) bailOut ("Error in kdbSetKeys");
	keyDel (k);
	
	k = newDirKey();
	if (kdbSetKey (k) == -1) bailOut ("Error in kdbSetKeys");
	keyDel (k);
*/	
}

void setKeys ()
{
	KeySet * ks = ksNew();
	ksAppend (ks, newNulKey());
	ksAppend (ks, newNewKey());
	ksAppend (ks, newAnoKey());
	ksAppend (ks, newDirKey());
	
	if (kdbSetKeys (ks) == -1) bailOut ("Error in kdbSetKeys");
	ksDel (ks);
}


/**This removes all the keys*/
void delKeys ()
{
	headerOut ("Will remove all keys now");

	//TODO: Ini backend remove key does not work, manual:
	remove ("/home/markus/.kdb/user/test/nul");
	remove ("/home/markus/.kdb/user/test/new");
	rmdir  ("/home/markus/.kdb/user/test/dir");
	return;
	
	Key * k;
	k = keyNew(NUL_KEY, KEY_SWITCH_END);
	if (kdbRemoveKey (k) == -1) bailOut ("Error Remove Nul Key");
	keyDel (k);
	k = keyNew(NEW_KEY, KEY_SWITCH_END);
	if (kdbRemoveKey (k) == -1) bailOut ("Error Remove New Key");
	keyDel (k);
	k = keyNew(ANO_KEY, KEY_SWITCH_END);
	if (kdbRemoveKey (k) == -1) bailOut ("Error Remove Ano Key");
	keyDel (k);
	k = keyNew(DIR_KEY, KEY_SWITCH_END);
	if (kdbRemoveKey (k) == -1) bailOut ("Error Remove Dir Key");
	keyDel (k);
	
}

int main(int argc, char **argv) {
	/* Open the kdb */
	kdbOpen();

	headerOut ("start tests");
	
	/**First delete all keys*/
	delKeys();
	

	/**Now test with single set and get function*/
	setKey();
	getKey();	
	delKeys();

	/**Now set them at once, get them one after another*/
/*	setKeys ();
	getKey ();
	delKeys();
*/
	/**Now set them at once and get them recursively*/
/*	setKeys ();
	getKeys ();
*/
	/**Clean the mess up*/
	delKeys ();
	
	headerOut ("finished tests");
	
	/* Close the Key database */
	kdbClose();
	return 0;
}

