#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
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

#define NUL_KEY ROOT_KEY "/nul"

#define NEW_KEY ROOT_KEY "/new"
#define NEW_VAL "value"
#define NEW_COM "comme"

#define ANO_KEY ROOT_KEY "/ano"
#define ANO_VAL "ano=th;va\nl\0ue"
#define ANO_COM "com=en;tt\nh\0hi"

#define DIR_KEY ROOT_KEY "/dir"

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
void testOut (Key * orig, Key * read, const char * msg);

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
	fprintf (stderr, "******** %29s ********\n", msg);
	//TODO:
	// kdbPrintError (msg);
	// perror (msg);
	// exit (0);
}

/**Prints header for what is done now*/
void headerOut (const char * msg)
{
	fprintf (stderr, "-------- %29s --------\n", msg);
}

void testOut (Key * orig, Key * read, const char * keyname)
{
	uint32_t failed=0;
	
	fprintf (stderr, "Test ");
	failed=keyCompare (orig, read);
	if (failed) fprintf (stderr, "failed");
	else fprintf (stderr, "suceeded");
	fprintf (stderr, " for : %s\n", keyname);
	if (!failed) return;
	if (failed & KEY_SWITCH_TYPE) 
		fprintf (stderr, "type differs: is %d, was %d\n",
			keyGetType(read),keyGetType(orig));
	if (failed & KEY_SWITCH_NAME)
		fprintf (stderr, "name differs: is \"%s\", was \"%s\"\n",
			keyStealName(read),keyStealName(orig));
	if (failed & KEY_SWITCH_VALUE)
		fprintf (stderr, "value differs: is \"%s\", was \"%s\"\n",
			keyStealValue(read),keyStealValue(orig));
	if (failed & KEY_SWITCH_OWNER)
		fprintf (stderr, "owner differs: is \"%s\", was \"%s\"\n",
			keyStealOwner(read),keyStealOwner(orig));
	if (failed & KEY_SWITCH_COMMENT)
		fprintf (stderr, "comment differs: is \"%s\", was \"%s\"\n",
			keyStealComment(read),keyStealComment(orig));
	if (failed & KEY_SWITCH_UID)
		fprintf (stderr, "uid differs: is \"%d\", was \"%d\"\n",
			keyGetUID(read),keyGetUID(orig));
	if (failed & KEY_SWITCH_GID)
		fprintf (stderr, "gid differs: is \"%d\", was \"%d\"\n",
			keyGetGID(read),keyGetGID(orig));
	if (failed & KEY_SWITCH_MODE)
		fprintf (stderr, "mode differs: is \"0%o\", was \"0%o\"\n",
			keyGetAccess(read),keyGetAccess(orig));
	if (failed & KEY_SWITCH_NEEDSYNC)
		fprintf (stderr, "needsync differs\n");
	if (failed & KEY_SWITCH_FLAG)
		fprintf (stderr, "flag differs\n");

	keyDel (orig);
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
	
	printf ("Name[%d,%d]: %s\t", n, strblen(nam), nam);
	printf ("String[%d,%d]: %s\t", s, strblen(str), str);
	printf ("Kommentar[%d,%d]: %s\n", c, strblen(com), com);

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
	Key * t;

	Key *parentKey;
        ssize_t rc;
	set = ksNew();

	/* Get all value keys for this application */
	headerOut ("getChildKeys");
	set = ksNew();
	if (kdbGetChildKeys (ROOT_KEY, set, KDB_O_RECURSIVE | KDB_O_DIR) == -1)
		bailOut("kdbGetChildKeys");

	t = ksLookupByName (set, NUL_KEY, 0);
	if (t) testOut (newNulKey(), t, NUL_KEY);
	else bailOut ("Did not find nulkey");
	
	ksRewind(set);
	t = ksLookupByName (set, NEW_KEY, 0);
	if (t) testOut (newNewKey(), t, NEW_KEY);
	else bailOut ("Did not find newkey");
	
	ksRewind(set);
	t = ksLookupByName (set, ANO_KEY, 0);
	if (t) testOut (newAnoKey(), t, ANO_KEY);
	else bailOut ("Did not find anokey");
	
	ksRewind(set);
	t = ksLookupByName (set, DIR_KEY, 0);
	if (t) testOut (newDirKey(), t, DIR_KEY);
	else bailOut ("Did not find dirkey");
		
	ksDel (set);
}


void getKey ()
{
	Key * k;

	headerOut ("getKey");

	k = keyNew(NUL_KEY, KEY_SWITCH_END);
	if (kdbGetKey (k) == -1) bailOut ("newkey kdbGetKey");
	else testOut (newNulKey(), k, NUL_KEY); 
	keyDel (k);
	
	k = keyNew(NEW_KEY, KEY_SWITCH_END);
	if (kdbGetKey (k) == -1) bailOut ("newkey kdbGetKey");
	else testOut (newNewKey(), k, NEW_KEY); 
	keyDel (k);
	
	k = keyNew(ANO_KEY, KEY_SWITCH_END);
	if (kdbGetKey (k) == -1) bailOut ("kdbGetKey");
	else testOut (newAnoKey(), k, ANO_KEY); 
	keyDel (k);
	
	k = keyNew(DIR_KEY, KEY_SWITCH_END);
	if (kdbGetKey (k) == -1) bailOut ("kdbGetKey");
	else testOut (newDirKey(), k, DIR_KEY); 
	keyDel (k);
}

void setKey ()
{
	Key * k;
	k = newNulKey();
	if (kdbSetKey (k) == -1) bailOut ("Error in kdbSetKeys");
	keyDel (k);
	
	k = newNewKey();
	if (kdbSetKey (k) == -1) bailOut ("Error in kdbSetKeys");
	keyDel (k);
	
	k = newAnoKey();
	if (kdbSetKey (k) == -1) bailOut ("Error in kdbSetKeys");
	keyDel (k);
	
	k = newDirKey();
	if (kdbSetKey (k) == -1) bailOut ("Error in kdbSetKeys");
	keyDel (k);
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
	kdbOpen();

	headerOut ("start tests");
	
	headerOut ("single set, single get");
	setKey();
	getKey();	
	delKeys();

	headerOut ("multiple set, single get");
	setKeys ();
	getKey ();
	delKeys();
	
	headerOut ("single set, multiple get");
	setKey();
	getKeys();	
	delKeys();

	headerOut ("multiple set, multiple get");
	setKeys ();
	getKeys ();
	delKeys ();
	
	headerOut ("finished tests");
	
	kdbClose();
	return 0;
}

