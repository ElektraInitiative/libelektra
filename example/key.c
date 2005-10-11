#include <kdb.h>

/**key.c
 * This is comprehensive Test for the
 * whole Elektra api (Key and KeySets).
 * main() can be used to start the
 * wished test-functions.
 */

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

int main(int argc, char **argv) {
	useKeySet();
	
	return 0;
}

