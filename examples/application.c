/*********************************************************

To compile this example:

	$ cc `pkg-config --libs elektra` -o example example.c
		or
	$ cc -L/lib -lelektra -o example example.c

This should be a ready to use template to write an 
full-elektra compatible application.

It should be able to read and write a full Config in
the system/ and user/ hierachy of the Application.

TODO: Not well tested yet.


**********************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <kdb.h>

#define MY_APP_ROOT   "system/sw/MyApp/current"


/* Read config keys for this application */
int readConfig(KDBHandle handle, KeySet *myConfig) {
	int rc;

	/* Get all value keys for this application */
	rc=kdbGetChildKeys(handle,MY_APP_ROOT, myConfig, KDB_O_RECURSIVE);
	
	return rc;
}


/* Change some keys */
void changeConfig(KeySet *myConfig) {
	Key *current;
	
	ksRewind(myConfig);
	while ((current=ksNext(myConfig))) {
		char keyName[200];
		char value[300];
		
		keyGetFullName(current,keyName,sizeof(keyName));
		keyGetString(current,value,sizeof(value));
		
		printf("Key %s was %s. ", keyName, value);
		
		/* Add "- modified" to the end of the string */
		strcat(value,"- modified");
		
		/* change the key value */
		keySetString(current,value);
		
		/* reget it, just as an example */
		keyGetString(current,value,sizeof(value));
		
		printf("Now is %s\n", value);
	}
}


/* Save the modified keys */
int saveConfig(KDBHandle handle, KeySet *myConfig) {
	return kdbSetKeys(handle,myConfig);
}



int main(int argc, char **argv) {
	KeySet *myConfig=ksNew();
	KDBHandle handle=0;
	
	kdbOpen(&handle);
	
	/* Get configuration values, and just continue if there is no error */
	if (readConfig(handle,myConfig)) {
		perror("Couldn't get my configuration. Reason");
		exit(1);
	} else {
		printf("Retrieved %d keys\n",ksGetSize(myConfig));
	}
		
	changeConfig(myConfig);
	saveConfig(handle,myConfig);
	
	kdbClose(&handle);
	
	/* Free all keys and resources in the key set */
	ksDel(myConfig);
	
	return 0;
}
