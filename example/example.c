/*********************************************************

To compile this example:

	$ cc -l kdb -o example example.c


**********************************************************/


#include <stdio.h>
#include <kdb.h>

#define MY_APP_ROOT   "system/sw/MyApp"


/* Read config keys for this application */
int readConfig(KeySet *myConfig) {
	int rc;

	/* Open the kdb */
	kdbOpen();
	
	/* Get all value keys for this application */
	rc=kdbGetChildKeys(MY_APP_ROOT, myConfig, RG_O_RECURSIVE);
	
	/* Close the Key database */
	kdbClose();
	
	return rc;
}


/* Change some keys */
void changeConfig(KeySet *myConfig) {
	Key *current;
	
	for (current=myConfig->start; current; current=current->next) {
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
int saveConfig(KeySet *myConfig) {
	kdbOpen();
	kdbSetKeys(myConfig);
	kdbClose();
}



int main(int argc, char **argv) {
	KeySet myConfig;
	
	ksInit(&myConfig);
	
	/* Get configuration values, and just continue if there is no error */
	if (readConfig(&myConfig)) {
		perror("Couldn't get my configuration. Reason");
		exit(1);
	} else {
		printf("Retrieved %d keys\n",myConfig.size);
	}
		
	changeConfig(&myConfig);
	saveConfig(&myConfig);
	
	/* Free all keys and resources in the key set */
	ksClose(&myConfig);
	
	return 0;
}
