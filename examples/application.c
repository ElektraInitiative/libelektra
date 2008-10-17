/*********************************************************

To compile this example:

	$ cc `pkg-config --libs elektra` `pkg-config --cflags elektra` -o application application.c
		or static
	$ cc -c `pkg-config --cflags elektra` application.c

This application shows how to read (resp. update) and save
configuration using elektra.

**********************************************************/

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <kdb.h>

#define MY_APP_ROOT   "/sw/MyApp/current"

int generateConfig(KeySet *myConfig)
{
	ksAppendKey(myConfig, keyNew("user/sw/MyApp/current/dir1",
			KEY_VALUE, "directory value",
			KEY_COMMENT, "this is the first directory",
			KEY_DIR, KEY_END));
	ksAppendKey(myConfig, keyNew("user/sw/MyApp/current/dir1/key1",
			KEY_VALUE, "the first application key",
			KEY_COMMENT, "some useful comment",
			KEY_DIR, KEY_END));
	ksAppendKey(myConfig, keyNew("user/sw/MyApp/current/dir1/key2",
			KEY_VALUE, "the second application key",
			KEY_COMMENT, "some useful comment",
			KEY_DIR, KEY_END));
	ksAppendKey(myConfig, keyNew("user/sw/MyApp/current/dir1/key3",
			KEY_VALUE, "the third application key",
			KEY_COMMENT, "some useful comment",
			KEY_DIR, KEY_END));
	ksAppendKey(myConfig, keyNew("user/sw/MyApp/current/dir2",
			KEY_VALUE, "directory value",
			KEY_DIR, KEY_END));
	ksAppendKey(myConfig, keyNew("user/sw/MyApp/current/dir3",
			KEY_VALUE, "directory value",
			KEY_DIR, KEY_END));
	ksAppendKey(myConfig, keyNew("user/sw/MyApp/current/dir4",
			KEY_VALUE, "directory value",
			KEY_DIR, KEY_END));
}


/* Read config keys for this application */
int readConfig(KDB * handle, KeySet *myConfig)
{
	int rc = 0;
	int size = 0;

	/* Get all value keys for this application */
	if (kdbGetByName(handle, myConfig, "system" MY_APP_ROOT, 0) == -1)
	{
		rc = -1;
		perror("Couldn't get system configuration. Reason");
	} else {
		size = (int)ksGetSize(myConfig);
		printf("Retrieved %d keys\n", size);
	}

	if (kdbGetByName(handle, myConfig,"user" MY_APP_ROOT,  0) == -1)
	{
		rc = -1;
		perror("Couldn't get user configuration. Reason");
	} else {
		size = (int)ksGetSize(myConfig);
		printf("Retrieved %d keys\n", size);
	}

	if (rc == -1) return -1;
	return size;
}


/* Change some keys */
void changeConfig(KeySet *myConfig)
{
	Key *current;
	
	ksRewind(myConfig);
	while ((current=ksNext(myConfig))) {
		char keyName[MAX_KEY_LENGTH];
		char value[MAX_KEY_LENGTH];
		
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

/* Make keys empty */
void emptyConfig(KeySet *myConfig)
{
	ksRewind(myConfig);
	while (ksNext(myConfig))
	{
		keySetString(ksCurrent(myConfig), "");
	}
}


/* Make keys empty */
void printConfig(KeySet *myConfig)
{
	ksRewind(myConfig);
	while (ksNext(myConfig))
	{
		printf ("Key %s, Value %s, Comment %s\n",
			(char*)keyName(ksCurrent(myConfig)),
			(char*)keyValue(ksCurrent(myConfig)),
			(char*)keyComment(ksCurrent(myConfig)));
	}
}



/* Save the modified keys */
int saveConfig(KDB * handle, KeySet *myConfig)
{
	return kdbSet(handle,myConfig,0,0);
}


int main(int argc, char **argv)
{
	int command;
	KeySet *myConfig=ksNew(0);
	KDB * handle=kdbOpen();

	printf ("Following commands are available:\n\n");
	printf ("g ... generate new Configuration\n");
	printf ("c ... change Configuration\n");
	printf ("r ... read Configuration\n");
	printf ("s ... save Configuration\n");
	printf ("p ... print Configuration\n");
	printf ("e ... empty Configuration\n");
	printf ("d ... clear Configuration\n");

	while (1)
	{
		/* Get configuration values, and just continue if there is no error */

		command = fgetc (stdin);
		if (command == 'q') break;
		switch (command)
		{
			case 'g':
				generateConfig(myConfig);
				break;
			case 'c':
				changeConfig(myConfig);
				break;
			case 'r':
				readConfig(handle, myConfig);
				break;
			case 's':
				saveConfig(handle, myConfig);
				break;
			case 'p':
				printConfig(myConfig);
				break;
			case 'e':
				emptyConfig(myConfig);
				break;
			case 'd':
				ksClear(myConfig);
				break;
			case '\n': case '\r': case '\f':
				break;
			default:
				printf ("unkown command\n");
				break;
		}
	}

	kdbClose(handle);

	/* Free all keys and resources in the key set */
	ksDel(myConfig);

	return 0;
}

