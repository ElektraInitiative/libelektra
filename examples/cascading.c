#include <stdio.h>
#include <kdb.h>

void BailOut (char * msg) {
//	kdbPrintError (msg);
	perror (msg);
	exit (1);
}

int main() {
	KeySet * myConfig = ksNew();
	Key    * myKey;
	char   * myValue;

	/* Open the Key Database */
	if (kdbOpen() == -1)
		BailOut ("Could not open Key Database");
	
	/* Get myapps keyset */
	if (kdbGetChildKeys("user/myapp", myConfig, 0 ) == -1)
		BailOut ("Could not get Keys");
	
	/* Get myapps keyset */
	if (kdbGetChildKeys("system/myapp", myConfig, 0 ) == -1)
		BailOut ("Could not get Keys");

	/* Find the key in the keyset */
	if ((myKey = ksLookupByName (myConfig, "/myapp/key", 0)) == NULL)
		BailOut ("Could not Lookup Key");

	/* Get the value of the key */
	if ((myValue = (char*) keyStealValue (myKey)) == NULL)
		BailOut ("Could not get Keyvalue");
		
	/* Actually print the key */
	printf ("%s\n", myValue);
	/* Close the Key database */
	kdbClose();
	ksClose(myConfig);
}

