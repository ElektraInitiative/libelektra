#include <stdio.h>
#include <stdlib.h>
#include <kdb.h>

void BailOut (char * msg)
{
	perror (msg);
}

int main()
{
	KDB * h;
	KeySet * myConfig = ksNew(0);
	Key    * myKey;
	char   * myValue;

	/* Open the Key Database */
	if ((h = kdbOpen()) != 0)
		BailOut ("Could not open Key Database");
	
	/* Get myapps keyset */
	if (kdbGetByName(h, myConfig, "/examples", 0 ) == -1)
		BailOut ("Could not get Keys");

	/* Find the key in the keyset */
	if ((myKey = ksLookupByName (myConfig, "/examples/key", 0)) == NULL)
		BailOut ("Could not Lookup Key");

	/* Get the value of the key */
	if ((myValue = (char*) keyValue (myKey)) == NULL)
		BailOut ("Could not get Keyvalue");
		
	/* Actually print the key */
	printf ("%s\n", myValue);
	/* Close the Key database */
	kdbClose(h);
	ksDel(myConfig);
	return 0;
}

