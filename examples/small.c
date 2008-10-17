#include <stdlib.h>
#include <stdio.h>
#include <kdb.h>

void BailOut (char * msg)
{
	fprintf (stderr, "%s\n", msg);
	exit (1);
}

int main(void)
{
	KDB * h;
	KeySet * myConfig = ksNew(0);
	Key    * myKey;
	char   * myValue;

	/* Open the Key Database */
	if (!(h = kdbOpen()))
		BailOut ("Could not open Key Database");

	/* Get the hello world keyset */
	if (kdbGetByName(h, myConfig, "/", 0) == -1)
		BailOut ("Could not get Keys");

	/* Find the key in the keyset */
	if ((myKey = ksLookupByName (myConfig, "/hello", 0)) == NULL)
		BailOut ("Could not Lookup Key");

	/* Get the value of the key */
	if ((myValue = (char*) keyValue (myKey)) == NULL)
		BailOut ("Could not get Keyvalue");

	/* Actually print the key */
	printf ("%s\n", myValue);
	/* Close and free KeySet */
	ksDel(myConfig);
	/* Close the Key database */
	kdbClose(h);

	return 0;
}

