#include <stdlib.h>
#include <stdio.h>
#include <kdb.h>

void BailOut (char * msg) {
        kdbPrintError (msg);
        exit (1);
}

int main() {
	KDBHandle h;
        KeySet * myConfig = ksNew();
        Key    * myKey;
        char   * myValue;

        /* Open the Key Database */
        if (kdbOpen(&h) == -1)
                BailOut ("Could not open Key Database");

        /* Get the hello world keyset */
        if (kdbGetChildKeys(h,"user", myConfig, KDB_O_RECURSIVE ) == -1)
                BailOut ("Could not get Keys");

        /* Find the key in the keyset */
        if ((myKey = ksLookupByName (myConfig, "/hello", 0)) == NULL)
                BailOut ("Could not Lookup Key");

        /* Get the value of the key */
        if ((myValue = (char*) keyStealValue (myKey)) == NULL)
                BailOut ("Could not get Keyvalue");

        /* Actually print the key */
        printf ("%s\n", myValue);
        /* Close and free KeySet */
        ksDel(myConfig);
        /* Close the Key database */
        kdbClose(&h);
}

