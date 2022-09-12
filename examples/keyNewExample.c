/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <stdio.h>

int main (void)
{
	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);
	ElektraKey * key = 0;

	printf ("Generate some keys...");

	ksAppendKey (ks, keyNew ("user:/sw", ELEKTRA_KEY_END)); /* a simple key */

	ksAppendKey (ks, keyNew ("/", ELEKTRA_KEY_END)); /* an empty key */

	ksAppendKey (ks, keyNew ("system:/sw", ELEKTRA_KEY_END));

	ksAppendKey (ks, keyNew ("user:/tmp/ex1", ELEKTRA_KEY_VALUE, "some data", /* with a simple value */
				 ELEKTRA_KEY_END));				  /* end of args */

	ksAppendKey (ks, keyNew ("user:/tmp/ex4", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_COMMENT, "value is truncated", ELEKTRA_KEY_SIZE, 7, ELEKTRA_KEY_VALUE,
				 "some data", /* value that will be truncated to 7 bytes */
				 ELEKTRA_KEY_END));   /* end of args */

	ksAppendKey (ks, keyNew ("user:/tmp/ex5", ELEKTRA_KEY_VALUE, "some data", /* value  */
				 ELEKTRA_KEY_COMMENT, "some comment",		  /* a comment */
				 ELEKTRA_KEY_END));				  /* end of args */

	ksAppendKey (ks, keyNew ("user:/env/alias/ls", /* a key we know we have */
				 ELEKTRA_KEY_END));	       /* do nothing more */

	ksAppendKey (ks, keyNew ("user:/env/alias/ls",	     /* same key, to compare in output */
				 ELEKTRA_KEY_COMMENT, "new comment", /* set new comment */
				 ELEKTRA_KEY_END));		     /* end of args */

	key = keyNew ("user:/test//", ELEKTRA_KEY_END);

	/* we are providing a lot of '/' to see it being removed */
	keySetName (key, "system:/");
	keySetName (key, "user:/");
	keySetName (key, "user:aviram");
	keySetName (key, "user:///abc//////def///");
	keySetName (key, "user:root///aaa//////bbb///");

	keyAddBaseName (key, "tmp");
	keyAddBaseName (key, "////ex6///exx7///");
	keySetBaseName (key, "ex8");
	keySetBaseName (key, "ex9");
	keyAddBaseName (key, "///exxx9///ex10///ex\\/11///");
	keySetBaseName (key, "ex12");
	keySetBaseName (key, "ex13///");
	ksAppendKey (ks, key);

	ksDel (ks);

	printf ("finished\n");

	return 0;
}
