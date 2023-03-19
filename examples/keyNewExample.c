/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/core/keyset.h>
#include <elektra/core/key.h>
#include <stdio.h>

int main (void)
{
	KeySet * ks = ksNew (0, KS_END);
	Key * key = 0;

	printf ("Generate some keys...");

	ksAppendKey (ks, keyNew ("user:/sw", KEY_END)); /* a simple key */

	ksAppendKey (ks, keyNew ("/", KEY_END)); /* an empty key */

	ksAppendKey (ks, keyNew ("system:/sw", KEY_END));

	ksAppendKey (ks, keyNew ("user:/tmp/ex1", KEY_VALUE, "some data", /* with a simple value */
				 KEY_END));				  /* end of args */

	ksAppendKey (ks, keyNew ("user:/tmp/ex4", KEY_BINARY, KEY_META, "comment/#0", "value is truncated", KEY_SIZE, 7, KEY_VALUE,
				 "some data", /* value that will be truncated to 7 bytes */
				 KEY_END));   /* end of args */

	ksAppendKey (ks, keyNew ("user:/tmp/ex5", KEY_VALUE, "some data", /* value  */
				 KEY_META, "comment/#0", "some comment",  /* a comment */
				 KEY_END));				  /* end of args */

	ksAppendKey (ks, keyNew ("user:/env/alias/ls", /* a key we know we have */
				 KEY_END));	       /* do nothing more */

	ksAppendKey (ks, keyNew ("user:/env/alias/ls",			/* same key, to compare in output */
				 KEY_META, "comment/#0", "new comment", /* set new comment */
				 KEY_END));				/* end of args */

	key = keyNew ("user:/test//", KEY_END);

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
