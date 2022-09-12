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
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * key = 0;

	printf ("Generate some keys...");

	elektraKeysetAppendKey (ks, elektraKeyNew ("user:/sw", ELEKTRA_KEY_END)); /* a simple key */

	elektraKeysetAppendKey (ks, elektraKeyNew ("/", ELEKTRA_KEY_END)); /* an empty key */

	elektraKeysetAppendKey (ks, elektraKeyNew ("system:/sw", ELEKTRA_KEY_END));

	elektraKeysetAppendKey (ks, elektraKeyNew ("user:/tmp/ex1", ELEKTRA_KEY_VALUE, "some data", /* with a simple value */
				 ELEKTRA_KEY_END));				  /* end of args */

	elektraKeysetAppendKey (ks, elektraKeyNew ("user:/tmp/ex4", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_COMMENT, "value is truncated", ELEKTRA_KEY_SIZE, 7, ELEKTRA_KEY_VALUE,
				 "some data", /* value that will be truncated to 7 bytes */
				 ELEKTRA_KEY_END));   /* end of args */

	elektraKeysetAppendKey (ks, elektraKeyNew ("user:/tmp/ex5", ELEKTRA_KEY_VALUE, "some data", /* value  */
				 ELEKTRA_KEY_COMMENT, "some comment",		  /* a comment */
				 ELEKTRA_KEY_END));				  /* end of args */

	elektraKeysetAppendKey (ks, elektraKeyNew ("user:/env/alias/ls", /* a key we know we have */
				 ELEKTRA_KEY_END));	       /* do nothing more */

	elektraKeysetAppendKey (ks, elektraKeyNew ("user:/env/alias/ls",	     /* same key, to compare in output */
				 ELEKTRA_KEY_COMMENT, "new comment", /* set new comment */
				 ELEKTRA_KEY_END));		     /* end of args */

	key = elektraKeyNew ("user:/test//", ELEKTRA_KEY_END);

	/* we are providing a lot of '/' to see it being removed */
	elektraKeySetName (key, "system:/");
	elektraKeySetName (key, "user:/");
	elektraKeySetName (key, "user:aviram");
	elektraKeySetName (key, "user:///abc//////def///");
	elektraKeySetName (key, "user:root///aaa//////bbb///");

	elektraKeyAddBaseName (key, "tmp");
	elektraKeyAddBaseName (key, "////ex6///exx7///");
	elektraKeySetBaseName (key, "ex8");
	elektraKeySetBaseName (key, "ex9");
	elektraKeyAddBaseName (key, "///exxx9///ex10///ex\\/11///");
	elektraKeySetBaseName (key, "ex12");
	elektraKeySetBaseName (key, "ex13///");
	elektraKeysetAppendKey (ks, key);

	elektraKeysetDel (ks);

	printf ("finished\n");

	return 0;
}
