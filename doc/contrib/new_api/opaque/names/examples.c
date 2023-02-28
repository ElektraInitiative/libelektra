#include "../core/public.h"
#include "public.h"

#include <stdio.h>
#include <stdlib.h>

int main (void)
{
	// just check if valid, and print error
	const char * errorLoc;
	ElektraReturnCode error1 = elektraNameProcessEscaped (NULL, "system:foo", NULL, NULL, &errorLoc);
	if (error1 != 0)
	{
		// prints e.g.:
		// Name invalid, error 1: :foo
		printf ("Name invalid, error %d: %s\n", error1, errorLoc);
	}

	// find canonical form and size
	char * canonical;
	size_t csize;
	ElektraReturnCode error2 = elektraNameProcessEscaped (NULL, "system://foo/#123", &canonical, &csize, NULL);
	if (error2 == 0)
	{
		// prints:
		// Canonical size: 19
		// Canonical name: system:/foo/#__123
		printf ("Canonical size: %zd\n", csize);
		printf ("Canonical name: %s\n", canonical);
	}
	else
	{
		printf ("error: %d\n", error2);
	}

	// produce unescaped form and create key
	ElektraNameBuffer * buffer = elektraNameBufferNew ();
	ElektraReturnCode error3 = elektraNameProcessEscaped (buffer, "system://foo/#123", NULL, NULL, NULL);
	if (error3 != 0)
	{
		printf ("error: %d\n", error3);
		exit (1);
	}

	ElektraEntry * key = elektraEntryNew (elektraNameBufferFinish (buffer));
	elektraNameBufferFree (buffer);

	elektraEntryDel (key);

	ElektraNameBuffer * buffer2 = elektraNameBufferNew ();
	elektraNameBufferAppend (buffer2, "foo\0bar", 9);
	elektraNameBufferAppendPart (buffer2, "part1");
	elektraNameBufferAppendPart (buffer2, "part2");
	elektraNameBufferAppendPart (buffer2, "part3");
	elektraNameBufferRemoveLastPart (buffer2);
	elektraNameBufferReplaceLastPart (buffer2, "part11");

	ElektraEntry * key2 = elektraEntryNew (elektraNameBufferFinish (buffer2));
	elektraNameBufferFree (buffer2);

	elektraEntryDel (key2);


	return 0;
}