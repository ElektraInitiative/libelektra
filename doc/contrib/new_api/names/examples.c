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
	ElektraName name;
	ElektraReturnCode error3 = elektraNameProcessEscaped (&name, "system://foo/#123", NULL, NULL, NULL);
	if (error3 != 0)
	{
		printf ("error: %d\n", error3);
		exit (1);
	}

	ElektraEntry * key = elektraEntryNew (&name);
	elektraNameFree (&name);

	elektraEntryDel (key);
	return 0;
}