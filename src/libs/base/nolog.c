/**
 * @file
 *
 * @brief C99-compatible Fake Logger Implementation
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <internal/utility/logger.h>

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>


int elektraLog (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED, const char * absFile ELEKTRA_UNUSED,
		const int line ELEKTRA_UNUSED, const char * mmsg ELEKTRA_UNUSED, ...)
{
	return 0;
}

void elektraAbort (const char * expression, const char * function, const char * file, const int line, const char * msg, ...)
{
	fprintf (stderr, "%s:%d:%s: Assertion `%s' failed: ", file, line, function, expression);
	{
		va_list args;
		va_start (args, msg);
		vfprintf (stderr, msg, args);
		va_end (args);
	}
	fprintf (stderr, "\n");
	fflush (stderr);
	abort ();
}
