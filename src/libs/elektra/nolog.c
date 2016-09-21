/**
 * @file
 *
 * @brief Fake Logger Implementation
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <kdblogger.h>

int elektraLog (int level ELEKTRA_UNUSED, const char * function ELEKTRA_UNUSED, const char * absFile ELEKTRA_UNUSED,
		const int line ELEKTRA_UNUSED, const char * mmsg ELEKTRA_UNUSED, ...)
{
	return 0;
}
