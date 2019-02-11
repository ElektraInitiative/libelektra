/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off


#include "empty.actual.h"




/**
 * Initializes an instance of Elektra for the application '/tests/script/gen/elektra/empty'.
 *
 * @param error An instance of ElektraError passed to elektraOpen().
 *
 * @return A newly allocated instance of Elektra. Has to bee disposed of with elektraClose().
 *
 * @see elektraOpen
 */
Elektra * loadConfiguration (ElektraError ** error)
{
	return elektraOpen ("/tests/script/gen/elektra/empty", NULL, error);
}
