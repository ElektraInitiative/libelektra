/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off


#include "simple.actual.h"




/**
 * Initializes an instance of Elektra for the application '/tests/script/gen/elektra/simple'.
 *
 * @param error An instance of ElektraError passed to elektraOpen().
 *
 * @return A newly allocated instance of Elektra. Has to bee disposed of with elektraClose().
 *
 * @see elektraOpen
 */
Elektra * loadConfiguration (ElektraError ** error)
{
	return elektraOpen ("/tests/script/gen/elektra/simple", NULL, error);
}
