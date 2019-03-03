/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

// clang-format on

#include "empty.actual.h"



#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h>
#include <string.h>

#include <kdbhelper.h>

#include <elektra/conversion.h>


/**
 * Initializes an instance of Elektra for the application '/tests/script/gen/elektra/empty'.
 *
 * @param error A reference to an ElektraError pointer. Will be passed to elektraOpen().
 *
 * @return A newly allocated instance of Elektra. Has to be disposed of with elektraClose().
 *
 * @see elektraOpen
 */// 
Elektra * loadConfiguration (ElektraError ** error)
{
	KeySet * defaults = ksNew (0,
	KS_END);
;
	return elektraOpen ("/tests/script/gen/elektra/empty", defaults, error);
}

// -------------------------
// Enum conversion functions
// -------------------------



// -------------------------
// Enum accessor functions
// -------------------------



// -------------------------
// Struct accessor functions
// -------------------------



#ifdef __cplusplus
}
#endif
