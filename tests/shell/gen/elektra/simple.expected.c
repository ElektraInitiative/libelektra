/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

// clang-format on

#include "simple.actual.h"



#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h>
#include <string.h>

#include <kdbhelper.h>

#include <elektra/conversion.h>


/**
 * Initializes an instance of Elektra for the application '/tests/script/gen/elektra/simple'.
 *
 * @param error A reference to an ElektraError pointer. Will be passed to elektraOpen().
 *
 * @return A newly allocated instance of Elektra. Has to be disposed of with elektraClose().
 *
 * @see elektraOpen
 */// 
Elektra * loadConfiguration (ElektraError ** error)
{
	KeySet * defaults = ksNew (5,
	keyNew ("spec/tests/script/gen/elektra/simple/mydouble", KEY_META, "default", "0.0", KEY_META, "type", "double", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/myfloatarray/#", KEY_META, "default", "0.0", KEY_META, "type", "float", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/myint", KEY_META, "default", "0", KEY_META, "type", "long", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/mystring", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/print", KEY_META, "default", "0", KEY_META, "type", "boolean", KEY_END),
	KS_END);
;
	return elektraOpen ("/tests/script/gen/elektra/simple", defaults, error);
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
