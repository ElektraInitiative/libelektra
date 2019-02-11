/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off


#include <elektra.h>

#ifndef SIMPLE_ACTUAL_H
#define SIMPLE_ACTUAL_H






/**
 * Tag value for key '/tests/script/gen/elektra/simple/mydouble'.
 */
ELEKTRA_TAG_VALUE (MYDOUBLE, "/tests/script/gen/elektra/simple/mydouble", Double)

/**
 * Tag value for key '/tests/script/gen/elektra/simple/myfloatarray/#'.
 */
ELEKTRA_TAG_VALUE (MYFLOATARRAY, "/tests/script/gen/elektra/simple/myfloatarray/#", Float)

/**
 * Tag value for key '/tests/script/gen/elektra/simple/myint'.
 */
ELEKTRA_TAG_VALUE (MYINT, "/tests/script/gen/elektra/simple/myint", Long)

/**
 * Tag value for key '/tests/script/gen/elektra/simple/mystring'.
 */
ELEKTRA_TAG_VALUE (MYSTRING, "/tests/script/gen/elektra/simple/mystring", String)

/**
 * Tag value for key '/tests/script/gen/elektra/simple/print'.
 */
ELEKTRA_TAG_VALUE (PRINT, "/tests/script/gen/elektra/simple/print", Boolean)
;

Elektra * loadConfiguration (ElektraError ** error);

#endif // SIMPLE_ACTUAL_H
