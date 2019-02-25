/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off


#include "enum.actual.h"

// tag definitions for enum of key '/tests/script/gen/elektra/enum/disjointed'
ELEKTRA_TAG_DEFINITIONS (ElektraEnumDisjointed, EnumDisjointed, KDB_TYPE_ENUM, elektraLongToString, elektraKeyToLong)
// tag definitions for enum of key '/tests/script/gen/elektra/enum/existinggentype'
ELEKTRA_TAG_DEFINITIONS (ExistingColors, EnumExistingColors, KDB_TYPE_ENUM, elektraLongToString, elektraKeyToLong)
// tag definitions for enum of key '/tests/script/gen/elektra/enum/gentype'
ELEKTRA_TAG_DEFINITIONS (Colors, EnumColors, KDB_TYPE_ENUM, elektraLongToString, elektraKeyToLong)
// tag definitions for enum of key '/tests/script/gen/elektra/enum/gentype2'
ELEKTRA_TAG_DEFINITIONS (Colors, EnumColors, KDB_TYPE_ENUM, elektraLongToString, elektraKeyToLong)
// tag definitions for enum of key '/tests/script/gen/elektra/enum/myenum'
ELEKTRA_TAG_DEFINITIONS (ElektraEnumMyenum, EnumMyenum, KDB_TYPE_ENUM, elektraLongToString, elektraKeyToLong)


/**
 * Initializes an instance of Elektra for the application '/tests/script/gen/elektra/enum'.
 *
 * @param error An instance of ElektraError passed to elektraOpen().
 *
 * @return A newly allocated instance of Elektra. Has to bee disposed of with elektraClose().
 *
 * @see elektraOpen
 */
Elektra * loadConfiguration (ElektraError ** error)
{
	return elektraOpen ("/tests/script/gen/elektra/enum", NULL, error);
}
