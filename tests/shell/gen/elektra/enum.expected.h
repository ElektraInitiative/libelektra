/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off


#include <elektra.h>

#ifndef ENUM_ACTUAL_H
#define ENUM_ACTUAL_H

typedef enum
{
	ELEKTRA_ENUM_DISJOINTED_BLACK = 0,
	ELEKTRA_ENUM_DISJOINTED_WHITE = 2,
} ElektraEnumDisjointed;

typedef enum
{
	COLORS_RED = 0,
	COLORS_GREEN = 1,
	COLORS_BLUE = 2,
} Colors;

typedef enum
{
	ELEKTRA_ENUM_MYENUM_RED = 0,
	ELEKTRA_ENUM_MYENUM_GREEN = 1,
	ELEKTRA_ENUM_MYENUM_BLUE = 2,
} ElektraEnumMyenum;

// tag declarations for enum of key '/tests/script/gen/elektra/enum/disjointed'
ELEKTRA_TAG_DECLARATIONS (ElektraEnumDisjointed, EnumDisjointed)
// tag declarations for enum of key '/tests/script/gen/elektra/enum/existinggentype'
ELEKTRA_TAG_DECLARATIONS (ExistingColors, EnumExistingColors)
// tag declarations for enum of key '/tests/script/gen/elektra/enum/gentype'
ELEKTRA_TAG_DECLARATIONS (Colors, EnumColors)
// tag declarations for enum of key '/tests/script/gen/elektra/enum/gentype2'
ELEKTRA_TAG_DECLARATIONS (Colors, EnumColors)
// tag declarations for enum of key '/tests/script/gen/elektra/enum/myenum'
ELEKTRA_TAG_DECLARATIONS (ElektraEnumMyenum, EnumMyenum)


/**
 * Tag value for key '/tests/script/gen/elektra/enum/disjointed'.
 */
ELEKTRA_TAG_VALUE (DISJOINTED, "/tests/script/gen/elektra/enum/disjointed", EnumDisjointed)

/**
 * Tag value for key '/tests/script/gen/elektra/enum/existinggentype'.
 */
ELEKTRA_TAG_VALUE (EXISTINGGENTYPE, "/tests/script/gen/elektra/enum/existinggentype", EnumExistingColors)

/**
 * Tag value for key '/tests/script/gen/elektra/enum/gentype'.
 */
ELEKTRA_TAG_VALUE (GENTYPE, "/tests/script/gen/elektra/enum/gentype", EnumColors)

/**
 * Tag value for key '/tests/script/gen/elektra/enum/gentype2'.
 */
ELEKTRA_TAG_VALUE (GENTYPE2, "/tests/script/gen/elektra/enum/gentype2", EnumColors)

/**
 * Tag value for key '/tests/script/gen/elektra/enum/myenum'.
 */
ELEKTRA_TAG_VALUE (MYENUM, "/tests/script/gen/elektra/enum/myenum", EnumMyenum)
;

Elektra * loadConfiguration (ElektraError ** error);

#endif // ENUM_ACTUAL_H
