/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */


/**
 * Enums
 */

typedef enum {
ELEKTRA_ENUM_SERVER_SERVERSCREEN_OFF = 0, 
ELEKTRA_ENUM_SERVER_SERVERSCREEN_ON = 1, 
ELEKTRA_ENUM_SERVER_SERVERSCREEN_BLANK = 2, 
} ElektraEnumServerServerscreen;

/**
 * Default KeySet
 */

#define ELEKTRA_DEFAULTS ksNew (0, \
keyNew ("/floatkey", KEY_VALUE, "1.1", KEY_META, "type", "float", KEY_END), \
keyNew ("/longdoublekey", KEY_VALUE, "1.1", KEY_META, "type", "long_double", KEY_END), \
keyNew ("/octetkey", KEY_VALUE, "1", KEY_META, "type", "octet", KEY_END), \
keyNew ("/shortkey", KEY_VALUE, "1", KEY_META, "type", "short", KEY_END), \
keyNew ("/booleankey", KEY_VALUE, "1", KEY_META, "type", "boolean", KEY_END), \
keyNew ("/longlongkey", KEY_VALUE, "1", KEY_META, "type", "long_long", KEY_END), \
keyNew ("/unsignedshortkey", KEY_VALUE, "1", KEY_META, "type", "unsigned_short", KEY_END), \
keyNew ("/charkey", KEY_VALUE, "c", KEY_META, "type", "char", KEY_END), \
keyNew ("/unsignedlongkey", KEY_VALUE, "1", KEY_META, "type", "unsigned_long", KEY_END), \
keyNew ("/doublekey", KEY_VALUE, "1.1", KEY_META, "type", "double", KEY_END), \
keyNew ("/customtypekey", KEY_VALUE, "A value", KEY_META, "type", "string", KEY_END), \
keyNew ("/server/serverScreen", KEY_VALUE, "0", KEY_META, "type", "enum", KEY_END), \
keyNew ("/unsignedlonglongkey", KEY_VALUE, "1", KEY_META, "type", "unsigned_long_long", KEY_END), \
keyNew ("/longkey", KEY_VALUE, "1", KEY_META, "type", "long", KEY_END), \
keyNew ("/stringkey", KEY_VALUE, "A string", KEY_META, "type", "string", KEY_END), \
KS_END)

/**
 * Elektra Tags
 */

#define ELEKTRA_TAG_FLOATKEY (ElektraFloatTag){"/floatkey"}
#define ELEKTRA_TAG_LONGDOUBLEKEY (ElektraLongDoubleTag){"/longdoublekey"}
#define ELEKTRA_TAG_OCTETKEY (ElektraOctetTag){"/octetkey"}
#define ELEKTRA_TAG_SHORTKEY (ElektraShortTag){"/shortkey"}
#define ELEKTRA_TAG_BOOLEANKEY (ElektraBooleanTag){"/booleankey"}
#define ELEKTRA_TAG_LONGLONGKEY (ElektraLongLongTag){"/longlongkey"}
#define ELEKTRA_TAG_UNSIGNEDSHORTKEY (ElektraUnsignedShortTag){"/unsignedshortkey"}
#define ELEKTRA_TAG_CHARKEY (ElektraCharTag){"/charkey"}
#define ELEKTRA_TAG_UNSIGNEDLONGKEY (ElektraUnsignedLongTag){"/unsignedlongkey"}
#define ELEKTRA_TAG_DOUBLEKEY (ElektraDoubleTag){"/doublekey"}
#define ELEKTRA_TAG_CUSTOMTYPEKEY (ElektraStringTag){"/customtypekey"}
#define ELEKTRA_TAG_SERVER_SERVERSCREEN (ElektraEnumServerServerscreenTag){"/server/serverScreen"}
#define ELEKTRA_TAG_UNSIGNEDLONGLONGKEY (ElektraUnsignedLongLongTag){"/unsignedlonglongkey"}
#define ELEKTRA_TAG_LONGKEY (ElektraLongTag){"/longkey"}
#define ELEKTRA_TAG_STRINGKEY (ElektraStringTag){"/stringkey"}

/**
 * Types
 */

ELEKTRA_DECLARATIONS(ElektraEnumServerServerscreen, EnumServerServerscreen)

#undef ELEKTRA_TAG_NAMES_GEN
#define ELEKTRA_TAG_NAMES_GEN(X) \
X(EnumServerServerscreen) \

#include <elektra_generic.h>
