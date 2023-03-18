/**
 * @file
 *
 * @brief Source for rgbcolor plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "rgbcolor.h"
#include <elektra/kdb/errors.h>
#include <elektra/type/types.h>
#include <internal/utility/old_helper.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct
{
	const char * name;
	const kdb_unsigned_long_t value;
} NamedColor;

static const NamedColor NamedColors[] = { { "aliceblue", 0xf0f8ffff },
					  { "antiquewhite", 0xfaebd7ff },
					  { "aqua", 0x00ffffff },
					  { "aquamarine", 0x7fffd4ff },
					  { "azure", 0xf0ffffff },
					  { "beige", 0xf5f5dcff },
					  { "bisque", 0xffe4c4ff },
					  { "black", 0x000000ff },
					  { "blanchedalmond", 0xffebcdff },
					  { "blue", 0x0000ffff },
					  { "blueviolet", 0x8a2be2ff },
					  { "brown", 0xa52a2aff },
					  { "burlywood", 0xdeb887ff },
					  { "cadetblue", 0x5f9ea0ff },
					  { "chartreuse", 0x7fff00ff },
					  { "chocolate", 0xd2691eff },
					  { "coral", 0xff7f50ff },
					  { "cornflowerblue", 0x6495edff },
					  { "cornsilk", 0xfff8dcff },
					  { "crimson", 0xdc143cff },
					  { "cyan", 0x00ffffff },
					  { "darkblue", 0x00008bff },
					  { "darkcyan", 0x008b8bff },
					  { "darkgoldenrod", 0xb8860bff },
					  { "darkgray", 0xa9a9a9ff },
					  { "darkgreen", 0x006400ff },
					  { "darkgrey", 0xa9a9a9ff },
					  { "darkkhaki", 0xbdb76bff },
					  { "darkmagenta", 0x8b008bff },
					  { "darkolivegreen", 0x556b2fff },
					  { "darkorange", 0xff8c00ff },
					  { "darkorchid", 0x9932ccff },
					  { "darkred", 0x8b0000ff },
					  { "darksalmon", 0xe9967aff },
					  { "darkseagreen", 0x8fbc8fff },
					  { "darkslateblue", 0x483d8bff },
					  { "darkslategray", 0x2f4f4fff },
					  { "darkslategrey", 0x2f4f4fff },
					  { "darkturquoise", 0x00ced1ff },
					  { "darkviolet", 0x9400d3ff },
					  { "deeppink", 0xff1493ff },
					  { "deepskyblue", 0x00bfffff },
					  { "dimgray", 0x696969ff },
					  { "dimgrey", 0x696969ff },
					  { "dodgerblue", 0x1e90ffff },
					  { "firebrick", 0xb22222ff },
					  { "floralwhite", 0xfffaf0ff },
					  { "forestgreen", 0x228b22ff },
					  { "fuchsia", 0xff00ffff },
					  { "gainsboro", 0xdcdcdcff },
					  { "ghostwhite", 0xf8f8ffff },
					  { "gold", 0xffd700ff },
					  { "goldenrod", 0xdaa520ff },
					  { "gray", 0x808080ff },
					  { "green", 0x008000ff },
					  { "greenyellow", 0xadff2fff },
					  { "grey", 0x808080ff },
					  { "honeydew", 0xf0fff0ff },
					  { "hotpink", 0xff69b4ff },
					  { "indianred", 0xcd5c5cff },
					  { "indigo", 0x4b0082ff },
					  { "ivory", 0xfffff0ff },
					  { "khaki", 0xf0e68cff },
					  { "lavender", 0xe6e6faff },
					  { "lavenderblush", 0xfff0f5ff },
					  { "lawngreen", 0x7cfc00ff },
					  { "lemonchiffon", 0xfffacdff },
					  { "lightblue", 0xadd8e6ff },
					  { "lightcoral", 0xf08080ff },
					  { "lightcyan", 0xe0ffffff },
					  { "lightgoldenrodyellow", 0xfafad2ff },
					  { "lightgray", 0xd3d3d3ff },
					  { "lightgreen", 0x90ee90ff },
					  { "lightgrey", 0xd3d3d3ff },
					  { "lightpink", 0xffb6c1ff },
					  { "lightsalmon", 0xffa07aff },
					  { "lightseagreen", 0x20b2aaff },
					  { "lightskyblue", 0x87cefaff },
					  { "lightslategray", 0x778899ff },
					  { "lightslategrey", 0x778899ff },
					  { "lightsteelblue", 0xb0c4deff },
					  { "lightyellow", 0xffffe0ff },
					  { "lime", 0x00ff00ff },
					  { "limegreen", 0x32cd32ff },
					  { "linen", 0xfaf0e6ff },
					  { "magenta", 0xff00ffff },
					  { "maroon", 0x800000ff },
					  { "mediumaquamarine", 0x66cdaaff },
					  { "mediumblue", 0x0000cdff },
					  { "mediumorchid", 0xba55d3ff },
					  { "mediumpurple", 0x9370dbff },
					  { "mediumseagreen", 0x3cb371ff },
					  { "mediumslateblue", 0x7b68eeff },
					  { "mediumspringgreen", 0x00fa9aff },
					  { "mediumturquoise", 0x48d1ccff },
					  { "mediumvioletred", 0xc71585ff },
					  { "midnightblue", 0x191970ff },
					  { "mintcream", 0xf5fffaff },
					  { "mistyrose", 0xffe4e1ff },
					  { "moccasin", 0xffe4b5ff },
					  { "navajowhite", 0xffdeadff },
					  { "navy", 0x000080ff },
					  { "oldlace", 0xfdf5e6ff },
					  { "olive", 0x808000ff },
					  { "olivedrab", 0x6b8e23ff },
					  { "orange", 0xffa500ff },
					  { "orangered", 0xff4500ff },
					  { "orchid", 0xda70d6ff },
					  { "palegoldenrod", 0xeee8aaff },
					  { "palegreen", 0x98fb98ff },
					  { "paleturquoise", 0xafeeeeff },
					  { "palevioletred", 0xdb7093ff },
					  { "papayawhip", 0xffefd5ff },
					  { "peachpuff", 0xffdab9ff },
					  { "peru", 0xcd853fff },
					  { "pink", 0xffc0cbff },
					  { "plum", 0xdda0ddff },
					  { "powderblue", 0xb0e0e6ff },
					  { "purple", 0x800080ff },
					  { "red", 0xff0000ff },
					  { "rosybrown", 0xbc8f8fff },
					  { "royalblue", 0x4169e1ff },
					  { "saddlebrown", 0x8b4513ff },
					  { "salmon", 0xfa8072ff },
					  { "sandybrown", 0xf4a460ff },
					  { "seagreen", 0x2e8b57ff },
					  { "seashell", 0xfff5eeff },
					  { "sienna", 0xa0522dff },
					  { "silver", 0xc0c0c0ff },
					  { "skyblue", 0x87ceebff },
					  { "slateblue", 0x6a5acdff },
					  { "slategray", 0x708090ff },
					  { "slategrey", 0x708090ff },
					  { "snow", 0xfffafaff },
					  { "springgreen", 0x00ff7fff },
					  { "steelblue", 0x4682b4ff },
					  { "tan", 0xd2b48cff },
					  { "teal", 0x008080ff },
					  { "thistle", 0xd8bfd8ff },
					  { "tomato", 0xff6347ff },
					  { "turquoise", 0x40e0d0ff },
					  { "violet", 0xee82eeff },
					  { "wheat", 0xf5deb3ff },
					  { "white", 0xffffffff },
					  { "whitesmoke", 0xf5f5f5ff },
					  { "yellow", 0xffff00ff },
					  { "yellowgreen", 0x9acd32ff } };

typedef enum
{
	COLOR_INVALID,
	HEX_THREE,
	HEX_FOUR,
	HEX_SIX,
	HEX_EIGHT,
	NAMED_COLOR
} ColorVariant;

/**
 * Internal function used for bsearch
 */
static int compareNamedColors (const void * c1, const void * c2)
{
	NamedColor * nc1 = (NamedColor *) c1;
	NamedColor * nc2 = (NamedColor *) c2;
	return strcmp (nc1->name, nc2->name);
}

/**
 * Finds a named color with the given name
 *
 * @param colorName the color name to search for
 * @retval NULL if no color with the given name exists
 * @retval The NamedColor with the given name
 */
static const NamedColor * findNamedColor (const char * colorName)
{
	const NamedColor key = { .name = colorName };
	void * found;
	found = bsearch (&key, &NamedColors[0], 147, sizeof (NamedColor), compareNamedColors);
	if (found == NULL) return NULL;
	return (NamedColor *) found;
}

/**
 * Checks wheter the given key contains a valid hex-formatted string or a named color
 *
 * @param key the key whose value should be checked
 * @param parentKey the parent key
 */
static ColorVariant is_valid_key (Key * key, Key * parentKey)
{
	const Key * meta = keyGetMeta (key, "check/rgbcolor");
	if (!meta) return 1;
	const char * value = keyString (key);

	const NamedColor * namedColor = findNamedColor (value);

	if (namedColor != NULL)
	{
		return NAMED_COLOR;
	}

	if (*value == '#')
	{
		size_t len = strlen (value + 1);
		size_t lengthValidChars = strspn (value + 1, "0123456789abcdefABCDEF");
		if (len == lengthValidChars)
		{
			switch (len)
			{
			case 3:
				return HEX_THREE;
			case 4:
				return HEX_FOUR;
			case 6:
				return HEX_SIX;
			case 8:
				return HEX_EIGHT;
			}
		}
	}

	ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
						 "Key '%s' with value '%s' is neither a valid hex formatted color nor a named color",
						 keyName (key), keyString (key));
	return COLOR_INVALID;
}

/**
 * Sets the keys value to the given integer formatted as a string
 *
 * @param key the key to set the value for
 * @param c the integer to set as the new value
 */
static void elektraColorSetInteger (Key * key, kdb_unsigned_long_t c)
{
	char colorStr[11];
	snprintf (colorStr, 11, "%u", c);

	ELEKTRA_LOG_DEBUG ("Set %s to integer %s", keyName (key), colorStr);
	keySetString (key, colorStr);
}

/**
 * Expands all color variants to the full RRGGBBAA variant.
 *
 * @param str the string to expand.
 * @param colVar the ColorVariant of str
 * @param expandedStr pre-allocated memory of length 10 to store the expanded string.
 */
static void elektraColorExpand (const char * str, ColorVariant colVar, char * expandedStr)
{
	if (colVar == HEX_THREE || colVar == HEX_FOUR)
	{
		expandedStr[0] = '#';

		for (size_t i = 1; i < strlen (str); i++)
		{
			expandedStr[2 * i - 1] = str[i];
			expandedStr[2 * i] = str[i];
		}
	}
	else
	{
		strcpy (expandedStr, str);
	}

	if (colVar == HEX_THREE || colVar == HEX_SIX)
	{
		expandedStr[7] = 'f';
		expandedStr[8] = 'f';
	}

	expandedStr[9] = '\0';
}

/**
 * Normalizes a rgb, rgba, rrggbb hexstring to rrggbbaa
 *
 * @param key the key to normalize
 * @param colVar the ColorVariant of the string in key
 */
static void elektraColorNormalizeHexString (Key * key, ColorVariant colVar)
{
	const char * str = keyString (key);
	char * origvalue = elektraStrDup (str);

	kdb_unsigned_long_t color;
	if (colVar == NAMED_COLOR)
	{
		const NamedColor * namedColor = findNamedColor (str);
		color = namedColor->value;
	}
	else if (colVar != HEX_EIGHT)
	{
		char expandedStr[10];
		elektraColorExpand (str, colVar, expandedStr);
		ELEKTRA_LOG_DEBUG ("Expanded %s to %s", str, expandedStr);
		color = ELEKTRA_UNSIGNED_LONG_LONG_S (expandedStr + 1, NULL, 16);
	}
	else
	{
		color = ELEKTRA_UNSIGNED_LONG_LONG_S (str + 1, NULL, 16);
	}
	elektraColorSetInteger (key, color);

	ELEKTRA_LOG_DEBUG ("Setting origvalue of %s to %s", keyName (key), origvalue);
	keySetMeta (key, "origvalue", origvalue);
	elektraFree (origvalue);
}

/**
 * Restores the original value
 *
 * @param key the key to restore the value for
 */
static void elektraColorRestore (Key * key)
{
	const Key * orig = keyGetMeta (key, "origvalue");
	if (orig != NULL)
	{
		ELEKTRA_LOG_DEBUG ("Restore %s to %s", keyName (key), keyString (orig));
		keySetString (key, keyString (orig));
	}
}

int elektraRgbcolorGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/rgbcolor"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/rgbcolor", KEY_VALUE, "rgbcolor plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/rgbcolor/exports", KEY_END),
			       keyNew ("system:/elektra/modules/rgbcolor/exports/get", KEY_FUNC, elektraRgbcolorGet, KEY_END),
			       keyNew ("system:/elektra/modules/rgbcolor/exports/set", KEY_FUNC, elektraRgbcolorSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/rgbcolor/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
		const Key * meta = keyGetMeta (cur, "check/rgbcolor");
		if (!meta) continue;
		ColorVariant colVar = is_valid_key (cur, parentKey);
		elektraColorNormalizeHexString (cur, colVar);
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraRgbcolorSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
		const Key * meta = keyGetMeta (cur, "check/rgbcolor");
		if (!meta) continue;

		elektraColorRestore (cur);

		ColorVariant colVar = is_valid_key (cur, parentKey);
		if (colVar == COLOR_INVALID) return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("rgbcolor",
		ELEKTRA_PLUGIN_GET,	&elektraRgbcolorGet,
		ELEKTRA_PLUGIN_SET,	&elektraRgbcolorSet,
		ELEKTRA_PLUGIN_END);
}
