/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

elektraKeysetNew( 19 ,
	elektraKeyNew ("user:/tests/iconv/PerfectBinaryKey",
		ELEKTRA_KEY_VALUE, "BinaryValue",
		ELEKTRA_KEY_COMMENT, "Binary key with standard name",
		ELEKTRA_KEY_BINARY,
		ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/iconv/PerfectStringKey",
		ELEKTRA_KEY_VALUE, "StringValue",
		ELEKTRA_KEY_COMMENT, "String key with standard name",
		ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/iconv/UmlautsKey",
		ELEKTRA_KEY_VALUE, "Ein ""\xD6""sterreichisches L""\xFC""der s""\xE4""\xDF""e auf d""\xF6""s Schr""\xFC""tt. ""\xC4""ber ""\xDC""ber EUR s""\xE4""gen.",
		ELEKTRA_KEY_COMMENT, "String key with standard name",
		ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/iconv/MoreUmlautsKey",
		ELEKTRA_KEY_VALUE, "W""\xF6""rtersee im !\"""\xA7""$%&/()=?""\xB4""`^""\xB0"" @|<> ""\xB5""#' .",
		ELEKTRA_KEY_COMMENT, "String key with standard name",
		ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/iconv/CommentUmlautsKey",
		ELEKTRA_KEY_VALUE, "EasyValue",
		ELEKTRA_KEY_COMMENT, "\xC4""\xD6""\xDC""\xDF""\xE4""\xF6""\xFC",
		ELEKTRA_KEY_END),
	ELEKTRA_KS_END);
