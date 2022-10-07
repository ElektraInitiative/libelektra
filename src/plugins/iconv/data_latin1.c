/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

ksNew( 19 ,
	keyNew ("user:/tests/iconv/PerfectBinaryKey",
		KEY_VALUE, "BinaryValue",
		KEY_META, "comment/#0", "Binary key with standard name",
		KEY_BINARY,
		KEY_END),
	keyNew ("user:/tests/iconv/PerfectStringKey",
		KEY_VALUE, "StringValue",
		KEY_META, "comment/#0", "String key with standard name",
		KEY_END),
	keyNew ("user:/tests/iconv/UmlautsKey",
		KEY_VALUE, "Ein ""\xD6""sterreichisches L""\xFC""der s""\xE4""\xDF""e auf d""\xF6""s Schr""\xFC""tt. ""\xC4""ber ""\xDC""ber EUR s""\xE4""gen.",
		KEY_META, "comment/#0", "String key with standard name",
		KEY_END),
	keyNew ("user:/tests/iconv/MoreUmlautsKey",
		KEY_VALUE, "W""\xF6""rtersee im !\"""\xA7""$%&/()=?""\xB4""`^""\xB0"" @|<> ""\xB5""#' .",
		KEY_META, "comment/#0", "String key with standard name",
		KEY_END),
	keyNew ("user:/tests/iconv/CommentUmlautsKey",
		KEY_VALUE, "EasyValue",
		KEY_META, "comment/#0", "\xC4""\xD6""\xDC""\xDF""\xE4""\xF6""\xFC",
		KEY_END),
	KS_END);
