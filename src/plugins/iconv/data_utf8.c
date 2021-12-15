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
		KEY_VALUE, "Ein Österreichisches Lüder säße auf dös Schrütt. Äber Über EUR sägen.",
		KEY_META, "comment/#0", "String key with standard name",
		KEY_END),
	keyNew ("user:/tests/iconv/MoreUmlautsKey",
		KEY_VALUE, "Wörtersee im !\"§$%&/()=?´`^° @|<> µ#' .",
		KEY_META, "comment/#0", "String key with standard name",
		KEY_END),
	keyNew ("user:/tests/iconv/CommentUmlautsKey",
		KEY_VALUE, "EasyValue",
		KEY_META, "comment/#0", "ÄÖÜßäöü",
		KEY_END),
	KS_END);
