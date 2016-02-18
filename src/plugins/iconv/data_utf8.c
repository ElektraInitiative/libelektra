/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

// clang-format off

ksNew( 19 ,
	keyNew ("user/tests/iconv/PerfectBinaryKey",
		KEY_VALUE, "BinaryValue",
		KEY_COMMENT, "Binary key with standard name",
		KEY_BINARY,
		KEY_END),
	keyNew ("user/tests/iconv/PerfectStringKey",
		KEY_VALUE, "StringValue",
		KEY_COMMENT, "String key with standard name",
		KEY_END),
	keyNew ("user/tests/iconv/UmlautsKey",
		KEY_VALUE, "Ein Österreichisches Lüder säße auf dös Schrütt. Äber Über EUR sägen.",
		KEY_COMMENT, "String key with standard name",
		KEY_END),
	keyNew ("user/tests/iconv/MoreUmlautsKey",
		KEY_VALUE, "Wörtersee im !\"§$%&/()=?´`^° @|<> µ#' .",
		KEY_COMMENT, "String key with standard name",
		KEY_END),
	keyNew ("user/tests/iconv/CommentUmlautsKey",
		KEY_VALUE, "EasyValue",
		KEY_COMMENT, "ÄÖÜßäöü",
		KEY_END),
	KS_END);
