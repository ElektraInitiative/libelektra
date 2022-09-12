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
		ELEKTRA_KEY_VALUE, "BinaryValue",
		ELEKTRA_KEY_COMMENT, "Binary key with standard name",
		ELEKTRA_KEY_BINARY,
		ELEKTRA_KEY_END),
	keyNew ("user:/tests/iconv/PerfectStringKey",
		ELEKTRA_KEY_VALUE, "StringValue",
		ELEKTRA_KEY_COMMENT, "String key with standard name",
		ELEKTRA_KEY_END),
	keyNew ("user:/tests/iconv/UmlautsKey",
		ELEKTRA_KEY_VALUE, "Ein Österreichisches Lüder säße auf dös Schrütt. Äber Über EUR sägen.",
		ELEKTRA_KEY_COMMENT, "String key with standard name",
		ELEKTRA_KEY_END),
	keyNew ("user:/tests/iconv/MoreUmlautsKey",
		ELEKTRA_KEY_VALUE, "Wörtersee im !\"§$%&/()=?´`^° @|<> µ#' .",
		ELEKTRA_KEY_COMMENT, "String key with standard name",
		ELEKTRA_KEY_END),
	keyNew ("user:/tests/iconv/CommentUmlautsKey",
		ELEKTRA_KEY_VALUE, "EasyValue",
		ELEKTRA_KEY_COMMENT, "ÄÖÜßäöü",
		ELEKTRA_KEY_END),
	ELEKTRA_KS_END);
