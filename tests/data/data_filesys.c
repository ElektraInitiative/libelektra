/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

ksNew( 19 ,
	keyNew ("user/tests/filesys/.HiddenBinaryKey"
		, KEY_VALUE, "BinaryValue"
		, KEY_COMMENT, "Binary key with hidden name"
		, KEY_TYPE, KEY_TYPE_BINARY
	, KEY_END),
	keyNew ("user/tests/filesys/.HiddenDirectoryKey"
		, KEY_DIR
		, KEY_VALUE, "DirectoryValue"
		, KEY_COMMENT, "Directory key with hidden name"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/filesys/.HiddenStringKey"
		, KEY_VALUE, "StringValue"
		, KEY_COMMENT, "String key with hidden name"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/filesys/PerfectBinaryKey"
		, KEY_VALUE, "BinaryValue"
		, KEY_COMMENT, "Binary key with standard name"
		, KEY_TYPE, KEY_TYPE_BINARY
	, KEY_END),
	keyNew ("user/tests/filesys/PerfectDirectoryKey"
		, KEY_DIR
		, KEY_VALUE, "DirectoryValue"
		, KEY_COMMENT, "Directory key with standard name"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/filesys/PerfectStringKey"
		, KEY_VALUE, "StringValue"
		, KEY_COMMENT, "String key with standard name"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/filesys/Ug.ly:Bin@a€ry Key"
		, KEY_COMMENT, "Binary key with ugly name"
		, KEY_TYPE, KEY_TYPE_BINARY
	, KEY_END),
	keyNew ("user/tests/filesys/Ug.ly:Dir@ect€ory Key"
		, KEY_DIR
		, KEY_COMMENT, "Directory with ugly name"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/filesys/Ug.ly:St@ri€n.g Key"
		, KEY_VALUE, "With a string value"
		, KEY_COMMENT, "string key with ugly name"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),KS_END);
