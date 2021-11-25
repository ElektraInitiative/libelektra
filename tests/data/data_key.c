/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

ksNew( 19 ,
	keyNew ("user:/tests/filesys/.HiddenBinaryKey"
		, KEY_VALUE, "BinaryValue"
		, KEY_META, "comment", "Binary key with hidden name"
	, KEY_END),
	keyNew ("user:/tests/filesys/.HiddenDirectoryKey"
		, KEY_VALUE, "DirectoryValue"
		, KEY_META, "comment", "Directory key with hidden name"
	, KEY_END),
	keyNew ("user:/tests/filesys/.HiddenStringKey"
		, KEY_VALUE, "StringValue"
		, KEY_META, "comment", "String key with hidden name"
	, KEY_END),
	keyNew ("user:/tests/filesys/PerfectBinaryKey"
		, KEY_VALUE, "BinaryValue"
		, KEY_META, "comment", "Binary key with standard name"
	, KEY_END),
	keyNew ("user:/tests/filesys/PerfectDirectoryKey"
		, KEY_VALUE, "DirectoryValue"
		, KEY_META, "comment", "Directory key with standard name"
	, KEY_END),
	keyNew ("user:/tests/filesys/PerfectStringKey"
		, KEY_VALUE, "StringValue"
		, KEY_META, "comment", "String key with
standard name"
	, KEY_END),
	keyNew ("user:/tests/filesys/Ug.ly:Bin@a€ry Key"
		, KEY_META, "comment", "Binary key with ugly name"
		, KEY_BINARY,
	, KEY_END),
	keyNew ("user:/tests/filesys/Ug.ly:Dir@ect€ory Key"
		, KEY_META, "comment", "Directory with ugly name"
	, KEY_END),
	keyNew ("user:/tests/filesys/Ug.ly:St@ri€n.g Key"
		, KEY_VALUE, "With a string value"
		, KEY_META, "comment", "string key with ugly name"
	, KEY_END),KS_END);
