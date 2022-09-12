/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

elektraKeysetNew( 19 ,
	elektraKeyNew ("user:/tests/filesys/.HiddenBinaryKey"
		, ELEKTRA_KEY_VALUE, "BinaryValue"
		, ELEKTRA_KEY_COMMENT, "Binary key with hidden name"
		, ELEKTRA_KEY_BINARY,
		, ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/filesys/.HiddenDirectoryKey"
		, ELEKTRA_KEY_VALUE, "DirectoryValue"
		, ELEKTRA_KEY_COMMENT, "Directory key with hidden name"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/filesys/.HiddenStringKey"
		, ELEKTRA_KEY_VALUE, "StringValue"
		, ELEKTRA_KEY_COMMENT, "String key with hidden name"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/filesys/PerfectBinaryKey"
		, ELEKTRA_KEY_VALUE, "BinaryValue"
		, ELEKTRA_KEY_COMMENT, "Binary key with standard name"
		, ELEKTRA_KEY_BINARY,
		, ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/filesys/PerfectDirectoryKey"
		, ELEKTRA_KEY_VALUE, "DirectoryValue"
		, ELEKTRA_KEY_COMMENT, "Directory key with standard name"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/filesys/PerfectStringKey"
		, ELEKTRA_KEY_VALUE, "StringValue"
		, ELEKTRA_KEY_COMMENT, "String key with standard name"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/filesys/Ug.ly:Bin@a€ry Key"
		, ELEKTRA_KEY_COMMENT, "Binary key with ugly name"
		, ELEKTRA_KEY_BINARY,
		, ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/filesys/Ug.ly:Dir@ect€ory Key"
		, ELEKTRA_KEY_COMMENT, "Directory with ugly name"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/filesys/Ug.ly:St@ri€n.g Key"
		, ELEKTRA_KEY_VALUE, "With a string value"
		, ELEKTRA_KEY_COMMENT, "string key with ugly name"
	, ELEKTRA_KEY_END),ELEKTRA_KS_END);
