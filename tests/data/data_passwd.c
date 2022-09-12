/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

elektraKeysetNew( 47 ,
	elektraKeyNew ("system:/groups/guests", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/groups/guests/gid"
		, ELEKTRA_KEY_VALUE, "800"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/groups/guests/members"
		, ELEKTRA_KEY_VALUE, "jdoe,miriam,ana"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/groups/root", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/groups/root/gid"
		, ELEKTRA_KEY_VALUE, "0"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/groups/root/members"
		, ELEKTRA_KEY_VALUE, "root"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/groups/sys", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/groups/sys/gid"
		, ELEKTRA_KEY_VALUE, "3"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/groups/sys/members"
		, ELEKTRA_KEY_VALUE, "root,bin,adm"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe/gecos"
		, ELEKTRA_KEY_VALUE, "John Doe"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe/gid"
		, ELEKTRA_KEY_VALUE, "800"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe/home"
		, ELEKTRA_KEY_VALUE, "/root"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe/passwdChangeAfter"
		, ELEKTRA_KEY_VALUE, "99999"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe/passwdChangeBefore"
		, ELEKTRA_KEY_VALUE, "0"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe/passwdDisableAfter"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe/passwdDisabledSince"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe/passwdReserved"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe/passwdWarnBefore"
		, ELEKTRA_KEY_VALUE, "7"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe/password"
		, ELEKTRA_KEY_VALUE, "x"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe/shadowPassword"
		, ELEKTRA_KEY_VALUE, "an encrypted passwd should appear here"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe/shell"
		, ELEKTRA_KEY_VALUE, "/bin/bash"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/jdoe/uid"
		, ELEKTRA_KEY_VALUE, "500"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root/gecos"
		, ELEKTRA_KEY_VALUE, "root"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root/gid"
		, ELEKTRA_KEY_VALUE, "0"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root/home"
		, ELEKTRA_KEY_VALUE, "/root"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root/passwdChangeAfter"
		, ELEKTRA_KEY_VALUE, "99999"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root/passwdChangeBefore"
		, ELEKTRA_KEY_VALUE, "0"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root/passwdDisableAfter"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root/passwdDisabledSince"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root/passwdReserved"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root/passwdWarnBefore"
		, ELEKTRA_KEY_VALUE, "7"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root/password"
		, ELEKTRA_KEY_VALUE, "x"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root/shadowPassword"
		, ELEKTRA_KEY_VALUE, "an encrypted passwd should appear here"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root/shell"
		, ELEKTRA_KEY_VALUE, "/bin/bash"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/users/root/uid"
		, ELEKTRA_KEY_VALUE, "0"
	, ELEKTRA_KEY_END),ELEKTRA_KS_END);
