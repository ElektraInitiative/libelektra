/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

ksNew( 47 ,
	keyNew ("system/groups/guests"
		, KEY_DIR
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/groups/guests/gid"
		, KEY_VALUE, "800"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/groups/guests/members"
		, KEY_VALUE, "jdoe,miriam,ana"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/groups/root"
		, KEY_DIR
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/groups/root/gid"
		, KEY_VALUE, "0"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/groups/root/members"
		, KEY_VALUE, "root"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/groups/sys"
		, KEY_DIR
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/groups/sys/gid"
		, KEY_VALUE, "3"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/groups/sys/members"
		, KEY_VALUE, "root,bin,adm"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/users/jdoe"
		, KEY_DIR
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/users/jdoe/gecos"
		, KEY_VALUE, "John Doe"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/users/jdoe/gid"
		, KEY_VALUE, "800"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/users/jdoe/home"
		, KEY_VALUE, "/root"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/users/jdoe/passwdChangeAfter"
		, KEY_VALUE, "99999"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/jdoe/passwdChangeBefore"
		, KEY_VALUE, "0"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/jdoe/passwdDisableAfter"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/jdoe/passwdDisabledSince"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/jdoe/passwdReserved"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/jdoe/passwdWarnBefore"
		, KEY_VALUE, "7"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/jdoe/password"
		, KEY_VALUE, "x"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0644
	, KEY_END),
	keyNew ("system/users/jdoe/shadowPassword"
		, KEY_VALUE, "an encrypted passwd should appear here"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/jdoe/shell"
		, KEY_VALUE, "/bin/bash"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/users/jdoe/uid"
		, KEY_VALUE, "500"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/users/root"
		, KEY_DIR
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/users/root/gecos"
		, KEY_VALUE, "root"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/users/root/gid"
		, KEY_VALUE, "0"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/users/root/home"
		, KEY_VALUE, "/root"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/users/root/passwdChangeAfter"
		, KEY_VALUE, "99999"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/root/passwdChangeBefore"
		, KEY_VALUE, "0"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/root/passwdDisableAfter"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/root/passwdDisabledSince"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/root/passwdReserved"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/root/passwdWarnBefore"
		, KEY_VALUE, "7"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/root/password"
		, KEY_VALUE, "x"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0644
	, KEY_END),
	keyNew ("system/users/root/shadowPassword"
		, KEY_VALUE, "an encrypted passwd should appear here"
		, KEY_TYPE, KEY_TYPE_STRING
		, KEY_MODE, 0600
	, KEY_END),
	keyNew ("system/users/root/shell"
		, KEY_VALUE, "/bin/bash"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("system/users/root/uid"
		, KEY_VALUE, "0"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),KS_END);
