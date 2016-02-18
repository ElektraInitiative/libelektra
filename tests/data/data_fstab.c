/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

// clang-format off

ksNew( 17 ,
	keyNew ("user/tests/fstab/rootfs"
		, KEY_DIR
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/fstab/rootfs/device"
		, KEY_VALUE, "LABEL=/"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/fstab/rootfs/dumpfreq"
		, KEY_VALUE, "1"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/fstab/rootfs/mpoint"
		, KEY_VALUE, "/"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/fstab/rootfs/options"
		, KEY_VALUE, "defaults"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/fstab/rootfs/passno"
		, KEY_VALUE, "1"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/fstab/rootfs/type"
		, KEY_VALUE, "ext3"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),KS_END);
