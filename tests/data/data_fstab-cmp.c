/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

elektraKeysetNew( 17 ,
	elektraKeyNew ("user:/tests/fstab/rootfs", ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/fstab/rootfs/device"
		, ELEKTRA_KEY_VALUE, "LABEL=/"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/fstab/rootfs/dumpfreq"
		, ELEKTRA_KEY_VALUE, "1"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/fstab/rootfs/mpoint"
		, ELEKTRA_KEY_VALUE, "/"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/fstab/rootfs/options"
		, ELEKTRA_KEY_VALUE, "defaults"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/fstab/rootfs/passno"
		, ELEKTRA_KEY_VALUE, "1"
	, ELEKTRA_KEY_END),
	elektraKeyNew ("user:/tests/fstab/rootfs/type"
		, ELEKTRA_KEY_VALUE, "ext3"
	, ELEKTRA_KEY_END),ELEKTRA_KS_END);
