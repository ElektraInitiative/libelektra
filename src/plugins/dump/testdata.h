#ifndef ELEKTRA_DUMP_TESTDATA
#define ELEKTRA_DUMP_TESTDATA

#include <elektra/old_kdb.h>

static inline KeySet * testdata_oneValue (void)
{
	return ksNew (1, keyNew ("user:/tests/script", KEY_VALUE, "root", KEY_END), KS_END);
}

static inline KeySet * testdata_twoValue (void)
{
	return ksNew (2, keyNew ("user:/tests/script", KEY_VALUE, "root", KEY_END),
		      keyNew ("user:/tests/script/key", KEY_VALUE, "value", KEY_END), KS_END);
}

static inline KeySet * testdata_threeValue (void)
{
	return ksNew (3, keyNew ("user:/tests/script", KEY_VALUE, "root", KEY_END),
		      keyNew ("user:/tests/script/key", KEY_VALUE, "value", KEY_END),
		      keyNew ("user:/tests/script/key/subkey", KEY_VALUE, "another value", KEY_END), KS_END);
}

static inline KeySet * testdata_againTwoValue (void)
{
	return ksNew (2, keyNew ("user:/tests/script", KEY_VALUE, "root", KEY_END),
		      keyNew ("user:/tests/script/key/subkey", KEY_VALUE, "another value", KEY_END), KS_END);
}

static inline KeySet * testdata_metaData (void)
{
	Key * k1 = keyNew ("user:/tests/script", KEY_VALUE, "root", KEY_META, "meta", "metavalue", KEY_END);
	Key * k2 = keyNew ("user:/tests/script/key", KEY_VALUE, "", KEY_END);

	keyCopyMeta (k2, k1, "meta");

	return ksNew (2, k1, k2, KS_END);
}

static inline KeySet * testdata_demo (void)
{
	Key * k1 = keyNew ("system:/elektra/mountpoints", KEY_BINARY, KEY_SIZE, 0, KEY_META, "comment/#0", "Below are the mount points.",
			   KEY_END);
	Key * k2 = keyNew ("system:/elektra/mountpoints/dbus", KEY_VALUE, "serialized Backend", KEY_END);
	Key * k3 = keyNew ("system:/elektra/mountpoints/dbus/config", KEY_VALUE, "", KEY_META, "comment/#0",
			   "This is a configuration for a backend,\nsee subkeys for more information", KEY_END);
	Key * k4 = keyNew ("system:/elektra/mountpoints/fstab/config", KEY_END);

	keyCopyMeta (k4, k3, "comment/#0");

	return ksNew (4, k1, k2, k3, k4, KS_END);
}

static inline KeySet * testdata_demo_root (void)
{
	Key * k1 = keyNew ("system:/", KEY_BINARY, KEY_SIZE, 0, KEY_META, "comment/#0", "Below are the mount points.", KEY_END);
	Key * k2 = keyNew ("system:/dbus", KEY_VALUE, "serialized Backend", KEY_END);
	Key * k3 = keyNew ("system:/dbus/config", KEY_VALUE, "", KEY_META, "comment/#0",
			   "This is a configuration for a backend,\nsee subkeys for more information", KEY_END);
	Key * k4 = keyNew ("system:/fstab/config", KEY_END);

	keyCopyMeta (k4, k3, "comment/#0");

	return ksNew (4, k1, k2, k3, k4, KS_END);
}

#endif // ELEKTRA_DUMP_TESTDATA
