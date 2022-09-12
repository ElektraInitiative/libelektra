#ifndef ELEKTRA_DUMP_TESTDATA
#define ELEKTRA_DUMP_TESTDATA

#include <kdb.h>

static inline ElektraKeyset * testdata_oneValue (void)
{
	return ksNew (1, keyNew ("user:/tests/script", ELEKTRA_KEY_VALUE, "root", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

static inline ElektraKeyset * testdata_twoValue (void)
{
	return ksNew (2, keyNew ("user:/tests/script", ELEKTRA_KEY_VALUE, "root", ELEKTRA_KEY_END),
		      keyNew ("user:/tests/script/key", ELEKTRA_KEY_VALUE, "value", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

static inline ElektraKeyset * testdata_threeValue (void)
{
	return ksNew (3, keyNew ("user:/tests/script", ELEKTRA_KEY_VALUE, "root", ELEKTRA_KEY_END),
		      keyNew ("user:/tests/script/key", ELEKTRA_KEY_VALUE, "value", ELEKTRA_KEY_END),
		      keyNew ("user:/tests/script/key/subkey", ELEKTRA_KEY_VALUE, "another value", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

static inline ElektraKeyset * testdata_againTwoValue (void)
{
	return ksNew (2, keyNew ("user:/tests/script", ELEKTRA_KEY_VALUE, "root", ELEKTRA_KEY_END),
		      keyNew ("user:/tests/script/key/subkey", ELEKTRA_KEY_VALUE, "another value", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

static inline ElektraKeyset * testdata_metaData (void)
{
	ElektraKey * k1 = keyNew ("user:/tests/script", ELEKTRA_KEY_VALUE, "root", ELEKTRA_KEY_META, "meta", "metavalue", ELEKTRA_KEY_END);
	ElektraKey * k2 = keyNew ("user:/tests/script/key", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);

	keyCopyMeta (k2, k1, "meta");

	return ksNew (2, k1, k2, ELEKTRA_KS_END);
}

static inline ElektraKeyset * testdata_demo (void)
{
	ElektraKey * k1 = keyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, 0, ELEKTRA_KEY_META, "comment", "Below are the mount points.",
			   ELEKTRA_KEY_END);
	ElektraKey * k2 = keyNew ("system:/elektra/mountpoints/dbus", ELEKTRA_KEY_VALUE, "serialized Backend", ELEKTRA_KEY_END);
	ElektraKey * k3 = keyNew ("system:/elektra/mountpoints/dbus/config", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_META, "comment",
			   "This is a configuration for a backend,\nsee subkeys for more information", ELEKTRA_KEY_END);
	ElektraKey * k4 = keyNew ("system:/elektra/mountpoints/fstab/config", ELEKTRA_KEY_END);

	keyCopyMeta (k4, k3, "comment");

	return ksNew (4, k1, k2, k3, k4, ELEKTRA_KS_END);
}

static inline ElektraKeyset * testdata_demo_root (void)
{
	ElektraKey * k1 = keyNew ("system:/", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, 0, ELEKTRA_KEY_META, "comment", "Below are the mount points.", ELEKTRA_KEY_END);
	ElektraKey * k2 = keyNew ("system:/dbus", ELEKTRA_KEY_VALUE, "serialized Backend", ELEKTRA_KEY_END);
	ElektraKey * k3 = keyNew ("system:/dbus/config", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_META, "comment",
			   "This is a configuration for a backend,\nsee subkeys for more information", ELEKTRA_KEY_END);
	ElektraKey * k4 = keyNew ("system:/fstab/config", ELEKTRA_KEY_END);

	keyCopyMeta (k4, k3, "comment");

	return ksNew (4, k1, k2, k3, k4, ELEKTRA_KS_END);
}

#endif // ELEKTRA_DUMP_TESTDATA
