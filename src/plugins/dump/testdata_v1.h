#ifndef ELEKTRA_DUMP_TESTDATA
#define ELEKTRA_DUMP_TESTDATA

#include <kdb.h>

static inline KeySet * testdata_v1_oneValue (void)
{
	return ksNew (1, keyNew ("user/tests/script", KEY_VALUE, "root", KEY_END), KS_END);
}

static inline KeySet * testdata_v1_twoValue (void)
{
	return ksNew (2, keyNew ("user/tests/script", KEY_VALUE, "root", KEY_END),
		      keyNew ("user/tests/script/key", KEY_VALUE, "value", KEY_END), KS_END);
}

static inline KeySet * testdata_v1_threeValue (void)
{
	return ksNew (3, keyNew ("user/tests/script", KEY_VALUE, "root", KEY_END),
		      keyNew ("user/tests/script/key", KEY_VALUE, "value", KEY_END),
		      keyNew ("user/tests/script/key/subkey", KEY_VALUE, "another value", KEY_END), KS_END);
}

static inline KeySet * testdata_v1_againTwoValue (void)
{
	return ksNew (2, keyNew ("user/tests/script", KEY_VALUE, "root", KEY_END),
		      keyNew ("user/tests/script/key/subkey", KEY_VALUE, "another value", KEY_END), KS_END);
}

static inline KeySet * testdata_v1_metaData (void)
{
	Key * k1 = keyNew ("user/tests/script", KEY_VALUE, "root", KEY_META, "meta", "metavalue", KEY_END);
	Key * k2 = keyNew ("user/tests/script/key", KEY_VALUE, "", KEY_END);

	keyCopyMeta (k2, k1, "meta");

	return ksNew (2, k1, k2, KS_END);
}

#endif // ELEKTRA_DUMP_TESTDATA