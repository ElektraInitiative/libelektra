/**
 * @file
 *
 * @brief Source for quickdump plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

static KeySet * test_quickdump_expected (void)
{
	Key * k1 = keyNew ("dir:/tests/bench/__112", KEY_VALUE, "gQHLlzB36CqIFlf", KEY_META, "meta/_35", "O6xNya6srhNhMFC", KEY_META,
			   "meta/_39", "ublVuvyh1DgfOKU", KEY_META, "meta/_58", "5Nyde2MHJODCBAT", KEY_META, "meta/_79", "ZK2xlaRMfobquxp",
			   KEY_META, "meta/_90", "0kCcc1pK7hOgY3F", KEY_END);
	Key * k8 = keyNew ("dir:/tests/bench/__911", KEY_VALUE, "PgNbwPxfeqD30pH", KEY_END);
	keyCopyMeta (k8, k1, "meta/_35");
	return ksNew (8, k1, keyNew ("dir:/tests/bench/__114", KEY_BINARY, KEY_META, "binary", "", KEY_END),
		      keyNew ("dir:/tests/bench/__333", KEY_VALUE, "SxTUAjM6OIpUV6s", KEY_END),
		      keyNew ("dir:/tests/bench/__506", KEY_VALUE, "cGqEvmXxUayNCf8", KEY_END),
		      keyNew ("dir:/tests/bench/__859", KEY_VALUE, "rOI5aVFGlnjPLYJ", KEY_END),
		      keyNew ("dir:/tests/bench/__863", KEY_VALUE, "8IBjbd5pzYBehrs", KEY_END),
		      keyNew ("dir:/tests/bench/__868", KEY_VALUE, "UVM0OPTf68yNXij", KEY_END), k8, KS_END);
}

static unsigned char test_quickdump_parentKeyValue_data[] = { 0x45, 0x4b, 0x44, 0x42, 0x00, 0x00, 0x00, 0x03, 0x01,
							      0x73, 0x0b, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x00 };

static size_t test_quickdump_parentKeyValue_dataSize = 17;

static unsigned char test_quickdump_noParent_data[] = {
	0x45, 0x4b, 0x44, 0x42, 0x00, 0x00, 0x00, 0x03, 0x01, 0x73, 0x0b, 0x76, 0x61, 0x6c,
	0x75, 0x65, 0x00, 0x03, 0x61, 0x73, 0x0d, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x31, 0x00
};

static size_t test_quickdump_noParent_dataSize = 28;
