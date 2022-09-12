/**
 * @file
 *
 * @brief Source for quickdump plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

static ElektraKeyset * test_quickdump_expected (void)
{
	ElektraKey * k1 = elektraKeyNew ("dir:/tests/bench/__112", ELEKTRA_KEY_VALUE, "gQHLlzB36CqIFlf", ELEKTRA_KEY_META, "meta/_35", "O6xNya6srhNhMFC", ELEKTRA_KEY_META,
			   "meta/_39", "ublVuvyh1DgfOKU", ELEKTRA_KEY_META, "meta/_58", "5Nyde2MHJODCBAT", ELEKTRA_KEY_META, "meta/_79", "ZK2xlaRMfobquxp",
			   ELEKTRA_KEY_META, "meta/_90", "0kCcc1pK7hOgY3F", ELEKTRA_KEY_END);
	ElektraKey * k8 = elektraKeyNew ("dir:/tests/bench/__911", ELEKTRA_KEY_VALUE, "PgNbwPxfeqD30pH", ELEKTRA_KEY_END);
	elektraKeyCopyMeta (k8, k1, "meta/_35");
	return elektraKeysetNew (8, k1, elektraKeyNew ("dir:/tests/bench/__114", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_META, "binary", "", ELEKTRA_KEY_END),
		      elektraKeyNew ("dir:/tests/bench/__333", ELEKTRA_KEY_VALUE, "SxTUAjM6OIpUV6s", ELEKTRA_KEY_END),
		      elektraKeyNew ("dir:/tests/bench/__506", ELEKTRA_KEY_VALUE, "cGqEvmXxUayNCf8", ELEKTRA_KEY_END),
		      elektraKeyNew ("dir:/tests/bench/__859", ELEKTRA_KEY_VALUE, "rOI5aVFGlnjPLYJ", ELEKTRA_KEY_END),
		      elektraKeyNew ("dir:/tests/bench/__863", ELEKTRA_KEY_VALUE, "8IBjbd5pzYBehrs", ELEKTRA_KEY_END),
		      elektraKeyNew ("dir:/tests/bench/__868", ELEKTRA_KEY_VALUE, "UVM0OPTf68yNXij", ELEKTRA_KEY_END), k8, ELEKTRA_KS_END);
}

static unsigned char test_quickdump_parentKeyValue_data[] = { 0x45, 0x4b, 0x44, 0x42, 0x00, 0x00, 0x00, 0x03, 0x01,
							      0x73, 0x0b, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x00 };

static size_t test_quickdump_parentKeyValue_dataSize = 17;

static unsigned char test_quickdump_noParent_data[] = {
	0x45, 0x4b, 0x44, 0x42, 0x00, 0x00, 0x00, 0x03, 0x01, 0x73, 0x0b, 0x76, 0x61, 0x6c,
	0x75, 0x65, 0x00, 0x03, 0x61, 0x73, 0x0d, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x31, 0x00
};

static size_t test_quickdump_noParent_dataSize = 28;
