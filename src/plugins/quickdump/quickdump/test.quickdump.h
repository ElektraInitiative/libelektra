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
	return ksNew (8,
		      keyNew ("dir/tests/bench/__112", KEY_VALUE, "gQHLlzB36CqIFlf", KEY_META, "meta/_35", "O6xNya6srhNhMFC", KEY_META,
			      "meta/_39", "ublVuvyh1DgfOKU", KEY_META, "meta/_58", "5Nyde2MHJODCBAT", KEY_META, "meta/_79",
			      "ZK2xlaRMfobquxp", KEY_META, "meta/_90", "0kCcc1pK7hOgY3F", KEY_END),
		      keyNew ("dir/tests/bench/__114", KEY_META, "binary", "", KEY_END),
		      keyNew ("dir/tests/bench/__333", KEY_VALUE, "SxTUAjM6OIpUV6s", KEY_END),
		      keyNew ("dir/tests/bench/__506", KEY_VALUE, "cGqEvmXxUayNCf8", KEY_END),
		      keyNew ("dir/tests/bench/__859", KEY_VALUE, "rOI5aVFGlnjPLYJ", KEY_END),
		      keyNew ("dir/tests/bench/__863", KEY_VALUE, "8IBjbd5pzYBehrs", KEY_END),
		      keyNew ("dir/tests/bench/__868", KEY_VALUE, "UVM0OPTf68yNXij", KEY_END),
		      keyNew ("dir/tests/bench/__911", KEY_VALUE, "PgNbwPxfeqD30pH", KEY_END), KS_END);
}
