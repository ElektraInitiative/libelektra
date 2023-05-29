// clang-format off
kdb::KeySet
{
	10,
	ckdb::keyNew (PREFIX "longish key", KEY_VALUE, "10-9-8-7 everybody’s coming to burn this city", KEY_END),
	ckdb::keyNew (PREFIX "double quoted", KEY_VALUE, "single quoted", KEY_END),
	ckdb::keyNew (PREFIX "forward\\/slash", KEY_VALUE, "value containing spaces", KEY_END),
	ckdb::keyNew (PREFIX "hello", KEY_VALUE, "world", KEY_END),
	ckdb::keyNew (PREFIX "different", KEY_VALUE, "nice", KEY_META, "meta:/elektra/export/variable", "my_variable", KEY_END),
	ckdb::keyNew (PREFIX "level1", KEY_VALUE, "l1", KEY_END),
	ckdb::keyNew (PREFIX "level1/level2-1", KEY_VALUE, "l2-1", KEY_END),
	ckdb::keyNew (PREFIX "level1/level2-2", KEY_VALUE, "l2-2", KEY_END),
	ckdb::keyNew (PREFIX "withmeta", KEY_VALUE, "cool", KEY_META, "meta:/my/order", "123", KEY_END),
	ckdb::keyNew (PREFIX "thisisdeleted", KEY_VALUE, "cool", KEY_META, "meta:/elektra/deleted", "1", KEY_END),
	ckdb::keyNew ("system:/mountpoint/user:\\/test", KEY_VALUE, "test", KEY_END),
	ckdb::keyNew ("system:/elektra/mountpoints/user:\\/test", KEY_VALUE, "test", KEY_END),
	KS_END
}
