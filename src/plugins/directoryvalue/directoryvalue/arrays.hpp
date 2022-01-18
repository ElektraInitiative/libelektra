// clang-format off
kdb::KeySet
{
	10,
	keyNew (PREFIX "key", KEY_END),
	keyNew (PREFIX "key/map", KEY_END),
	keyNew (PREFIX "key/array", KEY_META, "array", "#4", KEY_END),
	keyNew (PREFIX "key/array/#0", KEY_END),
	keyNew (PREFIX "key/array/#1", KEY_END),
	keyNew (PREFIX "key/array/#2/nested", KEY_META, "array", "#1", KEY_END),
	keyNew (PREFIX "key/array/#2/nested/#0", KEY_END),
	keyNew (PREFIX "key/array/#2/nested/#1", KEY_END),
	keyNew (PREFIX "key/array/#3/not/an/array", KEY_END),
	keyNew (PREFIX "key/array/#3/not/an/array/#0", KEY_END),
	keyNew (PREFIX "key/array/#3/not/an/array/key", KEY_END),
	keyNew (PREFIX "key/array/#4/no/array/#0", KEY_END),
	keyNew (PREFIX "key/array/#4/no/array/#1", KEY_END),
	keyNew (PREFIX "key/empty/array", KEY_META, "array", "", KEY_END),
	KS_END
}
