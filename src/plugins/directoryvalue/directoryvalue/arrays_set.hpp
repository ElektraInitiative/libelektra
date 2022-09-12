// clang-format off
kdb::KeySet
{
	10,
	keyNew (PREFIX "key", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/" DIRECTORY_POSTFIX, ELEKTRA_KEY_END),
	keyNew (PREFIX "key/map", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array", ELEKTRA_KEY_META, "array", "#5", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array/#0", ELEKTRA_KEY_VALUE, ARRAY_VALUE_PREFIX " ", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array/#1", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array/#2", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array/#3/nested", ELEKTRA_KEY_META, "array", "#2", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array/#3/nested/#0", ELEKTRA_KEY_VALUE, ARRAY_VALUE_PREFIX " ", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array/#3/nested/#1", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array/#3/nested/#2", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array/#4/not/an/array", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array/#4/not/an/array/" DIRECTORY_POSTFIX, ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array/#4/not/an/array/#0", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array/#4/not/an/array/key", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array/#5/no/array/#0", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/array/#5/no/array/#1", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/empty/array", ELEKTRA_KEY_META, "array", "#0", ELEKTRA_KEY_END),
	keyNew (PREFIX "key/empty/array/#0", ELEKTRA_KEY_VALUE, ARRAY_VALUE_PREFIX " ", ELEKTRA_KEY_END),
	ELEKTRA_KS_END
}
