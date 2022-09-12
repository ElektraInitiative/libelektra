// clang-format off
kdb::KeySet
{
	10,
	keyNew (PREFIX "grandparent", ELEKTRA_KEY_END),
	keyNew (PREFIX "grandparent/" DIRECTORY_POSTFIX, ELEKTRA_KEY_VALUE, "Grandparent", ELEKTRA_KEY_END),
	keyNew (PREFIX "grandparent/leaf", ELEKTRA_KEY_VALUE, "Leaf", ELEKTRA_KEY_END),
	keyNew (PREFIX "grandparent/parent", ELEKTRA_KEY_END),
	keyNew (PREFIX "grandparent/parent/" DIRECTORY_POSTFIX, ELEKTRA_KEY_VALUE, "Parent", ELEKTRA_KEY_END),
	keyNew (PREFIX "grandparent/parent/child", ELEKTRA_KEY_VALUE, "Child", ELEKTRA_KEY_END),
	keyNew (PREFIX "mother", ELEKTRA_KEY_END),
	keyNew (PREFIX "mother/" DIRECTORY_POSTFIX, ELEKTRA_KEY_VALUE, "Mother", ELEKTRA_KEY_END),
	keyNew (PREFIX "mother/daughter", ELEKTRA_KEY_VALUE, "Daughter", ELEKTRA_KEY_END),
	keyNew (PREFIX "mother/son", ELEKTRA_KEY_VALUE, "Son", ELEKTRA_KEY_END),
	ELEKTRA_KS_END
}
