// clang-format off
kdb::KeySet
{
	10,
	keyNew (PREFIX "grandparent", KEY_VALUE, "Grandparent", KEY_END),
	keyNew (PREFIX "grandparent/leaf", KEY_VALUE, "Leaf", KEY_END),
	keyNew (PREFIX "grandparent/parent", KEY_VALUE, "Parent", KEY_END),
	keyNew (PREFIX "grandparent/parent/child", KEY_VALUE, "Child", KEY_END),
	keyNew (PREFIX "mother", KEY_VALUE, "Mother", KEY_END),
	keyNew (PREFIX "mother/daughter", KEY_VALUE, "Daughter", KEY_END),
	keyNew (PREFIX "mother/son", KEY_VALUE, "Son", KEY_END), KS_END
}
