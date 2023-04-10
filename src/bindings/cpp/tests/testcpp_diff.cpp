/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests.hpp>

#include <elektradiff.hpp>

TEST (diff, calculateDiff)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/hello"));
	ksOld.append (Key ("user:/goodbye"));
	Key oldKeyWithValue = Key ("user:/greeting");
	oldKeyWithValue.setString ("world");
	ksOld.append (oldKeyWithValue);

	KeySet ksNew;
	ksNew.append (Key ("user:/hello"));
	Key newKeyWithValue = Key ("user:/greeting");
	newKeyWithValue.setString ("elektra");
	ksNew.append (newKeyWithValue);
	ksNew.append (Key ("user:/new"));

	Key parentKey;

	// Act
	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);

	// Assert
	EXPECT_TRUE (diff.getRemovedKeys ().lookup ("user:/goodbye").isValid ());
	EXPECT_TRUE (diff.getAddedKeys ().lookup ("user:/new").isValid ());
	EXPECT_TRUE (diff.getModifiedKeys ().lookup ("user:/greeting").isValid ());
}

TEST (diff, meta)
{
	// Arrange
	KeySet ksOld;
	Key oldKey ("user:/hello");
	ksOld.append (oldKey);
	oldKey.setMeta ("meta:/same", "123");
	oldKey.setMeta ("meta:/modified", "123");
	oldKey.setMeta ("meta:/removed", "123");

	KeySet ksNew;
	Key newKey ("user:/hello");
	ksNew.append (newKey);
	newKey.setMeta ("meta:/same", "123");
	newKey.setMeta ("meta:/modified", "456");
	newKey.setMeta ("meta:/added", "123");

	Key parentKey;

	// Act
	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);

	// Assert
	EXPECT_TRUE (diff.getAddedMetaKeys (newKey).lookup ("meta:/added").isValid ());
	EXPECT_TRUE (diff.getRemovedMetaKeys (newKey).lookup ("meta:/removed").isValid ());
	EXPECT_TRUE (diff.getModifiedMetaKeys (newKey).lookup ("meta:/modified").isValid ());
}

TEST (diff, cut)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/test1/a"));
	ksOld.append (Key ("user:/test1/b"));
	ksOld.append (Key ("user:/test2/a"));
	ksOld.append (Key ("user:/test2/b"));

	KeySet ksNew;
	ksNew.append (Key ("user:/test1/a"));
	ksNew.append (Key ("user:/test2/a"));

	Key parentKey;

	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);

	Key cutPoint = Key ("user:/test2");

	// Act
	ElektraDiff diff2 = diff.cut (cutPoint);

	// Assert
	EXPECT_TRUE (diff.getRemovedKeys ().lookup ("user:/test1/b").isValid ());
	EXPECT_FALSE (diff.getRemovedKeys ().lookup ("user:/test2/b").isValid ());
	EXPECT_TRUE (diff2.getRemovedKeys ().lookup ("user:/test2/b").isValid ());
	EXPECT_FALSE (diff2.getRemovedKeys ().lookup ("user:/test1/b").isValid ());
}

TEST (diff, removeOther)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/test1/a"));
	ksOld.append (Key ("user:/test1/b"));
	ksOld.append (Key ("user:/test2/a"));
	ksOld.append (Key ("user:/test2/b"));

	KeySet ksNew;
	ksNew.append (Key ("user:/test1/a"));
	ksNew.append (Key ("user:/test2/a"));

	Key parentKey;

	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);

	Key cutPoint = Key ("user:/test2");

	// Act
	diff.removeOther (cutPoint);

	// Assert
	EXPECT_TRUE (diff.getRemovedKeys ().lookup ("user:/test2/b").isValid ());
	EXPECT_FALSE (diff.getRemovedKeys ().lookup ("user:/test1/b").isValid ());
}

TEST (diff, removeSameOrBelow)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/test1/a"));
	ksOld.append (Key ("user:/test1/b"));
	ksOld.append (Key ("user:/test2/a"));
	ksOld.append (Key ("user:/test2/b"));

	KeySet ksNew;
	ksNew.append (Key ("user:/test1/a"));
	ksNew.append (Key ("user:/test2/a"));

	Key parentKey;

	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);

	Key cutPoint = Key ("user:/test2");

	// Act
	diff.removeSameOrBelow (cutPoint);

	// Assert
	EXPECT_TRUE (diff.getRemovedKeys ().lookup ("user:/test1/b").isValid ());
	EXPECT_FALSE (diff.getRemovedKeys ().lookup ("user:/test2/b").isValid ());
}

TEST (diff, assign)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/hello"));
	ksOld.append (Key ("user:/goodbye"));

	KeySet ksNew;
	ksNew.append (Key ("user:/hello"));

	Key parentKey;

	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);

	// Act & Assert
	ElektraDiff diffAssign1;
	diffAssign1 = diff;
	EXPECT_EQ (diff.getReferenceCounter (), 2);

	ElektraDiff diffAssign2;
	diffAssign2 = diffAssign1;
	EXPECT_EQ (diff.getReferenceCounter (), 3);

	diffAssign1 = ElektraDiff ();
	EXPECT_EQ (diff.getReferenceCounter (), 2);
}

TEST (diff, isEmpty)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/hello"));
	ksOld.append (Key ("user:/goodbye"));

	KeySet ksNew;
	ksNew.append (Key ("user:/hello"));

	Key parentKey;

	ElektraDiff diffWithDifferences = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);
	ElektraDiff diffWithoutDifferences = ElektraDiff::calculateDiff (ksOld, ksOld, parentKey);

	// Act & Assert
	EXPECT_EQ (diffWithDifferences.isEmpty (), false);
	EXPECT_EQ (diffWithoutDifferences.isEmpty (), true);
}

TEST (diff, dup)
{
	// Arrange
	KeySet ksOld;
	ksOld.append (Key ("user:/test1/a"));
	ksOld.append (Key ("user:/test1/b"));
	ksOld.append (Key ("user:/test2/a"));
	ksOld.append (Key ("user:/test2/b"));

	KeySet ksNew;
	ksNew.append (Key ("user:/test1/a"));
	ksNew.append (Key ("user:/test2/a"));

	Key parentKey;

	ElektraDiff diff = ElektraDiff::calculateDiff (ksNew, ksOld, parentKey);

	// Act
	ElektraDiff second = diff.dup ();

	// Assert
	EXPECT_TRUE (diff.getDiff () != nullptr);
	EXPECT_TRUE (second.getDiff () != nullptr);

	EXPECT_EQ (diff.getReferenceCounter (), 1);
	EXPECT_EQ (second.getReferenceCounter (), 1);
}
